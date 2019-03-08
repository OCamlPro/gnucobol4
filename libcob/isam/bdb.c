/*
   Copyright (C) 2002-2012, 2014-2019 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "../../libcob.h"
#include "../coblocal.h"
#include "isam.h"

#ifdef WITH_DB

/*
 * jkl: My version of db.h claims to define u_int & friends if not
 *      defined by the system, but doesn't.  
 *      The defintions below suffice to make this module compile.
 */
#define u_int unsigned int 
#define u_long unsigned long 

#include <db.h>

static int indexed_open		(cob_file *, char *, const int, const int);
static int indexed_close	(cob_file *, const int);
static int indexed_start	(cob_file *, const int, cob_field *);
static int indexed_read		(cob_file *, cob_field *, const int);
static int indexed_read_next	(cob_file *, const int);
static int indexed_write	(cob_file *, const int);
static int indexed_delete	(cob_file *);
static int indexed_rewrite	(cob_file *, const int);

const struct cob_fileio_funcs indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete
};

//// 600: #ifdef WITH_BDB_OR_MDB
static char		*db_buff = NULL;
static const char	**db_data_dir = NULL;

//// 604: #ifdef WITH_DB
#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
//// 611: #endif

//// 626: #ifdef	WITH_DB

#define	cob_dbtsize_t		u_int32_t

static DB_ENV		*db_env = NULL;
static void		*record_lock_object = NULL;
static size_t		rlo_size = 0;
static unsigned int	db_lock_id = 0;

struct indexed_file {
	DB		**db;		/* Database handlers */
	DBC		**cursor;
	char		*filename;	/* Needed for record locks */
	unsigned char	*last_key;	/* The last key written */
	unsigned char	*temp_key;	/* Used for temporary storage */
	unsigned char	**last_readkey;	/* The last key read */
	unsigned int	*last_dupno;	/* The last number of duplicates read */
	int		*rewrite_sec_key;
	int		maxkeylen;
	int		primekeylen;
	unsigned char	*savekey;	/* Work area for saving key value */
	unsigned char	*suppkey;	/* Work area for saving key value */
	unsigned char	*saverec;	/* For saving copy of record */
	DBT		key;
	DBT		data;
	DB_LOCK		db_file_lock;
	DB_LOCK		db_record_lock;
	size_t		key_index;
	unsigned int	db_lock_id;
	int		write_cursor_open;
	int		record_locked;
	int		filenamelen;
};

#define DBT_SET(key_or_data,fld)			\
	key_or_data.data = fld->data;			\
	key_or_data.size = (cob_dbtsize_t) fld->size

/* Is given key data all SUPPRESS char,
   returns 1 if key has all SUPPRESS char */
static int
db_suppresskey (cob_file *f, int idx)
{
	unsigned char ch_sprs;
	int 	i,len;
	struct indexed_file	*p;

	if (!f->keys[idx].tf_suppress) {
		return 0;
	}
	ch_sprs = f->keys[idx].char_suppress & 0xFF;
	p = f->file;
	len = db_savekey(f, p->suppkey, f->record->data, idx);
	for (i = 0; i < len; i++) {
		if (p->suppkey[i] != ch_sprs)
			return 0;
	}
	return 1;
}

static void
db_setkey (cob_file *f, int idx)
{
	struct indexed_file	*p = f->file;
	int	len;

	memset (p->savekey, 0, p->maxkeylen);
	len = db_savekey (f, p->savekey, f->record->data, idx);

	p->key.data = p->savekey;			\
	p->key.size = len;
}

/* Compare key for given index 'keyarea' to 'record'.
   returns compare status */
static int
db_cmpkey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx, int partlen)
{
	int sts, part, totlen;
	size_t	cl;

	if (partlen <= 0) {
		partlen = db_keylen(f, idx);
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components && partlen > 0; part++) {
			cl = partlen > f->keys[idx].component[part]->size ? f->keys[idx].component[part]->size : partlen;
			sts = memcmp (keyarea + totlen,
					record + (f->keys[idx].component[part]->data - f->record->data),
					cl);
			if (sts != 0) {
				return sts;
			}
			totlen += f->keys[idx].component[part]->size;
			partlen -= f->keys[idx].component[part]->size;
		}
		return 0;
	}
	cl = partlen > f->keys[idx].field->size ? f->keys[idx].field->size : partlen;
	return memcmp (keyarea,
			record  + (f->keys[idx].field->data - f->record->data),
			cl);
}


/* Local functions */

void
isam_sync (cob_file *f)
{
	struct indexed_file	*p = f->file;
	size_t			i;

	if (!p) {
		return;
	}
	for (i = 0; i < f->nkeys; ++i) {
		if (p->db[i]) {
			DB_SYNC (p->db[i]);
		}
	}
}

/* INDEXED */

#if	0	/* RXWRXW - BDB msg */
static void
db_msgcall_set (DB_ENV *dbe, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s"), err);
	cob_stop_run (1);
}

static void
db_errcall_set (DB_ENV *dbe, const char *prefix, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s %s"), prefix, err);
	cob_stop_run (1);
}
#endif

static void
join_environment (void)
{
	cob_u32_t	flags;
	int		ret;

	if (cobsetptr->db_home == NULL) {
		return;
	}
	ret = db_env_create (&db_env, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env_create", ret, db_strerror (ret));
		cob_stop_run (1);
	}
#if	0	/* RXWRXW - BDB msg */
	db_env->set_errcall (db_env, db_errcall_set);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	db_env->set_msgcall (db_env, db_msgcall_set);
#endif
#else
	db_env->set_errfile (db_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	db_env->set_msgfile (db_env, stderr);
#endif
#endif
	db_env->set_cachesize (db_env, 0, 2*1024*1024, 0);
	db_env->set_alloc (db_env, cob_malloc, realloc, cob_free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = db_env->open (db_env, cobsetptr->db_home, flags, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env->open", ret, db_strerror (ret));
		db_env->close (db_env, 0);
		db_env = NULL;
		cob_stop_run (1);
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	db_env->get_data_dirs (db_env, &db_data_dir);
#endif
	db_env->lock_id (db_env, &db_lock_id);
}

static int
lock_record (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p = f->file;
	size_t			len;
	int			ret;
	DBT			dbt;

	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename,
		(size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key,
		(size_t)keylen);
	memset (&dbt, 0, sizeof (dbt));
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = db_env->lock_get (db_env, p->db_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &p->db_record_lock);
	if (!ret) {
		p->record_locked = 1;
	}
	return ret;
}

static int
test_record_lock (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p = f->file;
	size_t			len;
	int			ret;
	DBT			dbt;
	DB_LOCK			test_lock;

	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename,
		(size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key,
		(size_t)keylen);
	memset (&dbt, 0, sizeof (dbt));
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = db_env->lock_get (db_env, p->db_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &test_lock);
	if (!ret) {
		db_env->lock_put (db_env, &test_lock);
	}
	return ret;
}

static int
unlock_record (cob_file *f)
{
	struct indexed_file	*p = f->file;
	int			ret;

	if (p->record_locked == 0) {
		return 0;
	}
	ret = db_env->lock_put (db_env, &p->db_record_lock);
	p->record_locked = 0;
	return ret;
}

/* Get the next number in a set of duplicates */
static unsigned int
get_dupno (cob_file *f, const cob_u32_t i)
{
	struct indexed_file	*p = f->file;
	int			ret;
	cob_u32_t		dupno = 0;

	db_setkey(f, i);
	memcpy (p->temp_key, p->key.data, (size_t)p->maxkeylen);
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE);
	while (ret == 0 && memcmp (p->key.data, p->temp_key, (size_t)p->key.size) == 0) {
		memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
		ret = DB_SEQ (p->cursor[i], DB_NEXT);
	}
	p->cursor[i]->c_close (p->cursor[i]);
	p->cursor[i] = NULL;
	dupno = COB_DUPSWAP(dupno);
	return ++dupno;
}

/* read file with all alternate keys that don't allow duplicates
   to check if records exist already, returns 1 if true */
static int
check_alt_keys (cob_file* f, const int rewrite)
{
	struct indexed_file* p;
	size_t			i;

	p = f->file;
	for (i = 1; i < f->nkeys; ++i) {
		if (!f->keys[i].tf_duplicates) {
			int	ret;
			db_setkey(f, i);
			ret = DB_GET(p->db[i], 0);
			if (ret == 0) {
				if (rewrite) {
					if (db_cmpkey(f, p->data.data, f->record->data, 0, 0)) {
						return 1;
					}
				} else {
					return 1;
				}
			}
		}
	}
	return 0;
}

static int
indexed_write_internal (cob_file *f, const int rewrite, const int opt)
{
	struct indexed_file	*p = f->file;
	cob_u32_t		i, len;
	cob_u32_t		dupno;
	cob_u32_t		flags;
	int			close_cursor, ret;

	if (db_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	ret = COB_STATUS_00_SUCCESS;
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1;
		close_cursor = 1;
	}

	/* Check duplicate alternate keys */
	if (f->nkeys > 1 && !rewrite) {
		if (check_alt_keys (f, 0)) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_22_KEY_EXISTS;
		}
		db_setkey (f, 0);
	}

	/* Write data */
	if (p->cursor[0]->c_get (p->cursor[0], &p->key, &p->data, DB_SET) == 0) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0;
		}
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	p->cursor[0]->c_put (p->cursor[0], &p->key, &p->data, DB_KEYFIRST);

	/* Write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; ++i) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (db_suppresskey (f, i)) {
			continue;
		}
		db_setkey (f, i);
		if (f->keys[i].tf_duplicates) {
			flags =  0;
			dupno = get_dupno(f, i);
			if (dupno > 1) {
				ret = COB_STATUS_02_SUCCESS_DUPLICATE;
			}
			dupno = COB_DUPSWAP (dupno);
			len = db_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = len;
			memcpy (((char*)(p->data.data)) + p->data.size, &dupno, sizeof (unsigned int));
			p->data.size += sizeof (unsigned int);
		} else {
			len = db_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = len;
			flags = DB_NOOVERWRITE;
			dupno = 0;
		}
		db_setkey (f, i);

		if (DB_PUT (p->db[i], flags) != 0) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

	if (opt & COB_WRITE_LOCK) {
		if (db_env != NULL) {
			db_setkey (f, 0);
			if (lock_record (f, p->key.data, p->key.size)) {
				if (close_cursor) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
					p->write_cursor_open = 0;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
	}
	return ret;
}

static int
indexed_start_internal (cob_file *f, const int cond, cob_field *key,
			const int read_opts, const int test_lock)
{
	struct indexed_file	*p = f->file;
	int			len, fullkeylen, partlen;
	int			ret = 0;
	cob_u32_t		dupno = 0;
	int			key_index;

	/* Look up for the key */
	key_index = db_findkey (f, key, &fullkeylen, &partlen);
	if (key_index < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	p->key_index = (unsigned int)key_index;

	/* Search */
	db_setkey (f, p->key_index);
	p->key.size = partlen;		/* may be partial key */
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
	if (cond == COB_FI) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_FIRST);
	} else if (cond == COB_LA) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
	} else {
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
	}
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = db_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen);
		}
		break;
	case COB_LT:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		}
		break;
	case COB_LE:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else if (db_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		} else if (f->keys[p->key_index].tf_duplicates) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT_NODUP);
			if (ret != 0) {
				ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
			} else {
				ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
			}
		}
		break;
	case COB_GT:
		while (ret == 0 && db_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) == 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	case COB_FI:
		/* nothing */
		break;
	case COB_LA:
		/* nothing */
		break;
	}

	if (ret == 0 && p->key_index > 0) {
		/* Temporarily save alternate key */
		len = p->key.size;
		memcpy (p->temp_key, p->key.data, len);
		if (f->keys[p->key_index].tf_duplicates) {
			memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
			dupno = COB_DUPSWAP (dupno);
		}
		p->key.data = p->data.data;
		p->key.size = p->primekeylen;
		ret = DB_GET (p->db[0], 0);
	}

	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			ret = test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}

	if (ret == 0) {
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, p->primekeylen);
		} else {
			memcpy (p->last_readkey[p->key_index],
				    p->temp_key, db_keylen(f,p->key_index));
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_delete_internal (cob_file *f, const int rewrite)
{
	struct indexed_file	*p = f->file;
	int			i, len;
	DBT			prim_key;
	int			ret;
	cob_u32_t		flags;
	int			close_cursor;

	if (db_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1;
		close_cursor = 1;
	}
	if (db_env != NULL) {
		unlock_record (f);
	}
	/* Find the primary key */
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		db_setkey(f, 0);
	}
	ret = DB_SEQ (p->cursor[0], DB_SET);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0;
		}
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (db_env != NULL) {
		ret = test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
	prim_key = p->key;
	memcpy(p->saverec, p->data.data, p->data.size);		/* Save old record image */
	memcpy(p->temp_key,prim_key.data,prim_key.size);	/* Save primary key value */
	prim_key.data = p->temp_key;

	/* Delete the secondary keys */
	for (i = 1; i < f->nkeys; ++i) {
		len = db_savekey(f, p->suppkey, p->data.data, i);
		memset(p->savekey, 0, p->maxkeylen);
		len = db_savekey(f, p->savekey, p->saverec, i);
		p->key.data = p->savekey;
		p->key.size = (cob_dbtsize_t) len;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			p->rewrite_sec_key[i] = db_cmpkey(f, p->suppkey, f->record->data, i, 0);
			if (!p->rewrite_sec_key[i]) {
				continue;
			}
		}
		if (!f->keys[i].tf_duplicates) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;

			p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], flags);
			if (DB_SEQ (p->cursor[i], DB_SET_RANGE) == 0) {
				while (sec_key.size == p->key.size
				&& memcmp (p->key.data, sec_key.data, (size_t)sec_key.size) == 0) {
					if (memcmp (p->data.data, prim_key.data, (size_t)prim_key.size) == 0) {
						p->cursor[i]->c_del (p->cursor[i], 0);
					}
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
						break;
					}
				}
			}
			p->cursor[i]->c_close (p->cursor[i]);
			p->cursor[i] = NULL;
		}
	}

	/* Delete the record */
	p->cursor[0]->c_del (p->cursor[0], 0);

	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
	}
	return COB_STATUS_00_SUCCESS;
}

/* Check if a file exists in bdb data dirs */
static int
is_absolute (const char *filename)
{
#ifdef	_WIN32
	if (filename[0] == '/' || filename[0] == '\\') {
		return 1;
	} else {
		if (isalpha (filename[0]) && filename[1] == ':' &&
		  (filename[2] == '/' || filename[2] == '\\')) {
			return 1;
		} else {
			return 0;
		}
	}
#else
	if (filename[0] == '/') {
		return 1;
	} else {
		return 0;
	}
#endif
}


/* Delete file */
/*
 * todo: return proper error code
 */
static int
indexed_file_delete (cob_file *f, const char *filename)
{
//// 3777: #elif	defined(WITH_DB)
	size_t	i;

	for (i = 0; i < f->nkeys; ++i) {
		if (i == 0) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.%d",
				  filename, (int)i);
		}
		file_open_buff[COB_FILE_MAX] = 0;
		unlink (file_open_buff);
	}
	return COB_STATUS_00_SUCCESS;
}

/* OPEN INDEXED file */

static int
indexed_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */
	
	struct indexed_file	*p;
	size_t			i;
	size_t			j;
	size_t			maxsize;
	db_lockmode_t		lock_mode;
	int			handle_created;
	cob_u32_t		flags = 0;
	int			ret = 0;
	int			nonexistent;
#if	0	/* RXWRXW - Access check BDB Human */
	int			checkvalue;
#endif

	COB_UNUSED (sharing);
	if (cobsetptr->db_home != NULL
	 && db_env == NULL) {		/* Join BDB, on first OPEN of INDEXED file */
		join_environment ();
	}
	cob_chk_file_mapping ();

#if	0	/* RXWRXW - Access check BDB Human */
	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}
#endif

	nonexistent = 0;
	if (db_nofile (filename)) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	p = cob_malloc (sizeof (struct indexed_file));
	if (db_env != NULL) {
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE) ||
		    (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
		} else {
			lock_mode = DB_LOCK_READ;
		}
		p->key.size = (cob_dbtsize_t) strlen (filename);
		p->key.data = filename;
		ret = db_env->lock_get (db_env, db_lock_id, DB_LOCK_NOWAIT,
					&p->key, lock_mode, &p->db_file_lock);
		if (ret) {
			cob_free (p);
			if (ret == DB_LOCK_NOTGRANTED) {
				return COB_STATUS_61_FILE_SHARING;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	switch (mode) {
	case COB_OPEN_INPUT:
		flags |= DB_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		flags |= DB_CREATE;
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
		flags |= DB_CREATE;
		break;
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = p->primekeylen = db_keylen(f, 0);
	for (i = 1; i < f->nkeys; ++i) {
		j = db_keylen(f, i);
		if( j > maxsize)
			maxsize = j;
	}
	p->maxkeylen = maxsize;

	for (i = 0; i < f->nkeys; ++i) {
		/* File name */
		runtime_buffer[COB_FILE_MAX] = 0;
		if (i == 0) {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s.%d",
				 filename, (int)i);
		}
#if	0	/* RXWRXW - Access check BDB Human */
		ret = access (runtime_buffer, checkvalue);
		if (ret != 0) {
			if (errno == ENOENT &&
			    (mode == COB_OPEN_OUTPUT || f->flag_optional == 1)) {
				ret = 0;
				/* Check here if the directory exists ? */
#if	0	/* RXWRXW - Check dir */
				if (!directory exists) {
					ret = ENOENT;
				} else {
					ret = 0;
				}
#endif
			} else {
				ret = errno;
			}
			if (ret != 0) {
				switch (ret) {
				case ENOENT:
					ret = COB_STATUS_35_NOT_EXISTS;
					break;
				case EACCES:
					ret = COB_STATUS_37_PERMISSION_DENIED;
					break;
				default:
					ret = COB_STATUS_30_PERMANENT_ERROR;
					break;
				}
				/* FIXME: BDB cleanup is missing here */
				return ret;
			}
		}
#endif

		/* btree info */
		ret = db_create (&p->db[i], db_env, 0);
		if (!ret) {
			handle_created = 1;
			if (mode == COB_OPEN_OUTPUT) {
				if (db_env) {
					db_env->dbremove (db_env, NULL, runtime_buffer, NULL, 0);
				} else {
					/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
					   on OPEN-OUTPUT results with MinGW & BDB 6 in
					   BDB1565 DB->pget: method not permitted before handle's open method
					*/
					p->db[i]->remove (p->db[i], runtime_buffer, NULL, 0);
					ret = db_create (&p->db[i], db_env, 0);
				}
			}
			if (!ret) {
				if (f->keys[i].tf_duplicates) {
					p->db[i]->set_flags (p->db[i], DB_DUP);
				}
			}
		} else {
			handle_created = 0;
		}
		/* Open db */
		if (!ret) {
			/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
			   on OPEN-OUTPUT results with MinGW & BDB 6 in
			   BDB0588 At least one secondary cursor must be specified to DB->join
			*/
			ret = p->db[i]->open (p->db[i], NULL, runtime_buffer, NULL,
						DB_BTREE, flags, COB_FILE_MODE);
		}
		if (ret) {
			for (j = 0; j < i; ++j) {
				DB_CLOSE (p->db[j]);
			}
			if (handle_created) {
				DB_CLOSE (p->db[i]);
			}
			cob_free (p->db);
			cob_free (p->last_readkey);
			cob_free (p->last_dupno);
			cob_free (p->rewrite_sec_key);
			cob_free (p->cursor);
			if (db_env != NULL) {
				db_env->lock_put (db_env, &p->db_file_lock);
			}
			cob_free (p);
			switch (ret) {
			case DB_LOCK_NOTGRANTED:
				return COB_STATUS_61_FILE_SHARING;
			case ENOENT:
				if (mode == COB_OPEN_EXTEND ||
				    mode == COB_OPEN_OUTPUT) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
				if (f->flag_optional) {
					if (mode == COB_OPEN_I_O) {
						return COB_STATUS_30_PERMANENT_ERROR;
					}
					f->open_mode = mode;
					f->flag_nonexistent = 1;
					f->flag_end_of_file = 1;
					f->flag_begin_of_file = 1;
					/* RXWRXW - Check directory exists? */
					return COB_STATUS_05_SUCCESS_OPTIONAL;
				}
				return COB_STATUS_35_NOT_EXISTS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}

		}

		p->last_readkey[i] = cob_malloc (maxsize);
		p->last_readkey[f->nkeys + i] = cob_malloc (maxsize);
	}

	p->temp_key = cob_malloc (maxsize + sizeof (unsigned long));
	p->savekey  = cob_malloc (maxsize + sizeof (unsigned long));
	p->suppkey  = cob_malloc (maxsize + sizeof (unsigned long));
	p->saverec  = cob_malloc (f->record_max + sizeof (unsigned long));
	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	memset ((void *)&p->key, 0, sizeof (DBT));
	memset ((void *)&p->data, 0, sizeof (DBT));
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy (p->filename, filename);
	p->write_cursor_open = 0;
	p->record_locked = 0;
	if (db_env != NULL) {
		db_env->lock_id (db_env, &p->db_lock_id);
	}

	db_setkey(f, 0);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	if (!ret) {
		memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		if (p->data.data != NULL
		 && p->data.size > 0
		 && p->data.size > f->record_max) {
			return COB_STATUS_39_CONFLICT_ATTRIBUTE;
		}
	} else {
		p->data.data = NULL;
	}

	f->open_mode = mode;
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return COB_STATUS_00_SUCCESS;
}

/* Close the INDEXED file */

static int
indexed_close (cob_file *f, const int opt)
{
//// 4535: #elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			i;

	COB_UNUSED (opt);

	p = f->file;
	/* Close DB's */
	for (i = 0; i < (int)f->nkeys; ++i) {
		if (p->cursor[i]) {
			p->cursor[i]->c_close (p->cursor[i]);
		}
	}
	for (i = (int)f->nkeys - 1; i >= 0; --i) {
		if (p->db[i]) {
			DB_CLOSE (p->db[i]);
		}
		cob_free (p->last_readkey[i]);
		cob_free (p->last_readkey[f->nkeys + i]);
	}

	if (p->last_key) {
		cob_free (p->last_key);
	}
	cob_free (p->temp_key);
	cob_free (p->savekey);
	cob_free (p->suppkey);
	cob_free (p->saverec);
	cob_free (p->db);
	cob_free (p->last_readkey);
	cob_free (p->last_dupno);
	cob_free (p->rewrite_sec_key);
	cob_free (p->filename);
	cob_free (p->cursor);
	if (db_env != NULL) {
		unlock_record (f);
		db_env->lock_put (db_env, &p->db_file_lock);
		db_env->lock_id_free (db_env, p->db_lock_id);
	}
	cob_free (p);

	return COB_STATUS_00_SUCCESS;
}


/* START INDEXED file with positioning */

static int
indexed_start (cob_file *f, const int cond, cob_field *key)
{
//// 4704: #elif	defined(WITH_BDB_OR_MDB)

	return indexed_start_internal (f, cond, key, 0, 0);
}

/* Random READ of the INDEXED file  */

static int
indexed_read (cob_file *f, cob_field *key, const int read_opts)
{
//// 4798: #elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			db_opts;
	int			test_lock = 0;

	p = f->file;
	db_opts = read_opts;
	if (db_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			db_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
			   !(db_opts & COB_READ_NO_LOCK)) {
			db_opts |= COB_READ_LOCK;
		}
		unlock_record (f);
		test_lock = 1;
	} else {
		db_opts &= ~COB_READ_LOCK;
	}

	ret = indexed_start_internal (f, COB_EQ, key, db_opts, test_lock);
	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	return ret;
}

/* Sequential READ of the INDEXED file */

static int
indexed_read_next (cob_file *f, const int read_opts)
{
//// 5119: #elif	defined(WITH_DB)

	struct indexed_file	*p = f->file;
	int			ret;
	int			read_nextprev;
	cob_u32_t		nextprev;
	int			file_changed;
	int			db_opts;
	cob_u32_t		dupno = 0;

	nextprev = DB_NEXT;
	file_changed = 0;

	db_opts = read_opts;
	if (db_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			db_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
			   !(db_opts & COB_READ_NO_LOCK)) {
			db_opts |= COB_READ_LOCK;
		}
		unlock_record (f);
	} else {
		db_opts &= ~COB_READ_LOCK;
	}

	if (unlikely (db_opts & COB_READ_PREVIOUS)) {
		if (f->flag_end_of_file) {
			nextprev = DB_LAST;
		} else {
			nextprev = DB_PREV;
		}
	} else if (f->flag_begin_of_file) {
		nextprev = DB_FIRST;
	}
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);

	if (f->flag_first_read) {
		/* Data is read in indexed_open or indexed_start */
		if (p->data.data == NULL
		 || (f->flag_first_read == 2 && nextprev == DB_PREV)) {
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			p->cursor[p->key_index] = NULL;
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
			}
			return COB_STATUS_10_END_OF_FILE;
		}
		/* Check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) db_keylen(f,p->key_index);
		p->key.data = p->last_readkey[p->key_index];
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET);
		if (!ret && p->key_index > 0) {
			if (f->keys[p->key_index].tf_duplicates) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
				dupno = COB_DUPSWAP (dupno);
				while (ret == 0
				   && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
				   && dupno < p->last_dupno[p->key_index]) {
					ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
					memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
					dupno = COB_DUPSWAP (dupno);
				}
				if (ret == 0
				 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
				 && dupno == p->last_dupno[p->key_index]) {
					ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, p->primekeylen);
				} else {
					ret = 1;
				}
			} else {
				ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, p->primekeylen);
			}
			if (!ret) {
				p->key.size = (cob_dbtsize_t) p->primekeylen;
				p->key.data = p->last_readkey[p->key_index + f->nkeys];
				ret = DB_GET (p->db[0], 0);
			}
		}
		file_changed = ret;
		if (db_env != NULL && !file_changed) {
			if (!(db_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (db_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) db_keylen(f,p->key_index);
			p->key.data = p->last_readkey[p->key_index];
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].tf_duplicates) {
						memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
						dupno = COB_DUPSWAP (dupno);
						while (ret == 0
						 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
						 && dupno < p->last_dupno[p->key_index]) {
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
							memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
							dupno = COB_DUPSWAP (dupno);
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
								p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
								p->cursor[p->key_index] = NULL;
								if (p->key_index != 0) {
									p->cursor[0]->c_close (p->cursor[0]);
									p->cursor[0] = NULL;
								}
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
								dupno == p->last_dupno[p->key_index]) {
								read_nextprev = 1;
							} else {
								if (nextprev == DB_PREV) {
									read_nextprev = 1;
								} else {
									read_nextprev = 0;
								}
							}
						}
					} else {
						read_nextprev = 1;
					}
				} else {
					if (nextprev == DB_PREV) {
						read_nextprev = 1;
					} else {
						read_nextprev = 0;
					}
				}
			}
		}
		if (read_nextprev) {
			ret = DB_SEQ (p->cursor[p->key_index], nextprev);
			if (ret != 0) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* Temporarily save alternate key */
			memcpy (p->temp_key, p->key.data, (size_t)p->key.size);
			if (f->keys[p->key_index].tf_duplicates) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
				dupno = COB_DUPSWAP (dupno);
			}
			p->key.data = p->data.data;
			p->key.size = p->primekeylen;
			ret =  DB_GET (p->db[0], 0);
			if (ret != 0) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
		if (db_env != NULL) {
			if (!(db_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (db_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		} else {
			memcpy (p->last_readkey[p->key_index], p->temp_key,
				    db_keylen(f,p->key_index));
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	return ret;
}

/* WRITE to the INDEXED file  */

static int
indexed_write (cob_file *f, const int opt)
{
//// 5697: #elif	defined (WITH_BDB_OR_MDB)

	struct indexed_file	*p;
//// 5702: #endif

	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	p = f->file;
//// 5708: #if	defined (WITH_DB)
	if (db_env != NULL) {
		unlock_record (f);
	}

	/* Check record key */
	db_setkey (f, 0);
	if (!p->last_key) {
		p->last_key = cob_malloc ((size_t)p->maxkeylen);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
		   memcmp (p->last_key, p->key.data, (size_t)p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	memcpy (p->last_key, p->key.data, (size_t)p->key.size);
	return indexed_write_internal (f, 0, opt);
}

/* DELETE record from the INDEXED file  */

static int
indexed_delete (cob_file *f)
{
//// 5800: #elif	defined(WITH_BDB_OR_MDB)

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	return indexed_delete_internal (f, 0);
}

/* REWRITE record to the INDEXED file  */

static int
indexed_rewrite (cob_file *f, const int opt)
{
//// 5939: #elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	cob_u32_t		flags;

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	p = f->file;
	if (db_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1;
	if (db_env != NULL) {
		unlock_record (f);
	}

	/* Check duplicate alternate keys */
	if (check_alt_keys (f, 1)) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* Delete the current record */
	ret = indexed_delete_internal (f, 1);

	if (ret != COB_STATUS_00_SUCCESS) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
		return ret;
	}

	/* Write data */
	db_setkey(f, 0);
	ret = indexed_write_internal (f, 1, opt);

	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	p->write_cursor_open = 0;
	return ret;
}

/*
 * Public "helper" functions. 
 * These are called from the API fuctions in fileio.c in strategic places to 
 * execute implementation-specific code.  
 */

void
cob_exit_fileio_aide (void)
{
	if (db_env) {
		db_env->lock_id_free (db_env, db_lock_id);
		db_env->close (db_env, 0);
		db_env = NULL;
	}
	if (record_lock_object) {
		cob_free (record_lock_object);
		record_lock_object = NULL;
	}
	if (db_buff) {
		cob_free (db_buff);
		db_buff = NULL;
	}
}

void
cob_init_fileio_epilogue (cob_global *lptr, cob_settings *sptr)
{
	db_data_dir = NULL;
	db_buff = cob_malloc ((size_t)COB_SMALL_BUFF);
	db_env = NULL;
	rlo_size = 1024;
	record_lock_object = cob_malloc (rlo_size);
}

void
cob_file_unlock_internal (cob_file *f) {
	if (f->file) {
		if (db_env != NULL) {
			struct indexed_file	*p = f->file;
			unlock_record (f);
			db_env->lock_put (db_env, &p->db_file_lock);
		}
	}
}



#endif

