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
#include "xisam.h"

#ifdef WITH_XISAM

/*
 * Implementation module for ISAM with cisam, disam, or vbisam. 
 * No support for Berkeley DB or LMDB in this file.
 */

/* Return total length of the key */
static int
indexed_keylen (struct indexfile *fh, int idx)
{
	int totlen, part;
	totlen = 0;
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Save key for given index into 'savekey'
   Return total length of the key */
static int
indexed_savekey (struct indexfile *fh, unsigned char *data, int idx)
{
	int totlen, part;
	totlen = 0;
	if (data == NULL) {
		data = (unsigned char*)fh->recwrk;
	}
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		memcpy (fh->savekey + totlen,
			data  + fh->key[idx].k_part[part].kp_start,
			fh->key[idx].k_part[part].kp_leng);
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Copy key for given index from 'savekey' back to recwrk
   Return total length of the key */
static int
indexed_restorekey (struct indexfile *fh, unsigned char *data, int idx)
{
	int totlen, part;
	totlen = 0;
	if (data == NULL) {
		data = (unsigned char*)fh->recwrk;
	}
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		memcpy (data  + fh->key[idx].k_part[part].kp_start,
			fh->savekey + totlen,
			fh->key[idx].k_part[part].kp_leng);
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Compare key for given index 'savekey' to recwrk
   Return compare status */
static int
indexed_cmpkey (struct indexfile *fh, unsigned char *data, int idx, int partlen)
{
	int sts, part, totlen,cl;
	totlen = sts = 0;
	if (partlen <= 0) {
		partlen = indexed_keylen(fh, idx);
	}
	for (part = 0; part < fh->key[idx].k_nparts && partlen > 0; part++) {
		cl = partlen > fh->key[idx].k_part[part].kp_leng ? fh->key[idx].k_part[part].kp_leng : partlen;
		sts = memcmp(	data  + fh->key[idx].k_part[part].kp_start,
				fh->savekey + totlen,
				cl);
		if (sts != 0) {
			return sts;
		}
		totlen += fh->key[idx].k_part[part].kp_leng;
		partlen -= fh->key[idx].k_part[part].kp_leng;
	}
	return sts;
}

/* Build 'keydesc' from 'cob_file_key'
   Return total length of the key */
static int
indexed_keydesc (cob_file *f, struct keydesc *kd, cob_file_key *key)
{
	int	keylen,part;
	memset (kd,0,sizeof (struct keydesc));
	kd->k_flags = key->tf_duplicates ? ISDUPS : ISNODUPS;
	if (key->count_components <= 1) {
		kd->k_nparts = 1;		/* Single field key */
		kd->k_start = key->offset;
		kd->k_leng = key->field->size;
		kd->k_type = CHARTYPE;
#ifdef NULLKEY
		if (key->tf_suppress) {
			kd->k_flags |= NULLKEY;
			kd->k_type = CHARTYPE | (key->char_suppress << 8);
		}
#endif
		keylen = kd->k_leng;
	} else {
		keylen = 0;
		for (part=0; part < key->count_components && part < COB_MAX_KEYCOMP; part++) {
			kd->k_part[part].kp_start = key->component[part]->data - f->record->data;
			kd->k_part[part].kp_leng = key->component[part]->size;
			keylen += kd->k_part[part].kp_leng;
			kd->k_part[part].kp_type = CHARTYPE;
#ifdef NULLKEY
			if (key->tf_suppress) {
				kd->k_flags |= NULLKEY;
				kd->k_part[part].kp_type = CHARTYPE | (key->char_suppress << 8);
			}
#endif
		}
		kd->k_nparts = part;
	}
#if defined(WITH_DISAM) || 1
	kd->k_len = keylen;		/* Total length of this key */
#endif
	return keylen;
}

/* Compare 'keydesc' to 'keydesc'
   Return 0 if equal, else 1 */
static int
indexed_keycmp (struct keydesc *k1, struct keydesc *k2)
{
	int	part;
	if (k1->k_flags != k2->k_flags) {
		return 1;
	}
	if (k1->k_nparts != k2->k_nparts) {
		return 1;
	}
	for (part=0; part < k1->k_nparts; part++) {
		if (k1->k_part[part].kp_start != k2->k_part[part].kp_start) {
			return 1;
		}
		if (k1->k_part[part].kp_leng != k2->k_part[part].kp_leng) {
			return 1;
		}
		if (k1->k_part[part].kp_type != k2->k_part[part].kp_type) {
			return 1;
		}
	}
	return 0;
}

/* Return index number for given key */
static int
indexed_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k,part;
	*fullkeylen = *partlen = 0;
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		&&  f->keys[k].count_components <= 1
		&&  f->keys[k].field->data == kf->data) {
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ((f->keys[k].field
			&&  f->keys[k].field->data == kf->data
			&&  f->keys[k].field->size == kf->size)
			||  (f->keys[k].component[0]->data == kf->data)) {
				for (part=0; part < f->keys[k].count_components; part++) {
					*fullkeylen += f->keys[k].component[part]->size;
				}
				if (f->keys[k].field && f->keys[k].field->data == kf->data) {
					*partlen = kf->size;
				} else {
					*partlen = *fullkeylen;
				}
				return k;
			}
		}
	}
	return -1;
}

/* Local variables */

static int dummy_delete		(cob_file *);
static int dummy_rnxt_rewrite	(cob_file *, const int);
static int dummy_read		(cob_file *, cob_field *, const int);
static int dummy_start		(cob_file *, const int, cob_field *);

/* Local functions */

static int indexed_open		(cob_file *, char *, const int, const int);
static int indexed_close	(cob_file *, const int);
static int indexed_start	(cob_file *, const int, cob_field *);
static int indexed_read		(cob_file *, cob_field *, const int);
static int indexed_read_next	(cob_file *, const int);
static int indexed_write	(cob_file *, const int);
static int indexed_delete	(cob_file *);
static int indexed_rewrite	(cob_file *, const int);
////static int indexed_file_delete  (cob_file *, const char *filename);

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

static int
dummy_delete (cob_file *f)
{
	COB_UNUSED (f);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_rnxt_rewrite (cob_file *f, const int opt)
{
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_read (cob_file *f, cob_field *key, const int read_opts)
{
	COB_UNUSED (f);
	COB_UNUSED (key);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_start (cob_file *f, const int cond, cob_field *key)
{
	COB_UNUSED (f);
	COB_UNUSED (cond);
	COB_UNUSED (key);

	return COB_STATUS_91_NOT_AVAILABLE;
}


void
isam_sync(cob_file *f) {
	struct indexfile	*fh;

	fh = f->file;
	if (fh) {
		isflush (fh->isfd);
	}
}

/* Translate ISAM status to COBOL status */
static int
fisretsts (const int default_status)
{
	switch (ISERRNO) {
	case 0:
		return COB_STATUS_00_SUCCESS;
	case ENOREC:
		return COB_STATUS_23_KEY_NOT_EXISTS;
	case EENDFILE:
		if (default_status != COB_STATUS_23_KEY_NOT_EXISTS) {
			return COB_STATUS_10_END_OF_FILE;
		}
		break;
	case EDUPL:
	case EKEXISTS:
		return COB_STATUS_22_KEY_EXISTS;
	case EPERM:
	case EACCES:
	case EISDIR:
		return COB_STATUS_37_PERMISSION_DENIED;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	case EBADFILE:
		return COB_STATUS_30_PERMANENT_ERROR;
	case ELOCKED:
		return COB_STATUS_51_RECORD_LOCKED;
	case EFLOCKED:
		return COB_STATUS_61_FILE_SHARING;
	case ENOCURR:
		if (default_status != COB_STATUS_10_END_OF_FILE) {
			return COB_STATUS_21_KEY_INVALID;
		}
		break;
	default:
		break;
	}
	return default_status;
}

/* Free memory for indexfile packet */

static void
freefh (struct indexfile *fh)
{
	if (fh == NULL) {
		return;
	}
	if (fh->filename) {
		cob_free ((void *)fh->filename);
	}
	if (fh->savekey) {
		cob_free ((void *)fh->savekey);
	}
	if (fh->recwrk) {
		cob_free ((void *)fh->recwrk);
	}
	cob_free ((void *)fh);
}

/* Restore ISAM file positioning */
static void
restorefileposition (cob_file *f)
{
	struct indexfile	*fh;
	struct keydesc		k0;

	fh = f->file;
	memset ((void *)&k0, 0, sizeof (k0));
	if (fh->saverecnum >= 0) {
		/* Switch back to index */
		ISRECNUM = fh->saverecnum;
		/* Switch to recnum mode */
		isstart (fh->isfd, &k0, 0, (void *)fh->recwrk, ISEQUAL);
		/* Read by record number */
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		isstart (fh->isfd, &fh->key[fh->curkey], 0,
			 (void *)fh->recwrk, ISEQUAL);
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		while (ISRECNUM != fh->saverecnum) {
			/* Read back into position */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				break;
			}
		}
		if (ISRECNUM == fh->saverecnum) {
			if (fh->readdir == ISNEXT) {
				/* Back off by one so next read gets this */
				isread (fh->isfd, (void *)fh->recwrk, ISPREV);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISNEXT);
			}
		}
	} else if (fh->readdone && fh->curkey == 0) {
		indexed_restorekey(fh, NULL, 0);
		isstart (fh->isfd, &fh->key[fh->curkey], 0,
			 (void *)fh->recwrk, ISGTEQ);
	}
}

/* Save ISAM file positioning information for later 'restorefileposition' */

static void
savefileposition (cob_file *f)
{
	struct indexfile	*fh;

	fh = f->file;
	if (fh->curkey >= 0 && fh->readdir != -1) {
		/* Switch back to index */
		if (fh->wrkhasrec != fh->readdir) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			/* Read next record in file */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				fh->saverecnum = -1;
				fh->saveerrno = ISERRNO;
				if (fh->saveerrno == EENDFILE ||
				    fh->saveerrno == ENOREC)  {
					fh->eofpending = fh->readdir;
				}
			} else {
				fh->saverecnum = ISRECNUM;
				fh->saveerrno = 0;
			}
			/* Restore saved record data */
			memcpy (fh->recwrk, f->record->data, f->record_max);
		}
	} else {
		fh->saverecnum = -1;
	}
}




/* START INDEXED file with positioning */

void
cob_file_unlock_internal (cob_file *f) {
	struct indexfile	*fh = f->file;
	isrelease (fh->isfd);
}


/* Global functions */

static int
indexed_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */
//// #line 3842 "../orig/fileio.c"
	struct indexfile	*fh;
	size_t			k;
	int			ret,len;
	int			omode;
	int			lmode;
	int			vmode;
	int			dobld;
	int			isfd;
	int			checkvalue;
	struct keydesc		kd;
	struct dictinfo		di;		/* Defined in (c|d|vb)isam.h */

	COB_UNUSED (sharing);

	cob_chk_file_mapping ();

	if (mode == COB_OPEN_INPUT) {
		checkvalue = 4;
	} else {
		checkvalue = 4 | 2;
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT &&
		      (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT &&
		      (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	ret = COB_STATUS_00_SUCCESS;
	omode = 0;
	lmode = 0;
	vmode = 0;
	dobld = 0;
	isfd = -1;






	if (!f->lock_mode) {
		if (mode != COB_OPEN_INPUT) {
			lmode = ISEXCLLOCK;
		} else {
			lmode = ISMANULOCK;
		}
	} else if ((f->lock_mode & COB_FILE_EXCLUSIVE)) {
		lmode = ISEXCLLOCK;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && mode != COB_OPEN_INPUT) {
		lmode = ISAUTOLOCK;
	} else {
		lmode = ISMANULOCK;
	}
	switch (mode) {
	case COB_OPEN_INPUT:
		omode = ISINPUT;
		break;
	case COB_OPEN_OUTPUT:
		lmode = ISEXCLLOCK;
		omode = ISOUTPUT;
		iserrno = 0;
		isfd = isopen ((void *)filename, ISINPUT | ISEXCLLOCK | vmode);
		if (iserrno == EFLOCKED) {
			return COB_STATUS_61_FILE_SHARING;
		} else {
			if (isfd >= 0) {
				isfullclose (isfd);
			}
			iserase ((void *)filename);
			iserrno = 0;
		}
		dobld = 1;
		break;
	case COB_OPEN_I_O:
		omode = ISINOUT;
		break;
	case COB_OPEN_EXTEND:
		lmode = ISEXCLLOCK;
		omode = ISINOUT;
		break;
	}
	fh = cob_malloc (sizeof (struct indexfile) +
			 ((sizeof (struct keydesc)) * (f->nkeys + 1)));
	/* Copy index information */
	for (k = 0; k < f->nkeys; ++k) {
		len = indexed_keydesc(f, &fh->key[k], &f->keys[k]);
		if (fh->lenkey < len) {
			fh->lenkey = len;
		}
	}
	iserrno = 0;
	fh->lmode = 0;
	if (dobld) {
dobuild:
		isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
				vmode | ISINOUT | ISEXCLLOCK);



		if (iserrno == EEXIST

		 && omode == ISOUTPUT

		 && isfd < 0) {
			/* Erase file and redo the 'isbuild' */
			iserase ((void *)filename);
			isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
					vmode | ISINOUT | ISEXCLLOCK);



		}
	} else {
		if (lmode == ISAUTOLOCK) {
			fh->lmode = ISLOCK;
			lmode = ISMANULOCK;
		}
		isfd = isopen ((void *)filename, omode | lmode | vmode);
		if (isfd < 0) {
			if (f->flag_optional) {
				if (mode == COB_OPEN_EXTEND ||
				    mode == COB_OPEN_I_O) {
					dobld = 1;
					ret = COB_STATUS_05_SUCCESS_OPTIONAL;
					goto dobuild;
				}
				freefh (fh);
				f->open_mode = mode;
				f->flag_end_of_file = 1;
				f->flag_begin_of_file = 1;
				if (f->flag_nonexistent) {
					return COB_STATUS_00_SUCCESS;
				}
				f->flag_nonexistent = 1;
				return COB_STATUS_05_SUCCESS_OPTIONAL;
			}
		} else {
			memset(&di, 0, sizeof (di));
			isindexinfo (isfd, (void *)&di, 0);
			/* Mask off ISVARLEN */
			fh->nkeys = di.di_nkeys & 0x7F;
			if (fh->nkeys != f->nkeys) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
			if (f->record_max != di.di_recsize) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
			for (k = 0; k < fh->nkeys && !ret; ++k) {
				memset (&fh->key[k], 0, sizeof (struct keydesc));
				isindexinfo (isfd, &fh->key[k], (int)(k+1));
				if (fh->lenkey < indexed_keylen(fh, k)) {
					fh->lenkey = indexed_keylen(fh, k);
				}
				/* Verify that COBOL keys match real ISAM keys */
				len = indexed_keydesc(f, &kd, &f->keys[k]);
				if (fh->lenkey < len) {
					fh->lenkey = len;
				}
				if(indexed_keycmp(&kd, &fh->key[k]) != 0) {
					ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					break;
				}
			}
		}
	}
	if (isfd < 0) {
		ret = fisretsts (COB_STATUS_30_PERMANENT_ERROR);
		freefh (fh);
		return ret;
	}
	if (ret > 9) {
		isfullclose (isfd);
		freefh (fh);
		return ret;
	}
	if (dobld) {
		for (k = 1; k < f->nkeys; ++k) {
			iserrno = 0;
			if (isaddindex (isfd, &fh->key[k])) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
		}
		if (ret > 9) {
			isfullclose (isfd);
			iserase ((void *)filename);
			freefh (fh);
			return ret;
		}
	}
	f->file = fh;
	f->open_mode = mode;
	fh->isfd = isfd;
	fh->filename = cob_strdup (filename);
	fh->savekey = cob_malloc ((size_t)(fh->lenkey + 1));
	fh->recwrk = cob_malloc ((size_t)(f->record_max + 1));
	/* Active index is unknown at this time */
	fh->curkey = -1;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	return ret;
//// #line 4507 "../orig/fileio.c"
}

/* Close the INDEXED file */

static int
indexed_close (cob_file *f, const int opt)
{






	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (fh == NULL) {
		return COB_STATUS_00_SUCCESS;
	}
	if (fh->isfd >= 0) {
		isfullclose (fh->isfd);
	}
	freefh (fh);
	f->file = NULL;
	return COB_STATUS_00_SUCCESS;
//// #line 4602 "../orig/fileio.c"
}


/* START INDEXED file with positioning */

static int
indexed_start (cob_file *f, const int cond, cob_field *key)
{






	struct indexfile	*fh;
	int			k;
	int			mode;
	int			klen,fullkeylen,partlen;
	int			savecond;

	fh = f->file;
	f->flag_read_done = 0;
	f->flag_first_read = 0;
	fh->readdone = 0;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = indexed_findkey(f, key, &fullkeylen, &partlen);
	if(k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	/* Use size of data field; This may indicate a partial key */
	klen = partlen;
	if (klen < 1 || klen > fullkeylen) {
		/* Max key length for this index */
		klen = fullkeylen;
	}
	mode = ISGTEQ;
	fh->startiscur = 1;
	savecond = cond;
	switch (cond) {
	case COB_EQ:
		mode = ISEQUAL;
		fh->readdir = ISNEXT;
		break;
	case COB_GE:
		mode = ISGTEQ;
		fh->readdir = ISNEXT;
		break;
	case COB_GT:
		mode = ISGREAT;
		fh->readdir = ISNEXT;
		break;
	case COB_LE:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_LT:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_FI:
		mode = ISFIRST;
		fh->readdir = ISNEXT;
		break;
	case COB_LA:
		mode = ISLAST;
		fh->readdir = ISPREV;
		break;
	default:
		return COB_STATUS_21_KEY_INVALID;
	}
	if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, mode)) {
		if (cond == COB_LE || cond == COB_LT) {
			if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, ISLAST)) {
				fh->curkey = -1;
				fh->startcond = -1;
				fh->readdir = -1;
				fh->startiscur = 0;
				return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
			} else {
				savecond = COB_LA;
			}
		} else {
			fh->curkey = -1;
			fh->startcond = -1;
			fh->readdir = -1;
			fh->startiscur = 0;
			return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
		}
	}
	fh->startcond = savecond;
	indexed_savekey(fh, f->record->data, k);
	fh->curkey = k;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 1;
	return COB_STATUS_00_SUCCESS;
//// #line 4715 "../orig/fileio.c"
}

/* Random READ of the INDEXED file  */

static int
indexed_read (cob_file *f, cob_field *key, const int read_opts)
{






	struct indexfile	*fh;
	int			k, fullkeylen, partlen;
	int			ret;
	int			lmode;

	fh = f->file;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = indexed_findkey(f, key, &fullkeylen, &partlen);
	if(k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (fh->curkey != (int)k) {
		/* Switch to this index */
		isstart (fh->isfd, &fh->key[k], 0,
			 (void *)f->record->data, ISEQUAL);
		fh->curkey = k;
		fh->wrkhasrec = 0;
	}
	fh->startcond = -1;
	lmode = 0;
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
		if (f->open_mode != COB_OPEN_INPUT) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				lmode = ISLOCK;
			}
		}
	}





	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	iserrno = 0;
	fh->readdir = -1;
	ret = COB_STATUS_00_SUCCESS;
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | lmode)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, fh->lenkey);
		fh->recnum = 0;
		fh->readdone = 0;
		return ret;
	}
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	indexed_savekey(fh, f->record->data, 0);
	fh->recnum = isrecnum;





	return 0;
//// #line 4865 "../orig/fileio.c"
}

/* Sequential READ of the INDEXED file */

static int
indexed_read_next (cob_file *f, const int read_opts)
{






	struct indexfile	*fh;
	int			ret;
	int			lmode;
	int			domoveback;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	lmode = 0;

	if (fh->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0, NULL, ISFIRST);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
		fh->startcond = -1;
		fh->startiscur = 0;
		fh->wrkhasrec = 0;
	}
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		   f->open_mode != COB_OPEN_INPUT) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			lmode = ISLOCK;
		}
	}





	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	iserrno = 0;
	ret = COB_STATUS_00_SUCCESS;
	switch (read_opts & COB_READ_MASK) {
	case COB_READ_NEXT:
		fh->readdir = ISNEXT;
		if (fh->eofpending == ISNEXT) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_LA) {
				if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_FI) {
				if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_GE:
					domoveback = 0;
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, iserrno == 0 ? ISNEXT : ISFIRST);
					}
					break;
				case COB_LE:
					domoveback = 0;
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, iserrno == 0 ? ISPREV : ISLAST);
					}
					break;
				case COB_LT:
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISPREV) {
				isread (fh->isfd, (void *)f->record->data, ISNEXT);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_PREVIOUS:
		fh->readdir = ISPREV;
		if (fh->eofpending == ISPREV) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_FI) {
				if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_LA) {
				if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_LE:
					domoveback = 0;
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_LT:
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				case COB_GE:
					while (iserrno == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) < 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISPREV) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISNEXT) {
				isread (fh->isfd, (void *)f->record->data, ISPREV);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISPREV | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		fh->readdir = ISPREV;
		if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, fh->lenkey);
		fh->recnum = 0;
		fh->readdone = 0;
		fh->wrkhasrec = 0;
		return ret;
	}
	fh->eofpending = 0;
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	indexed_savekey(fh, f->record->data, 0);
	fh->recnum = isrecnum;
//// #line 5117 "../orig/fileio.c"
	return 0;
//// #line 5654 "../orig/fileio.c"
}

/* WRITE to the INDEXED file  */

static int
indexed_write (cob_file *f, const int opt)
{






	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) <= 0) {
		return COB_STATUS_21_KEY_INVALID;
	}






	if (unlikely (iswrite (fh->isfd, (void *)f->record->data))) {
		return fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	indexed_savekey(fh, f->record->data, 0);






	return 0;
//// #line 5756 "../orig/fileio.c"
}


/* DELETE record from the INDEXED file  */

static int
indexed_delete (cob_file *f)
{






	struct indexfile	*fh;
	int			ret;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	if (fh->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0,
			 (void *)f->record->data, ISEQUAL);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
	} else {
		savefileposition (f);
		if (fh->curkey != 0) {
			/* Switch to primary index */
			isstart (fh->isfd, &fh->key[0], 0,
				 (void *)f->record->data, ISEQUAL);
		}
	}
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	} else if (isdelete (fh->isfd, (void *)f->record->data)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	restorefileposition (f);
	return ret;
//// #line 5812 "../orig/fileio.c"
}

/* REWRITE record to the INDEXED file  */

static int
indexed_rewrite (cob_file *f, const int opt)
{






	struct indexfile	*fh;
	size_t			k;
	int			ret, retdup;

	COB_UNUSED (opt);

	fh = f->file;
	ret = retdup = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) != 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	if (fh->curkey >= 0) {
		/* Index is active */
		/* Save record data */
		memcpy (fh->recwrk, f->record->data, f->record_max);
/* RXWRXW - readdir */
		fh->readdir = ISNEXT;
		savefileposition (f);
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (fh->curkey != 0) {
			/* Activate primary index */
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk,
				 ISEQUAL);
		}
		/* Verify record exists */
		if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)) {
			restorefileposition (f);
			return COB_STATUS_21_KEY_INVALID;
		}
		for (k = 1; k < f->nkeys && ret == COB_STATUS_00_SUCCESS; ++k) {
			if (fh->key[k].k_flags & ISDUPS) {
				continue;
			}
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[k], fh->key[k].k_leng,
				 (void *)fh->recwrk, ISEQUAL);
			if (!isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)
			 && isrecnum != fh->recnum) {
				ret = COB_STATUS_22_KEY_EXISTS;
				break;
			}
		}
		if (ret == COB_STATUS_00_SUCCESS) {
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk,
				 ISEQUAL);
			if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			} else {





				if (isrewcurr (fh->isfd, (void *)f->record->data)) {
					ret = fisretsts (COB_STATUS_49_I_O_DENIED);
				}





			}
		}
		restorefileposition (f);
//// #line 5902 "../orig/fileio.c"
		return ret;
	}

	memcpy (fh->recwrk, f->record->data, f->record_max);
	if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	} else {





		if (isrewrite (fh->isfd, (void *)f->record->data)) {
			ret = fisretsts (COB_STATUS_49_I_O_DENIED);
		}





	}
	if (!ret) {
		if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		    !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			isrelease (fh->isfd);
		}





		if (retdup) {
			return retdup;
		}
	}
	return ret;
//// #line 6021 "../orig/fileio.c"
}

/* Initialization/Termination
   cobsetpr-values with type ENV_PATH or ENV_STR
   like db_home and cob_file_path are taken care in cob_exit_common()!
*/

void
cob_exit_fileio_aide (void) {
#ifndef	WITH_DISAM
	(void)iscleanup ();
#endif
}



#endif
