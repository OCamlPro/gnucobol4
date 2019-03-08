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

#include "config.h"

#define _LFS64_LARGEFILE		1
#define _LFS64_STDIO			1
#define _FILE_OFFSET_BITS		64
#define _LARGEFILE64_SOURCE		1
#ifdef	_AIX
#define _LARGE_FILES			1
#endif	/* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T		1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef	_WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#include <io.h>
#if defined (__WATCOMC__) || defined (__ORANGEC__)
#define	fdcobsync	fsync
#else
#define	fdcobsync	_commit
#endif
#if !defined(__BORLANDC__) && !defined(__WATCOMC__) && !defined(__ORANGEC__)
#define	getcwd		_getcwd
#define	chdir		_chdir
#define	mkdir(x,y)		_mkdir(x)
#define	rmdir		_rmdir
#define	open		_open
#define	close		_close
#define	unlink		_unlink
#define	fdopen		_fdopen
#ifndef lseek
#define lseek		_lseeki64
#endif
#endif

#ifndef off_t
#define off_t		cob_s64_t
#endif


#ifndef	_O_TEMPORARY
#define	_O_TEMPORARY	0
#endif

#else	/* _WIN32 */

#include <unistd.h>

#if	defined(HAVE_FDATASYNC)
#define	fdcobsync	fdatasync
#else
#define	fdcobsync	fsync
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#endif	/* _WIN32 */

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#if !defined (EDEADLK) && defined (EDEADLOCK)
#define EDEADLK EDEADLOCK
#endif

#define COUNT_OF(x) (sizeof(x)/sizeof(x[0]))

#define WARN(format, ...)  {						\
   cob_runtime_warning("%s:%d: " format "\n",				\
		       __func__, __LINE__, ## __VA_ARGS__);		\
}

#if	defined(WORDS_BIGENDIAN)
/* Big Endian then leave 'int' alone */
# define	COB_DUPSWAP(x)		((unsigned int)(x))

#elif	defined (WITH_DB) && (defined(COB_BDB_BAD_DUPNO) || 1)
/* FIXME: may be added to a file specific flag */
/* Want to retain incorrect storing of Little Endian value backwards */
# define	COB_DUPSWAP(x)		((unsigned int)(x))

#else
/* Little Endian so swap byte around to have dupno value stored in
   bigendian sequence */
# define	COB_DUPSWAP(x)		(COB_BSWAP_32((unsigned int)(x)))
#endif

/*
 * Common ISAM definitions
 */

struct file_list {
	struct file_list	*next;
	cob_file		*file;
};

#ifdef	WORDS_BIGENDIAN
#define	COB_MAYSWAP_16(x)	((unsigned short)(x))
#define	COB_MAYSWAP_32(x)	((unsigned int)(x))
#else
#define	COB_MAYSWAP_16(x)	(COB_BSWAP_16((unsigned short)(x)))
#define	COB_MAYSWAP_32(x)	(COB_BSWAP_32((unsigned int)(x)))
#endif

/*
 * External file I/O variables
 */
static const char	* const prefix[] = { "DD_", "dd_", "" };

extern cob_global	*cobglobptr;
extern cob_settings	*cobsetptr;
extern unsigned int	eop_status;
extern unsigned int	check_eop_status;
extern struct file_list	*file_cache;
extern char		*file_open_env;
extern char		*file_open_name;
extern char		*file_open_buff;
extern char		*runtime_buffer;
extern const int	status_exception[];
extern const char	* const prefix[];

/* SORT definitions */

#define COBSORTEND		1
#define COBSORTABORT		2
#define COBSORTFILEERR		3
#define COBSORTNOTOPEN		4


/* Sort item */
struct cobitem {
	struct cobitem		*next;
	unsigned char		end_of_block;
	unsigned char		block_byte;
	unsigned char		unique[sizeof (size_t)];
	unsigned char		item[1];
};

/* Sort memory chunk */
struct sort_mem_struct {
	struct sort_mem_struct	*next;
	unsigned char		*mem_ptr;
};

/* Sort queue structure */
struct queue_struct {
	struct cobitem		*first;
	struct cobitem		*last;
	size_t			count;
};

/* Sort temporary file structure */
struct file_struct {
	FILE			*fp;
	size_t			count;	/* Count of blocks in temporary files */
};

/* Sort base structure */
struct cobsort {
	void			*pointer;
	struct cobitem		*empty;
	void			*sort_return;
	cob_field		*fnstatus;
	struct sort_mem_struct	*mem_base;
	size_t			unique;
	size_t			size;
	size_t			alloc_size;
	size_t			mem_size;
	size_t			mem_used;
	size_t			mem_total;
	size_t			chunk_size;
	size_t			r_size;
	size_t			w_size;
	size_t			switch_to_file;
	unsigned int		retrieving;
	unsigned int		files_used;
	int			destination_file;
	int			retrieval_queue;
	struct queue_struct	queue[4];
	struct file_struct	file[4];
};

/* End SORT definitions */

static COB_INLINE void
unique_copy (unsigned char *s1, const unsigned char *s2)
{
	size_t	size;

	size = sizeof (size_t);
	do {
		*s1++ = *s2++;
	} while (--size);
}





/*
 * file I/O utility functions
 */

void cob_cache_file (cob_file *f);
char * cob_chk_file_env (const char *src);
void save_status (cob_file *f, cob_field *fnstatus, const int status);
void cob_chk_file_mapping (void);
void isam_sync (cob_file *f);


int db_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen);
int db_keylen (cob_file *f, int idx);
int db_savekey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx);

#if 0
// implementation
int db_msgcall_set (DB_ENV *dbe, const char *err);
int db_errcall_set (DB_ENV *dbe, const char *prefix, const char *err);
#endif
int db_nofile (const char *filename);
bool local_file( dev_t device, char **pname /*output*/ );

void cob_copy_check (cob_file *to, cob_file *from);

int cob_file_sort_compare (struct cobitem *k1, struct cobitem *k2, void *);
int cob_file_sort_process (struct cobsort *hp);
int cob_file_sort_submit (cob_file *f, const unsigned char *p);
int cob_file_sort_retrieve (cob_file *f, unsigned char *p);
void cob_file_sort_using (cob_file *sort_file, cob_file *data_file);
void cob_file_sort_giving (cob_file *sort_file, const size_t varcnt, ...);

void cob_file_sort_init (cob_file *f, const unsigned int nkeys,
			 const unsigned char *collating_sequence,
			 void *sort_return, cob_field *fnstatus);
void cob_file_sort_init_key (cob_file *f, cob_field *field, const int flag,
			     const unsigned int offset);
void cob_file_sort_close (cob_file *f);

void cob_free_list (struct cobsort *hp);

void cob_exit_fileio_aide (void);
void cob_init_fileio_epilogue (cob_global *lptr, cob_settings *sptr);
void cob_file_unlock_internal (cob_file *f) ;
