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
#include "isam/isam.h"

#if 0
void
cob_copy_check (cob_file *to, cob_file *from)
{
	unsigned char	*toptr;
	unsigned char	*fromptr;
	size_t		tosize;
	size_t		fromsize;

	toptr = to->record->data;
	fromptr = from->record->data;
	tosize = to->record->size;
	fromsize = from->record->size;
	if (unlikely (tosize > fromsize)) {
		memcpy (toptr, fromptr, fromsize);
		memset (toptr + fromsize, ' ', tosize - fromsize);
	} else {
		memcpy (toptr, fromptr, tosize);
	}
}
#endif

int
db_nofile (const char *filename)
{
	if ((access(filename,(F_OK | R_OK | W_OK)) != 0)) {
		if (errno == ENOENT) {
			return 1;
		}
	}
	return 0;
}

int
db_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k, part;

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
			if ( (f->keys[k].field
			   && f->keys[k].field->data == kf->data
			   && f->keys[k].field->size == kf->size)
			 ||  (f->keys[k].component[0]->data == kf->data)) {
				for (part = 0; part < f->keys[k].count_components; part++) {
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

bool
local_file( dev_t device, char **pname /*output*/ ) {
#ifdef _WIN32
	/* TODO: Come back for Win32? */
	return true;
#endif
	static const char filename[] = "/proc/partitions";
	FILE *file;

	if( (file = fopen(filename, "r")) == NULL ) {
		WARN("could not open %s", filename);
	}

	int n, maj, min, nblock;
	char *s;
	static char line[128];
	static char devname[128];

	while( (s = fgets(line, sizeof(line), file)) != NULL ) {
		if( (n = sscanf(line, "%d%d%d%s", &maj, &min, &nblock, devname)) == EOF ) {
			continue;
		}
		if( n == 4 ) {
			if( maj == major(device) && min == minor(device) ) {
				*pname = devname;
				return true;
			}
		} 
	}

	return false;
}

/* Save key for given index from 'record' into 'keyarea',
   returns total length of the key */
int
db_savekey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx)
{
	int totlen, part;

	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			memcpy (keyarea + totlen,
				record + (f->keys[idx].component[part]->data - f->record->data),
				f->keys[idx].component[part]->size);
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	memcpy (keyarea,
		record + (f->keys[idx].field->data - f->record->data),
		f->keys[idx].field->size);
	return f->keys[idx].field->size;
}

int
db_cmpkey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx, int partlen)
{
	int sts, part, totlen;
	size_t cl;

	if (partlen <= 0) {
		partlen = db_keylen(f, idx);
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components && partlen > 0; part++) {
			cl = partlen > f->keys[idx].component[part]->size ? f->keys[idx].component[part]->size : partlen;
			sts = memcmp (keyarea + totlen, 
				record + (f->keys[idx].component[part]->data - f->record->data), cl);
			if (sts != 0) {
				return sts;
			}
			totlen += f->keys[idx].component[part]->size;
			partlen -= f->keys[idx].component[part]->size;
		}
		return 0;
	}
	cl = partlen > f->keys[idx].field->size ? f->keys[idx].field->size : partlen;
	return memcmp (keyarea, record + (f->keys[idx].field->data - f->record->data), cl);
}

/* Return total length of the key */
int
db_keylen (cob_file *f, int idx)
{
	int totlen, part;

	if (idx < 0 || idx > f->nkeys) {
		return -1;
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	return f->keys[idx].field->size;
}

