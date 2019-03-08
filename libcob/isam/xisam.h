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

/*
 * Definitions for cisam, disam, and vbisam. 
 */

#if	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)
#define	WITH_XISAM
#endif

#ifdef WITH_XISAM

#include <signal.h>

#define	COB_WITH_STATUS_02

#ifdef	WITH_CISAM
#include <isam.h>
#define	isfullclose(x)	isclose (x)
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif

#ifdef	WITH_DISAM
#ifndef DISAM_NO_ISCONFIG
#include <isconfig.h>
#ifndef ISCOBOL_STATS
#undef	COB_WITH_STATUS_02
#endif
#endif
#include <disam.h>
#define	isfullclose(x)	isclose (x)
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif

#ifdef	WITH_VBISAM
#include <vbisam.h>
/* VB-ISAM does not set dup key status */
#undef	COB_WITH_STATUS_02
#if defined(VB_RTD)
/* Since VBISAM 2.1.1: access to isrecnum iserrno etc is no longer global */
static	vb_rtd_t *vbisam_rtd = NULL;

#define ISRECNUM vbisam_rtd->isrecnum
#define ISERRNO  vbisam_rtd->iserrno
#define ISRECLEN vbisam_rtd->isreclen
#else
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif
#endif

/* Isam File handler packet */

struct indexfile {
	char		*filename;	/* ISAM data file name */
	char		*savekey;	/* Area to save last primary key read */
	char		*recwrk;	/* Record work/save area */
	size_t		nkeys;		/* Actual keys in file */
	int		isfd;		/* ISAM file number */
	int		recnum;		/* Last record number read */
	int		saverecnum;	/* isrecnum of next record to process */
	int		saveerrno;	/* savefileposition errno */
	int		lmode;		/* File lock mode for 'isread' */
	int		curkey;		/* Current active index */
	int		startcond;	/* Previous 'start' condition value */
	int		readdir;	/* Read direction: ISPREV or ISNEXT */
	int		lenkey;		/* Length of savekey area */
	int		eofpending;	/* End of file pending */
	int		readdone;	/* A 'read' has been successfully done */
	int		startiscur;	/* The 'start' record is current */
	int		wrkhasrec;	/* 'recwrk' holds the next|prev record */
	struct keydesc	key[1];		/* Table of key information */
					/* keydesc is defined in (d|c|vb)isam.h */
};

#endif // WITH_XISAM
