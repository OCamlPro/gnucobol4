/*
   Copyright (C) 2001,2002,2003,2004,2005,2006,2007 Keisuke Nishida
   Copyright (C) 2006-2010 Roger While

   This file is part of GNU Cobol.

   The GNU Cobol compiler is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   GNU Cobol is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Cobol; see the file COPYING. If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301 USA
*/

/*#define DEBUG_REPLACE*/

#include "config.h"
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>
#include <errno.h>
#include <ctype.h>
#ifdef	HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>
#include <sys/stat.h>
#ifdef  HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef _WIN32
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#endif

#ifdef	HAVE_KPATHSEA_GETOPT_H
#include <kpathsea/getopt.h>
#else
#ifdef	HAVE_GETOPT_H
#include <getopt.h>
#else
#include "lib/getopt.h"
#endif
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

#include <tarstamp.h>

#include "cobc.h"
#include "tree.h"

/* Compile level */
enum cb_compile_level {
	CB_LEVEL_PREPROCESS = 1,
	CB_LEVEL_TRANSLATE,
	CB_LEVEL_COMPILE,
	CB_LEVEL_ASSEMBLE,
	CB_LEVEL_MODULE,
	CB_LEVEL_LIBRARY,
	CB_LEVEL_EXECUTABLE
};

/* Info display limits */
#define	CB_IMSG_SIZE		24
#define	CB_IVAL_SIZE		(74 - CB_IMSG_SIZE - 4)

/* C version info */
#ifdef	__VERSION__
#define OC_C_VERSION_PRF	""
#define OC_C_VERSION	CB_XSTRINGIFY(__VERSION__)
#elif	defined(__xlc__)
#define OC_C_VERSION_PRF	"(IBM) "
#define OC_C_VERSION	CB_XSTRINGIFY(__xlc__)
#elif	defined(_MSC_VER)
#define OC_C_VERSION_PRF	"(Microsoft) "
#define OC_C_VERSION	CB_XSTRINGIFY(_MSC_VER)
#elif	defined(__BORLANDC__)
#define OC_C_VERSION_PRF	"(Borland) "
#define OC_C_VERSION	CB_XSTRINGIFY(__BORLANDC__)
#elif	defined(__WATCOMC__)
#define OC_C_VERSION_PRF	"(Watcom) "
#define OC_C_VERSION	CB_XSTRINGIFY(__WATCOMC__)
#elif	defined(__INTEL_COMPILER)
#define OC_C_VERSION_PRF	"(Intel) "
#define OC_C_VERSION	CB_XSTRINGIFY(__INTEL_COMPILER)
#else
#define OC_C_VERSION_PRF	""
#define OC_C_VERSION	"unknown"
#endif

#if	defined(_MSC_VER)
#define PATHSEPS ";"
#else
#define PATHSEPS ":"
#endif

#ifdef	_MSC_VER
#define	CB_COPT_1	" /O1"
#define	CB_COPT_2	" /O2"
#define	CB_COPT_S	" /Os"
#elif defined(__hpux) && !defined(__GNUC__)
#define	CB_COPT_1	" -O"
#define	CB_COPT_2	" +O2"
#define	CB_COPT_S	" +O2 +Osize"
#elif	defined(__xlc__)
#define	CB_COPT_1	" -O"
#define	CB_COPT_2	" -O2"
#define	CB_COPT_S	" -O"
#else
#define	CB_COPT_1	" -O"
#define	CB_COPT_2	" -O2"
#define	CB_COPT_S	" -Os"
#endif

/* Global variables */

int	cb_source_format = CB_FORMAT_FIXED;

#ifdef	COB_EBCDIC_MACHINE
int	cb_display_sign = COB_DISPLAY_SIGN_EBCDIC;	/* 1 */
#else
int	cb_display_sign = COB_DISPLAY_SIGN_ASCII;	/* 0 */
#endif

#undef	COB_EXCEPTION
#define	COB_EXCEPTION(code,tag,name,critical) {name, 0x##code, 0},
struct cb_exception cb_exception_table[] = {
	{NULL, 0, 0},		/* CB_EC_ZERO */
#include "libcob/exception.def"
	{NULL, 0, 0}		/* CB_EC_MAX */
};
#undef	COB_EXCEPTION

#undef	CB_FLAG
#define	CB_FLAG(var,name,doc)		int var = 0;
#include "flag.def"
#undef	CB_FLAG

#undef	CB_WARNDEF
#define	CB_WARNDEF(var,name,wall,doc)	int var = 0;
#include "warning.def"
#undef	CB_WARNDEF

int			cb_id = 1;
int			cb_attr_id = 1;
int			cb_literal_id = 1;
int			cb_field_id = 1;
int			cb_storage_id = 1;
int			cb_flag_main = 0;

int			errorcount = 0;
int			warningcount = 0;
int			exit_option = 0;
int			alt_ebcdic = 0;
int			optimize_flag = 0;

char			*cb_source_file = NULL;
char			*cb_oc_build_stamp = NULL;
char			*source_name;
char			*demangle_name;
int			cb_source_line = 0;

FILE			*cb_storage_file;
char			*cb_storage_file_name;

FILE			*cb_listing_file = NULL;
int			cb_listing_page = 0;
int			cb_listing_wide = 0;
int			cb_lines_per_page = CB_MAX_LINES;
char			cb_listing_date[48]; /* Date/Time buffer for listing */
struct list_files	*cb_listing_files = NULL;
struct list_files	*cb_current_file = NULL;

FILE			*cb_depend_file = NULL;
char			*cb_depend_target = NULL;
struct cb_text_list	*cb_depend_list = NULL;
struct cb_text_list	*cb_include_list = NULL;
struct cb_text_list	*cb_extension_list = NULL;

int			cb_saveargc;
char			**cb_saveargv;

const char		*cob_config_dir;

#ifdef	_MSC_VER 
#if	_MSC_VER >= 1400
static const char	*manicmd;
#endif
#endif

/* Local variables */

static const char	*const cob_csyns[] = {
	"NULL",
	"L_initextern",
	"LRET_initextern",
#ifndef __GNUC__
	"P_switch",
#endif
	"alignof",
	"asm",
	"auto",
	"break",
	"case",
	"char",
	"const",
	"continue",
	"default",
	"do",
	"double",
	"else",
	"enum",
	"exit_program",
	"extern",
	"float",
	"for",
	"frame_pointer",
	"frame_stack",
	"goto",
	"if",
	"inline",
	"int",
	"long",
	"offsetof",
	"register",
	"restrict",
	"return",
	"short",
	"signed",
	"sizeof",
	"static",
	"struct",
	"switch",
	"typedef",
	"typeof",
	"union",
	"unsigned",
	"void",
	"volatile",
	"_Bool",
	"_Complex",
	"_Imaginary"
};

#define COB_NUM_CSYNS	sizeof(cob_csyns) / sizeof(char *)

static enum cb_compile_level	cb_compile_level = 0;
static enum cb_compile_level	local_level = 0;

static size_t		iparams = 0;
static int		iargs;
static char		*cobcpy = NULL;
static char		*save_temps_dir = NULL;

static jmp_buf		cob_jmpbuf;

static int		wants_nonfinal = 0;
static int		cb_flag_module = 0;
static int		cb_flag_library = 0;
static int		save_temps = 0;
static int		save_csrc = 0;
static int		verbose_output = 0;
static int		cob_iteration = 0;
#ifndef _WIN32
static pid_t		cob_process_id = 0;
#endif

static int		cb_listing_linecount = 10000;
static int		cb_listing_eject = 0;
static int		cb_inreplace = 0;
static char		cb_listing_filename[FILENAME_MAX];
static char		cb_listing_ttl[133];	/* Listing subtitle */
static void		print_program_code (struct list_files *, int);

static int		strip_output = 0;
static int		gflag_set = 0;

static char		*output_name;

static const char	*cob_tmpdir;			/* /tmp */

static struct filename	*file_list;

/* NOTE fcopts MUST have at least one leading space */
#if defined (__GNUC__) && (__GNUC__ >= 3)
#if !defined (__clang__)
static const char fcopts[] = " -finline-functions -fno-gcse -freorder-blocks ";
#else
static const char fcopts[] = " -finline-functions -freorder-blocks ";
#endif
#elif defined(__xlc__)
static const char fcopts[] = " -Q -qro -qroconst ";
#else
static const char fcopts[] = " ";
#endif

#if defined (__GNUC__) && (__GNUC__ >= 3)
static const char	gccpipe[] = "-pipe";
#else
static const char	gccpipe[] = "\0";
#endif

#ifdef	HAVE_SIGNAL_H
typedef void (*cob_sighandler_t) (int);
static cob_sighandler_t		hupsig = NULL;
static cob_sighandler_t		intsig = NULL;
static cob_sighandler_t		qutsig = NULL;
#endif

static const char short_options[] = "hVivECScbmxOgwo:t:T:I:L:l:D:";

static const struct option long_options[] = {
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'V'},
#if	1	/* getopt_long_only has a problem with eg. -xv - remove */
	{"verbose", no_argument, NULL, 'v'},
#endif
	{"info",		no_argument, NULL, 'i'},
	{"list-reserved", no_argument, NULL, '5'},
	{"list-intrinsics", no_argument, NULL, '6'},
	{"list-mnemonics", no_argument, NULL, '7'},
	{"list-system", no_argument, NULL, '8'},
	{"save-temps", optional_argument, NULL, '_'},
	{"std", required_argument, NULL, '$'},
	{"conf", required_argument, NULL, '&'},
	{"debug", no_argument, NULL, 'd'},
	{"ext", required_argument, NULL, 'e'},
	{"free", no_argument, &cb_source_format, CB_FORMAT_FREE},
	{"fixed", no_argument, &cb_source_format, CB_FORMAT_FIXED},
	{"static", no_argument, &cb_flag_static_call, 1},
	{"dynamic", no_argument, &cb_flag_static_call, 0},
	{"O2", no_argument, NULL, '2'},
	{"Os", no_argument, NULL, 's'},
	{"Q", required_argument, NULL, 'Q'},
	{"A", required_argument, NULL, 'A'},
	{"MT", required_argument, NULL, '%'},
	{"MF", required_argument, NULL, '@'},
	{"tlines", required_argument, NULL, '#'},
#undef	CB_FLAG
#define	CB_FLAG(var,name,doc)			\
	{"f"name, no_argument, &var, 1},	\
	{"fno-"name, no_argument, &var, 0},
#include "flag.def"
#undef	CB_FLAG
	{"Wall", no_argument, NULL, 'W'},
	{"W", no_argument, NULL, 'Z'},
#undef	CB_WARNDEF
#define	CB_WARNDEF(var,name,wall,doc)		\
	{"W"name, no_argument, &var, 1},	\
	{"Wno-"name, no_argument, &var, 0},
#include "warning.def"
#undef	CB_WARNDEF
	{NULL, 0, NULL, 0}
};

static const char	*cob_cc;				/* gcc */
static char		cob_cflags[COB_SMALL_BUFF];		/* -I... */
static char		cob_libs[COB_MEDIUM_BUFF];		/* -L... -lcob */
static char		cob_define_flags[COB_SMALL_BUFF];	/* -D... */
static char		cob_ldflags[COB_SMALL_BUFF];
static const char	*cob_copy_dir;

/* cobc functions */

/* Global functions */

void
cobc_abort (const char *filename, const int linenum)
{
	fprintf (stderr, "%s:%d: Internal compiler error\n", filename, linenum);
	(void)longjmp (cob_jmpbuf, 1);
}

void
cobc_tree_cast_error (cb_tree x, const char *filen, const int linenum, const int tagnum)
{
	fprintf (stderr, "%s:%d: Invalid type cast from '%s'\n",
		filen, linenum, x ? cb_name (x) : "null");
	fprintf (stderr, "Tag 1 %d Tag 2 %d\n", x ? CB_TREE_TAG(x) : 0,
		tagnum);
	(void)longjmp (cob_jmpbuf, 1);
}

void *
cobc_malloc (const size_t size)
{
	void *mptr;

	mptr = calloc (1, size);
	if (!mptr) {
		fprintf (stderr, "Cannot allocate %d bytes of memory - Aborting\n", (int)size);
		fflush (stderr);
		(void)longjmp (cob_jmpbuf, 1);
	}
	return mptr;
}

void *
cobc_realloc (void *prevptr, const size_t size)
{
	void *mptr;

	mptr = realloc (prevptr, size);
	if (!mptr) {
		fprintf (stderr, "Cannot reallocate %d bytes of memory - Aborting\n", (int)size);
		fflush (stderr);
		(void)longjmp (cob_jmpbuf, 1);
	}
	return mptr;
}

struct cb_text_list *
cb_text_list_add (struct cb_text_list *list, const char *text)
{
	struct cb_text_list *p;
	struct cb_text_list *l;

	p = cobc_malloc (sizeof (struct cb_text_list));
	p->text = strdup (text);
	p->next = NULL;
	if (!list) {
		return p;
	} else {
		for (l = list; l->next; l = l->next) { ; }
		l->next = p;
		return list;
	}
}

size_t
cobc_check_valid_name (const char *name)
{
	size_t	n;

	for (n = 0; n < COB_NUM_CSYNS; ++n) {
		if (!strcmp (name, cob_csyns[n])) {
			return 1;
		}
	}
	return 0;
}

/* Local functions */

static void
cobc_init_var (char *var, const char *env, const char *def)
{
	char *p = getenv (env);

	if (p) {
		strcpy (var, p);
	} else {
		strcpy (var, def);
	}
}

static void
cobc_check_action (const char *name)
{
	struct stat		st;
	char			buff[COB_SMALL_BUFF];

	if (name && !stat (name, &st)) {
	if (!save_temps) {
			unlink (name);
		} else if (save_temps_dir) {
		memset (buff, 0, sizeof(buff));
			sprintf (buff, "%s/%s", save_temps_dir, name);
			(void)rename (name, buff);
		}
	}
}

static void
cobc_clean_up (const int status)
{
	struct filename		*fn;
	struct local_filename	*lf;
	int			i;
	char			buff[COB_SMALL_BUFF];

	if (cb_listing_file) {
		fclose (cb_listing_file);
		cb_listing_file = NULL;
	}
	for (fn = file_list; fn; fn = fn->next) {
		if (fn->need_preprocess
		    && (status || cb_compile_level > CB_LEVEL_PREPROCESS)) {
			cobc_check_action (fn->preprocess);
		}
		if (!save_csrc && fn->need_translate
		    && (status || cb_compile_level > CB_LEVEL_TRANSLATE)) {
			cobc_check_action (fn->translate);
			cobc_check_action (fn->trstorage);
			if (fn->localfile) {
				for (lf = fn->localfile; lf; lf = lf->next) {
					cobc_check_action (lf->local_name);
				}
			} else if (fn->translate) {
				/* If we get syntax errors, we do not
				   know the number of local include files */
				memset (buff, 0, sizeof(buff));
				for (i = 0; i < 30; i++) {
					if (i) {
						sprintf (buff, "%s.l%d.h",
							fn->translate, i);
					} else {
						sprintf (buff, "%s.l.h",
							fn->translate);
					}
					unlink (buff);
				}
			}
		}
		if (fn->need_assemble
		    && (status || cb_compile_level > CB_LEVEL_ASSEMBLE)) {
			cobc_check_action (fn->object);
		}
	}
}

static void
cobc_terminate (const char *str)
{
	int	save_errno;

	save_errno = errno;
	fprintf (stderr, "cobc: ");
	fflush (stderr);
	errno = save_errno;
	perror (str);
	fflush (stderr);
	cobc_clean_up (1);
	exit (1);
}

#ifdef	HAVE_SIGNAL_H
static void
cobc_sig_handler (int sig)
{
	save_temps = 0;
	cobc_clean_up (1);
	switch (sig) {
#ifdef	SIGHUP
	case SIGHUP:
		if ((hupsig != SIG_IGN) && (hupsig != SIG_DFL)) {
			(*hupsig) (SIGHUP);
		}
		break;
#endif
	case SIGINT:
		if ((intsig != SIG_IGN) && (intsig != SIG_DFL)) {
			(*intsig) (SIGINT);
		}
		break;
#ifdef	SIGQUIT
	case SIGQUIT:
		if ((qutsig != SIG_IGN) && (qutsig != SIG_DFL)) {
			(*qutsig) (SIGQUIT);
		}
		break;
#endif
	}
	exit (sig);
}
#endif

/* Command line */

static void
cobc_print_version (void)
{
	printf ("cobc (%s) %s.%d\n",
		PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2001,2002,2003,2004,2005,2006,2007 Keisuke Nishida");
	puts ("Copyright (C) 2006-2012 Roger While");
	puts (_("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."));
	printf (_("Built     %s"), cb_oc_build_stamp);
	putchar ('\n');
	printf (_("Packaged  %s"), COB_TAR_DATE);
	putchar ('\n');
	printf (_("C version %s%s"), OC_C_VERSION_PRF, OC_C_VERSION);
	putchar ('\n');
}

static void
cobc_var_print (const char *msg, const char *val)
{
	char	*p;
	char	*token;
	size_t	n;
	size_t	toklen;

	printf ("%-*.*s : ", CB_IMSG_SIZE, CB_IMSG_SIZE, msg);
	if (strlen(val) <= CB_IVAL_SIZE) {
		printf("%s\n", val);
		return;
	}
	p = strdup (val);
	n = 0;
	token = strtok (p, " ");
	for (; token; token = strtok (NULL, " ")) {
		toklen = strlen (token) + 1;
		if ((n + toklen) > CB_IVAL_SIZE) {
			if (n) {
				printf ("\n%*.*s", CB_IMSG_SIZE + 3,
					CB_IMSG_SIZE + 3, " ");
			}
			n = 0;
		}
		printf ("%s%s", (n ? " " : ""), token);
		n += toklen;
	}
	putchar ('\n');
	free (p);
}

static void
cobc_print_info (void)
{
	char	buff[16];

	cobc_print_version ();
	puts (_("Build information"));
	cobc_var_print (_("Build environment"),	COB_BLD_BUILD);
	cobc_var_print ("CC",			COB_BLD_CC);
	cobc_var_print ("CPPFLAGS",		COB_BLD_CPPFLAGS);
	cobc_var_print ("CFLAGS",		COB_BLD_CFLAGS);
	cobc_var_print ("LD",			COB_BLD_LD);
	cobc_var_print ("LDFLAGS",		COB_BLD_LDFLAGS);
	putchar ('\n');
	puts (_("GNU Cobol information"));
	cobc_var_print ("COB_CC",		COB_CC);
	cobc_var_print ("COB_CFLAGS",		COB_CFLAGS);
	cobc_var_print ("COB_LDFLAGS",		COB_LDFLAGS);
	cobc_var_print ("COB_LIBS",		COB_LIBS);
	cobc_var_print ("COB_CONFIG_DIR",	COB_CONFIG_DIR);
	cobc_var_print ("COB_COPY_DIR",		COB_COPY_DIR);
	cobc_var_print ("COB_LIBRARY_PATH",	COB_LIBRARY_PATH);
	cobc_var_print ("COB_MODULE_EXT",	COB_MODULE_EXT);
	cobc_var_print ("COB_EXEEXT",		COB_EXEEXT);

#if	defined(USE_LIBDL) || defined(_WIN32)
	cobc_var_print (_("Dynamic loading"),	_("System"));
#else
	cobc_var_print (_("Dynamic loading"),	_("Libtool"));
#endif

#ifdef	COB_PARAM_CHECK
	cobc_var_print ("\"CBL_\" param check",	_("Enabled"));
#else
	cobc_var_print ("\"CBL_\" param check",	_("Disabled"));
#endif

	snprintf (buff, sizeof(buff), "%d", WITH_VARSEQ);
	cobc_var_print (_("Variable format"),	buff);

#ifdef	COB_LI_IS_LL
	cobc_var_print ("BINARY-C-LONG",	_("8 bytes"));
#else
	cobc_var_print ("BINARY-C-LONG",	_("4 bytes"));
#endif

#ifdef	WITH_SEQRA_EXTFH
	cobc_var_print (_("Sequential handler"),	_("External"));
#else
	cobc_var_print (_("Sequential handler"),	_("Internal"));
#endif
#ifdef	WITH_INDEX_EXTFH
	cobc_var_print (_("ISAM handler"),		_("External"));
#endif
#ifdef	WITH_DB
	cobc_var_print (_("ISAM handler"),		_("BDB"));
#endif
#ifdef	WITH_CISAM
	cobc_var_print (_("ISAM handler"),		_("C-ISAM (Experimental)"));
#endif
#ifdef	WITH_DISAM
	cobc_var_print (_("ISAM handler"),		_("D-ISAM (Experimental)"));
#endif
#ifdef	WITH_VBISAM
	cobc_var_print (_("ISAM handler"),		_("VBISAM (Experimental)"));
#endif
}

static void
cobc_print_usage (void)
{
	puts (_("Usage: cobc [options] file ..."));
	putchar ('\n');
	puts (_("Options:"));
	puts (_("  --help                Display this message"));
	puts (_("  --version, -V         Display compiler version"));
	puts (_("  --info, -i            Display compiler build information"));
	puts (_("  -v                    Display the commands invoked by the compiler"));
	puts (_("  -x                    Build an executable program"));
	puts (_("  -m                    Build a dynamically loadable module (default)"));
	puts (_("  -std=<dialect>        Warnings/features for a specific dialect :"));
	puts (_("                          cobol2002   Cobol 2002"));
	puts (_("                          cobol85     Cobol 85"));
	puts (_("                          ibm         IBM Compatible"));
	puts (_("                          mvs         MVS Compatible"));
	puts (_("                          bs2000      BS2000 Compatible"));
	puts (_("                          mf          Micro Focus Compatible"));
	puts (_("                          default     When not specified"));
	puts (_("                        See config/default.conf and config/*.conf"));
	puts (_("  -free                 Use free source format"));
	puts (_("  -fixed                Use fixed source format (default)"));
	puts (_("  -O, -O2, -Os          Enable optimization"));
	puts (_("  -g                    Enable C compiler debug / stack check / trace"));
	puts (_("  -debug                Enable all run-time error checking"));
	puts (_("  -o <file>             Place the output into <file>"));
	puts (_("  -b                    Combine all input files into a single"));
	puts (_("                        dynamically loadable module"));
	puts (_("  -E                    Preprocess only; do not compile or link"));
	puts (_("  -C                    Translation only; convert COBOL to C"));
	puts (_("  -S                    Compile only; output assembly file"));
	puts (_("  -c                    Compile and assemble, but do not link"));
	puts (_("  -T <file>             Generate and place a wide program listing into <file>"));
	puts (_("  -t <file>             Generate and place a program listing into <file>"));
	puts (_("  --tlines=<lines>      Specify lines per page in listing"));
	puts (_("  -I <directory>        Add <directory> to copy/include search path"));
	puts (_("  -L <directory>        Add <directory> to library search path"));
	puts (_("  -l <lib>              Link the library <lib>"));
	puts (_("  -A <options>          Add <options> to the C compile phase"));
	puts (_("  -Q <options>          Add <options> to the C link phase"));
	puts (_("  -D <define>           Pass <define> to the C compiler"));
	puts (_("  -conf=<file>          User defined dialect configuration - See -std="));
	puts (_("  --list-reserved       Display reserved words"));
	puts (_("  --list-intrinsics     Display intrinsic functions"));
	puts (_("  --list-mnemonics      Display mnemonic names"));
	puts (_("  --list-system         Display system routines"));
	puts (_("  -save-temps(=<dir>)   Save intermediate files (default current directory)"));
	puts (_("  -MT <target>          Set target file used in dependency list"));
	puts (_("  -MF <file>            Place dependency list into <file>"));
	puts (_("  -ext <extension>      Add default file extension"));

	putchar ('\n');

	puts (_("  -W                    Enable ALL warnings"));
	puts (_("  -Wall                 Enable all warnings except as noted below"));
#undef	CB_WARNDEF
#define	CB_WARNDEF(var,name,wall,doc)		\
	printf ("  -W%-19s %s", name, gettext (doc)); \
	if (!wall) { \
		puts (_(" (NOT set with -Wall)")); \
	} else { \
		printf ("\n"); \
	}
#include "warning.def"
#undef	CB_WARNDEF
	putchar ('\n');

#undef	CB_FLAG
#define	CB_FLAG(var,name,doc)			\
	if (strcmp (name, "static-call"))	\
		printf ("  -f%-19s %s\n", name, gettext (doc));
#include "flag.def"
#undef	CB_FLAG
	putchar ('\n');
}

static void
cobc_options_error (void)
{
	fprintf (stderr, "Only one of options 'E', 'S', 'C' 'c' may be specified\n");
	exit (1);
}

static int
process_command_line (const int argc, char *argv[])
{
	int			c, idx;
	int 		argnum;
	enum cob_exception_id	i;
	struct stat		st;
	char			ext[COB_MINI_BUFF];

	/* Enable default I/O exceptions */
	CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;


	/* Translate command line arguments from WIN to UNIX style */
	argnum = 1;
	while (++argnum <= argc) {
		if (strrchr(argv[argnum - 1], '/') == argv[argnum - 1]) {
			argv[argnum - 1][0] = '-';
		}
	}
	
	while ((c = getopt_long_only (argc, argv, short_options,
					  long_options, &idx)) >= 0) {
		switch (c) {
		case 0:
			/* Defined flag */
			break;

		case '?':
			/* Unknown option or ambiguous */
			exit (1);

		case 'h':
			/* --help */
			cobc_print_usage ();
			exit (0);

		case 'V':
			/* --version */
			cobc_print_version ();
			exit_option = 1;
			break;

		case 'i':
			/* --info */
			cobc_print_info ();
			exit_option = 1;
			break;

		case '5':
			/* --list-reserved */
			cb_list_reserved ();
			exit_option = 1;
			break;

		case '6':
			/* --list-intrinsics */
			cb_list_intrinsics ();
			exit_option = 1;
			break;

		case '7':
			/* --list-mnemonics */
			cb_list_mnemonics ();
			exit_option = 1;
			break;

		case '8':
			/* --list-system */
			cb_list_system ();
			exit_option = 1;
			break;

		case 'E':
			/* -E : Preprocess */
			if (wants_nonfinal) {
				cobc_options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_PREPROCESS;
			break;

		case 'C':
			/* -C : Generate C code */
			if (wants_nonfinal) {
				cobc_options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_TRANSLATE;
			break;

		case 'S':
			/* -S : Generate assembler code */
			if (wants_nonfinal) {
				cobc_options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_COMPILE;
			break;

		case 'c':
			/* -c : Generate C object code */
			if (wants_nonfinal) {
				cobc_options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_ASSEMBLE;
			break;

		case 'b':
			/* -b : Generate combined library module */
			if (cb_flag_main || cb_flag_module) {
				fprintf (stderr, "Only one of options 'm', 'x', 'b' may be specified\n");
				exit (1);
			}
			cb_flag_library = 1;
			break;

		case 'm':
			/* -m : Generate loadable module (default) */
			if (cb_flag_main || cb_flag_library) {
				fprintf (stderr, "Only one of options 'm', 'x', 'b' may be specified\n");
				exit (1);
			}
			cb_flag_module = 1;
			break;

		case 'x':
			/* -x : Generate executable */
			if (cb_flag_module || cb_flag_library) {
				fprintf (stderr, "Only one of options 'm', 'x', 'b' may be specified\n");
				exit (1);
			}
			cb_flag_main = 1;
			break;

		case 'v':
			/* -v : Verbose reporting */
			verbose_output = 1;
			break;

		case 'o':
			/* -o : Output file */
			output_name = strdup (optarg);
			break;

		case 'O':
			/* -O : Optimize */
			strcat (cob_cflags, CB_COPT_1);
			strcat (cob_cflags, fcopts);
			strcat (cob_cflags, COB_EXTRA_FLAGS);
			optimize_flag = 1;
			break;

		case '2':
			/* -O2 : Optimize */
			strip_output = 1;
			strcat (cob_cflags, CB_COPT_2);
			strcat (cob_cflags, fcopts);
			strcat (cob_cflags, COB_EXTRA_FLAGS);
			optimize_flag = 2;
			break;

		case 's':
			/* -Os : Optimize */
			strip_output = 1;
			strcat (cob_cflags, CB_COPT_S);
			strcat (cob_cflags, fcopts);
			strcat (cob_cflags, COB_EXTRA_FLAGS);
			optimize_flag = 2;
			break;

		case 'g':
			/* -g : Generate C debug code */
			save_csrc = 1;
			gflag_set = 1;
			cb_flag_stack_check = 1;
			cb_flag_source_location = 1;
#ifndef _MSC_VER
			strcat (cob_cflags, " -g");
#endif
			break;

		case '$':
			/* -std=<xx> : Specify dialect */
			snprintf (ext, COB_MINI_MAX, "%s.conf", optarg);
			if (cb_load_std (ext) != 0) {
				fprintf (stderr, _("Invalid option -std=%s\n"), optarg);
				exit (1);
			}
			break;

		case '&':
			/* -conf=<xx> : Specify dialect configuration file */
			if (cb_load_conf (optarg, 1, 0) != 0) {
				exit (1);
			}
			break;

		case 'd':
			/* -debug : Turn on OC runtime checks */
			/* Turn on all exception conditions */
			for (i = 1; i < COB_EC_MAX; i++) {
				CB_EXCEPTION_ENABLE (i) = 1;
			}
			cb_flag_source_location = 1;
			cb_flag_stack_check = 1;
			break;

		case '_':
			/* --save-temps : Save intermediary files */
			save_temps = 1;
			if (optarg) {
				if (stat (optarg, &st) != 0 || !(S_ISDIR (st.st_mode))) {
					fprintf (stderr, "Warning - '%s' is not a directory, defaulting to current directory\n", optarg);
					fflush (stderr);
				} else {
					save_temps_dir = optarg;
				}
			}
			break;

		case 'T':
			/* -T : Generate wide listing */
			cb_listing_wide = 1;
			/* fall through */

		case 't':
			/* -t : Generate listing */
			cb_listing_file = fopen (optarg, "w");
			if (!cb_listing_file) {
				perror (optarg);
			} else {
			   time_t curtime;		/* Compile time */

			   curtime = time(NULL);
			   strcpy (cb_listing_date, ctime(&curtime));
			   *strchr (cb_listing_date, '\n') = '\0';
			}
			break;


		case '#':
			/* --tlines=nn : Lines per page */
			cb_lines_per_page = atoi(optarg);
			break;

		case 'D':
			/* -D  : Define for C-Compiler */
			if (!strcasecmp (optarg, "ebug")) {
				fprintf (stderr, "Warning - passing '%s' to compiler - did you intend to use -debug?\n", optarg);
				fflush (stderr);
			}
#ifdef _MSC_VER
			strcat (cob_define_flags, "/D \"");
			strcat (cob_define_flags, optarg);
			strcat (cob_define_flags, "\" ");
#else
			strcat (cob_define_flags, "-D");
			strcat (cob_define_flags, optarg);
			strcat (cob_define_flags, " ");
#endif
			break;

		case '%':
			/* -MT */
			cb_depend_target = strdup (optarg);
			break;

		case '@':
			/* -MF */
			cb_depend_file = fopen (optarg, "w");
			if (!cb_depend_file) {
				perror (optarg);
			}
			break;

		case 'I':
#ifdef _MSC_VER
			strcat (cob_define_flags, "/I ");
#else
			strcat (cob_define_flags, "-I");
#endif
			strcat (cob_define_flags, "\"");
			strcat (cob_define_flags, optarg);
			strcat (cob_define_flags, "\"");
			cb_include_list = cb_text_list_add (cb_include_list, optarg);
			break;

		case 'L':
			/* -L <xx> : Directory for library search */
			strcat (cob_libs, " -L");
			strcat (cob_libs, "\"");
			strcat (cob_libs, optarg);
			strcat (cob_libs, "\"");
			break;

		case 'l':
			/* -l <xx> : Add library to link phase */
#ifdef	_MSC_VER
			strcat (cob_libs, " ");
#else
			strcat (cob_libs, " -l");
#endif
			strcat (cob_libs, "\"");
			strcat (cob_libs, optarg);
			strcat (cob_libs, "\"");
			break;

		case 'e':
			/* -e <xx> : Add an extension suffix */
			snprintf (ext, COB_MINI_MAX, ".%s", optarg);
			cb_extension_list = cb_text_list_add (cb_extension_list, ext);
			break;

		case 'A':
			/* -A <xx> : Add options to C compile phase */
			strcat (cob_cflags, " ");
			strcat (cob_cflags, optarg);
			break;

		case 'Q':
			/* -Q <xx> : Add options to C link phase */
			strcat (cob_ldflags, " ");
			strcat (cob_ldflags, optarg);
			break;

		case 'w':
			/* -w(xx) : Turn off warnings */
#undef	CB_WARNDEF
#define	CB_WARNDEF(var,name,wall,doc)	var = 0;
#include "warning.def"
#undef	CB_WARNDEF
			break;

		case 'W':
			/* -W : Turn on warnings */
#undef	CB_WARNDEF
#define	CB_WARNDEF(var,name,wall,doc)	if (wall) var = 1;
#include "warning.def"
#undef	CB_WARNDEF
			break;

		case 'Z':
			/* -W : Turn on all warnings */
#undef	CB_WARNDEF
#define	CB_WARNDEF(var,name,wall,doc)	var = 1;
#include "warning.def"
#undef	CB_WARNDEF
			break;

		default:
			ABORT ();
		}
	}

	/* Exit if list options specified */
	if (exit_option) {
		exit (0);
	}

	/* Load default configuration file if necessary */
	if (cb_config_name == NULL) {
		if (cb_load_std ("default.conf") != 0) {
			fprintf (stderr, "Error: failed to load the initial config file\n");
			exit (1);
		}
	}

	if (cb_flag_fold_copy_lower && cb_flag_fold_copy_upper) {
		fprintf (stderr, "Error: Invalid option combination\n");
		exit (1);
	}

	/* If C debug, do not strip output */
	if (gflag_set) {
		strip_output = 0;
		optimize_flag = 0;
	}

	if (cb_flag_traceall) {
		cb_flag_trace = 1;
		cb_flag_source_location = 1;
	}
	if (cb_flag_source_location) {
		optimize_flag = 0;
	}
#if defined (__GNUC__) && (__GNUC__ >= 3)
	if (strip_output) {
		strcat (cob_cflags, " -fomit-frame-pointer");
	}
#endif

	/* default extension list */
	cb_extension_list = cb_text_list_add (cb_extension_list, ".CPY");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".CBL");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".COB");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".cpy");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".cbl");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".cob");
	cb_extension_list = cb_text_list_add (cb_extension_list, "");

	return optind;
}

static void 
process_env_copy_path (void) 
{
	char	*value;
	char	*token;

	cobcpy = getenv ("COBCPY");
	if (cobcpy == NULL || strlen (cobcpy) == 0) {
		/* env. not defined: nothing to do */
		cobcpy = NULL;
		return;
	}
	
	/* clone value to avoid memory corruption */
	value = strdup (cobcpy);

	/* Tokenize for path sep. */
	token = strtok (value, PATHSEPS);
	while (token) {
		cb_include_list = cb_text_list_add (cb_include_list, token);
		token = strtok (NULL, PATHSEPS);
	}

	/* release memory of clone */
	free (value);
	return;
}

#if	defined (_MSC_VER)
static void
file_stripext (char *buff)
{
	char	*endp;

	endp = buff + strlen (buff) - 1;
	while (endp > buff) {
		if (*endp == '/' || *endp == '\\') {
			break;
		}
		if (*endp == '.') {
			*endp = 0;
		}
		--endp;
	}
}
#endif

static void
file_basename (const char *filename, char *buff)
{
	const char	*startp;
	const char	*endp;
	size_t		len;

	/* Remove directory name */
	startp = strrchr (filename, '/');
	if (startp) {
		startp++;
	} else {
		startp = filename;
	}

	/* Remove extension */
	endp = strrchr (filename, '.');
	if (endp > startp) {
		len = endp - startp;
	} else {
		len = strlen (startp);
	}

	/* Copy base name */
	strncpy (buff, startp, len);
	buff[len] = '\0';
}

static const char *
file_extension (const char *filename)
{
	const char *p;

	p = strrchr (filename, '.');
	if (p) {
		return p + 1;
	}
	return "";
}

static char *
cobc_temp_name (const char *ext)
{
	char	buff[COB_MEDIUM_BUFF];
#ifdef _WIN32
	char	temp[MAX_PATH];

	GetTempPath (MAX_PATH, temp);
	GetTempFileName (temp, "cob", 0, buff);
	DeleteFile (buff);
	strcpy (buff + strlen (buff) - 4, ext);	/* replace ".tmp" by EXT */
#else
	sprintf (buff, "%s/cob%d_%d%s", cob_tmpdir, (int)cob_process_id,
		 (int)cob_iteration, ext);
#endif
	return strdup (buff);
}

static struct filename *
process_filename (const char *filename)
{
	const char	*extension;
	struct filename	*fn;
	struct filename	*ffn;
	struct stat	st;
	char		basename[COB_SMALL_BUFF];

	if (stat (filename, &st) != 0) {
		cobc_terminate (filename);
	}
	file_basename (filename, basename);
	if (cobc_check_valid_name (basename)) {
		fprintf (stderr, "Invalid file base name - %s\n", basename);
			return NULL;
		}
	fn = cobc_malloc (sizeof (struct filename));
	fn->need_preprocess = 1;
	fn->need_translate = 1;
	fn->need_assemble = 1;
	fn->next = NULL;

	if (!file_list) {
		file_list = fn;
	} else {
		for (ffn = file_list; ffn->next; ffn = ffn->next)
			;
		ffn->next = fn;
	}

	fn->demangle_source =  cb_encode_program_id (basename);
	extension = file_extension (filename);

	/* Check input file type */
	if (strcmp (extension, "i") == 0) {
		/* already preprocessed */
		fn->need_preprocess = 0;
	} else if (strcmp (extension, "c") == 0 ||
		   strcmp (extension, "s") == 0) {
		/* already compiled */
		fn->need_preprocess = 0;
		fn->need_translate = 0;
#if	defined (_MSC_VER)
	} else if (strcmp (extension, "obj") == 0 ||
		   strcmp (extension, "lib") == 0) {
#else
	} else if (strcmp (extension, "o") == 0 ||
#if	defined(__MINGW32__) || defined(__MINGW64__)
		   strcmp (extension, "lib") == 0 ||
#endif
		   strcmp (extension, "a") == 0 ||
		   strcmp (extension, "so") == 0 ||
		   strcmp (extension, "dylib") == 0 ||
		   strcmp (extension, "sl") == 0) {
#endif
		/* already assembled */
		fn->need_preprocess = 0;
		fn->need_translate = 0;
		fn->need_assemble = 0;
	}

	/* Set source filename */
	fn->source = cobc_malloc (strlen (filename) + 3);
	strcpy (fn->source, filename);

	/* Set preprocess filename */
	if (!fn->need_preprocess) {
		fn->preprocess = strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_PREPROCESS) {
		fn->preprocess = strdup (output_name);
	} else if (save_temps) {
		fn->preprocess = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->preprocess, "%s.i", basename);
	} else {
		fn->preprocess = cobc_temp_name (".cob");
	}

	/* Set translate filename */
	if (!fn->need_translate) {
		fn->translate = strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_TRANSLATE) {
		fn->translate = strdup (output_name);
	} else if (save_csrc || save_temps ||
		   cb_compile_level == CB_LEVEL_TRANSLATE) {
		fn->translate = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->translate, "%s.c", basename);
	} else {
		fn->translate = cobc_temp_name (".c");
	}

	/* Set storage filename */
	if (fn->need_translate) {
		fn->trstorage = cobc_malloc (strlen (fn->translate) + 5);
		sprintf (fn->trstorage, "%s.h", fn->translate);
	}

	/* Set object filename */
	if (!fn->need_assemble) {
		fn->object = strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_ASSEMBLE) {
		fn->object = strdup (output_name);
	} else if (save_temps || cb_compile_level == CB_LEVEL_ASSEMBLE) {
#if	defined(_MSC_VER)
		fn->object = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->object, "%s.obj", basename);
#else
		fn->object = cobc_malloc (strlen (basename) + 3);
		sprintf (fn->object, "%s.o", basename);
#endif
	} else {
#if	defined(_MSC_VER)
		fn->object = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->object, "%s.obj", basename);
#else
		fn->object = cobc_temp_name (".o");
#endif
	}

	cob_iteration++;

	return fn;
}

static int
process (const char *cmd)
{
	char	*p;
	char	*buffptr;
	size_t	clen;
	int	ret;
	char	buff[COB_MEDIUM_BUFF];

	if (strchr (cmd, '$') == NULL) {
		if (verbose_output) {
			fprintf (stderr, "%s\n", (char *)cmd);
		}
		return system (cmd);
	}
    	clen = strlen (cmd) + 32;
    	if (clen > COB_MEDIUM_BUFF) {
    		buffptr = cobc_malloc (clen);
    	} else {
    		buffptr = buff;
    	}
    	p = buffptr;
    	/* quote '$' */
	for (; *cmd; cmd++) {
    		if (*cmd == '$') {
    			p += sprintf (p, "\\$");
    		} else {
    			*p++ = *cmd;
    		}
    	}
    	*p = 0;
    
    	if (verbose_output) {
    		fprintf (stderr, "%s\n", buffptr);
    	}
	   ret = system (buffptr);
   	if (buffptr != buff) {
   		free (buffptr);
   	}
	return ret;
}

/* Preprocess source */

static int
preprocess (struct filename *fn)
{
	struct cb_text_list	*l;

	errorcount = 0;

	if (output_name || cb_compile_level > CB_LEVEL_PREPROCESS) {
		ppout = fopen (fn->preprocess, "w");
		if (!ppout) {
			cobc_terminate (fn->preprocess);
		}
	} else {
		ppout = stdout;
	}

	if (ppopen (fn->source, NULL) != 0) {
		if (ppout != stdout) {
			fclose (ppout);
			if (fn->preprocess) {
				unlink (fn->preprocess);
			}
		}
		exit (1);
	}

	if (verbose_output) {
		fprintf (stderr, "preprocessing %s into %s\n", fn->source, fn->preprocess);
	}

	ppparse ();

	if (ppout != stdout) {
		fclose (ppout);
	}
	fclose (ppin);

	if (errorcount > 0) {
		return -1;
	}

	/* Output dependency list */
	if (cb_depend_file) {
		if (!cb_depend_target) {
			fputs (_("-MT must be given to specify target file\n"), stderr);
			exit (1);
		}
		fprintf (cb_depend_file, "%s: \\\n", cb_depend_target);
		for (l = cb_depend_list; l; l = l->next) {
			fprintf (cb_depend_file, " %s%s\n", l->text, l->next ? " \\" : "");
		}
		for (l = cb_depend_list; l; l = l->next) {
			fprintf (cb_depend_file, "%s:\n", l->text);
		}
		fclose (cb_depend_file);
	}

	return 0;
}

static struct cb_program *
program_list_reverse (struct cb_program *p)
{
	struct cb_program	*next;
	struct cb_program	*last = NULL;

	for (; p; p = next) {
		next = p->next_program;
		p->next_program = last;
		last = p;
	}
	return last;
}

static void
print_program_header (void)
{
  if (++cb_listing_linecount >= cb_lines_per_page) {
      char version[20];

      sprintf (version, "%s.%d", PACKAGE_VERSION, PATCH_LEVEL);
      cb_listing_linecount = 0;
      if (cb_listing_eject) {
         fputs ("\f", cb_listing_file);
      } else {
         cb_listing_eject = 1;
      }

      if (cb_listing_wide)
         fprintf (cb_listing_file,
	         "%s %-6.6s  %-65.65s  %s  Page %04d\n\n",
		 PACKAGE_NAME,
	         version,
	         cb_listing_filename,
	         cb_listing_date,
	         ++cb_listing_page);
      else
         fprintf (cb_listing_file,
	         "%s %-6.6s  %-25.25s  %s  Page %04d\n\n",
		 PACKAGE_NAME,
	         version,
	         cb_listing_filename,
	         cb_listing_date,
	         ++cb_listing_page);
      fprintf (cb_listing_file, "%s\n\n", cb_listing_ttl);
    }
}

static void
print_fields (int lvl, struct cb_field *top)
{
	int i;
	int first = 1;
	int getcat;
	int oldlevel = 0;
	char type[20];

	while (top) {
	   if (top->level) {
	      char lclname[80];

	      lclname[0] = '\0';
	      for (i = 0; i < lvl; i++) strcat (lclname, "  ");
	      strcat (lclname, check_filler_name((char *)top->name));
	      getcat = 1;
	      if (top->level == 01) {
	         if (!first) {
		    print_program_header();
		    fprintf (cb_listing_file, "\n");
		 }
		 if (top->children) {
		    strcpy (type, "GROUP");
		    getcat = 0;
		 }
	      }
	      else if (top->level == 77) {
	         if (!first && (oldlevel != 77)) {
		    print_program_header();
		    fprintf (cb_listing_file, "\n");
		 }
	      }
	      if (getcat) switch (top->common.category) {
		 case CB_CATEGORY_ALPHABETIC:
		    strcpy (type, "ALPHABETIC");
		    break;
		 case CB_CATEGORY_UNKNOWN:
		 case CB_CATEGORY_ALPHANUMERIC:
		 case CB_CATEGORY_ALPHANUMERIC_EDITED:
		    strcpy (type, "ALPHANUMERIC");
		    break;
		 case CB_CATEGORY_BOOLEAN:
		    strcpy (type, "BOOLEAN");
		    break;
		 case CB_CATEGORY_INDEX:
		    strcpy (type, "INDEX");
		    break;
		 case CB_CATEGORY_NATIONAL:
		 case CB_CATEGORY_NATIONAL_EDITED:
		    strcpy (type, "NATIONAL");
		    break;
		 case CB_CATEGORY_NUMERIC:
		 case CB_CATEGORY_NUMERIC_EDITED:
		    strcpy (type, "NUMERIC");
		    break;
		 case CB_CATEGORY_OBJECT_REFERENCE:
		    strcpy (type, "REFERENCE");
		    break;
		 case CB_CATEGORY_DATA_POINTER:
		 case CB_CATEGORY_PROGRAM_POINTER:
		    strcpy (type, "POINTER");
		    break;
		 default:
		    strcpy (type, "UNKNOWN");
	      }
	      print_program_header();
	      fprintf (cb_listing_file, "%04d %-14.14s %02d   %-30.30s %s\n",
		     top->size, type, top->level, lclname,
		     top->pic ? top->pic->orig : " ");
	      first = 0;
	      oldlevel = top->level;
	      if (top->children) print_fields (lvl+1, top->children);
	   }
	   top = top->sister;
	}
}

static void
print_program_trailer (void)
{
	cb_tree l;
	char type[20];

	if (cb_listing_file) {

	   /* Print file/symbol tables */

	   strcpy (cb_listing_ttl,
	     "SIZE TYPE           LVL  NAME                           PICTURE");

	   cb_listing_linecount = cb_lines_per_page;
	   print_program_header();

	   if ((l = current_program->file_list) != NULL) {
	      strcpy (type, "FILE");
	      for (; l; l = CB_CHAIN (l)) {
		 print_program_header();
	         fprintf (cb_listing_file, "%04d %-14.14s      %s\n",
			  CB_FILE(CB_VALUE(l))->record_max,
			  type,
			  CB_FILE(CB_VALUE(l))->name);
	         if (CB_FILE(CB_VALUE(l))->record) {
		    print_fields (0, CB_FILE(CB_VALUE(l))->record);
		 }
		 print_program_header();
		 fprintf (cb_listing_file, "\n");
	      }
	   }

	   if (current_program->working_storage != NULL) {
	      print_fields (0, current_program->working_storage);
	      print_program_header();
	      fprintf (cb_listing_file, "\n");
	   }

	   if (current_program->local_storage != NULL) {
	      print_fields (0, current_program->local_storage);
	      print_program_header();
	      fprintf (cb_listing_file, "\n");
	   }

	   /* Print error counts */

	   fprintf (cb_listing_file, "%d Warnings in program\n", warningcount);
	   fprintf (cb_listing_file, "%d Errors in program\n", errorcount);
	   cb_listing_linecount = cb_lines_per_page;
	}
}

static char *
print_token (char *bp, char *token, char *term)
{
   char *otoken = token;

   do {
      while (*bp && isspace (*bp)) bp++;
      term[0] = 0;
      term[1] = 0;
      if (*bp == 0)
	 return NULL;
      while (*bp)
      {
	 if (*bp == '&') {
	    bp++;
	    continue;
	 }
	 if (isspace(*bp) || *bp == ',' || *bp == '.' || *bp == ';') {
	    term[0] = *bp++;
	    break;
	 }
	 *token++ = *bp++;
      }
      *token = 0;
   } while (*otoken == 0 && *term != 0);
   return (bp);
}

static int
print_read (FILE *fd, char *line, int fixed)
{
   char *pl, *il;
   int i;
   char iline[CB_LINE_LENGTH+2];

   memset (line, 0, CB_LINE_LENGTH);
   if (!fgets (iline, CB_LINE_LENGTH, fd)) {
      return -1;
   }

   if ((pl = strchr (iline, '\n')) != NULL) *pl = '\0';
   if ((pl = strchr (iline, '\r')) != NULL) *pl = '\0';

   il = iline;
   pl = line;
   for (i = 0; *il; il++, i++) {
      if (*il == '\t') {
	 int j, k;

	 k = 8 - (i % 8);
	 for (j = 0; j < k; j++) *pl++ = ' ';
      }
      else {
	 *pl++ = *il;
      }
   }
   if (fixed) {
      for (i = (pl - line); i < 80; i++);
         *pl++ = ' ';
   }
   else {
      *pl++ = ' ';
   }
   *pl = 0;
   return strlen(line);
}

static void
print_line (struct list_files *cfile, char *line, int linenum, int incopy, 
	    int fixed)
{
   char pch;
   char buffer[133];

   if (fixed && line[CB_INDICATOR] == '/')
     cb_listing_linecount = cb_lines_per_page;
   if (!fixed && !strncasecmp (line, ">>PAGE", 6))
     cb_listing_linecount = cb_lines_per_page;

   pch = incopy ? 'C' : ' ';
   if (fixed) {
      int j;

      if (line[CB_INDICATOR] == '&') {
         line[CB_INDICATOR] = '-';
	 pch = '+';
      }
      print_program_header();
      if (cb_listing_wide)
	 sprintf (buffer, "%05d%c %s", linenum, pch, line);
      else
	 sprintf (buffer, "%05d%c %-72.72s", linenum, pch, line);
      for (j = strlen(buffer) - 1; j; j--) {
         if (buffer[j] != ' ')
	    break;
         buffer[j] = 0;
      }
      fprintf (cb_listing_file, "%s\n", buffer);
   }
   else {
      int i, j;
      int len = strlen(line);
      int max = cb_listing_wide ? 112 : 72;
      for (i = 0; len > 0; i += max, len -= max) {
	 print_program_header();
	 if (cb_listing_wide)
	    sprintf (buffer, "%05d%c %-112.112s",
		     linenum, pch, &line[i]);
	 else
	    sprintf (buffer, "%05d%c %-72.72s",
		     linenum, pch, &line[i]);
	 for (j = strlen(buffer) - 1; j; j--) {
	    if (buffer[j] != ' ')
	       break;
	    buffer[j] = 0;
	 }
	 fprintf (cb_listing_file, "%s\n", buffer);
         pch = '+';
      }
   }
   if (cfile->err_head) {
      struct list_error *err;
      err = cfile->err_head;
      if (err->line == linenum) {
	 print_program_header();
	 fprintf (cb_listing_file, "%s%s\n", err->prefix, err->msg);
	 cfile->err_head = err->next;
	 free (err);
      }
   }
}

static int
compare_prepare (char *cmpline, char pline[CB_READ_AHEAD][CB_LINE_LENGTH+2],
		 int firstndx, int lastndx, int firstcol, int fixed)
{
   int i, j, k;
   int instring, lastcol;
   int last;

   cmpline[0] = 0;
   j = 0;
   instring = 0;
   lastcol = CB_SEQUENCE;
   for (k = firstndx; k < lastndx; k++) {
      if (!fixed) {
	 lastcol = strlen(pline[k]);
      }
      for (last = lastcol; isspace(pline[k][last-1]) && last > firstcol;
	   last--) ;
      for (i = firstcol; (i < last) && isspace(pline[k][i]); i++) ;
      while (i < last) {
	 if (isspace(pline[k][i])) {
	    cmpline[j++] = ' ';
	    for (i++; (i < last) && isspace(pline[k][i]); i++) ;
	    if (i == last) break;
	 }
	 else if (pline[k][i] == '"') {
	    if (instring) {
	       i++;
	    }
	    else {
	       cmpline[j++] = pline[k][i++];
	       instring = 1;
	    }
	    for (; (i < last) && (pline[k][i] != '"'); ) {
	       cmpline[j++] = pline[k][i++];
	    }
	    if (pline[k][i] == '"') {
	       cmpline[j++] = pline[k][i++];
	       instring = 0;
	    }
	    if (i == last) break;
	 }
	 else {
	    cmpline[j++] = pline[k][i++];
	 }
      }
   }
   cmpline[j] = 0;
#ifdef DEBUG_REPLACE
   fprintf (stdout, "   lastcol = %d\n   cmpline: %s\n", lastcol, cmpline);
#endif
   return lastcol;
}

static void
print_adjust (struct list_files *cfile, int linenum, int adjust)
{
   if (cfile->copy_head) {
      struct list_files *cur;
      for (cur = cfile->copy_head; cur; cur = cur->next) {
	 cur->copy_line += adjust;
      }
   }
   if (cfile->replace_head) {
      struct list_replace *rep;
      rep = cfile->replace_head;
      for (; rep; rep = rep->next) {
	 if (rep->firstline > linenum)
	    rep->firstline += adjust;
      }
   }
   if (cfile->err_head) {
      struct list_error *err;
      err = cfile->err_head;
      for (; err; err = err->next) {
	 err->line += adjust;
      }
   }
}

static int
is_debug_line (char *line, int fixed)
{
   if (fixed && !cb_flag_debugging_line && IS_DEBUG_LINE(line))
      return 1;
   if (!fixed && !cb_flag_debugging_line && !strncasecmp (line, "D ", 2))
      return 1;
   return 0;
}

static int
is_comment_line (char *line, int fixed)
{
   if (fixed && IS_COMMENT_LINE(line))
      return 1;
   if (!fixed && !strncmp (line, "*>", 2))
      return 1;
   return 0;
}

static int
is_continue_line (char *line, int fixed)
{
   if (fixed && IS_CONTINUE_LINE(line))
      return 1;
   if (!fixed) {
      int i = strlen(line) - 1;
      for (; i && isspace(line[i]); i--) ;
      if (line[i] == '&')
	 return 1;
   }
   return 0;
}

static int
print_replace_text (struct list_files *cfile, FILE *fd,
		    struct list_replace *rep,
		    char pline[CB_READ_AHEAD][CB_LINE_LENGTH+2],
		    int plinecnt, int linenum)
{
   char *rfp;
   char *fp;
   char *tp;
   int i, j, k;
   int firstcol, last;
   int multi_token, fixed;
   int match = 0;
   int eof;
   char fterm[2], ftoken[CB_LINE_LENGTH+2];
   char tterm[2], ttoken[CB_LINE_LENGTH+2];
   char cmpline[CB_LINE_LENGTH+2];
   char newline[CB_LINE_LENGTH+2];
   char frmline[CB_LINE_LENGTH+2];

   fixed = (cb_source_format == CB_FORMAT_FIXED);
   if (is_comment_line (pline[0], fixed))
      return plinecnt;

   rfp = rep->from;
   for (i = strlen(rfp)-1; i && isspace(rfp[i]); i--) rfp[i] = 0;
   while (*rfp && isspace(*rfp)) rfp++;
   multi_token = (strchr (rfp, ' ') != NULL);
   eof = 0;

#ifdef DEBUG_REPLACE
   fprintf (stdout, "print_replace_text: linenum = %d", linenum);
   fprintf (stdout, ", multi_token = %s, fixed = %s\n",
	    multi_token ? "TRUE" : "FALSE", fixed ? "TRUE" : "FALSE"); 
   fprintf (stdout, "   plinecnt = %d\n", plinecnt);
   for (i = 0; i < plinecnt; i++)
      fprintf (stdout, "   pline[%2d]: %s\n", i, pline[i]);
   fprintf (stdout, "   rep: first = %d, last = %d\n",
	    rep->firstline, rep->lastline);
   fprintf (stdout, "   from: '%s'\n", rfp);
   fprintf (stdout, "   to:   '%s'\n", rep->to);
#endif

   firstcol = 0;
   if (fixed) {
      firstcol = CB_MARGIN_A;
   }

   last = compare_prepare (cmpline, pline, 0, plinecnt, firstcol, fixed);

   k = 0;
   newline[0] = 0;
   if (multi_token) {
      int submatch = 0;
      int seccount = 0;
      char lterm[2];

      strcpy (frmline, rfp);
      fp = print_token (frmline, ftoken, fterm);
   NEXT_LINE:
      tp = print_token (cmpline, ttoken, tterm);
      while (tp && fp) {
	 while (tp && fp) {
	    if (!strcasecmp (ttoken, ftoken)) {
	       submatch = 1;
	       if (fterm[0] == tterm[0])
		  lterm[0] = 0;
	       else
		  lterm[0] = tterm[0];
	       lterm[1] = tterm[1];
	       tp = print_token (tp, ttoken, tterm);
	       fp = print_token (fp, ftoken, fterm);
	    }
	    else {
	       if (seccount == 0) {
		  strcat (newline, ttoken);
		  strcat (newline, tterm);
	       }
	       submatch = 0;
	       strcpy (frmline, rfp);
	       fp = print_token (frmline, ftoken, fterm);
	       tp = print_token (tp, ttoken, tterm);
	       break;
	    }
	 }
      }
      if (!fp && submatch) {
         match = 1;
	 strcat (newline, rep->to);
	 strcat (newline, lterm);
	 if (tp) {
	    strcat (newline, ttoken);
	    strcat (newline, tterm);
	    strcat (newline, tp);
	 }
      }
      else if (!tp && submatch) {
	 int overread = 0;
#ifdef DEBUG_REPLACE
	 fprintf (stdout, "   submatch = TRUE\n");
#endif
         if (eof) {
	    return plinecnt;
	 }
	 if (is_comment_line (pline[plinecnt], fixed)) {
	    print_adjust (cfile, linenum, -1);
	    overread = 1;
	 }
	 if (is_debug_line (pline[plinecnt], fixed)) {
	    print_adjust (cfile, linenum, -1);
	    overread = 1;
	 }
      NEXT_REC:
	 if (!is_comment_line (pline[plinecnt], fixed))
	    plinecnt++;
	 if (print_read (fd, pline[plinecnt], fixed) < 0) {
	    pline[plinecnt][0] = 0;
	    eof = 1;
	 }
	 if (is_debug_line (pline[plinecnt], fixed)) {
	    print_adjust (cfile, linenum, -1);
	    goto NEXT_REC;
	 }
	 if (is_comment_line (pline[plinecnt], fixed)) {
	    print_adjust (cfile, linenum, -1);
	    goto NEXT_REC;
	 }
#ifdef DEBUG_REPLACE
	 fprintf (stdout, "   pline[%2d]: %s\n", plinecnt-1,
		  pline[plinecnt-1]);
#endif
	 linenum++;
	 seccount++;
	 if (overread) {
	    overread = 0;
	    goto NEXT_REC;
	 }
	 last = compare_prepare (cmpline, pline, plinecnt-1, plinecnt,
				 firstcol, fixed);
	 strcat (newline, " ");
	 goto NEXT_LINE;
      }
   }

   else {
      int tokmatch = 0;
      int subword = 0;

      strcpy (frmline, rfp);
      fp = print_token (frmline, ftoken, fterm);
      if (ftoken[0] == ':' || ftoken[0] == '(')
         subword = 1;
      for (tp = print_token (cmpline, ttoken, tterm); tp;
           tp = print_token (tp, ttoken, tterm)) {
#ifdef DEBUG_REPLACE
	 fprintf (stdout, "   tterm = '%s', ttoken = '%s', ftoken = '%s'\n",
		  tterm, ttoken, ftoken);
#endif
	 if (subword) {
            tokmatch = !strncasecmp (ttoken, ftoken, strlen(ftoken));
	 } else {
            tokmatch = !strcasecmp (ttoken, ftoken);
	 }
         if (tokmatch) {
	    strcat (newline, rep->to);
	    if (subword) {
	       strcat (newline, &ttoken[strlen(ftoken)]);
	    }
	    strcat (newline, tterm);
	    match = 1;
	 }
	 else {
	    strcat (newline, ttoken);
	    strcat (newline, tterm);
	 }
      }
   }

   if (match) {
#ifdef DEBUG_REPLACE
      fprintf (stdout, "   match = TRUE\n   newline = %s\n", newline);
#endif
      if (fixed) {
	 fp = print_token (newline, ftoken, fterm);
	 for (j = firstcol; (j < last) && isspace(pline[k][j]); j++) ;
	 if (j >= CB_MARGIN_B) firstcol = CB_MARGIN_B;
	 for (k = 0; k < plinecnt; k++) {
	    int nextrec;
	    nextrec = 0;
	    j = firstcol;
	    while (fp && !nextrec) {
	       int ftlen;
	       ftlen = strlen(ftoken);
	       i = 0;
	       if (ftlen >= (CB_SEQUENCE - firstcol)) {
#ifdef DEBUG_REPLACE
		  fprintf (stdout, "   ftlen = %d\n", ftlen);
#endif
		  while (ftlen) {
		     pline[k][j++] = ftoken[i++];
		     ftlen--;
		     if (j == CB_SEQUENCE) {
#ifdef DEBUG_REPLACE
			fprintf (stdout, "   NEW pline[%2d] = %s\n",
				 k, pline[k]);
#endif
			j = firstcol;
			k++;
			if (k == plinecnt) {
			   strcpy (pline[plinecnt+1], pline[plinecnt]);
			   strcpy (pline[plinecnt], pline[plinecnt-1]);
			   memset (&pline[plinecnt][CB_MARGIN_A], ' ', 
				   CB_SEQUENCE - CB_MARGIN_A);
			   pline[plinecnt][CB_INDICATOR] = '&';
			   plinecnt++;
			}
		     }
		  }
		  pline[k][j++] = ' ';
	       }
	       else {
		  if ((j + 2 + ftlen) < last) {
		     for (i = 0; i < strlen(ftoken); i++)
			pline[k][j++] = ftoken[i];
		     pline[k][j++] = fterm[0] ? fterm[0] : ' ';
		     if (fterm[0] == '.')
			pline[k][j++] = ' ';
		  }
		  else
		     nextrec = 1;
	       }
	       fp = print_token (fp, ftoken, fterm);
	    }
	    if (j == firstcol) 
	       pline[k][CB_INDICATOR] = ' ';
	    while (j < last) pline[k][j++] = ' ';
#ifdef DEBUG_REPLACE
	    fprintf (stdout, "   NEW pline[%2d] = %s\n", k, pline[k]);
#endif
	 }
      }
      else {
	 fp = print_token (newline, ftoken, fterm);
	 for (k = 0; k < plinecnt; k++) {
	    j = firstcol;
	    pline[k][j] = 0;
	    while (fp) {
	       strcat (pline[k], ftoken);
	       strcat (pline[k], fterm);
	       j++;
	       fp = print_token (fp, ftoken, fterm);
	    }
	    if (j == firstcol) {
	       strcat (pline[k], " ");
	    }
	 }
      }
   }

   return plinecnt;
}

static int
print_replace_main (struct list_files *cfile, FILE *fd,
		    char pline[CB_READ_AHEAD][CB_LINE_LENGTH+2],
		    int plinecnt, int linenum)
{
   char *tp;
   struct list_replace *rep;
   int i, fixed, firstcol;
   int iscopy, isreplace, isoff;
   char tterm[2], ttoken[CB_LINE_LENGTH+2];
   char cmpline[CB_LINE_LENGTH+2];

   fixed = (cb_source_format == CB_FORMAT_FIXED);
   if (is_comment_line (pline[0], fixed))
      return plinecnt;

   firstcol = 0;
   if (fixed)
      firstcol = CB_MARGIN_A;

#ifdef DEBUG_REPLACE
   fprintf (stdout, "print_replace_main: linenum = %d\n", linenum);
   fprintf (stdout, "   plinecnt = %d\n", plinecnt);
   for (i = 0; i < plinecnt; i++)
      fprintf (stdout, "   pline[%2d]: %s\n", i, pline[i]);
#endif

   compare_prepare (cmpline, pline, 0, plinecnt, firstcol, fixed);
   tp = print_token (cmpline, ttoken, tterm);
   isreplace = !strcasecmp (ttoken, "REPLACE");
   iscopy = !strcasecmp (ttoken, "COPY");
   isoff = 0;
   if (isreplace) {
      tp = print_token (tp, ttoken, tterm);
      isoff = !strcasecmp (ttoken, "OFF");
   }

   if (!cb_inreplace && isreplace) {
	 if (!isoff) {
	    cb_inreplace = 1;
#ifdef DEBUG_REPLACE
            for (i = 0, rep = cfile->replace_head; rep; i++, rep = rep->next) {
	       if (rep->firstline < (linenum + 10)) {
		  if (i == 0)
		     fprintf (stdout, "   replace_list: \n");
		  fprintf (stdout, "      line[%d]: %d\n", i, rep->firstline);
		  fprintf (stdout, "      from[%d]: '%s'\n", i, rep->from);
		  fprintf (stdout, "      to  [%d]: '%s'\n", i, rep->to);
	       }
	    }
#endif
	 }
   }
   else if (cb_inreplace) {

      if (isreplace) {
	 if (isoff) {
	    cb_inreplace = 0;
	    rep = cfile->replace_head;
	    while (rep && rep->firstline < linenum) {
	       cfile->replace_head = rep->next;
	       free (rep);
	       rep = cfile->replace_head;
	    }
	 }
      }
      if (cb_inreplace) {
	 if (iscopy) {
	    if (cfile->copy_head) {
	       struct list_files *cur;

	       for (i = 0; i < plinecnt; i++) {
		  print_line (cfile, pline[i], linenum + i, 0, fixed);
		  pline[i][0] = 0;
	       }
	       cur = cfile->copy_head;
	       if (!cur->replace_head) {
		  struct list_replace *repl;

		  rep = cfile->replace_head;
		  while (rep && rep->firstline <= linenum) {
		     repl = cobc_malloc (sizeof (struct list_replace));
		     memcpy (repl, rep, sizeof (struct list_replace));
		     repl->next = NULL;
		     if (cur->replace_tail) {
			cur->replace_tail->next = repl;
		     }
		     if (!cur->replace_head) {
			cur->replace_head = repl;
		     }
		     cur->replace_tail = repl;
		     rep = rep->next;
		  }
	       }
	       print_program_code (cur, 1);
	       cfile->copy_head = cur->next;
	       free (cur);
	    }
	 }
	 else {
	    rep = cfile->replace_head;
	    while (rep && rep->firstline < linenum) {
	       plinecnt = print_replace_text (cfile, fd, rep, pline,
					      plinecnt, linenum);
	       rep = rep->next;
	    }
	 }
      }
   }
   return plinecnt;
}

static void
print_program_code (struct list_files *cfile, int incopy)
{
   FILE *fd;
   struct list_replace *rep;
   struct list_files *cur;
   int i;
   int linenum;
   int fixed, done, eof;
   int plinecnt;
   char pline[CB_READ_AHEAD][CB_LINE_LENGTH+2];

   fixed = (cb_source_format == CB_FORMAT_FIXED);
   if (cb_listing_file) {
#ifdef DEBUG_REPLACE
      struct list_error *err;

      fprintf (stdout, "print_program_code: incopy = %s\n",
	       incopy ? "YES" : "NO");
      fprintf (stdout, "   name: %s\n", cfile->name);
      fprintf (stdout, "   copy_line: %d\n", cfile->copy_line);
      for (i = 0, cur = cfile->copy_head; cur; i++, cur = cur->next) {
	 if (i == 0)
	    fprintf (stdout, "   copy_books: \n");
         fprintf (stdout, "      name[%d]: %s\n", i, cur->name);
         fprintf (stdout, "      line[%d]: %d\n", i, cur->copy_line);
      }
      for (i = 0, rep = cfile->replace_head; rep; i++, rep = rep->next) {
	 if (i == 0)
	    fprintf (stdout, "   replace_list: \n");
         fprintf (stdout, "      line[%d]: %d\n", i, rep->firstline);
         fprintf (stdout, "      from[%d]: '%s'\n", i, rep->from);
         fprintf (stdout, "      to  [%d]: '%s'\n", i, rep->to);
      }
      for (i = 0, err = cfile->err_head; err; i++, err = err->next) {
	 if (i == 0)
	    fprintf (stdout, "   error_list: \n");
         fprintf (stdout, "      line[%d]: %d\n", i, err->line);
         fprintf (stdout, "      pref[%d]: '%s'\n", i, err->prefix);
         fprintf (stdout, "      msg [%d]: '%s'\n", i, err->msg);
      }
#endif
      if ((fd = fopen (cfile->name, "r")) != NULL) {
 
 	 linenum = 1;
  	 plinecnt = 0;
  
  	 eof = 0;
  	 done = 0;
  	 if (print_read (fd, pline[plinecnt], fixed) >= 0) {
	    int prec;

	    while (!done) {
	    NEXT_REC:
	       if (print_read (fd, pline[plinecnt+1], fixed) < 0) {
		  if (eof) {
		     done = 1;
		     break;
		  }
		  eof = 1;
	       }
	       plinecnt++;
	       if (is_continue_line (pline[fixed ? plinecnt : plinecnt-1],
				     fixed)) {
		  goto NEXT_REC;
	       }

	       if (!incopy) {
		  plinecnt = print_replace_main (cfile, fd, pline, plinecnt,
						 linenum);
	       }
	       else {
		  if (cfile->replace_head) {
		     rep = cfile->replace_head;
		     while (rep) {
			plinecnt = print_replace_text (cfile, fd, rep, pline,
						       plinecnt, linenum);
			rep = rep->next;
		     }
		  }
	       }

	       prec = 0;
	       for (i = 0; i < plinecnt; i++) {
		  if (pline[i][0]) {
		     if (pline[i][CB_INDICATOR] == '&') {
		       print_line (cfile, pline[i], linenum, incopy, fixed);
		     }
		     else {
		       print_line (cfile, pline[i], linenum + i, incopy, fixed);
		       prec++;
		     }
		  }
	       }

	       if (cfile->copy_head) {
		  cur = cfile->copy_head;
		  if (cur->copy_line == linenum) {
		     struct list_replace *repl;

		     rep = cfile->replace_head;
		     /*  COPY in COPY, add replacement text to new COPY */
		     while (rep && incopy) {
		        repl = cobc_malloc (sizeof (struct list_replace));
		        memcpy (repl, rep, sizeof (struct list_replace));
		        repl->next = NULL;
		        if (cur->replace_tail) {
			   cur->replace_tail->next = repl;
		        }
		        if (!cur->replace_head) {
			   cur->replace_head = repl;
		        }
		        cur->replace_tail = repl;
		        rep = rep->next;
		     }
		     print_program_code (cur, 1);
		     cfile->copy_head = cur->next;
		     free (cur);
		  }
	       }
	       strcpy (pline[0], pline[plinecnt]);
	       linenum += prec;
	       plinecnt = 0;
	       if (pline[0][0] == 0)
	          eof = 1;
	    }
	 }
      }
   }
}

static int
process_translate (struct filename *fn)
{
	struct cb_program	*p;
	struct cb_program	*q;
	struct cb_program	*r;
	struct handler_struct	*hstr1;
	struct handler_struct	*hstr2;
	struct local_filename	*lf;
	struct list_files       *cfile;
	int			ret;
	int			i;

	/* initialize */
	cb_source_file = NULL;
	cb_source_line = 0;
	cb_init_constants ();
	cb_init_reserved ();

	cb_listing_page = 0;
        strcpy (cb_listing_filename, fn->source);
	strcpy (cb_listing_ttl, "LINE   ");
	if (cb_source_format == CB_FORMAT_FIXED) {
	   strcat (cb_listing_ttl,
		   "PG/LN  A...B......................................"
		   "......................");
	   if (cb_listing_wide) {
		   strcat (cb_listing_ttl, "SEQUENCE");
	   }
	}
	else {
	   if (cb_listing_wide) {
	      strcat (cb_listing_ttl, "................................");
	   }
	   strcat (cb_listing_ttl,
		   ".....................SOURCE......................."
		   "......................");
	   if (cb_listing_wide) {
		   strcat (cb_listing_ttl, "........");
	   }
	}


	/* open the input file */
	yyin = fopen (fn->preprocess, "r");
	if (!yyin) {
		cobc_terminate (fn->preprocess);
	}

	if (verbose_output) {      
		fprintf (stderr, "parsing %s (%s)\n", fn->preprocess, fn->source);
	}

	/* parse */
	ret = yyparse ();
	fclose (yyin);

	if (verbose_output) {
		fputs (_("Return status:"), stderr);
		fprintf (stderr, "\t%d\n", ret);
		fflush (stderr);
	}
 
	/* Print the listing for this file */
	cfile = cb_listing_files;
	print_program_code(cfile, 0);
	cb_listing_files = cb_listing_files->next;
	free (cfile);

	if (ret) {
		print_program_trailer();
		return ret;
	}
	if (cb_flag_syntax_only || current_program->entry_list == NULL) {
		print_program_trailer();
		return 0;
	}

	/* Set up USE GLOBAL handlers */
	p = current_program;
	for (q = p; q; q = q->next_program) {
		q->global_file_list = cb_list_reverse (q->global_file_list);
		if (q->nested_level) {
			for (r = q->next_program; r; r = r->next_program) {
				if (r->nested_level >= q->nested_level) {
					continue;
				}
				for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++) {
					hstr1 = &q->global_handler[i];
					hstr2 = &r->global_handler[i];
					if (!hstr1->handler_label &&
					    hstr2->handler_label &&
					    hstr2->handler_label->is_global) {
						hstr1->handler_label = hstr2->handler_label;
						hstr1->handler_prog = r;
					}
				}
				if (!r->nested_level) {
					break;
				}
			}
		}
	}

	if (verbose_output) {
		fprintf (stderr, "translating %s into %s (%s)\n",
			 fn->preprocess, fn->translate, fn->source);
	}

	/* open the output file */
	yyout = fopen (fn->translate, "w");
	if (!yyout) {
		cobc_terminate (fn->translate);
	}

	/* open the common storage file */
	cb_storage_file_name = fn->trstorage;
	cb_storage_file = fopen (cb_storage_file_name, "w");
	if (!cb_storage_file) {
		cobc_terminate (cb_storage_file_name);
	}

	p = program_list_reverse (current_program);

	/* set up local storage files */
	ret = 1;
	for (q = p; q; q = q->next_program, ret++) {
		lf = cobc_malloc (sizeof(struct local_filename));
		lf->local_name = cobc_malloc (strlen (fn->translate) + 9);
		if (q == p && !q->next_program) {
			sprintf (lf->local_name, "%s.l.h", fn->translate);
		} else {
			sprintf (lf->local_name, "%s.l%d.h", fn->translate, ret);
		}
		lf->local_fp = fopen (lf->local_name, "w");
		if (!lf->local_fp) {
			cobc_terminate (lf->local_name);
		}
		q->local_storage_file = lf->local_fp;
		q->local_storage_name = lf->local_name;
		lf->next = fn->localfile;
		fn->localfile = lf;
	}

	/* translate to C */
	codegen (p, 0);

	/* Print program trailer */
	print_program_trailer();

	/* close the files */
	fclose (cb_storage_file);
	fclose (yyout);
	for (q = p; q; q = q->next_program) {
		fclose (q->local_storage_file);
	}
	return 0;
}

static int
process_compile (struct filename *fn)
{
	char	buff[COB_MEDIUM_BUFF];
	char	name[COB_MEDIUM_BUFF];

	if (output_name) {
		strcpy (name, output_name);
#ifdef _MSC_VER
		file_stripext(name);
#endif
	} else {
		file_basename (fn->source, name);
#ifndef _MSC_VER
		strcat (name, ".s");
#endif
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /c %s %s /Od /MDd /Zi /FR /c /Fa\"%s\" /Fo\"%s\" %s" :
		"%s /c %s %s /MD /c /Fa\"%s\" /Fo\"%s\" %s",
			cob_cc, cob_cflags, cob_define_flags, name,
			name, fn->translate);
#else
	sprintf (buff, "%s %s -S -o \"%s\" %s %s %s", cob_cc, gccpipe, name,
			cob_cflags, cob_define_flags, fn->translate);
#endif
	return process (buff);
}

/* Create single-element assembled object */

static int
process_assemble (struct filename *fn)
{
	char buff[COB_MEDIUM_BUFF];

#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /c %s %s /Od /MDd /Zi /FR /Fo\"%s\" \"%s\"" :
		"%s /c %s %s /MD /Fo\"%s\" \"%s\"",
			cob_cc, cob_cflags, cob_define_flags,
			fn->object, fn->translate);
#else
	if (cb_compile_level == CB_LEVEL_MODULE ||
	    cb_compile_level == CB_LEVEL_LIBRARY) {
		sprintf (buff, "%s %s -c %s %s %s -o \"%s\" \"%s\"",
			 cob_cc, gccpipe, cob_cflags, cob_define_flags,
			 COB_PIC_FLAGS, fn->object, fn->translate);
	} else {
		sprintf (buff, "%s %s -c %s %s -o \"%s\" \"%s\"",
			 cob_cc, gccpipe, cob_cflags, cob_define_flags,
			 fn->object, fn->translate);
	}
#endif
	return process (buff);
}

static int
process_module_direct (struct filename *fn)
{
	int	ret;
	char	buff[COB_MEDIUM_BUFF];
	char	name[COB_MEDIUM_BUFF];

	if (output_name) {
		strcpy (name, output_name);
#if	defined(_MSC_VER)
		file_stripext (name);
#else
		if (strchr (output_name, '.') == NULL) {
			strcat (name, ".");
			strcat (name, COB_MODULE_EXT);
		}
#endif
	} else {
		file_basename (fn->source, name);
#if	!defined(_MSC_VER)
		strcat (name, ".");
		strcat (name, COB_MODULE_EXT);
#endif
	}


#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s %s %s /Od /MDd /LDd /Zi /FR /Fe\"%s\" /Fo\"%s\" %s \"%s\" %s" :
		"%s %s %s /MD /LD /Fe\"%s\" /Fo\"%s\" %s \"%s\" %s",
			cob_cc, cob_cflags, cob_define_flags, name, name,
			cob_ldflags, fn->translate, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff,
			 "%s /manifest \"%s.dll.manifest\" /outputresource:\"%s.dll\";#2",
			 manicmd, name, name);
		ret = process (buff);
		sprintf (buff, "%s.dll.manifest", name);
		cobc_check_action (buff);
	}
#endif
	sprintf (buff, "%s.exp", name);
	cobc_check_action (buff);
	sprintf (buff, "%s.lib", name);
	cobc_check_action (buff);
#else	/* _MSC_VER */
	sprintf (buff, "%s %s %s %s %s %s %s %s -o %s %s %s",
		 cob_cc, gccpipe, cob_cflags, cob_define_flags, COB_SHARED_OPT,
		 cob_ldflags, COB_PIC_FLAGS, COB_EXPORT_DYN, name,
		 fn->translate, cob_libs);
	ret = process (buff);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s \"%s\"", COB_STRIP_CMD, name);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

/* Create single-element loadable object */

static int
process_module (struct filename *fn)
{
	int	ret;
	char	buff[COB_MEDIUM_BUFF];
	char	name[COB_MEDIUM_BUFF];

	if (output_name) {
		strcpy (name, output_name);
#if	defined(_MSC_VER)
		file_stripext (name);
#else
		if (strchr (output_name, '.') == NULL) {
			strcat (name, ".");
			strcat (name, COB_MODULE_EXT);
		}
#endif
	} else {
		file_basename (fn->source, name);
#if	!defined(_MSC_VER)
		strcat (name, ".");
		strcat (name, COB_MODULE_EXT);
#endif
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /Od /MDd /LDd /Zi /FR /Fe\"%s\" %s \"%s\" %s" :
		"%s /MD /LD /Fe\"%s\" %s \"%s\" %s",
			cob_cc, name, cob_ldflags, fn->object, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff,
			 "%s /manifest \"%s.dll.manifest\" /outputresource:\"%s.dll\";#2",
			 manicmd, name, name);
		ret = process (buff);
		sprintf (buff, "%s.dll.manifest", name);
		cobc_check_action (buff);
	}
#endif
	sprintf (buff, "%s.exp", name);
	cobc_check_action (buff);
	sprintf (buff, "%s.lib", name);
	cobc_check_action (buff);
#else	/* _MSC_VER */
	sprintf (buff, "%s %s %s %s %s %s -o %s %s %s",
		 cob_cc, gccpipe, COB_SHARED_OPT, cob_ldflags, COB_PIC_FLAGS,
		 COB_EXPORT_DYN, name, fn->object, cob_libs);
	ret = process (buff);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s", COB_STRIP_CMD, name);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

static int
process_library (struct filename *l)
{
	char		*buffptr;
	char		*objsptr;
	struct filename	*f;
	size_t		bufflen;
	int		ret;
	char		buff[COB_MEDIUM_BUFF];
	char		name[COB_MEDIUM_BUFF];
	char		objs[COB_MEDIUM_BUFF] = "\0";

	bufflen = 0;
	for (f = l; f; f = f->next) {
		bufflen += strlen (f->object) + 2;
	}
	if (bufflen >= COB_MEDIUM_BUFF) {
		objsptr = cobc_malloc (bufflen);
	} else {
		objsptr = objs;
	}
	for (f = l; f; f = f->next) {
#ifdef _MSC_VER
		strcat (objsptr, "\"");
#endif
		strcat (objsptr, f->object);
#ifdef _MSC_VER
		strcat (objsptr, "\"");
#endif
		strcat (objsptr, " ");
	}

	if (output_name) {
		strcpy (name, output_name);
#ifdef _MSC_VER
		file_stripext(name);
#else
		if (strchr (output_name, '.') == NULL) {
			strcat (name, ".");
			strcat (name, COB_MODULE_EXT);
		}
#endif
	} else {
		file_basename (l->source, name);
#ifndef _MSC_VER
		strcat (name, ".");
		strcat (name, COB_MODULE_EXT);
#endif
	}

	bufflen = strlen (cob_cc) + strlen (gccpipe) + strlen (cob_ldflags)
			+ strlen (COB_EXPORT_DYN) + strlen (COB_SHARED_OPT)
			+ strlen (name) + strlen (objsptr) + strlen (cob_libs)
			+ strlen (COB_PIC_FLAGS) + 16;
	if (bufflen >= COB_MEDIUM_BUFF) {
		buffptr = cobc_malloc (bufflen);
	} else {
		buffptr = buff;
	}

#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /Od /MDd /LDd /Zi /FR /Fe\"%s\" %s %s %s" :
		"%s /MD /LD /Fe\"%s\" %s %s %s",
			cob_cc, name, cob_ldflags, objsptr, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff,
			 "%s /manifest \"%s.dll.manifest\" /outputresource:\"%s.dll\";#2",
			 manicmd, name, name);
		ret = process (buff);
		sprintf (buff, "%s.dll.manifest", name);
		cobc_check_action (buff);
	}
#endif
	sprintf (buff, "%s.exp", name);
	cobc_check_action (buff);
	sprintf (buff, "%s.lib", name);
	cobc_check_action (buff);
#else	/* _MSC_VER */
	sprintf (buffptr, "%s %s %s %s %s %s -o %s %s %s",
		 cob_cc, gccpipe, COB_SHARED_OPT, cob_ldflags, COB_PIC_FLAGS,
		 COB_EXPORT_DYN, name, objsptr, cob_libs);
	ret = process (buffptr);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s", COB_STRIP_CMD, name);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

static int
process_link (struct filename *l)
{
	char		*buffptr;
	char		*objsptr;
	struct filename	*f;
	size_t		bufflen;
	int		ret;
	char		buff[COB_MEDIUM_BUFF];
	char		name[COB_MEDIUM_BUFF];
	char		objs[COB_MEDIUM_BUFF] = "\0";

	bufflen = 0;
	for (f = l; f; f = f->next) {
		bufflen += strlen (f->object) + 2;
	}
	if (bufflen >= COB_MEDIUM_BUFF) {
		objsptr = cobc_malloc (bufflen);
	} else {
		objsptr = objs;
	}
	for (f = l; f; f = f->next) {
#ifdef _MSC_VER
		strcat (objsptr, "\"");
#endif
		strcat (objsptr, f->object);
#ifdef _MSC_VER
		strcat (objsptr, "\"");
#endif
		strcat (objsptr, " ");
	}

	if (output_name) {
		strcpy (name, output_name);
#ifdef _MSC_VER
		file_stripext(name);
#endif
	} else {
		file_basename (l->source, name);
	}

	bufflen = strlen (cob_cc) + strlen (gccpipe) + strlen (cob_ldflags)
			+ strlen (COB_EXPORT_DYN) + strlen (name)
			+ strlen (objsptr) + strlen (cob_libs) + 16;
	if (bufflen >= COB_MEDIUM_BUFF) {
		buffptr = cobc_malloc (bufflen);
	} else {
		buffptr = buff;
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /Od /MDd /Zi /FR /Fe\"%s\" %s %s %s" :
		"%s /MD /Fe\"%s\" %s %s %s",
			cob_cc, name, cob_ldflags, objsptr, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff, 
			 "%s /manifest \"%s.exe.manifest\" /outputresource:\"%s.exe\";#1",
			 manicmd, name, name);
		ret = process (buff);
		sprintf (buff, "%s.exe.manifest", name);
		cobc_check_action (buff);
	}
#endif
#else	/* _MSC_VER */
	sprintf (buffptr, "%s %s %s %s -o %s %s %s",
		 cob_cc, gccpipe, cob_ldflags, COB_EXPORT_DYN, name,
		 objsptr, cob_libs);

	ret = process (buffptr);
#ifdef	__hpux
	if (ret == 0) {
		sprintf (buff, "chatr -s +s enable %s%s 1>/dev/null 2>&1", name, COB_EXEEXT);
		process (buff);
	}
#endif
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s%s", COB_STRIP_CMD, name, COB_EXEEXT);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

int
main (int argc, char *argv[])
{
	struct filename		*fn;
	char			*p;
	int			status = 1;
	int			year;
	int			day;
	char			month[32];
	char			buff[COB_SMALL_BUFF];

#ifdef	HAVE_SIGNAL_H
	if ((intsig = signal (SIGINT, cobc_sig_handler)) == SIG_IGN) {
		(void)signal (SIGINT, SIG_IGN);
	}
#ifdef	SIGHUP
	if ((hupsig = signal (SIGHUP, cobc_sig_handler)) == SIG_IGN) {
		(void)signal (SIGHUP, SIG_IGN);
	}
#endif
#ifdef	SIGQUIT
	if ((qutsig = signal (SIGQUIT, cobc_sig_handler)) == SIG_IGN) {
		(void)signal (SIGQUIT, SIG_IGN);
	}
#endif
#endif

	cb_saveargc = argc;
	cb_saveargv = argv;

#ifdef	HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif

#ifdef	ENABLE_NLS
	bindtextdomain (PACKAGE, LOCALEDIR);
	textdomain (PACKAGE);
#endif

#ifndef _WIN32
	cob_process_id = getpid ();
#endif

	/* Initialize global variables */

	memset (buff, 0, sizeof(buff));
	/* Set up build time stamp */
	memset (month, 0, sizeof(month));
	day = 0;
	year = 0;
	sscanf (__DATE__, "%s %d %d", month, &day, &year);
	if (day && year) {
		sprintf (buff,
			  "%s %2.2d %4.4d %s", month, day, year, __TIME__);
	} else {
		sprintf (buff,
			  "%s %s", __DATE__, __TIME__);
	}
	cb_oc_build_stamp = cobc_malloc (strlen (buff) + 1);
	strcpy (cb_oc_build_stamp, buff);

	output_name = NULL;

	if ((p = getenv ("TMPDIR")) != NULL) {
		cob_tmpdir = p;
	} else if ((p = getenv ("TMP")) != NULL) {
		cob_tmpdir = p;
		sprintf (buff, "TMPDIR=%s", p);
		p = strdup (buff);
		putenv (p);
	} else {
		cob_tmpdir = "/tmp";
		putenv ((char *)"TMPDIR=/tmp");
	}

	cob_cc = getenv ("COB_CC");
	if (cob_cc == NULL) {
		cob_cc = COB_CC;
	}

	cob_config_dir = getenv ("COB_CONFIG_DIR");
	if (cob_config_dir == NULL) {
		cob_config_dir = COB_CONFIG_DIR;
	}

	cobc_init_var (cob_cflags, "COB_CFLAGS", COB_CFLAGS);

	cobc_init_var (cob_ldflags, "COB_LDFLAGS", COB_LDFLAGS);

	cobc_init_var (cob_libs, "COB_LIBS", COB_LIBS);
	memset (cob_define_flags, 0, sizeof (cob_define_flags));

	p = getenv ("COB_LDADD");
	if (p) {
		strcat (cob_libs, " ");
		strcat (cob_libs, p);
	}

	cob_copy_dir = getenv ("COB_COPY_DIR");
	if (cob_copy_dir == NULL) {
		cob_copy_dir = COB_COPY_DIR;
	}

	p = getenv ("COB_EBCDIC");
	if (p && (*p == 'F' || *p == 'f')) {
		alt_ebcdic = 1;
	}

	/* Compiler special options */

#if	defined(__INTEL_COMPILER)
	strcat (cob_cflags, " -vec-report0 -opt-report 0");
#elif	defined(__GNUC__)
	strcat (cob_cflags, " -Wno-unused -fsigned-char");
#ifdef	HAVE_PSIGN_OPT
	strcat (cob_cflags, " -Wno-pointer-sign");
#endif
#endif

	/* Process command line arguments */
	iargs = process_command_line (argc, argv);

	/* Check the filename */
	if (iargs == argc) {
		fprintf (stderr, "cobc: No input files\n");
		exit (1);
	}

	/* Windows stuff reliant upon verbose option */
#ifdef	_MSC_VER
	if (!verbose_output) {
		strcat (cob_cflags, " /nologo");
	}
#if	_MSC_VER >= 1400
	if (!verbose_output) {
		manicmd = "mt /nologo";
	} else {
		manicmd = "mt";
	}
#endif
#endif

	/* processes COBCPY environment variable */
	process_env_copy_path ();

	cb_include_list = cb_text_list_add (cb_include_list, cob_copy_dir);
	
	file_list = NULL;

	if (setjmp (cob_jmpbuf) != 0) {
		fprintf (stderr, "Aborting compile of %s at line %d\n",
			 cb_source_file, cb_source_line);
		fflush (stderr);
		if (yyout) {
			fflush (yyout);
		}
		if (cb_storage_file) {
			fflush (cb_storage_file);
		}
		status = 1;
		cobc_clean_up (status);
		return status;
	}

	/* Defaults are set here */
	if (!cb_flag_syntax_only) {
		if (!wants_nonfinal) {
			if (cb_flag_main) {
				cb_compile_level = CB_LEVEL_EXECUTABLE;
			} else if (cb_flag_module) {
				cb_compile_level = CB_LEVEL_MODULE;
			} else if (cb_flag_library) {
				cb_compile_level = CB_LEVEL_LIBRARY;
			} else if (cb_compile_level == 0) {
				cb_compile_level = CB_LEVEL_MODULE;
				cb_flag_module = 1;
			}
		}
		if (wants_nonfinal && cb_compile_level != CB_LEVEL_PREPROCESS &&
		    !cb_flag_main && !cb_flag_module && !cb_flag_library) {
			cb_flag_module = 1;
		}
	} else {
			cb_compile_level = CB_LEVEL_TRANSLATE;
	}

	if (output_name && cb_compile_level < CB_LEVEL_LIBRARY &&
	    (argc - iargs) > 1) {
		fprintf (stderr, "cobc: -o option invalid in this combination\n");
		exit (1);
	}
	if (cb_flag_sign_ascii && cb_flag_sign_ebcdic) {
		fprintf (stderr, "Only one of -fsign-ascii or -fsign-ebcdic may be specified\n");
		exit (1);
	}
	if (cb_flag_sign_ascii) {
		cb_display_sign = COB_DISPLAY_SIGN_ASCII;
	}
	if (cb_flag_sign_ebcdic) {
		cb_display_sign = COB_DISPLAY_SIGN_EBCDIC;
	}
	if (cb_flag_notrunc) {
		cb_binary_truncate = 0;
		cb_pretty_display = 0;
	}

	while (iargs < argc) {
		struct list_files *newfile;
		fn = process_filename (argv[iargs++]);
		if (!fn) {
			status = 1;
			cobc_clean_up (status);
			return status;
		}

		newfile = cobc_malloc (sizeof (struct list_files));
		memset (newfile, 0, sizeof (struct list_files));
		if (cb_current_file) {
			cb_current_file->next = newfile;
		}
		if (!cb_listing_files) {
		   cb_listing_files = newfile;
		}
		cb_current_file = newfile;

		/* Preprocess */
		if (cb_compile_level >= CB_LEVEL_PREPROCESS && fn->need_preprocess) {
			if (preprocess (fn) != 0) {
				cobc_clean_up (status);
				return status;
			}
		}
	}
	for (fn = file_list; fn; fn = fn->next) {
		cb_current_file = cb_listing_files;
		cb_id = 1;
		cb_attr_id = 1;
		cb_literal_id = 1;
		cb_field_id = 1;
		cb_storage_id = 1;
		iparams++;
		demangle_name = fn->demangle_source;
		if (iparams > 1 && cb_compile_level == CB_LEVEL_EXECUTABLE &&
		    !cb_flag_syntax_only) {
			local_level = cb_compile_level;
			cb_flag_main = 0;
			cb_compile_level = CB_LEVEL_ASSEMBLE;
		}
		/* Translate */
		if (cb_compile_level >= CB_LEVEL_TRANSLATE && fn->need_translate) {
			if (process_translate (fn) != 0) {
				cobc_clean_up (status);
				return status;
			}
		}
		if (cb_flag_syntax_only) {
			continue;
		}

		/* Compile */
		if (cb_compile_level == CB_LEVEL_COMPILE) {
			if (process_compile (fn) != 0) {
				cobc_clean_up (status);
				return status;
			}
		}

		/* Build module */
		if (cb_compile_level == CB_LEVEL_MODULE && fn->need_assemble) {
			if (process_module_direct (fn) != 0) {
				cobc_clean_up (status);
				return status;
			}
		} else {
			/* Assemble */
			if (cb_compile_level >= CB_LEVEL_ASSEMBLE && fn->need_assemble) {
				if (process_assemble (fn) != 0) {
					cobc_clean_up (status);
					return status;
				}
			}

			/* Build module */
			if (cb_compile_level == CB_LEVEL_MODULE) {
				if (process_module (fn) != 0) {
					cobc_clean_up (status);
					return status;
				}
			}
		}
	}

	if (!cb_flag_syntax_only) {
		/* Link */
		if (local_level == CB_LEVEL_EXECUTABLE) {
			cb_compile_level = CB_LEVEL_EXECUTABLE;
			cb_flag_main = 1;
		}
		if (cb_compile_level == CB_LEVEL_LIBRARY) {
			if (process_library (file_list) != 0) {
				cobc_clean_up (status);
				return status;
			}
		} else if (cb_compile_level == CB_LEVEL_EXECUTABLE) {
			if (process_link (file_list) != 0) {
				cobc_clean_up (status);
				return status;
			}
		}
	}

	/* We have successfully completed */
	status = 0;
	cobc_clean_up (status);

	return status;
}
