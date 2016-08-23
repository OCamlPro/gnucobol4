/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 27 "parser.y" /* yacc.c:339  */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#define	COB_IN_PARSER	1
#include "cobc.h"
#include "tree.h"

#ifndef	_STDLIB_H
#define	_STDLIB_H 1
#endif

#define YYSTYPE			cb_tree
#define yyerror(x)		cb_error ("%s", x)

#define emit_statement(x) \
do { \
  if (!skip_statements) { \
	CB_ADD_TO_CHAIN (x, current_program->exec_list); \
  } \
}  ONCE_COB

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

/* Statement terminator definitions */
#define TERM_NONE		0
#define TERM_ACCEPT		1U
#define TERM_ADD		2U
#define TERM_CALL		3U
#define TERM_COMPUTE		4U
#define TERM_DELETE		5U
#define TERM_DISPLAY		6U
#define TERM_DIVIDE		7U
#define TERM_EVALUATE		8U
#define TERM_IF			9U
#define TERM_MULTIPLY		10U
#define TERM_PERFORM		11U
#define TERM_READ		12U
#define TERM_RECEIVE		13U
#define TERM_RETURN		14U
#define TERM_REWRITE		15U
#define TERM_SEARCH		16U
#define TERM_START		17U
#define TERM_STRING		18U
#define TERM_SUBTRACT		19U
#define TERM_UNSTRING		20U
#define TERM_WRITE		21U
#define TERM_MAX		22U

#define	TERMINATOR_WARNING(x,z)	terminator_warning (x, TERM_##z, #z)
#define	TERMINATOR_ERROR(x,z)	terminator_error (x, TERM_##z, #z)
#define	TERMINATOR_CLEAR(x,z)	terminator_clear (x, TERM_##z)

/* Defines for duplicate checks */
/* Note - We use <= 16 for common item definitons and */
/* > 16 for non-common item definitions e.g. REPORT and SCREEN */
#define	SYN_CLAUSE_1		(1U << 0)
#define	SYN_CLAUSE_2		(1U << 1)
#define	SYN_CLAUSE_3		(1U << 2)
#define	SYN_CLAUSE_4		(1U << 3)
#define	SYN_CLAUSE_5		(1U << 4)
#define	SYN_CLAUSE_6		(1U << 5)
#define	SYN_CLAUSE_7		(1U << 6)
#define	SYN_CLAUSE_8		(1U << 7)
#define	SYN_CLAUSE_9		(1U << 8)
#define	SYN_CLAUSE_10		(1U << 9)
#define	SYN_CLAUSE_11		(1U << 10)
#define	SYN_CLAUSE_12		(1U << 11)
#define	SYN_CLAUSE_13		(1U << 12)
#define	SYN_CLAUSE_14		(1U << 13)
#define	SYN_CLAUSE_15		(1U << 14)
#define	SYN_CLAUSE_16		(1U << 15)
#define	SYN_CLAUSE_17		(1U << 16)
#define	SYN_CLAUSE_18		(1U << 17)
#define	SYN_CLAUSE_19		(1U << 18)
#define	SYN_CLAUSE_20		(1U << 19)
#define	SYN_CLAUSE_21		(1U << 20)
#define	SYN_CLAUSE_22		(1U << 21)
#define	SYN_CLAUSE_23		(1U << 22)
#define	SYN_CLAUSE_24		(1U << 23)
#define	SYN_CLAUSE_25		(1U << 24)
#define	SYN_CLAUSE_26		(1U << 25)
#define	SYN_CLAUSE_27		(1U << 26)
#define	SYN_CLAUSE_28		(1U << 27)
#define	SYN_CLAUSE_29		(1U << 28)
#define	SYN_CLAUSE_30		(1U << 29)
#define	SYN_CLAUSE_31		(1U << 30)
#define	SYN_CLAUSE_32		(1U << 31)

#define	EVAL_DEPTH		32
#define	PROG_DEPTH		16

/* Global variables */

struct cb_program		*current_program = NULL;
struct cb_statement		*current_statement = NULL;
struct cb_label			*current_section = NULL;
struct cb_label			*current_paragraph = NULL;
cb_tree				defined_prog_list = NULL;
char				*cobc_glob_line = NULL;
int				cb_exp_line = 0;

cb_tree				cobc_printer_node = NULL;
int				functions_are_all = 0;
int				non_const_word = 0;
unsigned int			cobc_in_id = 0;
unsigned int			cobc_in_procedure = 0;
unsigned int			cobc_in_repository = 0;
unsigned int			cobc_force_literal = 0;
unsigned int			cobc_cs_check = 0;
unsigned int			cobc_allow_program_name = 0;

/* Local variables */

enum tallying_phrase {
	NO_PHRASE,
	FOR_PHRASE,
	CHARACTERS_PHRASE,
	ALL_LEADING_TRAILING_PHRASES,
	VALUE_REGION_PHRASE
};

static struct cb_statement	*main_statement;

static cb_tree			current_expr;
static struct cb_field		*current_field;
static struct cb_field		*description_field;
static struct cb_file		*current_file;
static struct cb_report		*current_report;
static struct cb_report		*report_instance;

static struct cb_file		*linage_file;
static cb_tree			next_label_list;

static char			*stack_progid[PROG_DEPTH];

static enum cb_storage		current_storage;

static cb_tree			perform_stack;
static cb_tree			qualifier;

static cb_tree			save_tree;
static cb_tree			start_tree;

static unsigned int		check_unreached;
static unsigned int		in_declaratives;
static unsigned int		in_debugging;
static unsigned int		current_linage;
static unsigned int		report_count;
static unsigned int		prog_end;
static unsigned int		use_global_ind;
static unsigned int		samearea;
static unsigned int		inspect_keyword;
static unsigned int		main_flag_set;
static int			next_label_id;
static int			eval_level;
static int			eval_inc;
static int			eval_inc2;
static int			depth;
static int			first_nested_program;
static int			call_mode;
static int			size_mode;
static int			setattr_val_on;
static int			setattr_val_off;
static unsigned int		check_duplicate;
static unsigned int		check_on_off_duplicate;
static unsigned int		check_pic_duplicate;
static unsigned int		check_comp_duplicate;
static int			check_line_col_duplicate;
static unsigned int		skip_statements;
static unsigned int		start_debug;
static unsigned int		save_debug;
static unsigned int		needs_field_debug;
static unsigned int		needs_debug_item;
static unsigned int		env_div_seen;
static unsigned int		header_check;
static unsigned int		call_nothing;
static enum tallying_phrase	previous_tallying_phrase;

static enum cb_display_type	display_type;
static int			is_first_display_item;
static cb_tree			advancing_value;
static cb_tree			upon_value;
static cb_tree			line_column;

static int			term_array[TERM_MAX];
static cb_tree			eval_check[EVAL_DEPTH][EVAL_DEPTH];

/* Defines for header presence */

#define	COBC_HD_ENVIRONMENT_DIVISION	(1U << 0)
#define	COBC_HD_CONFIGURATION_SECTION	(1U << 1)
#define	COBC_HD_SPECIAL_NAMES		(1U << 2)
#define	COBC_HD_INPUT_OUTPUT_SECTION	(1U << 3)
#define	COBC_HD_FILE_CONTROL		(1U << 4)
#define	COBC_HD_I_O_CONTROL		(1U << 5)
#define	COBC_HD_DATA_DIVISION		(1U << 6)
#define	COBC_HD_FILE_SECTION		(1U << 7)
#define	COBC_HD_WORKING_STORAGE_SECTION	(1U << 8)
#define	COBC_HD_LOCAL_STORAGE_SECTION	(1U << 9)
#define	COBC_HD_LINKAGE_SECTION		(1U << 10)
#define	COBC_HD_COMMUNICATIONS_SECTION	(1U << 11)
#define	COBC_HD_REPORT_SECTION		(1U << 12)
#define	COBC_HD_SCREEN_SECTION		(1U << 13)
#define	COBC_HD_PROCEDURE_DIVISION	(1U << 14)
#define	COBC_HD_PROGRAM_ID		(1U << 15)

/* Static functions */

static void
begin_statement (const char *name, const unsigned int term)
{
	if (cb_warn_unreachable && check_unreached) {
		cb_warning (_("unreachable statement '%s'"), name);
	}
	current_paragraph->flag_statement = 1;
	current_statement = cb_build_statement (name);
	CB_TREE (current_statement)->source_file = cb_source_file;
	CB_TREE (current_statement)->source_line = cb_source_line;
	current_statement->statement = cobc_glob_line;
	current_statement->flag_in_debug = in_debugging;
	emit_statement (CB_TREE (current_statement));
	if (term) {
		term_array[term]++;
	}
	main_statement = current_statement;
}

static void
begin_implicit_statement (void)
{
	current_statement = cb_build_statement (NULL);
	current_statement->flag_in_debug = !!in_debugging;
	main_statement->body = cb_list_add (main_statement->body,
					    CB_TREE (current_statement));
}

# if 0 /* activate only for debugging purposes for attribs */
static
void printBits(unsigned int num){
	unsigned int size = sizeof(unsigned int);
	unsigned int maxPow = 1<<(size*8-1);
	int i=0;

	for(;i<size*8;++i){
		/* print last bit and shift left. */
		fprintf(stderr, "%u ",num&maxPow ? 1 : 0);
		num = num<<1;
	}
	fprintf(stderr, "\n");
}
#endif

static void
emit_entry (const char *name, const int encode, cb_tree using_list)
{
	cb_tree		l;
	cb_tree		label;
	cb_tree		x;
	struct cb_field	*f;
	int		parmnum;
	char		buff[COB_MINI_BUFF];

	snprintf (buff, (size_t)COB_MINI_MAX, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = cb_encode_program_id (name);
		CB_LABEL (label)->orig_name = name;
	} else {
		CB_LABEL (label)->name = name;
		CB_LABEL (label)->orig_name = current_program->orig_program_id;
	}
	CB_LABEL (label)->flag_begin = 1;
	CB_LABEL (label)->flag_entry = 1;
	label->source_file = cb_source_file;
	label->source_line = cb_source_line;
	emit_statement (label);

	if (current_program->flag_debugging) {
		emit_statement (cb_build_debug (cb_debug_contents,
						"START PROGRAM", NULL));
	}

	parmnum = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), cb_name (x));
			}
			if (!current_program->flag_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), cb_name (x));
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' cannot be BASED/EXTERNAL"), cb_name (x));
				}
				f->flag_is_pdiv_parm = 1;
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), cb_name (x));
				}
				f->flag_chained = 1;
				f->param_num = parmnum;
				parmnum++;
			}
			if (f->redefines) {
				cb_error_x (x, _("'%s' REDEFINES field not allowed here"), cb_name (x));
			}
		}
	}

	/* Check dangling LINKAGE items */
	if (cb_warn_linkage) {
		for (f = current_program->linkage_storage; f; f = f->sister) {
			if (current_program->returning) {
				if (cb_ref (current_program->returning) != cb_error_node) {
					if (f == CB_FIELD (cb_ref (current_program->returning))) {
						continue;
					}
				}
			}
			for (l = using_list; l; l = CB_CHAIN (l)) {
				x = CB_VALUE (l);
				if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
					if (f == CB_FIELD (cb_ref (x))) {
						break;
					}
				}
			}
			if (!l && !f->redefines) {
				cb_warning (_("LINKAGE item '%s' is not a PROCEDURE USING parameter"), f->name);
			}
		}
	}

	/* Check returning item against using items when FUNCTION */
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		for (l = using_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (CB_VALID_TREE (x) && current_program->returning &&
			    cb_ref (x) == cb_ref (current_program->returning)) {
				cb_error_x (x, _("'%s' USING item duplicates RETURNING item"), cb_name (x));
			}
		}
	}

	for (l = current_program->entry_list; l; l = CB_CHAIN (l)) {
		if (strcmp ((const char *)name,
			    (const char *)(CB_LABEL(CB_PURPOSE(l))->name)) == 0) {
			cb_error_x (CB_TREE (current_statement),
				    _("ENTRY '%s' duplicated"), name);
		}
	}

	current_program->entry_list =
		cb_list_append (current_program->entry_list,
				CB_BUILD_PAIR (label, using_list));
}

static size_t
increment_depth (void)
{
	if (++depth >= PROG_DEPTH) {
		cb_error (_("maximum nested program depth exceeded (%d)"),
			  PROG_DEPTH);
		return 1;
	}
	return 0;
}

static void
terminator_warning (cb_tree stmt, const unsigned int termid,
		    const char *name)
{
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
		if (cb_warn_terminator) {
			cb_warning_x (stmt,
				_("%s statement not terminated by END-%s"),
				name, name);
		}
	}
	/* Free tree associated with terminator */
	cobc_parse_free (stmt);
}

static void
terminator_error (cb_tree stmt, const unsigned int termid, const char *name)
{
	check_unreached = 0;
	cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated by END-%s"),
			name, name);
	if (term_array[termid]) {
		term_array[termid]--;
	}
	/* Free tree associated with terminator */
	cobc_parse_free (stmt);
}

static void
terminator_clear (cb_tree stmt, const unsigned int termid)
{
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	}
	/* Free tree associated with terminator */
	cobc_parse_free (stmt);
}

static int
literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if (x == cb_null) {
		return 0;
	} else if (x == cb_low) {
		return 0;
	} else if (x == cb_high) {
		return 255;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x);
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

static void
set_up_use_file (struct cb_file *fileptr)
{
	struct cb_file	*newptr;

	if (fileptr->organization == COB_ORG_SORT) {
		cb_error (_("USE statement invalid for SORT file"));
	}
	if (fileptr->flag_global) {
		newptr = cobc_parse_malloc (sizeof(struct cb_file));
		*newptr = *fileptr;
		newptr->handler = current_section;
		newptr->handler_prog = current_program;
		if (!use_global_ind) {
			current_program->local_file_list =
				cb_list_add (current_program->local_file_list,
					     CB_TREE (newptr));
		} else {
			current_program->global_file_list =
				cb_list_add (current_program->global_file_list,
					     CB_TREE (newptr));
		}
	} else {
		fileptr->handler = current_section;
	}
}

static void
check_relaxed_syntax (const unsigned int lev)
{
	const char	*s;

	switch (lev) {
	case COBC_HD_ENVIRONMENT_DIVISION:
		s = "ENVIRONMENT DIVISION";
		break;
	case COBC_HD_CONFIGURATION_SECTION:
		s = "CONFIGURATION SECTION";
		break;
	case COBC_HD_SPECIAL_NAMES:
		s = "SPECIAL-NAMES";
		break;
	case COBC_HD_INPUT_OUTPUT_SECTION:
		s = "INPUT-OUTPUT SECTION";
		break;
	case COBC_HD_FILE_CONTROL:
		s = "FILE-CONTROL";
		break;
	case COBC_HD_I_O_CONTROL:
		s = "I-O-CONTROL";
		break;
	case COBC_HD_DATA_DIVISION:
		s = "DATA DIVISION";
		break;
	case COBC_HD_FILE_SECTION:
		s = "FILE SECTION";
		break;
	case COBC_HD_WORKING_STORAGE_SECTION:
		s = "WORKING-STORAGE SECTION";
		break;
	case COBC_HD_LOCAL_STORAGE_SECTION:
		s = "LOCAL-STORAGE SECTION";
		break;
	case COBC_HD_LINKAGE_SECTION:
		s = "LINKAGE SECTION";
		break;
	case COBC_HD_COMMUNICATIONS_SECTION:
		s = "COMMUNICATIONS SECTION";
		break;
	case COBC_HD_REPORT_SECTION:
		s = "REPORT SECTION";
		break;
	case COBC_HD_SCREEN_SECTION:
		s = "SCREEN SECTION";
		break;
	case COBC_HD_PROCEDURE_DIVISION:
		s = "PROCEDURE DIVISION";
		break;
	case COBC_HD_PROGRAM_ID:
		s = "PROGRAM-ID";
		break;
	default:
		s = "Unknown";
		break;
	}
	if (cb_relaxed_syntax_checks) {
		cb_warning (_("%s header missing - assumed"), s);
	} else {
		cb_error (_("%s header missing"), s);
	}
}

static void
check_headers_present (const unsigned int lev1, const unsigned int lev2,
		       const unsigned int lev3, const unsigned int lev4)
{
	/* Lev1 is always present and checked */
	/* Lev2/3/4, if non-zero (forced) may be present */
	if (!(header_check & lev1)) {
		header_check |= lev1;
		check_relaxed_syntax (lev1);
	}
	if (lev2) {
		if (!(header_check & lev2)) {
			header_check |= lev2;
			check_relaxed_syntax (lev2);
		}
	}
	if (lev3) {
		if (!(header_check & lev3)) {
			header_check |= lev3;
			check_relaxed_syntax (lev3);
		}
	}
	if (lev4) {
		if (!(header_check & lev4)) {
			header_check |= lev4;
			check_relaxed_syntax (lev4);
		}
	}
}

static void
build_nested_special (const int ndepth)
{
	cb_tree		x;
	cb_tree		y;

	if (!ndepth) {
		return;
	}

	/* Inherit special name mnemonics from parent */
	for (x = current_program->mnemonic_spec_list; x; x = CB_CHAIN (x)) {
		y = cb_build_reference (cb_name(CB_PURPOSE(x)));
		if (CB_SYSTEM_NAME_P (CB_VALUE(x))) {
			cb_define (y, CB_VALUE(x));
		} else {
			cb_build_constant (y, CB_VALUE(x));
		}
	}
}

static void
clear_initial_values (void)
{
	perform_stack = NULL;
	current_statement = NULL;
	main_statement = NULL;
	qualifier = NULL;
	in_declaratives = 0;
	in_debugging = 0;
	use_global_ind = 0;
	check_duplicate = 0;
	check_pic_duplicate = 0;
	check_comp_duplicate = 0;
	skip_statements = 0;
	start_debug = 0;
	save_debug = 0;
	needs_field_debug = 0;
	needs_debug_item = 0;
	env_div_seen = 0;
	header_check = 0;
	next_label_id = 0;
	current_linage = 0;
	setattr_val_on = 0;
	setattr_val_off = 0;
	report_count = 0;
	current_storage = CB_STORAGE_WORKING;
	eval_level = 0;
	eval_inc = 0;
	eval_inc2 = 0;
	inspect_keyword = 0;
	check_unreached = 0;
	cobc_in_id = 0;
	cobc_in_procedure = 0;
	cobc_in_repository = 0;
	cobc_force_literal = 0;
	non_const_word = 0;
	samearea = 1;
	memset ((void *)eval_check, 0, sizeof(eval_check));
	memset ((void *)term_array, 0, sizeof(term_array));
	linage_file = NULL;
	current_file = NULL;
	current_report = NULL;
	report_instance = NULL;
	next_label_list = NULL;
	if (cobc_glob_line) {
		cobc_free (cobc_glob_line);
		cobc_glob_line = NULL;
	}
}

/*
  We must check for redefinitions of program-names and external program names
  outside of the usual reference/word_list methods as it may have to be done in
  a case-sensitive way.
*/
static void
begin_scope_of_program_name (struct cb_program *program)
{
	const char	*prog_name = program->program_name;
	const char	*prog_id = program->orig_program_id;
	const char	*elt_name;
	const char	*elt_id;
	cb_tree		l;

	/* Error if a program with the same name has been defined. */
	for (l = defined_prog_list; l; l = CB_CHAIN (l)) {
		elt_name = ((struct cb_program *) CB_VALUE (l))->program_name;
		elt_id = ((struct cb_program *) CB_VALUE (l))->orig_program_id;
		if (cb_fold_call && strcasecmp (prog_name, elt_name) == 0) {
			cb_error_x ((cb_tree) program,
				    _("redefinition of program name '%s'"),
				    elt_name);
		} else if (strcmp (prog_id, elt_id) == 0) {
		        cb_error_x ((cb_tree) program,
				    _("redefinition of program ID '%s'"),
				    elt_id);
			return;
		}
	}

	/* Otherwise, add the program to the list. */
	defined_prog_list = cb_list_add (defined_prog_list,
					 (cb_tree) program);
}

static void
remove_program_name (struct cb_list *l, struct cb_list *prev)
{
	if (prev == NULL) {
		defined_prog_list = l->chain;
	} else {
		prev->chain = l->chain;
	}
	cobc_parse_free (l);
}

/* Remove the program from defined_prog_list, if necessary. */
static void
end_scope_of_program_name (struct cb_program *program)
{
	struct	cb_list	*prev = NULL;
	struct	cb_list *l = (struct cb_list *) defined_prog_list;

	if (program->nested_level == 0) {
		return;
	}

	/* Remove any subprograms */
	l = CB_LIST (defined_prog_list);
        while (l) {
		if (CB_PROGRAM (l->value)->nested_level > program->nested_level) {
			remove_program_name (l, prev);
			l = CB_LIST (prev->chain);
		} else {
			prev = l;
			l = CB_LIST (l->chain);
		}
	}

	/* Remove the specified program, if it is not COMMON */
	if (!program->flag_common) {
		l = (struct cb_list *) defined_prog_list;
	        while (l) {
			if (strcmp (program->orig_program_id,
				    CB_PROGRAM (l->value)->orig_program_id)
			    == 0) {
				remove_program_name (l, prev);
				l = CB_LIST (prev->chain);
				break;
			} else {
				prev = l;
				l = CB_LIST (l->chain);
			}
		}
	}
}

static int
set_up_program (cb_tree id, cb_tree as_literal, const unsigned char type)
{
	current_section = NULL;
	current_paragraph = NULL;

	if (CB_LITERAL_P (id)) {
		stack_progid[depth] = (char *)(CB_LITERAL (id)->data);
	} else {
		stack_progid[depth] = (char *)(CB_NAME (id));
	}

	if (depth > 0) {
		if (first_nested_program) {
			check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0);
		}
		if (type == CB_FUNCTION_TYPE) {
			cb_error ("functions may not be defined within a program/function");
		}
	}
	first_nested_program = 1;

	if (prog_end) {
		if (!current_program->flag_validated) {
			current_program->flag_validated = 1;
			cb_validate_program_body (current_program);
		}

		clear_initial_values ();
		current_program = cb_build_program (current_program, depth);
		build_nested_special (depth);
		cb_build_registers ();
	} else {
		prog_end = 1;
	}

	if (increment_depth ()) {
	        return 1;
	}

	current_program->program_id = cb_build_program_id (id, as_literal, type == CB_FUNCTION_TYPE);
	current_program->prog_type = type;

	if (type == CB_PROGRAM_TYPE) {
		if (!main_flag_set) {
			main_flag_set = 1;
			current_program->flag_main = !!cobc_flag_main;
		}
	} else { /* CB_FUNCTION_TYPE */
		current_program->flag_recursive = 1;
	}

	if (CB_REFERENCE_P (id)) {
	        cb_define (id, CB_TREE (current_program));
	}

	begin_scope_of_program_name (current_program);

	return 0;
}

static void
decrement_depth (const char *name, const unsigned char type)
{
	int	d;

	if (depth) {
		depth--;
	}

	if (!strcmp (stack_progid[depth], name)) {
		return;
	}

	if (type == CB_FUNCTION_TYPE) {
		cb_error (_("END FUNCTION '%s' is different from FUNCTION-ID '%s'"),
			  name, stack_progid[depth]);
		return;
	}

	/* Set depth to that of whatever program we just ended, if it exists. */
	for (d = depth; d >= 0; --d) {
		if (!strcmp (stack_progid[d], name)) {
			depth = d;
			return;
		}
	}

	if (depth != d) {
		cb_error (_("END PROGRAM '%s' is different from PROGRAM-ID '%s'"),
			  name, stack_progid[depth]);
	}
}

static void
clean_up_program (cb_tree name, const unsigned char type)
{
	char		*s;

	end_scope_of_program_name (current_program);

	if (CB_LITERAL_P (name)) {
		s = (char *)(CB_LITERAL (name)->data);
	} else {
		s = (char *)(CB_NAME (name));
	}

	decrement_depth (s, type);

	current_section = NULL;
	current_paragraph = NULL;
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
}

static const char *
get_literal_or_word_name (const cb_tree x)
{
	if (CB_LITERAL_P (x)) {
		return (const char *) CB_LITERAL (x)->data;
	} else { /* CB_REFERENCE_P (x) */
		return (const char *) CB_NAME (x);
	}
}

/* Return 1 if the prototype name is the same as the current function's. */
static int
check_prototype_redefines_current_element (const cb_tree prototype_name)
{
	const char	*name = get_literal_or_word_name (prototype_name);

	if (strcasecmp (name, current_program->program_name) == 0) {
		cb_warning_x (prototype_name,
			_("prototype has same name as current function and will be ignored"));
		return 1;
	}

	return 0;
}

/* Returns 1 if the prototype has been duplicated. */
static int
check_for_duplicate_prototype (const cb_tree prototype_name,
			       const cb_tree prototype)
{
	cb_tree	dup;

	if (CB_WORD_COUNT (prototype_name) > 0) {
		/* Make sure the duplicate is a prototype */
		dup = cb_ref (prototype_name);
		if (!CB_PROTOTYPE_P (dup)) {
			redefinition_error (prototype_name);
			return 1;
		}

		/* Check the duplicate prototypes match */
		if (strcmp (CB_PROTOTYPE (prototype)->ext_name,
			    CB_PROTOTYPE (dup)->ext_name)
		    || CB_PROTOTYPE (prototype)->type != CB_PROTOTYPE (dup)->type) {
			cb_error_x (prototype_name,
				    _("duplicate REPOSITORY entries for '%s' do not match"),
				    get_literal_or_word_name (prototype_name));
		} else {
			cb_warning_x (prototype_name,
				      _("duplicate REPOSITORY entry for '%s'"),
				      get_literal_or_word_name (prototype_name));
		}
		return 1;
	}

	return 0;
}

static void
set_up_prototype (cb_tree prototype_name, cb_tree ext_name,
		  const int type, const int is_current_element)
{
	cb_tree 	prototype;
	int	        name_redefinition_allowed;

	if (!is_current_element
	    && check_prototype_redefines_current_element (prototype_name)) {
		return;
	}

	prototype = cb_build_prototype (prototype_name, ext_name, type);

	if (!is_current_element
	    && check_for_duplicate_prototype (prototype_name, prototype)) {
		return;
	}

	name_redefinition_allowed = type == CB_PROGRAM_TYPE
		&& is_current_element && cb_program_name_redefinition;
	if (!name_redefinition_allowed) {
		if (CB_LITERAL_P (prototype_name)) {
			cb_define (cb_build_reference ((const char *)CB_LITERAL (prototype_name)->data), prototype);
		} else {
			cb_define (prototype_name, prototype);
	}

		if (type == CB_PROGRAM_TYPE) {
			current_program->program_spec_list =
				cb_list_add (current_program->program_spec_list, prototype);
		} else { /* CB_FUNCTION_TYPE */
	current_program->user_spec_list =
				cb_list_add (current_program->user_spec_list, prototype);
		}
	}
}

static void
error_if_invalid_level_for_renames (cb_tree item)
{
	int	level = CB_FIELD (cb_ref (item))->level;

	if (level == 1 || level == 66 || level == 77) {
	        cb_verify (cb_renames_uncommon_levels,
			   _("RENAMES of 01-, 66- and 77-level items"));
	} else if (level == 88) {
		cb_error (_("RENAMES may not reference a level 88"));
	}
}

static int
set_current_field (cb_tree level, cb_tree name)
{
	cb_tree	x  = cb_build_field_tree (level, name, current_field,
					  current_storage, current_file, 0);
	cobc_parse_free (level);

	if (CB_INVALID_TREE (x)) {
	        return 1;
	} else {
		current_field = CB_FIELD (x);
		check_pic_duplicate = 0;
	}

	return 0;
}

static void
emit_duplicate_clause_message (const char *clause)
{
	if (cb_relaxed_syntax_checks) {
		cb_warning (_("duplicate %s clause"), clause);
	} else {
		cb_error (_("duplicate %s clause"), clause);
	}
}

static void
check_repeated (const char *clause, const unsigned int bitval, unsigned int *already_seen)
{
	if (*already_seen & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		*already_seen |= bitval;
	}
}

static void
check_not_both (const int flag1, const int flag2,
		const char *flag1_name, const char *flag2_name,
		const int flags, const int flag_to_set)
{
	if (flag_to_set == flag1 && (flags & flag2)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);
	} else if (flag_to_set == flag2 && (flags & flag1)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);

	}
}

static COB_INLINE COB_A_INLINE void
check_not_highlight_and_lowlight (const int flags, const int flag_to_set)
{
	check_not_both (COB_SCREEN_HIGHLIGHT, COB_SCREEN_LOWLIGHT,
			"HIGHLIGHT", "LOWLIGHT", flags, flag_to_set);
}

static void
check_screen_attr (const char *clause, const int bitval)
{
	if (current_field->screen_flag & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		current_field->screen_flag |= bitval;
	}
}

static void
emit_conflicting_clause_message (const char *clause, const char *conflicting)
{
	if (cb_relaxed_syntax_checks) {
		cb_warning (_("cannot specify both %s and %s; %s is ignored"),
			    clause, conflicting, clause);
	} else {
		cb_error (_("cannot specify both %s and %s"),
			  clause, conflicting);
	}

}

static void
check_attr_with_conflict (const char *clause, const int bitval,
			  const char *confl_clause, const int confl_bit,
			  int *flags)
{
	if (*flags & bitval) {
		emit_duplicate_clause_message (clause);
	} else if (*flags & confl_bit) {
		emit_conflicting_clause_message (clause, confl_clause);
	} else {
	        *flags |= bitval;
	}
}

static COB_INLINE COB_A_INLINE void
check_screen_attr_with_conflict (const char *clause, const int bitval,
			  const char *confl_clause, const int confl_bit)
{
	check_attr_with_conflict (clause, bitval, confl_clause, confl_bit,
				  &current_field->screen_flag);
}

static COB_INLINE COB_A_INLINE void
check_dispattr_with_conflict (const char *attrib_name, const int attrib,
			      const char *confl_name, const int confl_attrib)
{
	check_attr_with_conflict (attrib_name, attrib, confl_name, confl_attrib,
				  &current_statement->attr_ptr->dispattrs);
}

static void
bit_set_attr (const cb_tree onoff, const int attrval)
{
	if (onoff == cb_int1) {
		setattr_val_on |= attrval;
	} else {
		setattr_val_off |= attrval;
	}
}

static void
attach_attrib_to_cur_stmt (void)
{
	if (!current_statement->attr_ptr) {
		current_statement->attr_ptr =
			cobc_parse_malloc (sizeof(struct cb_attr_struct));
	}
}

static void
check_field_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
		     cb_tree timeout, cb_tree prompt, cb_tree size_is)
{
	/* [WITH] FOREGROUND-COLOR [IS] */
	if (fgc) {
		current_statement->attr_ptr->fgc = fgc;
	}
	/* [WITH] BACKGROUND-COLOR [IS] */
	if (bgc) {
		current_statement->attr_ptr->bgc = bgc;
	}
	/* [WITH] SCROLL UP | DOWN */
	if (scroll) {
		current_statement->attr_ptr->scroll = scroll;
	}
	/* [WITH] TIME-OUT [AFTER] */
	if (timeout) {
		current_statement->attr_ptr->timeout = timeout;
	}
	/* [WITH] PROMPT CHARACTER [IS] */
	if (prompt) {
		current_statement->attr_ptr->prompt = prompt;
	}
	/* [WITH] SIZE [IS] */
	if (size_is) {
		current_statement->attr_ptr->size_is = size_is;
	}
}

static void
check_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
	       cb_tree timeout, cb_tree prompt, cb_tree size_is,
	       const int attrib)
{
	attach_attrib_to_cur_stmt ();
	check_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	current_statement->attr_ptr->dispattrs |= attrib;
}

static void
check_attribs_with_conflict (cb_tree fgc, cb_tree bgc, cb_tree scroll,
			     cb_tree timeout, cb_tree prompt, cb_tree size_is,
			     const char *attrib_name, const int attrib,
			     const char *confl_name, const int confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	check_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	check_dispattr_with_conflict (attrib_name, attrib, confl_name,
				      confl_attrib);
}

static int
zero_conflicting_flag (const int screen_flag, int parent_flag, const int flag1, const int flag2)
{
	if (screen_flag & flag1) {
		parent_flag &= ~flag2;
	} else if (screen_flag & flag2) {
		parent_flag &= ~flag1;
	}

	return parent_flag;
}

static int
zero_conflicting_flags (const int screen_flag, int parent_flag)
{
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_BLANK_LINE,
					     COB_SCREEN_BLANK_SCREEN);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_ERASE_EOL,
					     COB_SCREEN_ERASE_EOS);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_HIGHLIGHT,
					     COB_SCREEN_LOWLIGHT);

	return parent_flag;
}

static void
remove_attrib (int attrib)
{
	/* Remove attribute from current_statement */
	if (!current_statement->attr_ptr) {
		return;
	}
	current_statement->attr_ptr->dispattrs ^= attrib;
}

static void
check_set_usage (const enum cb_usage usage)
{
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	current_field->usage = usage;
}

static void
check_preceding_tallying_phrases (const enum tallying_phrase phrase)
{
	switch (phrase) {
	case FOR_PHRASE:
		if (previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("FOR phrase cannot immediately follow ALL/LEADING/TRAILING"));
		} else if (previous_tallying_phrase == FOR_PHRASE) {
			cb_error (_("missing CHARACTERS/ALL/LEADING/TRAILING phrase after FOR phrase"));
		}
		break;

	case CHARACTERS_PHRASE:
	case ALL_LEADING_TRAILING_PHRASES:
		if (previous_tallying_phrase == NO_PHRASE) {
			cb_error (_("missing FOR phrase before CHARACTERS/ALL/LEADING/TRAILING phrase"));
		} else if (previous_tallying_phrase == CHARACTERS_PHRASE
			   || previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("missing value between CHARACTERS/ALL/LEADING/TRAILING words"));
		}
		break;

	case VALUE_REGION_PHRASE:
		if (!(previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES
		      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
			cb_error (_("missing ALL/LEADING/TRAILING before value"));
		}
		break;

	default:
		/* This should never happen */
		cb_error (_("unexpected tallying phrase"));
		COBC_ABORT();
	}

	previous_tallying_phrase = phrase;
}

static int
has_relative_pos (struct cb_field const *field)
{
	return !!(field->screen_flag
		  & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS
		     | COB_SCREEN_COLUMN_PLUS | COB_SCREEN_COLUMN_MINUS));
}

static int
is_recursive_call (cb_tree target)
{
	const char *target_name = "";

	if (CB_LITERAL_P (target)) {
		target_name = (const char *)(CB_LITERAL(target)->data);
	} else if (CB_REFERENCE_P (target)
		   && CB_PROTOTYPE_P (cb_ref (target))) {
		target_name = CB_PROTOTYPE (cb_ref (target))->ext_name;
	}

	return !strcmp (target_name, current_program->orig_program_id);
}

static void
check_not_88_level (cb_tree x)
{
	struct cb_field	*f;

	if (x == cb_error_node || x->tag != CB_TAG_REFERENCE) {
		return;
	}

	f = CB_FIELD (cb_ref (x));

	if (f != (struct cb_field *) cb_error_node && f->level == 88) {
		cb_error (_("88-level cannot be used here"));
	}
}

static int
is_screen_field (cb_tree x)
{
	if (CB_FIELD_P (x)) {
		return (CB_FIELD (x))->storage == CB_STORAGE_SCREEN;
	} else if (CB_REFERENCE_P (x)) {
		return is_screen_field (cb_ref (x));
	} else {
		return 0;
	}
}

static void
error_if_no_advancing_in_screen_display (cb_tree advancing)
{
	if (advancing != cb_int1) {
		cb_error (_("cannot specify NO ADVANCING in screen DISPLAY"));
	}
}

static cb_tree
get_default_display_device (void)
{
	if (current_program->flag_console_is_crt
	    || cb_console_is_crt) {
		return cb_null;
	} else {
		return cb_int0;
	}
}

static COB_INLINE COB_A_INLINE int
contains_one_screen_field (struct cb_list *x_list)
{
	return (cb_tree) x_list != cb_null
		&& cb_list_length ((cb_tree) x_list) == 1
		&& is_screen_field (x_list->value);
}

static int
contains_only_screen_fields (struct cb_list *x_list)
{
	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (!is_screen_field (x_list->value)) {
			return 0;
		}
	}

	return 1;
}

static int
contains_fields_and_screens (struct cb_list *x_list)
{
	int	field_seen = 0;
	int	screen_seen = 0;

	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (is_screen_field (x_list->value)) {
			screen_seen = 1;
		} else {
			field_seen = 1;
		}
	}

	return screen_seen && field_seen;
}

static enum cb_display_type
deduce_display_type (cb_tree x_list, cb_tree upon_value, cb_tree line_column,
		     struct cb_attr_struct * const attr_ptr)
{
	int	using_default_device_which_is_crt =
		upon_value == NULL && get_default_display_device () == cb_null;

	if (contains_only_screen_fields ((struct cb_list *) x_list)) {
		if (!contains_one_screen_field ((struct cb_list *) x_list)
		    || attr_ptr) {
			cb_verify_x (x_list, cb_accept_display_extensions,
				     _("non-standard DISPLAY"));
		}

		if (upon_value != NULL && upon_value != cb_null) {
			cb_error_x (x_list, _("screens may only be displayed on CRT"));
		}

		return SCREEN_DISPLAY;
	} else if (contains_fields_and_screens ((struct cb_list *) x_list)) {
		cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		return MIXED_DISPLAY;
	} else if (line_column || attr_ptr) {
		if (upon_value != NULL && upon_value != cb_null) {
			cb_error_x (x_list, _("screen clauses may only be used for DISPLAY on CRT"));
		}

		cb_verify_x (x_list, cb_accept_display_extensions,
			     _("non-standard DISPLAY"));

		return FIELD_ON_SCREEN_DISPLAY;
	} else if (upon_value == cb_null || using_default_device_which_is_crt) {
		/* This is the only format permitted by the standard */
		return FIELD_ON_SCREEN_DISPLAY;
	} else if (display_type == FIELD_ON_SCREEN_DISPLAY && upon_value == NULL) {
		/* This is for when fields without clauses follow fields with screen clauses */
		return FIELD_ON_SCREEN_DISPLAY;
	} else {
		return DEVICE_DISPLAY;
	}
}

static void
set_display_type (cb_tree x_list, cb_tree upon_value,
		  cb_tree line_column, struct cb_attr_struct * const attr_ptr)
{
	display_type = deduce_display_type (x_list, upon_value, line_column, attr_ptr);
}

static void
error_if_different_display_type (cb_tree x_list, cb_tree upon_value,
				 cb_tree line_column, struct cb_attr_struct * const attr_ptr)
{
        const enum cb_display_type	type =
		deduce_display_type (x_list, upon_value, line_column, attr_ptr);

	/* Avoid re-displaying the same error for mixed DISPLAYs */
	if (type == display_type || display_type == MIXED_DISPLAY) {
		return;
	}

	if (type != MIXED_DISPLAY) {
		if (type == SCREEN_DISPLAY || display_type == SCREEN_DISPLAY) {
			cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		} else {
			/*
			  The only other option is that there is a mix of
			  FIELD_ON_SCREEN_DISPLAY and DEVICE_DISPLAY.
			*/
			cb_error_x (x_list, _("ambiguous DISPLAY; put items to display on device in separate DISPLAY"));
		}
	}

	display_type = MIXED_DISPLAY;
}

static void
error_if_not_usage_display_or_nonnumeric_lit (cb_tree x)
{
	const int	is_numeric_literal = CB_NUMERIC_LITERAL_P (x);
	const int	is_field_with_usage_not_display =
		CB_REFERENCE_P (x) && CB_FIELD_P (cb_ref (x))
		&& CB_FIELD (cb_ref (x))->usage != CB_USAGE_DISPLAY;
	
	if (is_numeric_literal) {
		cb_error_x (x, _("%s is not an alphanumeric literal"), CB_LITERAL (x)->data);
	} else if (is_field_with_usage_not_display) {
		cb_error_x (x, _("'%s' is not USAGE DISPLAY"), cb_name (x));
	}
}
 

#line 1487 "parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "parser.h".  */
#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOKEN_EOF = 0,
    ACCEPT = 258,
    ACCESS = 259,
    ADD = 260,
    ADDRESS = 261,
    ADVANCING = 262,
    AFTER = 263,
    ALL = 264,
    ALLOCATE = 265,
    ALPHABET = 266,
    ALPHABETIC = 267,
    ALPHABETIC_LOWER = 268,
    ALPHABETIC_UPPER = 269,
    ALPHANUMERIC = 270,
    ALPHANUMERIC_EDITED = 271,
    ALSO = 272,
    ALTER = 273,
    ALTERNATE = 274,
    AND = 275,
    ANY = 276,
    ARE = 277,
    AREA = 278,
    ARGUMENT_NUMBER = 279,
    ARGUMENT_VALUE = 280,
    AS = 281,
    ASCENDING = 282,
    ASCII = 283,
    ASSIGN = 284,
    AT = 285,
    ATTRIBUTE = 286,
    AUTO = 287,
    AUTOMATIC = 288,
    AWAY_FROM_ZERO = 289,
    BACKGROUND_COLOR = 290,
    BASED = 291,
    BEFORE = 292,
    BELL = 293,
    BINARY = 294,
    BINARY_C_LONG = 295,
    BINARY_CHAR = 296,
    BINARY_DOUBLE = 297,
    BINARY_LONG = 298,
    BINARY_SHORT = 299,
    BLANK = 300,
    BLINK = 301,
    BLOCK = 302,
    BOTTOM = 303,
    BY = 304,
    BYTE_LENGTH = 305,
    CALL = 306,
    CANCEL = 307,
    CAPACITY = 308,
    CF = 309,
    CH = 310,
    CHAINING = 311,
    CHARACTER = 312,
    CHARACTERS = 313,
    CLASS = 314,
    CLASSIFICATION = 315,
    CLASS_NAME = 316,
    CLOSE = 317,
    CODE = 318,
    CODE_SET = 319,
    COLLATING = 320,
    COL = 321,
    COLS = 322,
    COLUMN = 323,
    COLUMNS = 324,
    COMMA = 325,
    COMMAND_LINE = 326,
    COMMA_DELIM = 327,
    COMMIT = 328,
    COMMON = 329,
    COMP = 330,
    COMPUTE = 331,
    COMP_1 = 332,
    COMP_2 = 333,
    COMP_3 = 334,
    COMP_4 = 335,
    COMP_5 = 336,
    COMP_6 = 337,
    COMP_X = 338,
    CONCATENATE_FUNC = 339,
    CONDITION = 340,
    CONFIGURATION = 341,
    CONSTANT = 342,
    CONTAINS = 343,
    CONTENT = 344,
    CONTINUE = 345,
    CONTROL = 346,
    CONTROLS = 347,
    CONVERSION = 348,
    CONVERTING = 349,
    COPY = 350,
    CORRESPONDING = 351,
    COUNT = 352,
    CRT = 353,
    CRT_UNDER = 354,
    CURRENCY = 355,
    CURRENT_DATE_FUNC = 356,
    CURSOR = 357,
    CYCLE = 358,
    DATA = 359,
    DATE = 360,
    DAY = 361,
    DAY_OF_WEEK = 362,
    DE = 363,
    DEBUGGING = 364,
    DECIMAL_POINT = 365,
    DECLARATIVES = 366,
    DEFAULT = 367,
    DELETE = 368,
    DELIMITED = 369,
    DELIMITER = 370,
    DEPENDING = 371,
    DESCENDING = 372,
    DETAIL = 373,
    DISC = 374,
    DISK = 375,
    DISPLAY = 376,
    DISPLAY_OF_FUNC = 377,
    DIVIDE = 378,
    DIVISION = 379,
    DOWN = 380,
    DUPLICATES = 381,
    DYNAMIC = 382,
    EBCDIC = 383,
    EC = 384,
    EIGHTY_EIGHT = 385,
    ELSE = 386,
    END = 387,
    END_ACCEPT = 388,
    END_ADD = 389,
    END_CALL = 390,
    END_COMPUTE = 391,
    END_DELETE = 392,
    END_DISPLAY = 393,
    END_DIVIDE = 394,
    END_EVALUATE = 395,
    END_FUNCTION = 396,
    END_IF = 397,
    END_MULTIPLY = 398,
    END_PERFORM = 399,
    END_PROGRAM = 400,
    END_READ = 401,
    END_RETURN = 402,
    END_REWRITE = 403,
    END_SEARCH = 404,
    END_START = 405,
    END_STRING = 406,
    END_SUBTRACT = 407,
    END_UNSTRING = 408,
    END_WRITE = 409,
    ENTRY = 410,
    ENVIRONMENT = 411,
    ENVIRONMENT_NAME = 412,
    ENVIRONMENT_VALUE = 413,
    EOL = 414,
    EOP = 415,
    EOS = 416,
    EQUAL = 417,
    ERASE = 418,
    ERROR = 419,
    ESCAPE = 420,
    EVALUATE = 421,
    EVENT_STATUS = 422,
    EXCEPTION = 423,
    EXCEPTION_CONDITION = 424,
    EXCLUSIVE = 425,
    EXIT = 426,
    EXPONENTIATION = 427,
    EXTEND = 428,
    EXTERNAL = 429,
    F = 430,
    FD = 431,
    FILE_CONTROL = 432,
    FILE_ID = 433,
    FILLER = 434,
    FINAL = 435,
    FIRST = 436,
    FIXED = 437,
    FLOAT_BINARY_128 = 438,
    FLOAT_BINARY_32 = 439,
    FLOAT_BINARY_64 = 440,
    FLOAT_DECIMAL_16 = 441,
    FLOAT_DECIMAL_34 = 442,
    FLOAT_DECIMAL_7 = 443,
    FLOAT_EXTENDED = 444,
    FLOAT_LONG = 445,
    FLOAT_SHORT = 446,
    FOOTING = 447,
    FOR = 448,
    FOREGROUND_COLOR = 449,
    FOREVER = 450,
    FORMATTED_DATE_FUNC = 451,
    FORMATTED_DATETIME_FUNC = 452,
    FORMATTED_TIME_FUNC = 453,
    FREE = 454,
    FROM = 455,
    FROM_CRT = 456,
    FULL = 457,
    FUNCTION = 458,
    FUNCTION_ID = 459,
    FUNCTION_NAME = 460,
    GENERATE = 461,
    GIVING = 462,
    GLOBAL = 463,
    GO = 464,
    GOBACK = 465,
    GREATER = 466,
    GREATER_OR_EQUAL = 467,
    GRID = 468,
    GROUP = 469,
    HEADING = 470,
    HIGHLIGHT = 471,
    HIGH_VALUE = 472,
    ID = 473,
    IDENTIFICATION = 474,
    IF = 475,
    IGNORE = 476,
    IGNORING = 477,
    IN = 478,
    INDEX = 479,
    INDEXED = 480,
    INDICATE = 481,
    INITIALIZE = 482,
    INITIALIZED = 483,
    INITIATE = 484,
    INPUT = 485,
    INPUT_OUTPUT = 486,
    INSPECT = 487,
    INTO = 488,
    INTRINSIC = 489,
    INVALID = 490,
    INVALID_KEY = 491,
    IS = 492,
    I_O = 493,
    I_O_CONTROL = 494,
    JUSTIFIED = 495,
    KEPT = 496,
    KEY = 497,
    KEYBOARD = 498,
    LABEL = 499,
    LAST = 500,
    LEADING = 501,
    LEFT = 502,
    LEFTLINE = 503,
    LENGTH = 504,
    LENGTH_OF = 505,
    LESS = 506,
    LESS_OR_EQUAL = 507,
    LIMIT = 508,
    LIMITS = 509,
    LINAGE = 510,
    LINAGE_COUNTER = 511,
    LINE = 512,
    LINE_COUNTER = 513,
    LINES = 514,
    LINKAGE = 515,
    LITERAL = 516,
    LOCALE = 517,
    LOCALE_DATE_FUNC = 518,
    LOCALE_TIME_FUNC = 519,
    LOCALE_TIME_FROM_FUNC = 520,
    LOCAL_STORAGE = 521,
    LOCK = 522,
    LOWER = 523,
    LOWER_CASE_FUNC = 524,
    LOWLIGHT = 525,
    LOW_VALUE = 526,
    MANUAL = 527,
    MEMORY = 528,
    MERGE = 529,
    MINUS = 530,
    MNEMONIC_NAME = 531,
    MODE = 532,
    MOVE = 533,
    MULTIPLE = 534,
    MULTIPLY = 535,
    NAME = 536,
    NATIONAL = 537,
    NATIONAL_EDITED = 538,
    NATIONAL_OF_FUNC = 539,
    NATIVE = 540,
    NEAREST_AWAY_FROM_ZERO = 541,
    NEAREST_EVEN = 542,
    NEAREST_TOWARD_ZERO = 543,
    NEGATIVE = 544,
    NEXT = 545,
    NEXT_PAGE = 546,
    NO = 547,
    NO_ECHO = 548,
    NORMAL = 549,
    NOT = 550,
    NOTHING = 551,
    NOT_END = 552,
    NOT_EOP = 553,
    NOT_ESCAPE = 554,
    NOT_EQUAL = 555,
    NOT_EXCEPTION = 556,
    NOT_INVALID_KEY = 557,
    NOT_OVERFLOW = 558,
    NOT_SIZE_ERROR = 559,
    NO_ADVANCING = 560,
    NUMBER = 561,
    NUMBERS = 562,
    NUMERIC = 563,
    NUMERIC_EDITED = 564,
    NUMVALC_FUNC = 565,
    OBJECT_COMPUTER = 566,
    OCCURS = 567,
    OF = 568,
    OFF = 569,
    OMITTED = 570,
    ON = 571,
    ONLY = 572,
    OPEN = 573,
    OPTIONAL = 574,
    OR = 575,
    ORDER = 576,
    ORGANIZATION = 577,
    OTHER = 578,
    OUTPUT = 579,
    OVERLINE = 580,
    PACKED_DECIMAL = 581,
    PADDING = 582,
    PAGE = 583,
    PAGE_COUNTER = 584,
    PARAGRAPH = 585,
    PERFORM = 586,
    PH = 587,
    PF = 588,
    PICTURE = 589,
    PICTURE_SYMBOL = 590,
    PLUS = 591,
    POINTER = 592,
    POSITION = 593,
    POSITIVE = 594,
    PRESENT = 595,
    PREVIOUS = 596,
    PRINT = 597,
    PRINTER = 598,
    PRINTER_1 = 599,
    PRINTING = 600,
    PROCEDURE = 601,
    PROCEDURES = 602,
    PROCEED = 603,
    PROGRAM = 604,
    PROGRAM_ID = 605,
    PROGRAM_NAME = 606,
    PROGRAM_POINTER = 607,
    PROHIBITED = 608,
    PROMPT = 609,
    PROTECTED = 610,
    QUOTE = 611,
    RANDOM = 612,
    RD = 613,
    READ = 614,
    READY_TRACE = 615,
    RECORD = 616,
    RECORDING = 617,
    RECORDS = 618,
    RECURSIVE = 619,
    REDEFINES = 620,
    REEL = 621,
    REFERENCE = 622,
    REFERENCES = 623,
    RELATIVE = 624,
    RELEASE = 625,
    REMAINDER = 626,
    REMOVAL = 627,
    RENAMES = 628,
    REPLACE = 629,
    REPLACING = 630,
    REPORT = 631,
    REPORTING = 632,
    REPORTS = 633,
    REPOSITORY = 634,
    REQUIRED = 635,
    RESERVE = 636,
    RESET = 637,
    RESET_TRACE = 638,
    RETURN = 639,
    RETURNING = 640,
    REVERSE_FUNC = 641,
    REVERSE_VIDEO = 642,
    REVERSED = 643,
    REWIND = 644,
    REWRITE = 645,
    RF = 646,
    RH = 647,
    RIGHT = 648,
    ROLLBACK = 649,
    ROUNDED = 650,
    RUN = 651,
    S = 652,
    SAME = 653,
    SCREEN = 654,
    SCREEN_CONTROL = 655,
    SCROLL = 656,
    SD = 657,
    SEARCH = 658,
    SECTION = 659,
    SECURE = 660,
    SEGMENT_LIMIT = 661,
    SELECT = 662,
    SEMI_COLON = 663,
    SENTENCE = 664,
    SEPARATE = 665,
    SEQUENCE = 666,
    SEQUENTIAL = 667,
    SET = 668,
    SEVENTY_EIGHT = 669,
    SHARING = 670,
    SIGN = 671,
    SIGNED = 672,
    SIGNED_INT = 673,
    SIGNED_LONG = 674,
    SIGNED_SHORT = 675,
    SIXTY_SIX = 676,
    SIZE = 677,
    SIZE_ERROR = 678,
    SORT = 679,
    SORT_MERGE = 680,
    SOURCE = 681,
    SOURCE_COMPUTER = 682,
    SPACE = 683,
    SPECIAL_NAMES = 684,
    STANDARD = 685,
    STANDARD_1 = 686,
    STANDARD_2 = 687,
    START = 688,
    STATIC = 689,
    STATUS = 690,
    STDCALL = 691,
    STEP = 692,
    STOP = 693,
    STRING = 694,
    SUBSTITUTE_FUNC = 695,
    SUBSTITUTE_CASE_FUNC = 696,
    SUBTRACT = 697,
    SUM = 698,
    SUPPRESS = 699,
    SYMBOLIC = 700,
    SYNCHRONIZED = 701,
    SYSTEM_DEFAULT = 702,
    SYSTEM_OFFSET = 703,
    TAB = 704,
    TALLYING = 705,
    TAPE = 706,
    TERMINATE = 707,
    TEST = 708,
    THAN = 709,
    THEN = 710,
    THRU = 711,
    TIME = 712,
    TIME_OUT = 713,
    TIMES = 714,
    TO = 715,
    TOK_AMPER = 716,
    TOK_CLOSE_PAREN = 717,
    TOK_COLON = 718,
    TOK_DIV = 719,
    TOK_DOT = 720,
    TOK_EQUAL = 721,
    TOK_FALSE = 722,
    TOK_FILE = 723,
    TOK_GREATER = 724,
    TOK_INITIAL = 725,
    TOK_LESS = 726,
    TOK_MINUS = 727,
    TOK_MUL = 728,
    TOK_NULL = 729,
    TOK_OVERFLOW = 730,
    TOK_OPEN_PAREN = 731,
    TOK_PLUS = 732,
    TOK_TRUE = 733,
    TOP = 734,
    TOWARD_GREATER = 735,
    TOWARD_LESSER = 736,
    TRAILING = 737,
    TRANSFORM = 738,
    TRIM_FUNC = 739,
    TRUNCATION = 740,
    TYPE = 741,
    U = 742,
    UNDERLINE = 743,
    UNIT = 744,
    UNLOCK = 745,
    UNSIGNED = 746,
    UNSIGNED_INT = 747,
    UNSIGNED_LONG = 748,
    UNSIGNED_SHORT = 749,
    UNSTRING = 750,
    UNTIL = 751,
    UP = 752,
    UPDATE = 753,
    UPON = 754,
    UPON_ARGUMENT_NUMBER = 755,
    UPON_COMMAND_LINE = 756,
    UPON_ENVIRONMENT_NAME = 757,
    UPON_ENVIRONMENT_VALUE = 758,
    UPPER = 759,
    UPPER_CASE_FUNC = 760,
    USAGE = 761,
    USE = 762,
    USER = 763,
    USER_DEFAULT = 764,
    USER_FUNCTION_NAME = 765,
    USING = 766,
    V = 767,
    VALUE = 768,
    VARIABLE = 769,
    VARYING = 770,
    WAIT = 771,
    WHEN = 772,
    WHEN_COMPILED_FUNC = 773,
    WITH = 774,
    WORD = 775,
    WORDS = 776,
    WORKING_STORAGE = 777,
    WRITE = 778,
    YYYYDDD = 779,
    YYYYMMDD = 780,
    ZERO = 781,
    SHIFT_PREFER = 782,
    OVERFLOW = 783
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 2068 "parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   8839

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  529
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  870
/* YYNRULES -- Number of rules.  */
#define YYNRULES  2007
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2856

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   783

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  2081,  2081,  2081,  2114,  2115,  2119,  2120,  2124,  2125,
    2129,  2129,  2152,  2163,  2169,  2170,  2174,  2175,  2179,  2187,
    2196,  2204,  2205,  2206,  2211,  2215,  2210,  2231,  2230,  2246,
    2257,  2261,  2262,  2266,  2267,  2270,  2271,  2275,  2284,  2293,
    2294,  2301,  2302,  2306,  2310,  2320,  2325,  2326,  2335,  2342,
    2343,  2353,  2354,  2355,  2356,  2357,  2370,  2369,  2379,  2380,
    2383,  2384,  2398,  2397,  2407,  2408,  2409,  2410,  2414,  2415,
    2419,  2420,  2421,  2422,  2426,  2434,  2441,  2448,  2459,  2463,
    2467,  2471,  2478,  2479,  2484,  2486,  2485,  2496,  2497,  2498,
    2505,  2506,  2510,  2514,  2520,  2521,  2531,  2536,  2546,  2547,
    2559,  2560,  2564,  2565,  2569,  2570,  2574,  2575,  2576,  2577,
    2578,  2579,  2580,  2581,  2582,  2583,  2584,  2585,  2593,  2592,
    2620,  2630,  2643,  2651,  2654,  2655,  2659,  2666,  2681,  2702,
    2701,  2725,  2731,  2737,  2743,  2749,  2755,  2765,  2769,  2776,
    2780,  2785,  2784,  2795,  2799,  2806,  2807,  2808,  2809,  2810,
    2811,  2815,  2816,  2823,  2838,  2841,  2848,  2856,  2860,  2871,
    2891,  2899,  2910,  2911,  2917,  2938,  2939,  2943,  2947,  2968,
    2991,  3073,  3076,  3085,  3104,  3120,  3138,  3156,  3173,  3190,
    3200,  3201,  3208,  3209,  3217,  3218,  3228,  3229,  3234,  3233,
    3263,  3264,  3268,  3269,  3270,  3271,  3272,  3273,  3274,  3275,
    3276,  3277,  3278,  3279,  3280,  3287,  3293,  3303,  3316,  3329,
    3356,  3357,  3358,  3362,  3363,  3364,  3365,  3368,  3369,  3375,
    3376,  3380,  3384,  3385,  3390,  3393,  3394,  3401,  3409,  3410,
    3411,  3418,  3442,  3444,  3449,  3459,  3467,  3482,  3489,  3491,
    3492,  3498,  3498,  3505,  3510,  3515,  3522,  3523,  3524,  3528,
    3539,  3540,  3544,  3549,  3554,  3559,  3570,  3581,  3591,  3599,
    3600,  3601,  3607,  3618,  3625,  3626,  3632,  3640,  3641,  3642,
    3648,  3649,  3650,  3657,  3658,  3662,  3663,  3669,  3697,  3698,
    3699,  3700,  3707,  3706,  3722,  3723,  3727,  3730,  3731,  3741,
    3738,  3754,  3755,  3763,  3764,  3772,  3773,  3777,  3798,  3797,
    3814,  3821,  3825,  3831,  3832,  3836,  3846,  3861,  3862,  3863,
    3864,  3865,  3866,  3867,  3868,  3869,  3876,  3883,  3883,  3883,
    3889,  3909,  3943,  3974,  3975,  3982,  3983,  3987,  3988,  3995,
    4006,  4011,  4022,  4023,  4027,  4028,  4034,  4045,  4063,  4064,
    4068,  4069,  4070,  4074,  4081,  4088,  4097,  4106,  4107,  4108,
    4109,  4110,  4119,  4120,  4126,  4161,  4162,  4175,  4190,  4191,
    4195,  4205,  4219,  4221,  4220,  4236,  4239,  4239,  4256,  4257,
    4261,  4262,  4263,  4265,  4264,  4279,  4292,  4300,  4305,  4311,
    4315,  4325,  4328,  4340,  4341,  4342,  4343,  4347,  4351,  4355,
    4359,  4363,  4367,  4371,  4375,  4379,  4383,  4387,  4391,  4395,
    4406,  4407,  4411,  4412,  4416,  4417,  4418,  4422,  4423,  4427,
    4452,  4455,  4463,  4462,  4475,  4499,  4498,  4512,  4516,  4525,
    4529,  4538,  4539,  4540,  4541,  4542,  4543,  4544,  4545,  4546,
    4547,  4548,  4549,  4550,  4557,  4581,  4609,  4612,  4620,  4621,
    4625,  4650,  4661,  4662,  4666,  4670,  4674,  4678,  4682,  4686,
    4690,  4694,  4698,  4702,  4706,  4710,  4714,  4719,  4724,  4728,
    4732,  4740,  4744,  4748,  4756,  4760,  4764,  4768,  4772,  4776,
    4780,  4784,  4788,  4796,  4804,  4808,  4812,  4816,  4820,  4824,
    4832,  4833,  4837,  4838,  4844,  4850,  4862,  4880,  4881,  4890,
    4922,  4952,  4953,  4957,  4958,  4961,  4962,  4968,  4969,  4976,
    4977,  4984,  5008,  5009,  5026,  5027,  5030,  5031,  5038,  5039,
    5044,  5055,  5066,  5077,  5088,  5117,  5116,  5125,  5126,  5130,
    5131,  5134,  5135,  5147,  5156,  5170,  5172,  5171,  5191,  5193,
    5192,  5208,  5210,  5209,  5218,  5219,  5226,  5225,  5238,  5239,
    5240,  5247,  5252,  5256,  5257,  5263,  5270,  5274,  5275,  5281,
    5318,  5322,  5327,  5333,  5334,  5339,  5340,  5341,  5342,  5343,
    5347,  5354,  5361,  5368,  5375,  5381,  5382,  5387,  5386,  5393,
    5394,  5398,  5399,  5400,  5401,  5402,  5403,  5404,  5405,  5406,
    5407,  5408,  5409,  5410,  5411,  5412,  5413,  5417,  5424,  5425,
    5426,  5427,  5428,  5429,  5430,  5433,  5434,  5435,  5438,  5439,
    5443,  5450,  5456,  5457,  5461,  5462,  5466,  5473,  5477,  5484,
    5485,  5489,  5496,  5497,  5501,  5502,  5506,  5507,  5508,  5512,
    5513,  5517,  5518,  5522,  5529,  5536,  5544,  5546,  5545,  5566,
    5567,  5571,  5572,  5576,  5578,  5577,  5637,  5655,  5656,  5660,
    5665,  5670,  5674,  5678,  5683,  5688,  5693,  5698,  5702,  5706,
    5711,  5716,  5721,  5725,  5729,  5733,  5737,  5742,  5746,  5750,
    5755,  5760,  5765,  5770,  5771,  5772,  5773,  5774,  5775,  5776,
    5777,  5778,  5787,  5792,  5803,  5804,  5808,  5809,  5813,  5814,
    5818,  5819,  5824,  5827,  5831,  5839,  5842,  5846,  5854,  5865,
    5873,  5875,  5885,  5874,  5912,  5912,  5945,  5949,  5948,  5962,
    5961,  5981,  5982,  5987,  6009,  6011,  6015,  6026,  6028,  6036,
    6044,  6052,  6081,  6114,  6117,  6130,  6135,  6145,  6176,  6178,
    6177,  6214,  6215,  6219,  6220,  6221,  6238,  6239,  6250,  6249,
    6299,  6300,  6304,  6352,  6372,  6375,  6394,  6399,  6393,  6412,
    6412,  6442,  6449,  6450,  6451,  6452,  6453,  6454,  6455,  6456,
    6457,  6458,  6459,  6460,  6461,  6462,  6463,  6464,  6465,  6466,
    6467,  6468,  6469,  6470,  6471,  6472,  6473,  6474,  6475,  6476,
    6477,  6478,  6479,  6480,  6481,  6482,  6483,  6484,  6485,  6486,
    6487,  6488,  6489,  6490,  6491,  6492,  6493,  6494,  6495,  6496,
    6497,  6498,  6512,  6524,  6523,  6540,  6539,  6557,  6561,  6565,
    6570,  6575,  6580,  6585,  6589,  6593,  6597,  6601,  6606,  6610,
    6614,  6618,  6622,  6626,  6630,  6637,  6638,  6644,  6646,  6650,
    6651,  6655,  6656,  6660,  6664,  6668,  6669,  6673,  6689,  6705,
    6718,  6722,  6723,  6727,  6734,  6738,  6744,  6748,  6752,  6756,
    6760,  6766,  6770,  6774,  6780,  6784,  6788,  6792,  6796,  6800,
    6804,  6808,  6812,  6816,  6820,  6826,  6830,  6834,  6838,  6842,
    6846,  6850,  6857,  6858,  6862,  6866,  6884,  6883,  6892,  6896,
    6900,  6906,  6907,  6914,  6918,  6929,  6928,  6937,  6941,  6953,
    6954,  6962,  6961,  6970,  6971,  6975,  6981,  6981,  6988,  6987,
    7000,  6999,  7027,  7031,  7036,  7041,  7061,  7062,  7070,  7074,
    7073,  7090,  7091,  7096,  7104,  7128,  7130,  7134,  7143,  7156,
    7159,  7163,  7167,  7172,  7195,  7196,  7200,  7201,  7205,  7209,
    7213,  7224,  7228,  7235,  7239,  7247,  7251,  7258,  7265,  7269,
    7280,  7279,  7291,  7295,  7302,  7303,  7313,  7312,  7320,  7325,
    7333,  7334,  7335,  7336,  7337,  7345,  7344,  7353,  7360,  7364,
    7374,  7385,  7403,  7402,  7411,  7415,  7419,  7424,  7432,  7436,
    7447,  7446,  7458,  7462,  7466,  7470,  7474,  7478,  7486,  7495,
    7496,  7501,  7500,  7545,  7549,  7557,  7558,  7562,  7566,  7571,
    7575,  7576,  7580,  7584,  7588,  7592,  7599,  7600,  7604,  7608,
    7614,  7620,  7624,  7628,  7634,  7640,  7646,  7652,  7656,  7660,
    7664,  7668,  7672,  7676,  7680,  7687,  7691,  7702,  7701,  7710,
    7714,  7718,  7722,  7726,  7733,  7737,  7748,  7747,  7756,  7775,
    7774,  7798,  7806,  7807,  7812,  7823,  7834,  7848,  7852,  7859,
    7860,  7865,  7874,  7883,  7888,  7897,  7898,  7903,  7965,  7966,
    7967,  7971,  7972,  7976,  7980,  7991,  7990,  8002,  8003,  8024,
    8038,  8060,  8082,  8102,  8125,  8126,  8134,  8133,  8142,  8153,
    8152,  8162,  8169,  8168,  8181,  8190,  8194,  8205,  8221,  8220,
    8229,  8233,  8237,  8244,  8248,  8259,  8258,  8266,  8274,  8275,
    8279,  8280,  8281,  8286,  8289,  8296,  8300,  8308,  8315,  8316,
    8317,  8318,  8319,  8320,  8321,  8326,  8329,  8339,  8338,  8347,
    8353,  8365,  8364,  8373,  8377,  8378,  8379,  8383,  8384,  8385,
    8386,  8393,  8392,  8413,  8423,  8432,  8436,  8443,  8448,  8453,
    8458,  8463,  8468,  8476,  8477,  8481,  8486,  8492,  8494,  8495,
    8496,  8497,  8501,  8529,  8532,  8536,  8540,  8544,  8551,  8558,
    8568,  8567,  8580,  8579,  8587,  8591,  8602,  8601,  8610,  8614,
    8621,  8625,  8636,  8635,  8643,  8664,  8688,  8689,  8690,  8691,
    8695,  8696,  8700,  8701,  8702,  8703,  8715,  8714,  8725,  8731,
    8730,  8741,  8749,  8757,  8764,  8768,  8781,  8788,  8800,  8803,
    8808,  8812,  8823,  8830,  8831,  8835,  8836,  8839,  8840,  8845,
    8856,  8855,  8864,  8891,  8892,  8897,  8900,  8904,  8908,  8912,
    8916,  8920,  8927,  8928,  8932,  8933,  8937,  8941,  8951,  8962,
    8961,  8969,  8979,  8990,  8989,  8998,  9005,  9009,  9020,  9019,
    9031,  9040,  9043,  9047,  9054,  9058,  9068,  9080,  9079,  9088,
    9092,  9101,  9102,  9107,  9110,  9118,  9122,  9129,  9137,  9141,
    9152,  9151,  9165,  9166,  9167,  9168,  9169,  9170,  9171,  9175,
    9176,  9180,  9181,  9187,  9196,  9203,  9204,  9208,  9212,  9216,
    9222,  9228,  9232,  9236,  9240,  9249,  9253,  9262,  9271,  9272,
    9276,  9285,  9286,  9290,  9294,  9303,  9313,  9312,  9321,  9320,
    9352,  9355,  9375,  9376,  9379,  9380,  9388,  9389,  9394,  9399,
    9409,  9425,  9430,  9440,  9457,  9456,  9466,  9479,  9482,  9490,
    9493,  9498,  9503,  9511,  9512,  9513,  9514,  9515,  9516,  9520,
    9528,  9529,  9533,  9537,  9548,  9547,  9557,  9570,  9573,  9577,
    9581,  9589,  9601,  9604,  9611,  9612,  9613,  9614,  9621,  9620,
    9630,  9637,  9638,  9642,  9657,  9658,  9663,  9664,  9668,  9669,
    9673,  9677,  9688,  9687,  9696,  9700,  9704,  9711,  9715,  9725,
    9736,  9737,  9744,  9743,  9752,  9758,  9770,  9769,  9777,  9791,
    9790,  9798,  9815,  9814,  9823,  9831,  9832,  9837,  9838,  9843,
    9850,  9851,  9856,  9863,  9864,  9868,  9869,  9873,  9874,  9878,
    9882,  9893,  9892,  9901,  9902,  9903,  9904,  9905,  9909,  9936,
    9939,  9951,  9961,  9966,  9971,  9976,  9984, 10022, 10023, 10027,
   10067, 10077, 10100, 10101, 10102, 10103, 10107, 10116, 10122, 10132,
   10141, 10150, 10151, 10158, 10157, 10169, 10179, 10180, 10185, 10188,
   10192, 10196, 10203, 10204, 10208, 10209, 10210, 10214, 10218, 10230,
   10231, 10232, 10242, 10246, 10253, 10261, 10262, 10266, 10267, 10271,
   10279, 10280, 10285, 10286, 10287, 10297, 10301, 10308, 10316, 10317,
   10321, 10331, 10332, 10333, 10343, 10347, 10354, 10362, 10363, 10367,
   10377, 10378, 10379, 10389, 10393, 10400, 10408, 10409, 10413, 10424,
   10425, 10432, 10434, 10443, 10447, 10454, 10462, 10463, 10467, 10477,
   10478, 10488, 10492, 10499, 10507, 10508, 10512, 10522, 10523, 10527,
   10528, 10538, 10542, 10549, 10557, 10558, 10562, 10572, 10576, 10586,
   10593, 10600, 10600, 10611, 10612, 10613, 10617, 10618, 10620, 10621,
   10623, 10624, 10625, 10626, 10627, 10629, 10630, 10631, 10632, 10633,
   10634, 10636, 10637, 10638, 10640, 10641, 10642, 10643, 10644, 10647,
   10648, 10652, 10653, 10657, 10658, 10662, 10663, 10667, 10671, 10677,
   10681, 10687, 10688, 10689, 10693, 10694, 10695, 10699, 10700, 10701,
   10705, 10709, 10713, 10714, 10715, 10718, 10719, 10729, 10741, 10750,
   10762, 10771, 10783, 10798, 10799, 10804, 10813, 10819, 10841, 10845,
   10866, 10907, 10921, 10922, 10927, 10933, 10934, 10939, 10951, 10952,
   10953, 10960, 10971, 10972, 10976, 10984, 10992, 10996, 11003, 11012,
   11013, 11019, 11028, 11039, 11056, 11060, 11067, 11068, 11069, 11076,
   11077, 11081, 11085, 11092, 11093, 11097, 11098, 11102, 11103, 11104,
   11105, 11109, 11113, 11117, 11121, 11125, 11146, 11150, 11157, 11158,
   11159, 11163, 11164, 11165, 11166, 11167, 11171, 11175, 11182, 11183,
   11187, 11188, 11192, 11199, 11206, 11207, 11208, 11212, 11213, 11217,
   11221, 11225, 11229, 11230, 11234, 11238, 11239, 11246, 11250, 11254,
   11258, 11262, 11266, 11267, 11273, 11277, 11281, 11282, 11286, 11293,
   11303, 11322, 11340, 11347, 11354, 11361, 11371, 11378, 11388, 11398,
   11408, 11421, 11425, 11433, 11441, 11445, 11455, 11469, 11492, 11514,
   11530, 11531, 11532, 11533, 11534, 11535, 11539, 11543, 11560, 11564,
   11571, 11572, 11573, 11574, 11575, 11576, 11577, 11583, 11587, 11591,
   11595, 11599, 11603, 11607, 11611, 11615, 11619, 11623, 11627, 11634,
   11635, 11639, 11640, 11641, 11645, 11646, 11647, 11648, 11652, 11656,
   11660, 11667, 11671, 11675, 11682, 11689, 11696, 11706, 11713, 11723,
   11730, 11740, 11744, 11757, 11761, 11776, 11784, 11785, 11789, 11790,
   11794, 11795, 11800, 11803, 11811, 11814, 11821, 11823, 11824, 11828,
   11829, 11833, 11834, 11835, 11840, 11843, 11856, 11860, 11868, 11872,
   11876, 11880, 11884, 11888, 11892, 11896, 11903, 11904, 11910, 11911,
   11912, 11913, 11914, 11915, 11916, 11917, 11918, 11919, 11920, 11921,
   11922, 11923, 11924, 11925, 11926, 11927, 11928, 11929, 11930, 11931,
   11932, 11933, 11934, 11935, 11936, 11937, 11938, 11939, 11940, 11941,
   11942, 11943, 11944, 11945, 11946, 11947, 11948, 11949, 11950, 11951,
   11952, 11953, 11954, 11955, 11956, 11957, 11958, 11959, 11960, 11961,
   11962, 11963, 11964, 11965, 11966, 11967, 11968, 11969, 11970, 11971,
   11972, 11973, 11974, 11975, 11976, 11977, 11978, 11979, 11986, 11986,
   11987, 11987, 11988, 11988, 11989, 11989, 11990, 11990, 11991, 11991,
   11992, 11992, 11993, 11993, 11994, 11994, 11995, 11995, 11996, 11996,
   11997, 11997, 11998, 11998, 11999, 11999, 12000, 12000, 12001, 12001,
   12002, 12002, 12003, 12003, 12004, 12004, 12004, 12005, 12005, 12006,
   12006, 12007, 12007, 12008, 12008, 12009, 12009, 12009, 12010, 12010,
   12011, 12011, 12011, 12012, 12012, 12012, 12013, 12013, 12013, 12014,
   12014, 12015, 12015, 12016, 12016, 12017, 12017, 12017, 12018, 12018,
   12019, 12019, 12020, 12020, 12020, 12020, 12021, 12021, 12022, 12022,
   12023, 12023, 12024, 12024, 12025, 12025, 12025, 12026, 12026, 12027,
   12027, 12028, 12028, 12029, 12029, 12029, 12030, 12030, 12031, 12031,
   12032, 12032, 12033, 12033, 12034, 12034, 12035, 12035, 12036, 12036,
   12037, 12037, 12037, 12038, 12038, 12039, 12039, 12040, 12040, 12044,
   12044, 12045, 12045, 12046, 12046, 12047, 12047, 12048, 12048, 12049,
   12049, 12050, 12050, 12051, 12051, 12052, 12052, 12053, 12053, 12054,
   12054, 12055, 12055, 12056, 12056, 12057, 12057, 12058, 12058, 12061,
   12062, 12063, 12067, 12067, 12068, 12068, 12069, 12069, 12070, 12070,
   12071, 12071, 12072, 12072, 12073, 12073, 12074, 12074
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "ACCEPT", "ACCESS", "ADD",
  "ADDRESS", "ADVANCING", "AFTER", "ALL", "ALLOCATE", "ALPHABET",
  "ALPHABETIC", "\"ALPHABETIC-LOWER\"", "\"ALPHABETIC-UPPER\"",
  "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"", "ALSO", "ALTER", "ALTERNATE",
  "AND", "ANY", "ARE", "AREA", "\"ARGUMENT-NUMBER\"", "\"ARGUMENT-VALUE\"",
  "AS", "ASCENDING", "ASCII", "ASSIGN", "AT", "ATTRIBUTE", "AUTO",
  "AUTOMATIC", "\"AWAY-FROM-ZERO\"", "\"BACKGROUND-COLOR\"", "BASED",
  "BEFORE", "BELL", "BINARY", "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"",
  "\"BINARY-DOUBLE\"", "\"BINARY-LONG\"", "\"BINARY-SHORT\"", "BLANK",
  "BLINK", "BLOCK", "BOTTOM", "BY", "\"BYTE-LENGTH\"", "CALL", "CANCEL",
  "CAPACITY", "CF", "CH", "CHAINING", "CHARACTER", "CHARACTERS", "CLASS",
  "CLASSIFICATION", "\"class-name\"", "CLOSE", "CODE", "\"CODE-SET\"",
  "COLLATING", "COL", "COLS", "COLUMN", "COLUMNS", "COMMA",
  "\"COMMAND-LINE\"", "\"comma delimiter\"", "COMMIT", "COMMON", "COMP",
  "COMPUTE", "\"COMP-1\"", "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"",
  "\"COMP-5\"", "\"COMP-6\"", "\"COMP-X\"", "\"FUNCTION CONCATENATE\"",
  "CONDITION", "CONFIGURATION", "CONSTANT", "CONTAINS", "CONTENT",
  "CONTINUE", "CONTROL", "CONTROLS", "CONVERSION", "CONVERTING", "COPY",
  "CORRESPONDING", "COUNT", "CRT", "\"CRT-UNDER\"", "CURRENCY",
  "\"FUNCTION CURRENT-DATE\"", "CURSOR", "CYCLE", "DATA", "DATE", "DAY",
  "\"DAY-OF-WEEK\"", "DE", "DEBUGGING", "\"DECIMAL-POINT\"",
  "DECLARATIVES", "DEFAULT", "DELETE", "DELIMITED", "DELIMITER",
  "DEPENDING", "DESCENDING", "DETAIL", "DISC", "DISK", "DISPLAY",
  "\"FUNCTION DISPLAY-OF\"", "DIVIDE", "DIVISION", "DOWN", "DUPLICATES",
  "DYNAMIC", "EBCDIC", "EC", "\"88\"", "ELSE", "END", "\"END-ACCEPT\"",
  "\"END-ADD\"", "\"END-CALL\"", "\"END-COMPUTE\"", "\"END-DELETE\"",
  "\"END-DISPLAY\"", "\"END-DIVIDE\"", "\"END-EVALUATE\"",
  "\"END FUNCTION\"", "\"END-IF\"", "\"END-MULTIPLY\"", "\"END-PERFORM\"",
  "\"END PROGRAM\"", "\"END-READ\"", "\"END-RETURN\"", "\"END-REWRITE\"",
  "\"END-SEARCH\"", "\"END-START\"", "\"END-STRING\"", "\"END-SUBTRACT\"",
  "\"END-UNSTRING\"", "\"END-WRITE\"", "ENTRY", "ENVIRONMENT",
  "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL", "EOP", "EOS",
  "EQUAL", "ERASE", "ERROR", "ESCAPE", "EVALUATE", "\"EVENT STATUS\"",
  "EXCEPTION", "\"EXCEPTION CONDITION\"", "EXCLUSIVE", "EXIT",
  "\"Exponentiation operator\"", "EXTEND", "EXTERNAL", "F", "FD",
  "\"FILE-CONTROL\"", "\"FILE-ID\"", "FILLER", "FINAL", "FIRST", "FIXED",
  "\"FLOAT-BINARY-128\"", "\"FLOAT-BINARY-32\"", "\"FLOAT-BINARY-64\"",
  "\"FLOAT-DECIMAL-16\"", "\"FLOAT-DECIMAL-34\"", "\"FLOAT-DECIMAL-7\"",
  "\"FLOAT-EXTENDED\"", "\"FLOAT-LONG\"", "\"FLOAT-SHORT\"", "FOOTING",
  "FOR", "\"FOREGROUND-COLOR\"", "FOREVER", "\"FUNCTION FORMATTED-DATE\"",
  "\"FUNCTION FORMATTED-DATETIME\"", "\"FUNCTION FORMATTED-TIME\"", "FREE",
  "FROM", "\"FROM CRT\"", "FULL", "FUNCTION", "\"FUNCTION-ID\"",
  "\"Intrinsic function name\"", "GENERATE", "GIVING", "GLOBAL", "GO",
  "GOBACK", "GREATER", "\"GREATER OR EQUAL\"", "GRID", "GROUP", "HEADING",
  "HIGHLIGHT", "\"HIGH-VALUE\"", "ID", "IDENTIFICATION", "IF", "IGNORE",
  "IGNORING", "IN", "INDEX", "INDEXED", "INDICATE", "INITIALIZE",
  "INITIALIZED", "INITIATE", "INPUT", "\"INPUT-OUTPUT\"", "INSPECT",
  "INTO", "INTRINSIC", "INVALID", "\"INVALID KEY\"", "IS", "\"I-O\"",
  "\"I-O-CONTROL\"", "JUSTIFIED", "KEPT", "KEY", "KEYBOARD", "LABEL",
  "LAST", "LEADING", "LEFT", "LEFTLINE", "LENGTH", "\"LENGTH OF\"", "LESS",
  "\"LESS OR EQUAL\"", "LIMIT", "LIMITS", "LINAGE", "\"LINAGE-COUNTER\"",
  "LINE", "\"LINE-COUNTER\"", "LINES", "LINKAGE", "\"Literal\"", "LOCALE",
  "\"FUNCTION LOCALE-DATE\"", "\"FUNCTION LOCALE-TIME\"",
  "\"FUNCTION LOCALE-TIME-FROM-SECONDS\"", "\"LOCAL-STORAGE\"", "LOCK",
  "LOWER", "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-VALUE\"",
  "MANUAL", "MEMORY", "MERGE", "MINUS", "\"Mnemonic name\"", "MODE",
  "MOVE", "MULTIPLE", "MULTIPLY", "NAME", "NATIONAL",
  "\"NATIONAL-EDITED\"", "\"FUNCTION NATIONAL-OF\"", "NATIVE",
  "\"NEAREST-AWAY-FROM-ZERO\"", "\"NEAREST-EVEN\"",
  "\"NEAREST-TOWARD-ZERO\"", "NEGATIVE", "NEXT", "\"NEXT PAGE\"", "NO",
  "\"NO-ECHO\"", "NORMAL", "NOT", "NOTHING", "\"NOT END\"", "\"NOT EOP\"",
  "\"NOT ESCAPE\"", "\"NOT EQUAL\"", "\"NOT EXCEPTION\"",
  "\"NOT INVALID KEY\"", "\"NOT OVERFLOW\"", "\"NOT SIZE ERROR\"",
  "\"NO ADVANCING\"", "NUMBER", "NUMBERS", "NUMERIC", "\"NUMERIC-EDITED\"",
  "\"FUNCTION NUMVAL-C\"", "\"OBJECT-COMPUTER\"", "OCCURS", "OF", "OFF",
  "OMITTED", "ON", "ONLY", "OPEN", "OPTIONAL", "OR", "ORDER",
  "ORGANIZATION", "OTHER", "OUTPUT", "OVERLINE", "\"PACKED-DECIMAL\"",
  "PADDING", "PAGE", "\"PAGE-COUNTER\"", "PARAGRAPH", "PERFORM", "PH",
  "PF", "PICTURE", "\"PICTURE SYMBOL\"", "PLUS", "POINTER", "POSITION",
  "POSITIVE", "PRESENT", "PREVIOUS", "PRINT", "PRINTER", "PRINTER_1",
  "PRINTING", "PROCEDURE", "PROCEDURES", "PROCEED", "PROGRAM",
  "\"PROGRAM-ID\"", "\"Program name\"", "\"PROGRAM-POINTER\"",
  "PROHIBITED", "PROMPT", "\"PROTECTED\"", "QUOTE", "RANDOM", "RD", "READ",
  "\"READY TRACE\"", "RECORD", "RECORDING", "RECORDS", "RECURSIVE",
  "REDEFINES", "REEL", "REFERENCE", "REFERENCES", "RELATIVE", "RELEASE",
  "REMAINDER", "REMOVAL", "RENAMES", "REPLACE", "REPLACING", "REPORT",
  "REPORTING", "REPORTS", "REPOSITORY", "REQUIRED", "RESERVE", "RESET",
  "\"RESET TRACE\"", "RETURN", "RETURNING", "\"FUNCTION REVERSE\"",
  "\"REVERSE-VIDEO\"", "REVERSED", "REWIND", "REWRITE", "RF", "RH",
  "RIGHT", "ROLLBACK", "ROUNDED", "RUN", "S", "SAME", "SCREEN",
  "\"SCREEN-CONTROL\"", "SCROLL", "SD", "SEARCH", "SECTION", "SECURE",
  "\"SEGMENT-LIMIT\"", "SELECT", "\"semi-colon\"", "SENTENCE", "SEPARATE",
  "SEQUENCE", "SEQUENTIAL", "SET", "\"78\"", "SHARING", "SIGN", "SIGNED",
  "\"SIGNED-INT\"", "\"SIGNED-LONG\"", "\"SIGNED-SHORT\"", "\"66\"",
  "SIZE", "\"SIZE ERROR\"", "SORT", "\"SORT-MERGE\"", "SOURCE",
  "\"SOURCE-COMPUTER\"", "SPACE", "\"SPECIAL-NAMES\"", "STANDARD",
  "\"STANDARD-1\"", "\"STANDARD-2\"", "START", "STATIC", "STATUS",
  "STDCALL", "STEP", "STOP", "STRING", "\"FUNCTION SUBSTITUTE\"",
  "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT", "SUM", "SUPPRESS",
  "SYMBOLIC", "SYNCHRONIZED", "\"SYSTEM-DEFAULT\"", "\"SYSTEM-OFFSET\"",
  "TAB", "TALLYING", "TAPE", "TERMINATE", "TEST", "THAN", "THEN", "THRU",
  "TIME", "\"TIME-OUT\"", "TIMES", "TO", "\"&\"", "\")\"", "\":\"",
  "\"/\"", "\".\"", "\"=\"", "\"FALSE\"", "\"FILE\"", "\">\"",
  "\"INITIAL\"", "\"<\"", "\"-\"", "\"*\"", "\"NULL\"", "\"OVERFLOW\"",
  "\"(\"", "\"+\"", "\"TRUE\"", "TOP", "\"TOWARD-GREATER\"",
  "\"TOWARD-LESSER\"", "TRAILING", "TRANSFORM", "\"FUNCTION TRIM\"",
  "TRUNCATION", "TYPE", "U", "UNDERLINE", "UNIT", "UNLOCK", "UNSIGNED",
  "\"UNSIGNED-INT\"", "\"UNSIGNED-LONG\"", "\"UNSIGNED-SHORT\"",
  "UNSTRING", "UNTIL", "UP", "UPDATE", "UPON", "\"UPON ARGUMENT-NUMBER\"",
  "\"UPON COMMAND-LINE\"", "\"UPON ENVIRONMENT-NAME\"",
  "\"UPON ENVIRONMENT-VALUE\"", "UPPER", "\"FUNCTION UPPER-CASE\"",
  "USAGE", "USE", "USER", "\"USER-DEFAULT\"", "\"User function name\"",
  "USING", "V", "VALUE", "VARIABLE", "VARYING", "WAIT", "WHEN",
  "\"FUNCTION WHEN-COMPILED\"", "WITH", "\"Identifier\"", "WORDS",
  "\"WORKING-STORAGE\"", "WRITE", "YYYYDDD", "YYYYMMDD", "ZERO",
  "SHIFT_PREFER", "OVERFLOW", "$accept", "start", "$@1", "nested_list",
  "source_element_list", "source_element", "simple_prog", "$@2",
  "program_definition", "function_definition", "_end_program_list",
  "end_program_list", "end_program", "end_function", "_program_body",
  "_identification_header", "program_id_paragraph", "$@3", "$@4",
  "function_id_paragraph", "$@5", "program_id_name", "end_program_name",
  "_as_literal", "_program_type", "program_type_clause",
  "init_or_recurse_and_common", "init_or_recurse", "_environment_division",
  "_environment_header", "_configuration_section", "_configuration_header",
  "_source_object_computer_paragraphs", "source_computer_paragraph", "$@6",
  "_source_computer_entry", "_with_debugging_mode",
  "object_computer_paragraph", "$@7", "_object_computer_entry",
  "object_clauses_list", "object_clauses", "object_computer_memory",
  "object_computer_sequence", "object_computer_segment",
  "object_computer_class", "locale_class", "computer_words",
  "_repository_paragraph", "$@8", "_repository_entry", "repository_list",
  "repository_name", "repository_name_list", "_special_names_paragraph",
  "_special_names_sentence_list", "special_names_sentence_list",
  "special_name_list", "special_name", "mnemonic_name_clause", "$@9",
  "mnemonic_choices", "_special_name_mnemonic_on_off", "on_off_clauses",
  "on_off_clauses_1", "alphabet_name_clause", "@10", "alphabet_definition",
  "alphabet_literal_list", "alphabet_literal", "@11",
  "alphabet_also_sequence", "alphabet_lits", "space_or_zero",
  "symbolic_characters_clause", "_sym_in_word", "_symbolic_collection",
  "symbolic_chars_list", "symbolic_chars_phrase", "char_list",
  "integer_list", "class_name_clause", "class_item_list", "class_item",
  "locale_clause", "currency_sign_clause", "_with_pic_symbol",
  "decimal_point_clause", "numeric_sign_clause", "cursor_clause",
  "crt_status_clause", "screen_control", "event_status",
  "_input_output_section", "_input_output_header", "_file_control_header",
  "_i_o_control_header", "_file_control_sequence", "file_control_entry",
  "$@12", "_select_clause_sequence", "select_clause", "assign_clause",
  "printer_name", "device_name", "_line_adv_file", "_ext_clause",
  "assignment_name", "_assignment_name", "access_mode_clause",
  "access_mode", "alternative_record_key_clause", "_suppress_clause",
  "collating_sequence_clause", "alphabet_name", "file_status_clause",
  "_file_or_sort", "lock_mode_clause", "$@13", "lock_mode", "_lock_with",
  "organization_clause", "organization", "padding_character_clause",
  "record_delimiter_clause", "record_key_clause", "key_or_split_keys",
  "relative_key_clause", "reserve_clause", "no_or_integer",
  "sharing_clause", "sharing_option", "_i_o_control", "i_o_control_list",
  "i_o_control_clause", "same_clause", "_same_option",
  "multiple_file_tape_clause", "$@14", "multiple_file_list",
  "multiple_file", "_multiple_file_position", "_data_division", "$@15",
  "_data_division_header", "_file_section_header",
  "_file_description_sequence", "file_description",
  "file_description_entry", "$@16", "file_type",
  "_file_description_clause_sequence", "file_description_clause",
  "block_contains_clause", "_records_or_characters", "record_clause",
  "_record_depending", "_from_integer", "_to_integer",
  "label_records_clause", "value_of_clause", "file_id", "valueof_name",
  "data_records_clause", "linage_clause", "_linage_sequence",
  "linage_lines", "linage_footing", "linage_top", "linage_bottom",
  "recording_mode_clause", "recording_mode", "u_or_s", "code_set_clause",
  "_for_sub_records_clause", "report_clause", "report_keyword",
  "rep_name_list", "_working_storage_section", "$@17",
  "_record_description_list", "$@18", "record_description_list_2",
  "data_description", "$@19", "level_number", "_entry_name",
  "user_entry_name", "const_global", "lit_or_length", "con_identifier",
  "fp32_usage", "fp64_usage", "fp128_usage", "pointer_len",
  "renames_entry", "_renames_thru", "condition_name_entry", "$@20",
  "constant_entry", "$@21", "constant_source",
  "_data_description_clause_sequence", "data_description_clause",
  "redefines_clause", "external_clause", "_as_extname", "_global_clause",
  "global_clause", "picture_clause", "usage_clause", "usage",
  "float_usage", "double_usage", "sign_clause", "report_occurs_clause",
  "_occurs_step", "occurs_clause", "_occurs_to_integer",
  "_occurs_from_integer", "_occurs_depending", "_capacity_in",
  "_occurs_initialized", "occurs_keys", "_occurs_key_list",
  "ascending_or_descending", "_occurs_indexed", "occurs_index_list",
  "occurs_index", "justified_clause", "synchronized_clause",
  "blank_clause", "based_clause", "value_clause", "$@22",
  "value_item_list", "value_item", "_false_is", "any_length_clause",
  "_local_storage_section", "$@23", "_linkage_section", "$@24",
  "_report_section", "$@25", "_report_description_sequence",
  "report_description", "$@26", "_report_description_options",
  "report_description_option", "control_clause", "control_field_list",
  "identifier_list", "page_limit_clause", "page_line_column",
  "_page_heading_list", "page_detail", "heading_clause", "first_detail",
  "last_heading", "last_detail", "footing_clause",
  "_report_group_description_list", "report_group_description_entry",
  "$@27", "_report_group_options", "report_group_option", "type_clause",
  "type_option", "_control_final", "_or_page", "next_group_clause",
  "sum_clause_list", "_reset_clause", "data_or_final",
  "present_when_condition", "varying_clause", "line_clause",
  "line_keyword_clause", "column_clause", "col_keyword_clause",
  "report_line_integer_list", "line_or_plus", "report_col_integer_list",
  "col_or_plus", "source_clause", "group_indicate_clause",
  "report_usage_clause", "_screen_section", "$@28",
  "_screen_description_list", "screen_description_list",
  "screen_description", "$@29", "_screen_options", "screen_option", "eol",
  "eos", "plus_plus", "minus_minus", "_screen_line_plus_minus",
  "_screen_col_plus_minus", "screen_occurs_clause", "global_screen_opt",
  "_procedure_division", "$@30", "$@31", "$@32",
  "_procedure_using_chaining", "$@33", "$@34", "procedure_param_list",
  "procedure_param", "_procedure_type", "_size_optional",
  "_procedure_optional", "_procedure_returning", "_procedure_declaratives",
  "$@35", "_procedure_list", "procedure", "section_header", "$@36",
  "_use_statement", "paragraph_header", "invalid_statement", "_segment",
  "statement_list", "@37", "@38", "statements", "$@39", "statement",
  "accept_statement", "$@40", "accept_body", "$@41", "accp_identifier",
  "_accept_clauses", "accept_clauses", "accept_clause", "lines_or_number",
  "at_line_column", "line_number", "column_number", "mode_is_block",
  "accp_attr", "update_default", "end_accept", "add_statement", "$@42",
  "add_body", "_add_to", "end_add", "allocate_statement", "$@43",
  "allocate_body", "allocate_returning", "alter_statement", "$@44",
  "alter_body", "alter_entry", "_proceed_to", "call_statement", "$@45",
  "call_body", "$@46", "mnemonic_conv",
  "id_or_lit_or_func_or_program_name", "call_using", "$@47",
  "call_param_list", "call_param", "call_type", "call_returning",
  "return_give", "null_or_omitted", "call_exception_phrases",
  "_call_on_exception", "call_on_exception", "_call_not_on_exception",
  "call_not_on_exception", "end_call", "cancel_statement", "$@48",
  "cancel_body", "id_or_lit_or_program_name", "close_statement", "$@49",
  "close_body", "close_option", "compute_statement", "$@50",
  "compute_body", "end_compute", "commit_statement", "continue_statement",
  "delete_statement", "$@51", "delete_body", "delete_file_list",
  "end_delete", "display_statement", "$@52", "display_body",
  "screen_or_device_display", "display_list", "display_atom", "$@53",
  "disp_list", "display_clauses", "display_clause", "display_upon",
  "crt_under", "disp_attr", "end_display", "divide_statement", "$@54",
  "divide_body", "end_divide", "entry_statement", "$@55", "entry_body",
  "evaluate_statement", "$@56", "evaluate_body", "evaluate_subject_list",
  "evaluate_subject", "evaluate_condition_list", "evaluate_case_list",
  "evaluate_case", "evaluate_other", "evaluate_when_list",
  "evaluate_object_list", "evaluate_object", "_evaluate_thru_expr",
  "end_evaluate", "exit_statement", "$@57", "exit_body",
  "exit_program_returning", "free_statement", "$@58", "free_body",
  "generate_statement", "$@59", "generate_body", "goto_statement", "$@60",
  "go_body", "goto_depending", "goback_statement", "if_statement", "$@61",
  "if_else_statements", "end_if", "initialize_statement", "$@62",
  "initialize_body", "initialize_filler", "initialize_value",
  "initialize_replacing", "initialize_replacing_list",
  "initialize_replacing_item", "initialize_category", "initialize_default",
  "initiate_statement", "$@63", "initiate_body", "inspect_statement",
  "$@64", "inspect_body", "send_identifier", "inspect_list",
  "inspect_tallying", "$@65", "inspect_replacing", "inspect_converting",
  "tallying_list", "tallying_item", "replacing_list", "replacing_item",
  "rep_keyword", "replacing_region", "inspect_region", "inspect_before",
  "inspect_after", "merge_statement", "$@66", "move_statement", "$@67",
  "move_body", "multiply_statement", "$@68", "multiply_body",
  "end_multiply", "open_statement", "$@69", "open_body", "open_mode",
  "open_sharing", "open_option", "perform_statement", "$@70",
  "perform_body", "$@71", "end_perform", "term_or_dot",
  "perform_procedure", "perform_option", "perform_test", "cond_or_exit",
  "perform_varying_list", "perform_varying", "read_statement", "$@72",
  "read_body", "read_into", "with_lock", "read_key", "read_handler",
  "end_read", "ready_statement", "release_statement", "$@73",
  "release_body", "reset_statement", "return_statement", "$@74",
  "return_body", "end_return", "rewrite_statement", "$@75", "rewrite_body",
  "write_lock", "end_rewrite", "rollback_statement", "search_statement",
  "$@76", "search_body", "search_varying", "search_at_end", "search_whens",
  "search_when", "end_search", "set_statement", "$@77", "set_body",
  "on_or_off", "up_or_down", "set_environment", "set_attr",
  "set_attr_clause", "set_attr_one", "set_to", "set_up_down",
  "set_to_on_off_sequence", "set_to_on_off", "set_to_true_false_sequence",
  "set_to_true_false", "set_last_exception_to_off", "sort_statement",
  "$@78", "sort_body", "@79", "sort_key_list", "_key_list",
  "_sort_duplicates", "sort_collating", "sort_input", "sort_output",
  "start_statement", "$@80", "start_body", "sizelen_clause", "start_key",
  "start_op", "disallowed_op", "not_equal_op", "end_start",
  "stop_statement", "$@81", "stop_returning", "_status_x", "stop_literal",
  "string_statement", "$@82", "string_body", "string_item_list",
  "string_item", "_string_delimited", "string_delimiter", "_with_pointer",
  "end_string", "subtract_statement", "$@83", "subtract_body",
  "end_subtract", "suppress_statement", "_printing", "terminate_statement",
  "$@84", "terminate_body", "transform_statement", "$@85",
  "transform_body", "unlock_statement", "$@86", "unlock_body",
  "unstring_statement", "$@87", "unstring_body", "_unstring_delimited",
  "unstring_delimited_list", "unstring_delimited_item", "unstring_into",
  "unstring_into_item", "_unstring_into_delimiter", "_unstring_into_count",
  "_unstring_tallying", "end_unstring", "use_statement", "$@88",
  "use_phrase", "use_file_exception", "use_global",
  "use_file_exception_target", "use_debugging", "debugging_list",
  "debugging_target", "_all_refs", "use_start_end", "program_start_end",
  "use_reporting", "use_exception", "use_ex_keyw", "write_statement",
  "$@89", "write_body", "from_option", "write_option", "before_or_after",
  "write_handler", "end_write", "_accept_exception_phrases",
  "_accp_on_exception", "accp_on_exception", "escape_or_exception",
  "_accp_not_on_exception", "accp_not_on_exception",
  "not_escape_or_not_exception", "_display_exception_phrases",
  "_disp_on_exception", "disp_on_exception", "_disp_not_on_exception",
  "disp_not_on_exception", "on_size_error_phrases", "_on_size_error",
  "on_size_error", "_not_on_size_error", "not_on_size_error",
  "_on_overflow_phrases", "_on_overflow", "on_overflow",
  "_not_on_overflow", "not_on_overflow", "return_at_end", "at_end",
  "_at_end_clause", "at_end_clause", "_not_at_end_clause",
  "not_at_end_clause", "at_eop_clauses", "_at_eop_clause", "at_eop_clause",
  "_not_at_eop_clause", "not_at_eop_clause", "_invalid_key_phrases",
  "invalid_key_phrases", "_invalid_key_sentence", "invalid_key_sentence",
  "_not_invalid_key_sentence", "not_invalid_key_sentence", "_scroll_lines",
  "condition", "expr", "partial_expr", "$@90", "expr_tokens", "expr_token",
  "eq", "gt", "lt", "ge", "le", "exp_list", "_e_sep", "exp", "exp_term",
  "exp_factor", "exp_unary", "exp_atom", "line_linage_page_counter",
  "arithmetic_x_list", "arithmetic_x", "record_name", "table_name",
  "file_name_list", "file_name", "report_name", "mnemonic_name_list",
  "mnemonic_name", "procedure_name_list", "procedure_name", "label",
  "integer_label", "reference_list", "reference", "single_reference",
  "optional_reference_list", "optional_reference", "reference_or_literal",
  "undefined_word", "unique_word", "target_x_list", "target_x", "_x_list",
  "x_list", "x", "call_x", "x_common", "report_x_list", "expr_x",
  "arith_x", "prog_or_entry", "alnum_or_id", "simple_display_value",
  "simple_display_all_value", "simple_value", "simple_all_value",
  "id_or_lit", "id_or_lit_or_func", "num_id_or_lit", "positive_id_or_lit",
  "pos_num_id_or_lit", "from_parameter", "sub_identifier",
  "sort_identifier", "sub_identifier_1", "display_identifier",
  "numeric_identifier", "identifier_or_file_name", "identifier",
  "identifier_1", "target_identifier", "target_identifier_1",
  "qualified_word", "subref", "refmod", "integer", "symbolic_integer",
  "report_integer", "class_value", "literal", "basic_literal",
  "basic_value", "function", "func_no_parm", "func_one_parm",
  "func_multi_parm", "func_refmod", "func_args", "trim_args",
  "numvalc_args", "locale_dt_args", "formatted_datetime_args",
  "formatted_time_args", "not_const_word", "flag_all", "flag_duplicates",
  "flag_initialized", "flag_initialized_to", "to_init_val", "_flag_next",
  "_flag_not", "flag_optional", "flag_rounded", "round_mode",
  "round_choice", "flag_separate", "error_stmt_recover", "_advancing",
  "_after", "_are", "_area", "_as", "_at", "_binary", "_by", "_character",
  "_characters", "_contains", "_data", "_end_of", "_file", "_final",
  "_for", "_from", "_in", "_in_order", "_indicate", "_initial", "_into",
  "_is", "_is_are", "_key", "_left_or_right", "_line_or_lines", "_limits",
  "_lines", "_mode", "_number", "_numbers", "_of", "_on", "_onoff_status",
  "_other", "_procedure", "_program", "_record", "_records", "_right",
  "_sign", "_signed", "_sign_is", "_size", "_standard", "_status", "_tape",
  "_then", "_times", "_to", "_to_using", "_when", "_when_set_to", "_with",
  "coll_sequence", "column_or_col", "columns_or_cols", "comp_equal",
  "exception_or_error", "in_of", "label_option", "line_or_lines",
  "lock_records", "object_char_or_word", "records", "reel_or_unit",
  "scroll_line_or_lines", "size_or_length", "with_dups",
  "prog_coll_sequence", "detail_keyword", "ch_keyword", "cf_keyword",
  "ph_keyword", "pf_keyword", "rh_keyword", "rf_keyword",
  "control_keyword", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,   782,   783
};
# endif

#define YYPACT_NINF -2474

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-2474)))

#define YYTABLE_NINF -1958

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -2474,   287,    10, -2474,   577,   772, -2474,   154, -2474, -2474,
     523, -2474, -2474,   -13,   480,   501, -2474,   969, -2474,  1125,
    1127, -2474, -2474,   523,   523, -2474, -2474,   707,  1133,  1194,
     803,   901,  1096,   521,   906,  1078,  1314,  1421, -2474,  1089,
    1444, -2474, -2474,  1177, -2474,  1137,  1206, -2474,  1438,  1160,
    1163,  1208,  1321,  1221,   729,   729,   770, -2474,  1314, -2474,
     770, -2474, -2474,    15,  3025,  3649,  1190,     7, -2474,  1195,
    1201, -2474, -2474, -2474,  1204,  1397, -2474, -2474, -2474, -2474,
    1618,  1618, -2474, -2474,  1215, -2474,  1219, -2474, -2474,  1288,
    4043, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
     -19, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1273, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,   650, -2474, -2474,  1342, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1170, -2474,   276,    76,
   -2474, -2474,   -62,   116,  1189, -2474,    81,    81,  1262,  1296,
    1487,  1487,  1487,    81,  1310,  1487,  1671, -2474,  1351,  1397,
    1213, -2474, -2474, -2474, -2474,  1508, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,  1473,  1270, -2474, -2474, -2474,
      90,    90,   454,  1272, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,   504,  4115,  7976,   -40,   652,
    -124,  1236,   700,  -208,  3263,  5966,  1488,   897,  1066,   700,
    1240,  1302, -2474, -2474,  5966, -2474, -2474,   700,  1244,  4426,
    1240,  5247,  5966, -2474,  1112,  3820,  1236,  1240,  1236,  1240,
      91,   378,  1240,  1236, -2474, -2474, -2474, -2474, -2474, -2474,
    5966,  5376, -2474, -2474,  1244,   222,  1240,  1236,  1240,  1240,
    1364,  1505, -2474,   253,  1308, -2474, -2474,  1311,  1021,   474,
   -2474, -2474,  1366,  1356,  1715,  1487, -2474, -2474, -2474,  1229,
   -2474, -2474, -2474, -2474, -2474,   942,  1724,  1487, -2474,   176,
   -2474, -2474, -2474,  1487,  1487, -2474,  1487, -2474,  1240,  1718,
    1240,  1487,  1487,  1240, -2474,  1265,  1029,  1324, -2474,  1263,
   -2474, -2474,  1285, -2474, -2474, -2474,   -91, -2474,   355, -2474,
    -170,   472,   366, -2474, -2474, -2474, -2474,   829,  1657, -2474,
    1610, -2474,  1331,  1498,  1425, -2474,  1240, -2474, -2474,  1336,
    1344,  1349, -2474,  4479,   829,   829, -2474,  1350,  1353,  1354,
   -2474, -2474, -2474,  1357,   829, -2474, -2474, -2474, -2474, -2474,
   -2474,  1360, -2474,  1349, -2474, -2474,  1683, -2474,  5459, -2474,
   -2474, -2474, -2474,  1371, -2474, -2474,  1361,  1363,  1372,  4479,
    8084,  7976,  8084, -2474,   180,  1023, -2474,  1646, -2474, -2474,
   -2474,   635,  1371, -2474, -2474,   -40, -2474,  1381, -2474,   829,
   -2474, -2474, -2474, -2474,  1712,  2573, -2474, -2474,  -124, -2474,
   -2474, -2474,  1236,   614,  1498,  1721,   636, -2474,  1463, -2474,
   -2474,  1331,  1371,  1236,  1725,  1500,  1084, -2474,  1728,    22,
    5588, -2474, -2474,  4810,  1199,  1252,  1729,   654,  1352, -2474,
   -2474, -2474,  1736,    61, -2474, -2474, -2474,  4573, -2474, -2474,
    1766,   -19, -2474, -2474, -2474,   700, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474,  1422, -2474, -2474,   621, -2474,  1244, -2474,
   -2474,   219, -2474, -2474, -2474, -2474, -2474, -2474,  1404,  5966,
   -2474,  1427,  1738,  1833, -2474, -2474, -2474, -2474,  1112,  1470,
   -2474,  1429, -2474, -2474,  5081,   -30,   913,  1435,  1433, -2474,
     383, -2474,  1441,  1749,   -39, -2474,  1698, -2474,  1758,  1500,
    1761,  1698,  1240,  1757,  1396, -2474,  4479,  1742, -2474, -2474,
   -2474, -2474, -2474, -2474,  1636, -2474,   700, -2474, -2474,   554,
   -2474,   672,  1882, -2474,    48, -2474,  1764,  1178,  5105,  1765,
    5671, -2474,  1801,  1240,  1768,  5813,  1244, -2474, -2474,  -169,
   -2474, -2474, -2474, -2474,  3204, -2474,  1722, -2474, -2474,   -82,
    1772,  1804,  1767,  1698,  1461,  1523,  1670,  1412,  1412,  1412,
     342,  1468,  6718, -2474, -2474, -2474,  1416, -2474, -2474, -2474,
    1619, -2474,    81, -2474,   825, -2474,   181, -2474, -2474, -2474,
   -2474,  1487,  1526,  1682, -2474, -2474, -2474, -2474,  1230,  1487,
    1426,  1480,  1838,  1487,  1496,  1240,  1687, -2474, -2474, -2474,
   -2474,  1688,  1471, -2474, -2474,  1265, -2474,    54, -2474, -2474,
   -2474, -2474, -2474, -2474,  1169,   -11,  1487,    67, -2474, -2474,
   -2474,  1489,    68, -2474,  1487,  1530,  1637, -2474, -2474,  1844,
   -2474, -2474,  1240, -2474, -2474,  7047,  1614,  7976,  1481, -2474,
   -2474,   121, -2474,  1499,  7976,  7976,  7320, -2474, -2474,  1371,
   -2474,  1442,  1443,  7976,  7976,  7976,  4479,  1445,  4479, -2474,
   -2474, -2474,  6086,  1759, -2474,  1425,  7976, -2474,  4479,  7976,
   -2474,  1371, -2474, -2474, -2474,  1073, -2474,  1739,  7976,  7976,
    7976,  7976,  7976, -2474,  1583, -2474,  1621,  1710, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,   614, -2474, -2474, -2474,     2,
      -8,  1240, -2474, -2474, -2474, -2474, -2474,  7976,  1696, -2474,
    1481, -2474,  1236, -2474, -2474, -2474, -2474,   757, -2474, -2474,
   -2474, -2474, -2474,  1673,  1807, -2474, -2474,  4810,   131,    22,
      22,    22,    22, -2474, -2474,  5966,  6086, -2474, -2474, -2474,
   -2474,   897,    80, -2474,  1459, -2474,  1460, -2474, -2474, -2474,
   -2474, -2474,  1302, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474,  4242, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,    -7, -2474,  1847,  1702,  1800,
   -2474,  4479,   262, -2474, -2474,  1611, -2474, -2474,   113,  7976,
   -2474,  1522,   700, -2474, -2474,  6086,  1470,  1472,  1236, -2474,
   -2474, -2474, -2474, -2474,  1816,  1240,   -40, -2474,  1377, -2474,
   -2474, -2474, -2474,  1500,  4426, -2474, -2474, -2474,  1760, -2474,
   -2474,   607,  1858, -2474, -2474,  1240,  1858,  1532, -2474, -2474,
    1371, -2474,  1534, -2474, -2474,   676,  1169, -2474, -2474,  2114,
   -2474,  1947,    98,   112, -2474, -2474, -2474,  1487, -2474,  -118,
    5966, -2474, -2474,    34, -2474, -2474,  1240, -2474,  1951, -2474,
    1802, -2474, -2474,  6086, -2474,  1682, -2474, -2474,  4479, -2474,
   -2474, -2474, -2474, -2474,  1951,  1770, -2474, -2474,  1377, -2474,
    1536,  1601,  1631, -2474, -2474, -2474,  1639,  1544, -2474,  1550,
   -2474, -2474,  1929, -2474,  1144, -2474, -2474,  1551, -2474, -2474,
   -2474,  1997,  1558, -2474, -2474,  1682, -2474, -2474, -2474,   670,
   -2474, -2474, -2474,  1747,  1403, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,  1496, -2474,  1571, -2474,   519, -2474,  1622, -2474,
   -2474, -2474, -2474,  1773,   -11, -2474,  1798,    81,    81, -2474,
    1169,   647, -2474,   524, -2474, -2474, -2474,  1689, -2474,  1969,
     134,  1487, -2474,  1524,  1581, -2474, -2474,   471, -2474,  1487,
    1091,  7047, -2474, -2474, -2474,   743,  1896, -2474,  1091, -2474,
   -2474, -2474,  1527,  1525, -2474,  4479,  1091,  1805,  1613,  1745,
   -2474, -2474,  1775, -2474, -2474, -2474, -2474,   252,  1086,  7976,
   -2474, -2474, -2474,   390, -2474,  1240,   290,   452,  1591,   303,
    1592, -2474,   341, -2474, -2474,   247,  1595,  1596,  1597,   374,
   -2474,  1371, -2474,  1602, -2474,   394,  1603,  1498,    86, -2474,
      75,   -75,   700, -2474,  1154,  1604,   396, -2474,  1600,  1583,
    1023,  1023, -2474, -2474, -2474,   700, -2474,  1612,   -40, -2474,
    1352, -2474, -2474,  1674, -2474,  1701, -2474,   805,  1487, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1776,  1834, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,   104, -2474, -2474,  1756, -2474,
   -2474,  1851, -2474, -2474, -2474, -2474,  1867,    86,  1868,    70,
   -2474, -2474, -2474, -2474,  2060, -2474,  1623,   192, -2474, -2474,
      80, -2474, -2474, -2474, -2474,  1769, -2474, -2474, -2474,  1949,
    1940,  1302, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1709,
    1302, -2474,  1626, -2474,  2038, -2474, -2474, -2474,  6344, -2474,
    4479,  3467, -2474, -2474, -2474,  1962,    40,  1035,    -6,   700,
     700,    86,  1884,  1236,   449,  -120, -2474,  1945, -2474, -2474,
   -2474,  2084, -2474,  1893, -2474, -2474, -2474, -2474,  1760, -2474,
   -2474, -2474, -2474,  1240,   531,   757,  1134, -2474,  1578, -2474,
    1579,  4479,  1786,   985, -2474,   390, -2474, -2474, -2474,  5966,
    1169,  1169,  1169,  1169,  1169,  1169,  1169,  1169,    98, -2474,
     630,   757,   576, -2474,  1666,  1666,  -125,  5942,  1240,    86,
    1895,  1638, -2474,  1644,  2097,  1240,   444,   607,  2100,   276,
   -2474,  1643,  1705,  1711,  1606,  1203,  1240, -2474, -2474, -2474,
    1203,  2023,  1487,  1214,  1214,  1487,     6,  1836,  1487,  2092,
   -2474,  1803, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,    81,    66, -2474, -2474,  1664, -2474,  1924, -2474,
      29, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   673,
   -2474,    73, -2474,  1496, -2474,  1789, -2474, -2474,  1773, -2474,
      81, -2474, -2474, -2474, -2474, -2474,    72,  1608, -2474,   179,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   688, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  2082, -2474, -2474, -2474,
   -2474, -2474,  1304, -2474,  1402, -2474, -2474, -2474, -2474,  1824,
    1824, -2474, -2474,  1824, -2474,  1487, -2474, -2474, -2474, -2474,
    1487, -2474, -2474, -2474, -2474, -2474,    51, -2474, -2474,  2074,
    1713, -2474, -2474,   -23, -2474,  1487, -2474,  2124, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1091, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,  7976,  7578,  1086, -2474, -2474,
   -2474,  1463,  7686,  1361,  7780,  1361, -2474,  1240,  1361,  1361,
    1361,  4479, -2474,   -89,  1361,   121, -2474, -2474, -2474,  1829,
    1714,   171,  1931,    86,  7882,  1361,  1361,   685, -2474, -2474,
   -2474, -2474, -2474,   -19, -2474, -2474, -2474,   634, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,  1487, -2474,   719, -2474, -2474,  1268,  1487, -2474,
   -2474, -2474, -2474, -2474,    -2,  1487, -2474, -2474,   700, -2474,
     700,  4899, -2474,   598,   100,    80, -2474, -2474, -2474,  2060,
    1240, -2474, -2474, -2474, -2474,  1627,  1367,   302,  1628,   685,
    4479, -2474, -2474,  2087, -2474,  1413, -2474, -2474,  3467, -2474,
    1413,  1946,  1952, -2474,  1731, -2474, -2474,  1487, -2474, -2474,
    1905,  1828, -2474, -2474,   700, -2474,   700,  -120,  1827,  1827,
    1835, -2474, -2474, -2474, -2474,  1179, -2474, -2474,  1240,  5966,
    1247, -2474, -2474, -2474, -2474,  1854,  2022, -2474, -2474,  1888,
   -2474, -2474, -2474, -2474,  1579, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   -35, -2474,
    1240, -2474, -2474, -2474,  1267, -2474, -2474, -2474,  7976, -2474,
    5966,  5966,   -96,  1821, -2474, -2474, -2474,  1463, -2474,   700,
   -2474,   685, -2474,  1840, -2474,  4479, -2474,  2046,  1717, -2474,
     991, -2474,   675, -2474,   276, -2474,  1697,  1771, -2474,   649,
   -2474,  1606, -2474,  1955,  1716,  6938,   149,  1961, -2474,  1682,
    1650,  1487,  2092,  1651,   758,   469,  1682,  1659, -2474,  1487,
   -2474, -2474, -2474,    87,  1245, -2474, -2474, -2474,  2508, -2474,
    2023,  1236, -2474, -2474, -2474, -2474, -2474,   673, -2474,  1916,
   -2474, -2474,  1948, -2474,  1618,  1217,  1618,  1719, -2474, -2474,
   -2474, -2474, -2474,  1012, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,   471,   471,   471,   471,   471, -2474,  1487,  1487,
     515,   515,   471, -2474,   540, -2474,   452, -2474,  1097,   622,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,  1974, -2474, -2474, -2474, -2474, -2474, -2474,  1976,
   -2474, -2474,  1142, -2474, -2474, -2474, -2474, -2474,   513,   101,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  2577,
     471, -2474, -2474,  1498, -2474, -2474, -2474, -2474,   761,   471,
     515,   515,   471,    86,  1809,    86,  1814, -2474, -2474,  5966,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1367,
   -2474,  2086, -2474,  1302, -2474,  1413, -2474,  1413,   685,  1723,
    1723, -2474,  2183,  2155, -2474, -2474, -2474, -2474,  -100,  1240,
   -2474, -2474, -2474,    86, -2474, -2474, -2474, -2474, -2474, -2474,
    1806, -2474,  2145,  1930,  1957,   574, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474,   452, -2474, -2474, -2474, -2474, -2474, -2474,  1897,  1726,
    1487,   622,    86,  1695, -2474,  2097, -2474,  1980,  2107,  1980,
     -96, -2474, -2474, -2474, -2474,  1907,  2047, -2474, -2474, -2474,
    1359, -2474,   276, -2474,  1741,    82, -2474, -2474,  1240, -2474,
     976, -2474, -2474,   749,   753,   756,   847,   883,  1692, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1817, -2474,   691, -2474,
   -2474, -2474, -2474,  1240,  1977, -2474, -2474, -2474,   618, -2474,
   -2474, -2474,  1487, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
      85,   -84, -2474,  1691, -2474,   228, -2474,  1762, -2474,  2024,
   -2474, -2474, -2474,  1651, -2474, -2474, -2474, -2474, -2474, -2474,
    1959,    83,  1980,   820,  1487, -2474, -2474,  1487, -2474,  1836,
    1500,   678, -2474,  1808,  1487,  2162,   214,   -87,  1059,  1472,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1788,
   -2474,  1958, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
    2185,  1487,  1236,  1236,   673, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474,   -17, -2474, -2474, -2474, -2474, -2474,   549,
     471, -2474,  1365, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1926,   150,
    1498, -2474, -2474, -2474, -2474, -2474,  1240, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,   700, -2474,   700, -2474,
   -2474, -2474, -2474, -2474, -2474,  2180,  2119, -2474, -2474,  1413,
   -2474,  5966,  5966, -2474, -2474,  1887,  1236,     9, -2474,  1240,
   -2474, -2474,  5966, -2474,  1487,  1017,  1967,  1968, -2474,  1970,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1240,
   -2474, -2474, -2474, -2474,  1774, -2474, -2474,  1240,  1980, -2474,
    1240, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474,    82, -2474,  1784, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,  1727, -2474, -2474,  2188,  1782, -2474, -2474, -2474,
   -2474, -2474,  7480,  2217,  1837,  1837, -2474,  1498,  1556,  1556,
   -2474, -2474,  1682,   110,  1240, -2474, -2474, -2474, -2474,  1682,
   -2474,  1823, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
     555,   555,  1487,  1905, -2474, -2474,  1109, -2474,  1008,  1487,
    1487,  1487,  1487, -2474,  1997, -2474,   449,  1487,  1836, -2474,
    1842,  1650,  1236, -2474,  1910,  2232, -2474,  2146, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
    1240, -2474,   622,   622,  5966, -2474, -2474, -2474, -2474,  1487,
    1236,  1236,  1912, -2474, -2474,  1763,  1240, -2474, -2474,  1854,
    2022, -2474, -2474, -2474, -2474, -2474,  1068, -2474, -2474,  1240,
   -2474,  1898,   628,  -219, -2474,    82, -2474,  1980,  2061,  1682,
    1811, -2474,  1999, -2474,  2162, -2474, -2474,  1556,  1799, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1240, -2474,    58,  1671,
   -2474,   945, -2474, -2474, -2474, -2474,     8,  1487, -2474, -2474,
    1061, -2474, -2474,   469,  1841,  1240,  1240, -2474, -2474,  1240,
    1487, -2474, -2474, -2474,  1682, -2474,   673,  1812, -2474, -2474,
   -2474, -2474,   -40,  1236,  1487, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1424, -2474, -2474, -2474,
   -2474, -2474,  1917,  2156, -2474,  1244, -2474,  6455, -2474, -2474,
     628,  1815,  1861, -2474,  1819, -2474,  1751,  1682,  1782, -2474,
   -2474,  2151, -2474, -2474, -2474, -2474, -2474,   469,   469, -2474,
   -2474, -2474, -2474,  2090, -2474, -2474,  1762,  1682, -2474, -2474,
   -2474, -2474,  1240, -2474, -2474,   556,   556,  2272, -2474, -2474,
   -2474, -2474, -2474,   556,   556,   563, -2474, -2474, -2474,   812,
   -2474, -2474,   802, -2474, -2474, -2474, -2474,   -40, -2474,  1911,
    1857,    -3,  1769, -2474,  1826, -2474,  1830, -2474,  1832,  1487,
   -2474, -2474,  2056,  1769, -2474, -2474, -2474,  2259,  1671, -2474,
     -14, -2474, -2474, -2474, -2474,  1551, -2474, -2474, -2474, -2474,
   -2474,  1487,  1240,  1777, -2474,  1777, -2474, -2474,  1240, -2474,
    1312, -2474, -2474, -2474,    89,   742, -2474, -2474, -2474, -2474,
      82, -2474, -2474,  1240,  2068,  1035,   469,  2178,  1855, -2474,
   -2474,  1240,  1240,   -43, -2474, -2474, -2474, -2474, -2474,  1954,
     949,    89, -2474, -2474,  1843,   890,  7432, -2474,  2068, -2474,
    1951, -2474,  1905, -2474,  1769, -2474,  1781, -2474,  1240,  1986,
   -2474, -2474,  1769, -2474, -2474,  1990,  1240, -2474, -2474,  1487,
    1487,  2092,  1284, -2474, -2474, -2474, -2474,  2098,  2129, -2474,
    1487, -2474,   461, -2474,  1268,  1487,  4426, -2474, -2474, -2474,
   -2474,  1824, -2474,  1682, -2474,  2257, -2474, -2474, -2474,  1240,
   -2474, -2474,  1240, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,  2108,  1824, -2474,  1795,  1487,  1240,    84,   882,
     792, -2474, -2474,   549, -2474, -2474,  1487,  2092,  2062,  1813,
   -2474, -2474, -2474,  1240,   471, -2474, -2474, -2474, -2474,   471,
   -2474,  1487,  1811,  1487, -2474, -2474, -2474,  1487, -2474,  1795,
   -2474,  1240, -2474,  1425, -2474, -2474, -2474,  1282, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1236, -2474, -2474, -2474,
   -2474,  1395,   -68, -2474,  1240, -2474, -2474, -2474,   648, -2474,
     549,   648, -2474,  1240, -2474, -2474,  1210, -2474, -2474,  2062,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   471,
   -2474, -2474, -2474,   471,   165,  1487,  1487,  1067, -2474, -2474,
   -2474, -2474, -2474, -2474,  1598, -2474, -2474, -2474, -2474, -2474,
    1487,  2062,  2062, -2474,  2106,  1487,  1487, -2474,  1973,  2062,
   -2474, -2474, -2474,  2062,  2062,  2096,  1384,  2092,  2110,  1682,
    1810,  1487,  1498, -2474,  1487,  1487,  1240, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
     722, -2474,   -36, -2474, -2474, -2474,  1384,  2092, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474,   649, -2474,  1487,  1782, -2474,
    8313,  8313,  1607,  2204,  2126, -2474,  1682,   722, -2474, -2474,
    1682,   -36, -2474, -2474,   649, -2474, -2474,   722,  1811, -2474,
    1463,  8193, -2474, -2474,  1289,  1338, -2474, -2474,  1341, -2474,
   -2474, -2474, -2474,    -9,    -9, -2474, -2474, -2474, -2474, -2474,
    8313, -2474, -2474, -2474, -2474, -2474, -2474,  2151, -2474,  1769,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474,  2008, -2474,  2008,
   -2474,  2280,  1899,   108,  2004, -2474, -2474,  8313,  1682, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,    10,     1,     0,     0,     3,    21,     6,     4,
      46,     8,     9,     0,     0,     0,     7,     0,    11,   291,
      49,    27,    24,    46,    46,    23,    22,     0,     0,   694,
     293,     0,   180,    51,     0,     0,    14,     0,    47,     0,
       0,    20,   739,     0,   295,     0,     0,    45,   182,     0,
       0,    98,    52,    53,     0,     0,     0,    12,    15,    16,
       0,    13,   292,   696,     0,     0,     0,   289,    50,     0,
       0,   186,    62,    56,     0,   100,    54,    55,    30,    29,
      33,    33,    32,    31,     0,    17,     0,   699,   697,   715,
       0,   793,   866,   875,   881,   888,   930,   936,   950,   945,
     951,   952,   960,  1007,  1016,  1019,  1045,  1056,  1059,  1062,
    1054,  1068,  1075,  1097,  1101,  1140,  1142,  1146,     0,  1152,
    1166,  1190,  1208,  1209,  1212,  1213,  1218,  1226,  1227,  1240,
    1276,  1294,     0,  1328,  1342,  1350,  1352,   721,  1356,  1359,
    1362,  1413,   741,   742,   743,   744,   745,   746,   747,   748,
     750,   749,   751,   752,   753,   754,   755,   756,   757,   758,
     759,   760,   761,   762,   763,   764,   765,   766,   767,   768,
     769,   770,   771,   772,   773,   774,   775,   776,   777,   778,
     779,   780,   781,   782,   783,   784,   785,   786,   787,   788,
     789,   790,   740,   294,   301,   302,   362,   296,   365,     0,
     181,   183,   184,    64,    58,    99,     0,     0,     0,  1929,
    1883,  1883,  1883,     0,     0,  1883,  1856,   118,    84,   101,
       0,   104,   106,   107,   108,   154,   110,   109,   111,   112,
     113,   114,   115,   116,   117,     0,     0,    25,    18,    19,
     704,   704,     0,     0,  1769,  1770,  1771,  1772,  1773,  1774,
    1775,  1776,  1777,  1778,  1779,  1780,  1781,  1782,  1818,  1819,
    1820,  1821,  1822,  1823,  1824,  1825,  1826,  1827,  1828,  1829,
    1830,  1831,  1832,  1833,  1834,  1835,  1836,  1837,  1783,  1784,
    1785,  1786,  1787,  1788,  1789,  1790,  1791,  1792,  1793,  1794,
    1795,  1796,  1797,  1798,  1799,  1800,  1801,  1802,  1803,  1804,
    1805,  1806,  1807,  1808,  1809,  1810,  1811,  1812,  1813,  1768,
    1814,  1815,  1816,  1817,   792,     0,     0,     0,     0,   892,
       0,     0,     0,     0,     0,     0,     0,  1501,  1047,     0,
       0,  1948,   915,   914,     0,  1067,  1501,     0,     0,     0,
       0,     0,     0,   791,     0,  1178,     0,     0,     0,     0,
       0,     0,     0,     0,  1324,  1327,  1314,  1325,  1326,  1316,
       0,     0,  1351,  1349,     0,   739,     0,     0,     0,     0,
       0,   525,   297,  1735,     0,  1570,   298,     0,  1751,   270,
     187,  1855,     0,     0,     0,  1883,  1991,    82,    63,  1854,
      68,    70,    71,    72,    73,  1854,     0,  1883,    57,    60,
    1592,  1591,   129,  1883,  1883,  1930,  1883,  1884,     0,     0,
       0,  1883,  1883,     0,  1857,     0,  1883,     0,    48,     0,
     102,   105,     0,   153,    34,    28,  1883,  1853,   704,   701,
     707,     0,   704,   716,   717,   691,   816,  1671,   864,   795,
     815,  1661,  1665,  1908,     0,  1714,     0,  1709,  1715,     0,
       0,  1721,  1694,     0,  1557,  1559,  1690,     0,     0,     0,
    1712,  1695,  1615,     0,  1561,  1693,  1713,  1691,  1716,  1717,
    1696,     0,  1711,  1721,  1710,  1692,   873,  1609,   871,  1601,
    1604,  1603,  1607,  1686,  1688,  1608,  1718,     0,     0,     0,
       0,     0,     0,   876,     0,  1546,  1549,  1551,  1554,  1624,
    1556,  1740,  1622,  1623,  1581,   882,   883,     0,  1577,  1579,
    1578,   895,   893,   894,   928,     0,  1640,   935,   931,   932,
     934,  1639,   937,   940,  1908,   948,     0,  1563,  1754,  1596,
    1666,  1670,  1597,     0,   958,  1922,  1690,   974,  1005,  1442,
    1599,   969,   971,   968,     0,  1603,  1014,     0,   898,  1017,
    1026,  1025,  1043,     0,  1022,  1024,  1500,     0,  1049,  1053,
    1051,  1054,  1052,  1046,  1057,  1058,  1594,  1060,  1061,  1949,
    1063,  1575,  1055,  1944,  1499,  1076,  1078,  1571,  1098,  1099,
    1102,     0,  1104,  1105,  1106,  1141,  1280,  1655,  1656,     0,
    1143,     0,  1150,     0,  1159,  1156,  1158,  1157,  1153,  1160,
    1180,  1581,  1958,  1167,  1178,  1169,     0,  1176,     0,  1641,
    1578,  1643,     0,  1206,  1746,  1210,  1416,  1566,  1216,  1922,
    1224,  1416,     0,  1238,  1231,  1567,     0,     0,  1574,  1241,
    1242,  1243,  1244,  1245,  1246,  1268,  1247,  1271,  1248,     0,
    1572,     0,     0,  1654,  1670,  1277,  1312,  1299,  1317,  1340,
       0,  1331,  1334,     0,  1347,     0,  1353,  1354,   727,   733,
     722,   723,   724,   726,     0,  1357,     0,  1658,  1360,  1924,
    1379,  1365,  1427,  1416,     0,     0,   528,     0,     0,     0,
     367,     0,     0,   371,   372,   370,     0,   300,   303,   185,
       0,  1752,     0,   282,   278,   179,     0,   273,   275,   276,
    1990,  1883,     0,     0,    67,    69,    65,    83,  1854,  1883,
       0,     0,     0,  1883,     0,     0,     0,   175,  1584,   173,
     178,     0,     0,   177,  1593,   156,   157,  1885,   160,  1676,
    1250,  1249,   119,   123,   126,  1912,  1883,     0,    85,   103,
     155,     0,     0,   702,  1883,     0,   713,   705,   706,   718,
    1969,  1970,     0,   865,   794,   817,     0,     0,  1663,  1664,
    1909,     0,  1687,     0,     0,     0,     0,  1707,  1610,  1611,
    1612,     0,     0,     0,     0,     0,     0,     0,     0,  1708,
     874,   867,     0,     0,  1602,     0,     0,  1697,     0,     0,
    1625,  1626,  1627,  1553,  1621,     0,  1552,  1742,     0,     0,
       0,     0,     0,  1741,   879,   884,   886,     0,   929,   889,
    1642,   897,   890,   896,   933,   940,  1981,  1982,   938,     0,
     941,     0,   949,   946,  1966,  1965,  1564,     0,  1756,  1565,
    1668,  1669,   955,   956,   959,   953,  1923,  1487,  1006,   961,
     736,   736,   966,  1448,  1445,   970,   967,  1600,  1957,  1442,
    1442,  1442,  1442,  1015,  1008,     0,     0,   899,  1018,  1044,
    1020,  1501,  1501,  1021,  1028,  1029,   736,  1526,  1527,  1528,
    1522,  1507,  1948,  1514,  1534,  1537,  1536,  1538,  1530,  1521,
    1520,  1525,  1524,  1523,  1529,  1509,  1513,  1531,  1533,  1535,
    1511,  1512,  1508,  1510,  1502,  1503,  1515,  1516,  1517,  1518,
    1519,  1506,  1050,  1048,  1595,  1065,  1945,   736,  1080,     0,
    1100,     0,  1127,  1111,  1103,  1108,  1109,  1110,  1284,     0,
    1657,     0,     0,  1151,  1147,     0,  1160,  1957,     0,  1168,
    1174,  1175,   736,  1171,  1501,     0,     0,  1179,     0,  1207,
    1191,  1747,  1748,  1922,     0,  1211,  1217,  1214,  1193,  1225,
    1219,  1221,  1233,  1239,  1228,     0,  1233,     0,  1632,  1634,
    1635,  1636,     0,  1269,  1272,     0,     0,  1573,  1252,     0,
    1251,     0,     0,  1668,  1313,  1295,  1301,  1883,  1302,  1297,
       0,  1315,  1319,     0,  1341,  1329,     0,  1332,  1852,  1333,
       0,  1348,  1343,     0,  1355,   734,   732,   725,     0,  1925,
    1926,  1361,  1380,  1363,  1852,     0,  1428,  1414,  1418,   363,
       0,     0,   531,   380,   412,   415,     0,     0,   368,     0,
     378,   373,   379,   376,  1883,  1753,   188,  1864,   279,   280,
     281,  1844,     0,   271,   274,     0,  1989,    76,    66,     0,
    1585,    75,    59,     0,     0,  1683,  1679,  1684,  1682,  1680,
    1685,  1681,   164,   165,   167,   176,   171,   169,     0,   158,
    1887,  1886,   161,     0,  1912,  1915,  1914,     0,     0,   120,
     124,    87,    26,    37,    40,    44,    43,  1920,    38,    39,
       0,  1883,   714,     0,     0,   692,  1672,  1849,   822,  1883,
    1429,   818,   819,   821,   823,     0,     0,   811,  1429,  1964,
    1963,   808,   800,   802,   803,     0,  1429,     0,     0,     0,
     825,   806,     0,   814,   797,   813,   798,  1541,  1539,     0,
    1662,  1629,  1628,     0,  1614,     0,  1541,  1539,     0,  1541,
       0,  1723,  1541,  1558,  1560,  1541,     0,     0,     0,  1541,
    1618,  1619,  1620,     0,  1562,  1541,     0,  1908,  1451,   872,
    1670,  1597,     0,  1689,     0,     0,  1541,  1555,  1744,   879,
    1545,  1544,  1548,  1547,  1550,     0,   877,     0,     0,  1580,
     898,   939,   944,     0,  1869,     0,  1598,  1451,  1883,  1755,
    1667,   957,   736,   736,   954,  1488,  1494,  1491,  1447,   737,
    1450,  1443,  1449,  1444,  1446,     0,   980,   979,   972,   975,
     977,     0,   964,   965,   962,   963,     0,  1451,     0,   905,
    1023,  1038,  1040,  1039,  1033,  1035,  1041,  1501,  1030,  1027,
    1501,  1031,  1532,  1504,  1505,  1910,  1064,  1576,   736,  1072,
    1073,  1948,  1088,  1089,  1091,  1093,  1094,  1090,  1092,  1083,
    1948,  1079,     0,  1128,     0,  1130,  1129,  1131,  1113,  1123,
       0,     0,  1107,  1988,  1911,     0,  1286,     0,  1874,     0,
    1144,  1451,     0,     0,     0,  1162,  1568,  1172,  1185,  1181,
    1186,  1182,  1187,     0,  1177,  1423,  1422,  1184,  1193,  1417,
    1651,  1652,  1653,     0,     0,  1487,     0,   736,     0,  1232,
       0,     0,     0,     0,  1270,     0,  1274,  1273,  1266,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1254,  1255,
    1749,  1487,     0,  1318,  1940,  1940,  1338,     0,     0,  1451,
       0,     0,   735,     0,  1736,     0,  1338,  1221,  1838,   365,
     526,     0,     0,   626,     0,   438,     0,   369,   375,   419,
     381,  1858,  1883,     0,     0,  1883,  1858,  1901,  1883,  1842,
     299,     0,   304,   307,   308,   309,   310,   311,   312,   313,
     314,   315,     0,     0,   190,  1865,  1942,  1845,  1868,   272,
       0,    79,    81,    80,    77,    78,    61,   135,   134,   149,
     145,   150,   131,   148,   146,   132,   133,   147,   130,   136,
     137,   139,   166,     0,   170,     0,   174,  1677,   159,   162,
       0,  1913,   127,   121,   122,   125,     0,     0,    86,     0,
      90,    42,  1921,    36,    41,   708,   709,   712,     0,   703,
     719,   721,  1646,   829,  1644,  1645,     0,  1435,  1436,  1440,
    1441,   796,  1437,   736,  1432,   736,   820,  1962,  1961,  1903,
    1903,   827,   828,  1903,   834,  1883,   836,   837,   838,   863,
    1883,   839,   840,   841,   842,   843,     0,   844,   845,   847,
       0,   848,   849,     0,   850,  1883,   835,  1840,   853,   862,
     856,   824,   855,   812,   799,   801,  1429,   809,   804,   805,
     826,   807,  1542,  1543,  1673,     0,     0,     0,  1631,  1613,
    1630,  1754,     0,  1718,     0,  1718,  1722,     0,  1718,  1718,
    1718,     0,  1701,     0,  1718,     0,   736,   736,   868,  1457,
    1454,  1668,  1669,  1451,     0,  1718,  1718,     0,  1743,   878,
     880,   887,   885,   909,   943,   942,   947,     0,  1493,  1496,
    1489,  1495,  1490,  1492,   739,   986,   987,   984,   983,   985,
     982,   976,  1883,   988,     0,   991,   992,  1862,  1883,   995,
     996,   978,   997,   998,     0,  1883,  1000,   981,     0,  1009,
       0,   900,   901,   707,     0,  1501,  1501,  1037,   736,  1034,
       0,  1071,   736,  1074,  1069,     0,     0,  1095,     0,     0,
       0,  1124,  1126,     0,  1119,  1133,  1120,  1121,  1112,  1115,
    1133,     0,  1634,  1987,     0,  1960,  1278,  1883,   504,   505,
    1888,     0,  1875,  1285,  1145,  1148,     0,  1162,  1916,  1916,
       0,  1161,  1165,  1154,  1569,     0,  1173,  1170,     0,     0,
    1195,  1194,   736,   736,  1215,  1476,     0,  1220,  1222,     0,
    1234,  1501,  1501,  1229,  1235,  1253,  1275,  1265,  1267,  1257,
    1258,  1259,  1263,  1260,  1264,  1261,  1262,  1256,  1750,  1311,
       0,  1308,  1309,  1303,     0,  1296,  1986,  1985,     0,  1941,
    1322,  1322,  1460,     0,  1336,  1335,  1337,  1754,  1344,     0,
     728,     0,  1737,  1366,  1367,     0,  1370,  1373,  1377,  1371,
    1424,  1839,     0,   364,   365,   529,     0,     0,   290,  1885,
     413,     0,   439,     0,   410,  1883,  1846,     0,  1859,     0,
       0,  1883,  1842,     0,     0,     0,     0,     0,  1902,  1883,
     358,  1843,   359,     0,     0,   360,   305,   306,  1922,  1943,
    1858,     0,  1977,  1978,    74,   138,   141,     0,   168,     0,
     163,   128,     0,    96,    33,     0,    33,     0,    88,    91,
     710,   711,   721,   739,   833,  1430,  1438,  1434,  1431,  1433,
    1439,  1904,     0,     0,     0,     0,     0,   854,  1883,  1883,
    1497,  1497,     0,  1841,     0,   810,  1540,  1674,     0,  1451,
    1732,  1705,  1734,  1706,  1730,  1702,  1703,  1704,  1728,  1725,
    1726,  1700,  1598,  1459,  1456,  1452,  1458,  1453,  1455,  1667,
     869,  1719,     0,  1698,  1699,  1745,  1637,  1638,   918,  1881,
    1758,  1759,  1760,  1761,  1762,  1763,  1764,  1765,  1757,     0,
       0,   989,   990,  1908,   674,   676,   993,   994,     0,     0,
    1497,  1497,     0,  1451,  1563,  1451,  1563,   902,   903,     0,
     907,   906,   908,  1036,  1042,  1032,  1066,  1070,  1081,  1084,
    1085,  1860,  1077,  1948,  1082,  1133,  1633,  1133,     0,  1879,
    1879,  1118,  1134,  1135,  1116,  1122,  1117,  1959,  1288,     0,
    1889,  1282,  1876,  1451,  1155,  1917,   267,   268,   269,  1164,
       0,  1188,     0,     0,  1202,     0,  1475,  1478,  1469,  1477,
    1470,  1223,   736,   736,  1236,  1310,  1300,  1304,  1305,  1306,
    1307,  1298,  1320,  1323,  1321,   736,   736,  1330,  1466,  1463,
    1883,  1451,  1451,   730,  1358,  1736,  1369,  1872,  1375,  1872,
    1460,   736,   736,  1415,  1426,  1484,  1481,  1425,  1421,  1420,
    1893,   527,   365,   532,     0,     0,   416,   440,     0,   409,
       0,   514,   444,  1931,  1931,  1931,  1931,  1931,  1953,   445,
     480,   482,   448,   449,   450,   451,   452,   453,   476,   474,
     475,   477,   478,   483,   481,   454,  1927,   479,     0,   455,
     441,   456,   457,     0,  1934,   459,   460,   458,  1890,   462,
     463,   461,  1883,   420,   421,   422,   423,   424,   425,   442,
     446,   447,   426,   427,   428,   429,   430,   431,   432,   433,
       0,     0,  1847,     0,   414,     0,   382,   327,   236,   355,
    1979,  1980,  1588,   336,  1586,  1972,  1971,   329,  1590,  1589,
    1899,  1856,  1872,     0,  1883,   333,   332,  1883,   361,  1901,
    1922,  1950,   252,     0,  1883,  1854,  1888,   254,     0,  1957,
     240,   189,   239,   191,   192,   193,   194,   195,   196,     0,
     197,     0,   198,   251,   199,   200,   201,   202,   203,   204,
    1850,  1883,     0,   277,     0,   140,   172,    92,    93,    97,
      94,    95,    89,   739,   830,   832,   831,   858,   857,     0,
       0,   860,     0,  1649,  1650,   859,   852,  1678,   861,  1647,
    1648,  1675,   870,  1720,   736,   736,   736,   891,   925,   921,
    1908,  1882,   912,   917,   916,   911,     0,  1002,  1863,   675,
     677,  1001,  1004,  1003,   999,  1011,     0,  1010,     0,   904,
    1606,  1605,  1660,  1086,  1861,     0,     0,  1114,  1125,  1133,
    1880,     0,     0,  1136,  1137,     0,     0,  1291,  1287,  1281,
    1149,  1163,     0,  1196,  1883,  1487,     0,     0,  1197,     0,
    1201,  1230,  1237,  1468,  1465,  1461,  1467,  1462,  1464,     0,
    1346,  1345,  1381,   729,     0,  1368,  1873,     0,  1872,  1372,
       0,  1364,  1483,  1486,  1479,  1485,  1480,  1482,  1894,  1895,
    1419,   530,   534,   627,   515,   517,   519,   411,   523,   524,
    1932,   473,   472,   465,   464,   471,   470,   469,   468,   467,
     466,  1954,     0,  1928,   511,   497,   491,   434,  1935,  1891,
    1892,   512,     0,   436,  1766,  1766,   418,  1908,     0,     0,
     417,   383,     0,   317,     0,   354,  1587,  1900,   338,     0,
     320,  1936,   347,   349,   353,   352,   348,   350,   346,   351,
       0,     0,  1883,  1888,  1951,  1952,   219,   255,  1922,  1883,
    1883,  1883,  1883,   264,  1844,   265,     0,  1883,  1901,  1851,
       0,     0,   283,   284,   287,   142,   143,     0,   846,   851,
    1983,  1984,  1498,   923,   927,   924,   919,   926,   920,   922,
       0,   910,  1451,  1451,     0,  1096,  1132,  1139,  1138,  1883,
    1289,     0,     0,  1279,  1283,     0,     0,  1192,  1205,  1476,
    1473,  1204,  1200,  1198,  1199,  1339,  1389,   731,  1374,     0,
    1378,   533,   629,   521,   518,     0,   513,  1872,   493,     0,
    1946,   443,     0,   435,  1854,   484,   485,     0,     0,   392,
     388,   391,   390,   389,   404,   400,   402,   403,   405,   401,
     406,   407,   408,   385,   396,   397,   398,   393,   394,   395,
     387,   384,   328,   319,   318,   316,   356,  1582,   337,  1856,
    1937,   325,   334,   331,   335,   330,     0,  1883,   221,   220,
     217,   254,   250,     0,     0,     0,     0,   263,   266,     0,
    1883,   253,   235,   285,     0,   286,     0,     0,   913,  1013,
    1012,  1087,     0,  1292,  1883,  1501,  1203,  1471,  1472,  1474,
    1849,  1412,  1411,  1390,  1382,  1383,  1840,  1384,  1385,  1386,
    1387,  1410,     0,     0,  1376,     0,   535,     0,   633,   628,
     630,     0,     0,   516,     0,   520,     0,     0,   491,   492,
    1947,   495,   437,  1767,   386,   399,  1583,     0,     0,   339,
     340,   341,   342,     0,   321,  1871,   327,     0,   229,   230,
     228,   227,     0,   213,   214,   224,   224,     0,   212,   210,
     211,   216,   215,   224,   224,     0,   256,   257,   258,   259,
     262,   237,     0,   288,   144,   720,  1290,     0,  1189,     0,
    1938,     0,  1910,   536,     0,   634,     0,   631,     0,  1883,
     498,   494,   499,  1910,   502,   345,   344,  1848,  1856,   326,
    1738,   225,   207,   226,   208,  1864,   209,   206,   222,   205,
     223,  1883,     0,   246,   245,   246,   242,  1293,     0,  1939,
       0,  1408,  1407,  1406,     0,     0,   636,   637,   632,  1956,
       0,   500,   502,     0,   506,   501,     0,   323,   232,  1739,
     218,     0,   260,     0,   244,   243,  1409,  1968,  1967,  1918,
    1402,  1396,  1397,  1399,     0,  1883,  1933,   522,   506,   496,
    1852,   489,  1888,   343,  1910,   322,     0,   231,   261,     0,
     249,  1919,  1910,  1405,  1400,  1403,     0,  1398,   540,  1883,
    1883,  1842,  1896,   565,   539,   543,   544,     0,  1866,   652,
    1883,   641,  1953,   642,  1862,  1883,     0,   655,   650,   645,
     651,  1903,   646,     0,   649,   657,   654,   647,   653,     0,
     658,   648,     0,   669,   663,   667,   666,   664,   668,   638,
     670,   665,     0,  1903,   490,     0,  1883,     0,     0,     0,
       0,  1404,  1401,     0,  2006,  2007,  1883,  1842,     0,   537,
     541,  1867,   545,     0,     0,   639,   640,   643,   644,     0,
     672,  1883,  1946,  1883,   673,   671,   689,  1883,   510,   507,
     508,     0,   324,     0,   151,   152,   234,     0,  1975,  1976,
     247,  1395,  1392,  1394,  1393,  1388,  1391,   542,  1897,  1898,
     553,   550,   377,   566,   546,   547,   662,   661,   682,   688,
       0,   685,   509,   503,   233,   248,   549,  1973,  1974,   552,
     567,   379,   548,   680,   678,   681,   679,   683,   684,     0,
     656,   686,   687,     0,     0,  1883,  1883,     0,   554,   555,
     556,   557,   558,   559,     0,   569,   659,   660,  1993,  1992,
    1883,     0,     0,  1995,     0,  1883,  1883,   551,  1933,     0,
     564,   560,  1994,     0,     0,  1877,  1905,  1842,     0,     0,
       0,  1883,  1908,   568,  1883,  1883,     0,   574,   576,   585,
     577,   579,   582,   570,   571,   572,   581,   583,   586,   573,
       0,   578,     0,   580,   584,   575,  1905,  1842,   561,   563,
     562,  1878,   624,  1906,  1907,  1885,   610,  1883,   491,  1501,
       0,     0,     0,     0,     0,   618,     0,   608,   614,   617,
       0,   611,   619,   622,  1885,   613,   609,     0,  1946,   606,
    1754,   602,  1616,  1997,     0,     0,  1999,  2001,     0,  2005,
    2003,   587,   591,   595,   595,   589,   593,   588,   594,   625,
       0,   616,   615,   621,   620,   612,   600,   495,   623,  1910,
     601,  1617,  1996,  2000,  1998,  2004,  2002,   598,   590,   598,
     592,     0,   487,     0,     0,   597,   596,     0,     0,   486,
     605,   603,   604,   599,   607,   488
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -2474, -2474, -2474, -2474, -2474,  2330, -2474, -2474, -2474, -2474,
   -2474, -2474,  2282, -2474,  1680, -2474, -2474, -2474, -2474, -2474,
   -2474,  2283,  2284,   -64, -2474, -2474, -2474,  1269, -2474, -2474,
   -2474, -2474, -2474,  2290, -2474, -2474, -2474,  2294, -2474, -2474,
    1953,  -255, -2474, -2474, -2474, -2474, -2474,  2143, -2474, -2474,
   -2474, -2474,   940, -2474, -2474, -2474, -2474,  2131,   -60, -2474,
   -2474, -2474, -2474,  1283, -2474, -2474, -2474, -2474, -2474,   963,
   -2474, -2474, -1684, -2474, -2474, -2474, -2474, -2474,  1630, -2474,
   -2474, -2474, -2474,  1305, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  -912, -2474,
   -2474, -2474, -2474, -2474,   105, -2474, -2474, -2474, -2474, -2474,
    -145, -2474,   123, -2474, -2474, -2474,   -79, -2474, -2474, -2474,
   -2474,   119, -2474, -2474,  1672, -2474, -2474, -2474, -2474, -2474,
     114, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   -69, -2474,
   -2474, -2474,   140, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -1257, -2474, -2474,  1693, -2474, -1589, -2262,  -666, -2474, -2474,
   -1978, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2055,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   681, -1724,
    -166,   184, -1267, -1254, -1720, -2474, -2474, -2474, -2329, -2474,
    -446, -2474, -2474,  -140, -2474,  -138,  -160, -2474,  -256, -1695,
   -2474, -1677, -2474, -1646, -2474, -2474,   223, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  -418,
    -438, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -1381, -2474,  -389, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474,    -4, -2474, -2474, -2474,  -181,  -180,  -274,  -272,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474,  2169,  1085, -2474,   849, -2474, -2474, -2474, -2474, -1303,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474,  -728, -2474, -2474,
     -24, -2474,  2348, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
    1323, -2474,  -564, -2474, -2474,  -532, -2474,   961, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1259, -2474, -2474,
   -2474,  1914, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1250,
   -2474, -2474,   860, -2474, -2474,  -596, -2474, -2474, -2474,   333,
   -2474,   335, -2474, -2474, -2474, -2474,  1908, -2474, -2474, -2474,
    1616, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1885, -2474, -2474,
   -2474,  1231, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1567, -2474, -2474,
    1569, -2474, -2474,  1220,   869, -2474, -2474, -2474, -2474, -2474,
    1874, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474,   600,  1529, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474,  1531, -2474, -2474,   853,
   -2474,  1196, -2474, -2474, -1515,   589,   593, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,  1849,
    1528,   841, -2474, -2474, -2474, -2474, -2474, -2474, -2141,  1845,
   -2474, -2474, -2474,   833, -2474, -2474, -2474,  1174, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474,  1126, -2474, -2474, -2474, -2474, -2474,
   -2474,  1501,   821, -2474, -2474, -2474, -2474, -2474,  -516, -2474,
   -2474, -2474, -2474,  1148, -2474, -2474, -2474,  1831, -2474,  1822,
   -2474, -2474, -2474,  2109, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474,   799, -2474, -2474, -2474, -2474, -2474,  1818,
   -2474, -2474,  1136, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474,   558, -2474,  1140, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   -74,
   -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474, -2474,   466,
   -2474,  1464, -2474, -2474,  -990, -2474,  1039, -2474, -2474,  1043,
   -2474,   994, -2474,  1632, -2474,  1634, -1108, -2474,   970, -2474,
     962,   571, -2474,   583, -2474,   585, -2474, -2474, -2474, -1565,
     195, -1230, -2474, -2474,   569, -2474,   572, -1226,   809, -2474,
    1303, -2474,  1306,  -881,  -920,  -298,  -798, -2474, -2474,  1599,
   -1189,   837,   842,   843,   851,   762,   725,   -47,   908,   642,
   -2474,  1187,  -184,  -731,  -285,   872,  1879, -1242,  -197,  -355,
   -2474,  -598, -2474,  -260, -1327,  1699, -2270,  -405,  1469, -2474,
     507, -1284,  -191,  1780,  -284,  -301, -2474,  -155,  -294, -2474,
     682, -2474,  -718, -1265, -2474,  1218,  -570,  -814, -2474,   999,
    -318,  2002,  -907, -2474, -2474,   -66,  -327, -2474,  1120, -2474,
   -2474, -2474,  -282,  -434, -2474, -2474,  1243,  -466,  -492,  -312,
    1123, -1668,  1129,  -324,  -234,  -437,   -28, -2474, -2474, -2474,
     300,  2045, -2474, -2474,   948, -2474, -2474, -2474, -2474, -2474,
   -2474, -2474, -2474, -2474, -2474, -2474, -1471, -2474, -2474,   318,
   -2474, -2474,   128, -1670,   281, -2474, -2129, -2474,  -942, -1897,
   -1931, -1249, -2474, -2474,    31, -2474, -1334, -2474, -1722, -2474,
   -2474,   679, -2474,  -211, -1664, -1953, -2474, -2474, -2474, -2474,
   -1871, -1413,  -238,  -514, -1202,  1475,   926, -2474, -2474,  -509,
   -2474, -2474, -2474,   -33, -2474, -2474, -2474,  1225, -2474,   964,
   -2422,  -828, -2474, -2474, -2474,  -279,   826, -1673, -2442, -2474,
   -2474,  1037, -2474, -2474,  -114, -2474,  1205, -2474, -2474, -2474,
      55, -2474, -2473,  -232, -2474, -2474, -2474, -2474, -2474, -2474
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     6,     7,     8,     9,    10,    11,    12,
      57,    58,    59,    61,    18,    13,    23,    35,   426,    24,
      34,    80,    84,   236,   741,  1077,  1078,  1079,    19,    20,
      32,    33,    51,    52,   204,   398,   711,    53,   203,   388,
     389,   390,   391,   392,   393,   394,  1374,   395,   418,  1071,
    1408,  1409,  1410,  1735,    75,   218,   219,   220,   221,   222,
     416,   732,  1404,   733,   734,   223,   713,  1388,  1389,  1390,
    2054,  2255,  1391,  2656,   224,   423,   225,   725,   726,   727,
    1398,   226,  1052,  1053,   227,   228,  1394,   229,   230,   231,
     232,   233,   234,    47,    48,    71,   379,   202,   380,  1364,
    1718,  2033,  2034,  2453,  2454,  2455,  2360,  2499,  2492,  2035,
    2441,  2036,  2557,  2037,  1999,  2038,  2039,  2040,  2041,  2506,
    2534,  2042,  2043,  2044,  2045,  2046,  2458,  2047,  2048,  2244,
    2049,  1611,   695,   696,   697,   698,  1031,   699,  1027,  2252,
    2253,  2375,    29,   196,    30,    44,    67,   197,   198,   688,
     199,  1024,  1352,  1353,  2345,  1354,  2555,  2436,  2213,  1355,
    1356,  2017,  2353,  1357,  1358,  2348,  2429,  2430,  2431,  2432,
    1359,  2228,  2229,  1360,  2215,  1361,  1362,  1714,   371,  1329,
     372,   373,   680,   681,  1339,   682,  1021,  1022,  1696,  2210,
    2333,  2334,  2335,  2336,  2337,   683,  1929,   684,  1334,   685,
    1335,  1994,  1695,  1973,  1974,  1975,  2313,  1691,  1692,  1977,
    1978,  1979,  1980,  1981,  1982,  2749,  2849,  1983,  2310,  2418,
    2484,  2308,  2522,  2524,  2525,  1600,  2551,  2649,  2650,  1984,
    1985,  1986,  1987,  1690,  2303,  2174,  2175,  2413,  1989,   676,
    1684,  1012,  1922,  1333,  2172,  2301,  2406,  2515,  2545,  2574,
    2575,  2632,  2674,  2576,  2670,  2686,  2708,  2709,  2710,  2711,
    2712,  2713,  2629,  2673,  2715,  2728,  2753,  2754,  2811,  2838,
    2845,  2755,  2756,  2830,  2851,  2757,  2758,  2759,  2760,  2761,
    2762,  2787,  2788,  2791,  2792,  2763,  2764,  2765,  1688,  2302,
    2409,  2410,  2411,  2517,  2546,  2609,  1816,  1817,  2697,  2698,
    2699,  2703,  2610,  2611,    41,   749,  1421,    42,    89,   241,
     240,   428,   429,   430,   746,  1083,   243,  1085,  1742,   365,
     660,   661,  1903,  2153,   662,   663,  1321,  1188,  1189,  1534,
     664,    65,   142,   143,   315,   438,   755,   439,  1090,  1091,
    1092,  1114,  1093,  1441,  1442,  1094,  1471,  1472,   754,   144,
     316,   476,   783,   781,   145,   317,   493,  1166,   146,   318,
     505,   506,  1168,   147,   319,   514,  1170,   515,   812,   858,
    1209,  1561,  1562,  1563,  1798,   334,  2095,  2087,  2268,  2088,
    2266,  2089,   809,   148,   320,   518,   519,   149,   321,   522,
     818,   150,   322,   525,   823,   151,   152,   153,   323,   534,
     832,   835,   154,   324,   538,   539,   540,   541,   848,   542,
    1198,  1199,  1200,  1539,  1557,   839,   155,   325,   546,   854,
     156,   326,   549,   157,   327,   552,   553,   554,   863,   864,
     865,  1219,   866,  1214,  1215,  1567,   860,   158,   328,   563,
     335,   159,   329,   564,   160,   330,   567,   161,   331,   570,
    1226,   162,   163,   336,  1230,  1574,   164,   337,   575,   908,
    1239,  1577,  1839,  1840,  1841,  1842,   165,   338,   578,   166,
     339,   580,   581,   914,   915,  1251,   916,   917,  1588,  1589,
    1248,  1249,  1250,  1582,  1851,  1852,  1853,   167,   340,   168,
     341,   590,   169,   342,   592,   924,   170,   344,   598,   599,
     928,  1613,   171,   345,   603,   932,  1617,   933,   604,   605,
     606,  1269,  1271,  1272,   172,   346,   613,  1284,  1874,  2135,
    2287,   940,   173,   174,   347,   615,   175,   176,   348,   618,
     947,   177,   349,   620,  1285,   950,   178,   179,   350,   623,
     956,  1288,  1633,  1634,   954,   180,   351,   629,   735,   971,
     630,   631,  1308,  1309,   632,   633,   634,   635,   636,   637,
     638,   181,   352,   585,  1858,   918,  2129,  1256,  1596,  2127,
    2283,   182,   353,   646,  1311,   979,  1650,  1651,  1652,   975,
     183,   648,   981,  1892,   359,   184,   360,   649,   650,   651,
     989,  1665,  1662,   985,   185,   361,   654,   992,   186,   363,
     187,   364,   656,   188,   366,   665,   189,   367,   668,   190,
     368,   670,  1005,  1673,  1674,  1326,  1676,  1908,  2159,  1910,
    1003,  2154,  2296,  2394,  2395,  2396,  2665,  2397,  2541,  2542,
    2566,  2398,  2513,  2399,  2400,  2401,   191,   369,   672,   945,
    1327,  1277,  1913,  1007,  1431,  1748,  1432,  1433,  1745,  1434,
    1435,   842,  1193,   843,  1191,   844,  1508,  1787,  1509,  1785,
    1510,  1897,  2147,  1898,  2145,  1899,  1624,  2288,  2388,  1625,
    1878,  1879,  1914,  2166,  1915,  2164,  1916,  1184,  1185,  1532,
    1186,  1530,  1187,  2071,   573,   574,   556,   557,   894,   895,
     896,   897,   898,   899,   900,  1117,  1485,  1127,   495,   496,
     497,   498,   477,   526,   826,   616,   624,  1265,  1266,   579,
     639,   640,   905,   607,   508,   509,  2346,  2009,  1041,  2003,
    2004,  2010,   402,   728,   565,   528,   846,   478,   479,  2109,
     480,  2801,  1139,   500,  1123,  1489,  1590,  1845,   958,  1846,
     520,   608,  1423,  2078,  2072,  1279,  1424,   586,   643,   666,
    1591,  2111,   481,   441,   529,   530,   442,   758,   759,  1425,
    1399,  2789,  1054,   482,   483,   484,   485,   486,   487,   488,
     787,   767,  1146,  1143,  1136,  1128,  1130,   686,  1675,  2528,
     804,  1159,  1518,   943,  1654,   692,   829,  1179,  1808,  2315,
     314,  1682,  1764,  1712,  1368,  1995,  1095,  2250,   431,   396,
     415,  1699,  2115,  1818,  1366,  2633,  1175,  2437,  2157,  1603,
    2772,  2121,  2096,   408,  1063,  1861,  2201,  2170,  2628,  2218,
    1709,  1752,  2775,   761,  1257,  1067,  1866,  2562,  1413,  2050,
    1001,  2194,   406,  2182,  1991,  2351,  2510,  1660,  1720,   907,
    2421,   571,  2236,  2192,  2414,   612,  1597,  1443,  1116,   827,
    2539,   752,  2007,  2689,  2660,  1724,  1703,   820,  2262,  1658,
    1258,   397,  2720,  2726,  2814,  2815,  2816,  2817,  2818,  2578
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     409,   410,   376,   717,   413,   720,   544,   762,   723,   657,
     821,  1014,  1015,  1016,  1270,   583,   403,   237,    64,   768,
    1769,  1607,   411,  1570,   642,  1925,   837,  1753,   566,   555,
    1754,   547,  2001,   440,  1721,   501,   566,   527,   521,   831,
     572,   967,   545,  2055,  1222,  1926,  1317,   591,   593,  1988,
     566,  1148,   980,   576,  1626,   790,   957,   582,   507,  1627,
    1145,  1880,  1324,   609,  1216,   830,   652,   641,   400,  1526,
    1155,    87,  1683,  2241,  2242,  1855,  1060,   374,   861, -1656,
    2220,  1732,   400,   502,   667,  1655,   671,  1722,   532,  2482,
    1726,   444,   524,  2653,  1698,   532,  2080,  1706,  2540,  1559,
     622,  1211,  1760,   532,   736,  1594,  2427,  2090,  1473,  1225,
     948,  1020,  1253,  1190,   930,  2257,  1477,   532,  1743,   427,
     712,  1653,   920,  1820,   523,  1207,   535,   872,  2239,  2511,
    2125, -1957, -1686,   499,   705,  2438,  1300,   516,  1221,   427,
   -1910,   414,  1073, -1657,  1301,  2475,   407, -1957,  2232,   614,
   -1888,   619,   831,  1605,    -5,  1860,   647,  1779,  1115, -1852,
     421,  1087,  2204,  1449,   763,  1069,  1415,  2402,  2343,   543,
     669,  2837, -1957,   381,   703,  1992, -1854,   377,   973,  1229,
    1737,   382,  1032,   194,   784,  1174,   710,  2160,   332,  1830,
     840,    21,   714,   715,  1261,   716,  1901, -1848,  1314, -1848,
     721,   722,  1535,  1536,  1267,   737,   655,  1895,   794,   794,
     794,  1668, -1957,  1211,   -21,   742,  2281,  1601,  1064,   769,
    2679,   504,  -695,   910,  2559,  2077,  1065,   517,     4,     5,
   -1910,  2341,  2532,   609,  2725,   995,   521,   444,   797,  1253,
    1716,  2466,  1074,   407,   819,   507,  1416,  2408, -1955,   784,
   -1957,   941,   744, -1923,   504,   791,   502,   502,   502,  2203,
     533,  2558,  1319,   901,   904,  2014,  1120,  1564,  1612,  1172,
     494,  1243,  2727,  2718,  1717,   904,  1121,  1733,  2207,   999,
    2357,  1000, -1665,  2719,  1196, -1957,  2767,     3,  2850,   503,
    2221,  1061,   532,  1927,  1173,   921,   996,   909,  2412,   452,
    2790,   994,   942,   452, -1957,  2015,   499,   499,   499,  2812,
    1055,   584,   375,   911,  1302,  1602,  1197,   611,  2084,  1482,
    1244,   745,   609,   841,  1482,   815,  2507,  1124,  1315,  2240,
    2117,   532,  2118,  2282,  2091,   566,   833,    22,  1180,  2424,
     904,  1242,   532,   456,   959,   378,  1303,   456,  1153,  1993,
     504,  2560,   965,   461,   982,  2408,   652,   461,  2526,   421,
     -21,   784,  1482,  -695, -1868,  2439,   333,  -695,  1304,   983,
    2256,   990,     4,     5,   -35,  1482,  2827,  2370, -1663,  1896,
     628,  2683,  1406,   677,   524,   847,   785,  1176, -1848,   383,
    1506,  1037,   960,  1780,   602,   729,   437,  2092,  2205,   602,
     375,   602,   532,  1575,   427,  1790,  -366,   532,  1089,   195,
    2690,  2126,  1578,  1482, -1944,   427,  2093,  2423,  2434,  1216,
    2440,  1513,  1216,  1305,  1066,   770,  -695,  1921,  1323,  1254,
    2512,   887,  1075, -1888,  1278,   931,  2299, -1852,   465,  2063,
    -695,  -695,   465,  1245,   795,   452,  1482,  1537,   658,  2798,
    1294,  1595,  1013,   705,  1528,  1529,  1860, -1852,  1608,  1885,
     693,   792,   503,   503,   503,   384,  1482,  1831,  1482, -1848,
    1122,  2052,   677,  2344,  1761,   794,  1096,  2208,  2209,  2053,
     437,  1180,   794,   794,   794,  1306,  1765,   611,  1149,   456,
    1035,   794,   794,   794,  1140,  1821,  1140,   527,  1039,   461,
    1571,  1026,  1044,   659,   794,  1255,  1140,   794,  1246,  1507,
     467,   437,  2654,   437,   467,  1568,   794,   794,   794,   794,
     794, -1883,   385,   502,   757,  1068,    88,   386,  1407,  1727,
     502,   502,   502,  1080,   626,  1476,   819,  2428,  1076,   502,
     502,   502,  1141,  2219,  1141,   794,  1070,  1212,  1151,  1469,
    1723,   757,   502,   784,  1141,   502,   470,  2527,  1213,  1630,
     470,  1206,  1208,   555,   502,   502,   502,   502,   502,  1201,
    2289,   527,  -695,   499,   724,  2094,   611,   602,   862,   694,
     499,   499,   499, -1852,   465,  2416,  1307,   401,  1119,   499,
     499,   499,  1734,   502,   912,   452,   375,  1540,   961,  2616,
     901,   401,   499, -1852,  2276,   499,   437,  2016,   475,   437,
    2655,   437,   475,  1832,   499,   499,   499,   499,   499,  1564,
    1281,   566,  1151,   627,  1538,  2086, -1923,   524,   437,   959,
    1195,  1262,  1255,  1505,  1196,  1181,   387,   794,  1260,   456,
     527,   437,   524,   499,  1738,  1227,  1033,  1119,  1264,   461,
     602,  1488,   798,  1273,   628,  1483,   467,   799,  1512,  1212,
    1483,  2082,  1280,  1622,   904,  2171,  1197,   678,  1800,   913,
    1213,  1060,  1286,  1289,   679,  1298,  1274,   960,   524,    17,
    1583,  2084,   524,  1322,  1511,   502,  1313,   658,   532,  1490,
    -366,  1151,  2464, -1621,   444,   602,   707,  -366,  1483,  1320,
    1312,    14,   470,   855,  1316,  1747,   524,  1750,   527, -1729,
    1118,  1483,  1883,  2407,  1484,  2105,   959,  2107,  2635,   798,
    1740,  1635, -1852,  1370,   799,  2389,  1135,  1135,  1135,   503,
    2008,   532,   729, -1852,   465,   499,   503,   503,   503,  1154,
    -700,  1609,   659,  2544,  1247,   503,   503,   503,  1142,  1483,
    1142,  -698, -1731,   693,   475,  2130,   678,   906,   503,  1151,
    1142,   503, -1944,   679,   960, -1733,  1310,  1216,  1417,   433,
     503,   503,   503,   503,   503,  1920,   729,  1395,  1783,  1784,
    1177, -1957,  1483,  1778,  1639,  1640,  1641,  1642,  1643,  1644,
    1645,  1646,   872,  2150,  2151,  2136,  -366,   968,   824,   503,
   -1957,  2077,  1483,  1496,  1483,  -538,   467, -1621,  1610,  1437,
     516,  1438,  1096,  1363,  2085,  2137,  2352,  2491,  2195,   436,
    -700,  2407,  2603,   959,  2498,  1656,  2605,  1823,  1623,  1825,
     628,  -698,    49,  -538,  -538,  2503, -1727,   794,   452,   747,
    1835,  2138, -1665,  1491,  1837,  2064,  2065,  2066,  2067,  2068,
    1406,  2606,   470,   452, -1957,  2076, -1724,  1904,  1516,   757,
    2636,  1490, -1735,   803,  1520,  2199,  2139,   527, -1852,  2607,
    1418,   960,   694,  2613, -1957,  1863,  1402,  1403,  1426, -1852,
    2075, -1957,   456,   961,  2280,   502,  1061,   856,  1075,   452,
    1379,   503,   461,   452,  1876,  1877,    15,   456,   437, -1957,
    2608,  2625,   452,  2097,   475,  2290, -1957,   461,  1522,  2291,
     437,   354,  2101,  1828,   532,  2104,  1282,   452,   532,  1201,
    1801,  1802,  1803,  2693,   798,  1648,  1506,  1799,   511,   799,
    1649,   532,  1371,   456,  1380,   499,   729,   456,  1902,  2102,
    2103,  2338,  2338,   461,  1381,    25,   456,   461,    50,   729,
    -538,   628,   729,  2569,  2339,  2339,   461,  2669,   566,   904,
    2671,   456,  2403,   602,   437,  2661,    26,  1527,   959,  1592,
     961,   461,  2504,   532,   434,  1604,  1811,   465,  2191,  -538,
     816,  2570,  2571,  2077,  2694,   748,  1615,  1804,  2086,   437,
      78,   437,   465,  1182,  1076,  2222,  1407,  1422,  1657,   381,
    1439,  1621,  2223,  1918,  2747,  1638,   355,   382,  2748,   959,
    1847,  2200,  -693,  2785,   966,  2116,   960,   960,  2099,  1789,
     744,  2714,  2662,  1666,   437,   532,   532,   532,   465,  1383,
    2663,    82,   465,  2750,  2119,   437,  1667,  1663,   602,  2383,
    2672,   465,   678,  1677,  1677,  1507,   356,  1663,  1286,   467,
    2338,  2751,   750,  2730,  2731,  2766,   465,   960,  2786,  1183,
     437,  2768,  1598,  2339,   467,  2769,  2770,  2776,  1614,   437,
    -538,  1782,  1487,  2005,  2505,   437,   437,   961,   357,  2456,
      79,  1440,  2752,   437,  1919,   532,   512,   951,   513,   745,
    2140,   503,  1774,    27,  2793,   470,   887,  2795,  2390, -1621,
     467,  1384,   825,   817,   467,  1906,  1741, -1621, -1621,  1506,
     470,  2796, -1621,   467,  1805,  1806,  2664,  1372,  1812,  1807,
    2695,    83,  2723,  2793,  1693,  2696,   602,   407,   467,  1697,
    2825,  1700,   969,   602,  1705,  1707,  1293,  1710,  2234,  1008,
     602,   437,   751,  2485,  2486,  2435,   470,   475, -1735,  1622,
     470,  1911,  1599,  -693,  2141,  2142,   437,  -693,  2724,   470,
    2100,  2657,   475,  2259,  2379,  2380,  2180,  2143,  2144,   970,
    2180,  1715,    38,  2180,   470,  2718,   358, -1868,  2641,  1373,
    2443,  2444,  2445,  2162,  2163,  2719,  1028,  2543,  2006,  2235,
    1040,  1341,   437,  1797,  -690,   437,   437,  2391,   475,  1387,
    2647,  1422,   475,   794,   794,   437, -1870,  -538,  1342,  1731,
     794,   475,   794,    31,  2543,   383,  -693,  2224,  2572,  1140,
     437,   621,   961,   961,   206,  2178,   475,  1182,  1507,    28,
    -693,  -693,   794,  2022,  1755,  1796,  2358,  2392,  2501,  1756,
    2181,   673,  2553,  2658,  2183,  2659,   400,  2185,  1343,  1029,
    1030,   502,   502,  1182,  1762,  1797,  1427,    39,   502,  1428,
     502,  1174,  2563,   961,  2180,  2023,   407,  1141,  1834,   558,
    2514,    43,   207,  1824,   206,  1826,  2393,   798,  2502,   532,
     502,  2523,   799,  2359,  2179,   594,   381,   381,  1836,  1912,
     729,   384,  -357,  1183,   382,   382,  2564,  1796,   959,  2098,
    2180,   499,   499,   904,  2446,    45,  1592,  2225,   499,  -357,
     499,   208,   375,   209,  1623,   210,   690,  2565,  2447,  1183,
     729,   527,   207,   211,   532,  1872,   532,    46,  1615,  2828,
     499,  1810,  2226,  1882,  2227,  -690,  1273,  1819,  2187,  -690,
     691,  1875,   595,   730,  1822,   731,   960,  1797,   385,  -357,
     596,  2243,  2617,   386,   960,  2573,  2263,  2264,  2265,   976,
    2620,   208,  -693,   209,   550,   210,  1893,  1893,  1886,   836,
     532,    54,   532,   211,  2189,   551,   794,  2361,  2666,  1232,
     212,   407,  1233,  1234,   527,  1275,  1859,  1997,  1344,  1796,
    1429,  2704,  1430,   959,  2011,  2112,   559,   560,  -690,  1345,
    1813,  1628,  2705,  2448,  2449,  2450,  2822,   706,   206,   934,
    1614, -1883,  -690,  -690,  1276,   561,  2826, -1848,  2451,  -357,
     977,  1849,  2059,   978,   502,  2706,  1629,  1814,   935,  1815,
     212,  1377,  1763,  2073,  2073,   532,   597,  2079,  1766,  1768,
     407,   960,  1162,  1163,  1164,  1766,  1869,  1766,  2074,  2074,
    1850,  2060,  1296,  -357,  2128,  2707,   207,   503,   503,    56,
     587,  2469,   707,  1297,   503,  2468,   503,  1792, -1957,  1873,
     562,  1870,   587,  1142,   499,   213,  2537,   658,   874,   875,
    2538,  2832,  -357,   730,  1990,   731,   503,   800, -1957,  -357,
    2000,   771,   772,  2073,  2073,   208,   801,   209,  2013,   210,
    -357,   777,   383,   383,  2732,  1346,  1347,   211,  2074,  2074,
    1809,  2233,  2452,   743, -1957,  2800,  2802,   743,   876,   877,
    1348,   214,  1349,  2018,  1797,   213,  1126,  1129,  1132,  2197,
    2833,  1378,   659,  2835,  2494,  1157,  2831,  2626,  2627, -1957,
      40,  2496,  2497,    55,  -690,   798,   807,  2069,  2070,  1486,
     799,  1156,   961,  2834,    62,  2841,  2836,  2318,   798,  2081,
     961,   510,    60,   799,   212,   531,  1796,  1427,    63,   798,
    1428,   214,   531,   568,   799,  1701,  2270,  1702,   384,   384,
     531,    66,  2854,   588, -1640, -1640, -1640, -1640,   610,   532,
     617,   532,   617,   625,   644,   588,  2319,  2320,  2321,  2322,
    2323,  2176,    68,  1429,  2083,  1430,  -357,  -357,  2615,  1350,
      69,  1891,   617,   215,   798,    70,  2168,  1514,  2169,   799,
    1379,  -357,  2260,  -357,  2261,    72,   798,  2843,    73,   532,
     503,   799,    49,  1940,  1941,   385,   385,    74,  1097,  1098,
     386,   386,   452,  2658,   235,  2659,  2196,   961,    50,  1235,
    1236,   718,  2687,   718,  2688,   193,   718,  1351,   216,   213,
     200,  2803,  2723,   215,  1380,  1099,   201,  1100,   532,   205,
    2058,  2211,  2061,   242,  1381,  1237,  1238,   793,   420,   796,
     238,  1099,   343,  1100,   239,  1101,   456,   362,  1382,  2149,
    2773,  2774,   370,  2317,   704,  1038,   461,   404,  2804,   849,
     850,   851,   852,    36,    37,   214,  1160,  1161,   216,   387,
    -357,  1231,   405,  1045,  1232,  2718,  2245,  1233,  1234,  1102,
    1103,  1104,  1137,  1138,   407,  2719,   412,  2676,   739,   414,
     417,   422,  2677,   217,   424,   425,   888,   435,   889,  2324,
    2325,  2326,  2327,  2328,  2329,  2330,  1953,  1954,   510,   548,
    2246,  2258, -1639, -1639, -1639, -1639,   375,  1046,  -357,  1383,
     437,  2202,   569, -1957,   577,   401,   602,  1047,   674,   531,
    1105,   675,  1106,   687,  2340,  2340,   689,   700,   701,  1107,
     702,   465,  1108,   217,   709,   724,  1087,   521,   719,   738,
     753, -1957,  2716,  1771, -1957,  1773,  2717,   215,  1775,  1776,
    1777, -1957, -1957,  2230,  1781,   740,  2231,   757,   531,  2347,
     756,   760,   764,  2238,  2271,  1793,  1794,   780,   802,   531,
     765,  2272, -1848,  2273, -1848,   766,   773,  2277,  2278,   774,
     775,  1384,   785,   776,  1385,  1386,   778,   786,  2285,   788,
    2251,   806,   216,  1202,  1203,  1204,  1205,   808,   789, -1957,
    2176,  1492,  1048,   467,  1494,  2254,  1614,   822,   828,  2799,
    1497,   836,   834,   857,  1501,   625,   838,  2295,   853,   902,
    1503,  1109,   532,  1110,   532,  2298,   859,   906,  2300,   531,
     919,   923,   925,  2340,   531,   927,  1542,   922, -1642,  1543,
     628,   936,   937,  2331,   938,   939,  1544,  1545,   944,   470,
    2342,  2184,  2186,  2188,  2190,   946,   953,  2349,  2332,   949,
     962,   955,   628,   972,   974,   988,   984,   217,  1004, -1957,
     991,  1006,   998,  2286,  1049,  1002,  1009,  1010,  1444,  1387,
    1011,  1445,  1013,  1018,  1446,  2805,  1023,  1036,  1025,  2806,
    2807,  2426,  1447,   729,  1546,  1042,  1040,  1043,  1056,  1057,
   -1957,   475,  1081,  1058,  1072,  1084,  1082,  1119,   718,  1125,
    2459,  2460,  1133,  1134,  2461,  1144,  1152,  1158,  1165,  1167,
    1050,   504, -1957,  1178,   841,   840,  1217,  1220,  1228,  1241,
    2381,  2415,  1259,  2808,  1235,  1236,   912,  1268,  2378,  1448,
    1287,   602,  1291,  1283,  1292,  1086,  1299,  2419,  2809,  2810,
     427,  1330,  1318,  1325,  2386,  1331,  2691,  1332,  1449,  1337,
    1237,  1238,  1336, -1848,  1547,  1338,  1340,  2404,  1938,  1365,
    1367,  2356,  1051,  1369,  1376,  1150, -1957,  1393,  2363,  2364,
    2365,  2366,  1396,  1089,  1397,  1401,  2369,  2459,  1412,  1437,
    1099,  1438,  1100,  1414,  1419,  1548,  1420,  1478,  1479,  1475,
    2473,  1480,  1474,  1493,  1495,  2254,  1481,  1498,  1499,  1500,
    1517, -1957,  2463,  1524,  1502,  1504,  1515,  1549,  2382,  2433,
    1182,  1111,  1521,  1525,  1558,  1560,   437,  1565,  1183,  1566,
    1572, -1957,  1573,  1614,  1576,  1254,  1579,  1580,  1593,  1616,
    1450,  1606,  1618,  1619, -1848,  1631,  1632,  2347,  1451,  1150,
    1636,  1659,  1669,  1670,  1671,  2481,  1672,  1681,  1685,  1686,
    1687,  1698,  1452,  1708,  1711,  1719,  1713,  1174,  2549,  1689,
     443,  1550,  1112,   444,  1729,  2489,  2347,  2426,  1736,  1744,
    1751,  1758,  1763,  1506,  1113,  1759,  1848,  1507, -1664,  1856,
    1838,  1844,  1857, -1957,  1453, -1659,  2442,  1860,   510,  1862,
    1865,  1623,  1868,  2426,  1622,  1881,  1551, -1957,  1900,  2462,
    1905,  1907,  1923,  1927,  1454,   531,  1455,  1909,  1150,  1996,
    1998,  2002,  1928,  2467,  2012,  1924,  1552,  2056, -1957,   510,
    2106, -1614,  2057, -1662,  2062,  2108,  1614,  2735,  1456,  1457,
    2114,  1849,  1850,  2120,  2132,  2131,  2547,  2133,   445,  2134,
    1895,  1896,  2152,  2156,  2158,  1912,  2173,  1911,   531,  2191,
    2193,  2206,  2652,  1956,  2198,   447,  2684,  2214,  2217,   381,
    2237,  1458,  2212,  2247,  2249,  2248,  2536,  2085,  2781,  2274,
    2736,  2275,  2737,  2279,  2292,  2293,  1150,  2294,  1553,  2297,
    2305,  2307,  2309,  2312, -1957,  2350,  2347,  2314,  2374,  2376,
    1459,  1460,  1554,  2306,  2371,  1195,  2405,  2377,  2384,  2385,
    2422,  2417,  1281,  2738,  2425,  2472,  2471,  2483,  2520,  1295,
    2420,  2480,  2457,  1555,  2478,   602,  1461,  2465,  2426,  2495,
    2477,  2642,  2487,  1462,  2521,  2739,  2479,  2509,  2508,  2390,
    2531,  2516,  2519,  2550,  2554,  2518,  2533,  1463,  2618,  2556,
    2561,  1464,  2619,  2621,  1280,  2667,  2630,  1960,  2568,  2631,
     448,   449,   450,  2740,  2643,  2648,  2646,  2644,  1465,   451,
    2645,  2732,  2771,  2077,  2777,  2819,  2820,  2779,  2844,  2847,
     588,   452,  2853, -1735,  2577,  2612,  2848,    16,    81,  1556,
      85,   521,  1411,    77,    86,  1466,    76,   399,   708,  1739,
     419,  2675,  1725,  1405,  1467,  1059,  2372,  1392,  2623,  2624,
    2535,  2362,  2700,  2490,   453,  2368,  2373,  2488,  1034,  2634,
     454,  2355,   455,  1017,  2639,   456,  1976,   457,   458,   459,
    2604,  2842,  2548,   460,  1468,   461,  2311,  2552,  2614,  1964,
     462,   531,  2692,  2682,  1469,   531,  2840,  2304,   521,  2741,
    1470,  2846,  2824,  2637,  2638,  2651,  2476,  2701,   531,  2702,
     432,   510,  1829,   192,  1436,  2668,  2742,  1757,  1519,   805,
    1523,  1827,  2269,  2267,   463,   845,   814,  2778,  1210,  1541,
    2678,  1171,  2680,  1218,  1833,   903,  2681,  1240,  2743,  2113,
    1569,  1854,  2124,   464,  1581,  2123,  1252,   926,  1864,   929,
     531,  1871,  1620,  1680,  1263,  1884,  1647,  1290,   964,  2744,
    1894,   645,  1678,  2155,  2784,   963,  1679,  2567,   987,  1614,
     465,  1786,  1328,  1749,  2821,  1746,  1194,  1192,  2823,  2745,
    1788,  2161,  2148,  2146,  2387,  2167,  1689,  2165,  2746,  1917,
    1533,  1887,  1531,  1224,  2721,  2722,  1888,  1889,   794,   794,
     466,   952,   531,   531,   531,  1890,  1169,  1062,  1375,  2729,
    2216,  2110,  2019,  1637,  2733,  2734,  1795,   813,   779,   794,
    2640,  1730,  1728,  2316,  2470,  2367,  2530,  2020,  2794,  2122,
    2780,  2839,  2839,  2782,  2783,  1867,  2855,  2021,   794,  1400,
    1661,  1843,   467,  2685,  2051,  2529,   502,   502,     0,  1704,
    2813,     0,     0,     0,   468,   469,     0,     0,  1282,     0,
       0,  2852,   531,     0,     0,   794,  2797,   502,     0,     0,
       0,     0,     0,  1594,     0,     0,     0,     0,    90,  1694,
      91,  1296,    92,     0,     0,     0,   502,    93,   470,     0,
       0,     0,  1297,     0,     0,    94,   499,   499,   471,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   502,     0,     0,     0,   499,     0,   472,
       0,     0,     0,     0,   473,     0,     0,     0,    95,    96,
       0,     0,   474,     0,   437,     0,   499,     0,     0,    97,
     475,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      98,     0,     0,    99,     0,     0,     0,   445,     0,     0,
       0,     0,     0,   499,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   447,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,   102,     0,
     103,     0,     0,     0,     0,     0,     0,     0,  -738,  -738,
    -738,  -738,  -738,  -738,  -738,  -738,  -738,  -738,     0,  -738,
    -738,  -738,     0,  -738,  -738,  -738,  -738,  -738,  -738,  -738,
    -738,  -738,   104,  2022,     0,     0,     0,  -738,     0,     0,
     718,     0,  -738,   105,     0,  -738,     0,     0,   106,     0,
       0,     0,   503,   503,     0,     0,   531,     0,     0,     0,
       0,     0,     0,     0,     0,  2023,     0,     0,     0,   448,
     449,   450,     0,   503,     0,  -241,   107,     0,   451,     0,
       0,     0,     0,   108,     0,     0,   109,   110,     0,     0,
       0,     0,   503,     0,     0,     0,     0,   111,     0,     0,
       0,   531,     0,   531,   112,     0,   113,     0,     0,   114,
       0,     0,     0,  -738,     0,     0,     0,     0,     0,   503,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2024,     0,     0,     0,   810,  2025,   457,   458,   459,     0,
       0,     0,   460,     0,     0,     0,     0,   531,     0,   531,
       0,   115,     0,     0,     0,   116,     0,   117,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,  2026,
       0,     0,     0,     0,  -738,  -738,  -738,  2027,  -738,  -738,
    -738,  -738,     0,   463,     0,     0,     0,     0,     0,  2028,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,   531,     0,     0,     0,     0,     0,     0,  1595,
       0,     0,     0,  2029,   811,   588,     0,     0,     0,     0,
       0,     0,  2030,     0,     0,     0,   121,   122,     0,     0,
       0,     0,     0,  -238,     0,     0,     0,   123,   718,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   466,
     124,   125,     0,     0,     0,     0,     0,   126,     0,     0,
       0,   127,     0,  2031,     0,     0,  2032,     0,     0,     0,
     128,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     129,     0,     0,     0,     0,   588,   588,   588,   588,   588,
    -738,   130,     0,   588,   588,   588,     0,   588,     0,     0,
     131,     0,     0,   468,   469,   132,   133,     0,     0,   134,
       0,   135,     0,     0,     0,     0,    90,     0,    91,   136,
      92,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,     0,  -738,    94,     0,     0,     0,     0,     0,     0,
       0,     0,  -738,   588,     0,     0,     0,   471,     0,     0,
     138,     0,   588,   588,   588,   588,   531,   139,   531,     0,
       0,     0,   140,     0,     0,     0,    95,    96,   472,     0,
       0,     0,     0,   473,     0,     0,     0,    97,     0,     0,
       0,   474,     0,   437,  -738,     0,     0,     0,    98,     0,
     141,    99,   718,     0,     0,     0,   531,     0,     0,     0,
       0,     0,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,   531,   102,     0,   103,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2177,     0,     0,     0,     0,     0,     0,     0,     0,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,    90,     0,    91,     0,    92,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
       0,     0,    94,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,     0,   112,     0,   113,    95,    96,   114,     0,     0,
       0,     0,     0,     0,     0,     0,    97,     0,     0,   443,
       0,     0,   444,     0,     0,     0,     0,    98,     0,     0,
      99,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,     0,     0,     0,     0,   115,
       0,     0,     0,   116,     0,   117,     0,     0,     0,     0,
       0,     0,     0,   588,     0,   118,     0,   101,     0,     0,
       0,     0,     0,     0,     0,   102,     0,   103,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,   445,     0,   531,
       0,   531,     0,     0,     0,     0,   120,     0,     0,   104,
       0,     0,     0,     0,   447,     0,     0,     0,     0,     0,
     105,     0,  2284,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,   121,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,   124,   125,
     108,     0,     0,   109,   110,   126,     0,     0,     0,   127,
       0,     0,     0,     0,   111,     0,     0,     0,   128,     0,
       0,   112,     0,   113,     0,     0,   114,     0,   129,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   130,
       0,     0,     0,     0,     0,     0,     0,   718,   131,   448,
     449,   450,     0,   132,   133,     0,     0,   134,   451,   135,
       0,     0,     0,  2354,  2354,     0,  1584,   136,   115,     0,
     452,     0,   116,     0,   117,     0,     0,     0,     0,     0,
     137,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   138,     0,
       0,     0,     0,   453,     0,   139,     0,     0,     0,   454,
     140,   455,   119,     0,   536,  1585,   457,   458,   459,     0,
       0,     0,   460,     0,   461,   120,     0,     0,     0,   462,
       0,     0,     0,     0,     0,     0,     0,     0,   141,     0,
       0,   445,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,   122,     0,     0,     0,   447,     0,
       0,     0,     0,   463,   123,     0,     0,     0,   537,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   125,   718,
       0,     0,   464,     0,   126,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,   718,   128,   718,   718,
       0,     0,   718,     0,     0,     0,     0,   129,     0,   465,
       0,     0,     0,     0,     0,   510,     0,     0,   130,     0,
       0,     0,     0,     0,     0,     0,     0,   131,     0,     0,
       0,     0,   132,   133,     0,     0,   134,     0,   135,   466,
      90,     0,    91,     0,    92,     0,   136,     0,     0,    93,
       0,     0,     0,   448,   449,   450,     0,    94,     0,   997,
     718,   718,   451,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   452,   718,     0,   138,  2493,  2493,
       0,   467,     0,     0,   139,     0,  2493,  2493,  2500,   140,
      95,    96,     0,   468,   469,     0,     0,     0,     0,     0,
     510,    97,     0,  1586,     0,     0,     0,     0,     0,     0,
       0,     0,    98,     0,     0,    99,     0,   141,   456,     0,
     457,   458,   459,     0,     0,     0,   460,   470,   461,   100,
       0,     0,     0,     0,     0,   718,     0,   471,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   510,     0,     0,
       0,     0,   101,     0,     0,     0,   718,     0,   472,   718,
     102,     0,   103,   473,   718,   718,     0,   463,     0,     0,
       0,   474,     0,   437,   510,     0,     0,     0,     0,   475,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   718,     0,     0,   104,     0,     0,     0,     0,  2622,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
     106,     0,     0,   465,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,   466,     0,   108,     0,     0,   109,   110,
     718,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,     0,   112,   588,   113,     0,
       0,   114,   588,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   718,   467,     0,     0,     0,     0,
       0,     0,     0,     0,   445,     0,     0,   468,   469,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   447,     0,   115,     0,     0,   718,   116,     0,   117,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,   470,   588,     0,     0,     0,   588,     0,     0,  1587,
       0,   471,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,   472,     0,     0,     0,     0,   473,     0,     0,
     120,     0,     0,     0,     0,   474,     0,   437,     0,     0,
       0,     0,     0,   475,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   121,   122,
       0,     0,     0,     0,     0,   600,   448,   449,   450,   123,
       0,     0,     0,     0,     0,   451,     0,     0,     0,     0,
       0,     0,   124,   125,     0,     0,     0,     0,     0,   126,
       0,     0,     0,   127,     0,     0,   244,     0,   245,     0,
       0,     0,   128,   246,     0,     0,     0,     0,     0,     0,
       0,   247,   129,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   130,     0,     0,     0,     0,     0,     0,
       0,   601,   131,   457,   458,   459,     0,   132,   133,   460,
       0,   134,     0,   135,   248,   249,     0,     0,     0,     0,
       0,   136,     0,     0,     0,   250,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   251,     0,     0,   252,
       0,   443,     0,     0,   444,     0,     0,     0,     0,     0,
     463,     0,   138,   253,     0,     0,     0,     0,     0,   139,
       0,     0,     0,     0,   140,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   254,     0,     0,     0,
       0,     0,     0,     0,   255,     0,   256,     0,     0,     0,
       0,     0,   141,     0,   257,     0,   258,   259,   260,   261,
     262,   263,   264,   265,     0,   266,   267,   268,     0,   269,
     270,   271,   272,   273,   274,   275,   276,   277,   278,   445,
       0,     0,     0,     0,     0,     0,   466,     0,     0,   279,
       0,   446,     0,     0,   280,     0,   447,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   281,     0,     0,     0,     0,     0,   443,   282,
       0,   444,   283,   284,   867,   868,   869,     0,     0,     0,
     468,   469,   870,   285,     0,     0,     0,     0,     0,     0,
     286,     0,   287, -1957,     0,   288,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   871,   471,     0,     0,     0,     0,     0,
       0,   448,   449,   450,     0,     0, -1183,   289,     0,     0,
     451,   290,     0,   291,     0,   472,   445,     0,     0,     0,
     473,     0,   452,   292,     0, -1183,     0,     0,   474,   602,
     437,     0,     0,   447,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   293,     0,     0,     0,   453,     0,     0,     0,     0,
       0,   454,     0,   455,   294,     0,   456,     0,   457,   458,
     459,     0,     0,     0,   460,     0,   461,     0,     0,     0,
       0,   462,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   295,     0,   872,     0,     0,     0,     0,     0,
       0,     0,     0,   296,   873,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   463,     0,   297,     0,     0,
       0,     0,     0,   298,     0,   444,     0,   299,   448,   449,
     450,     0,     0,     0,   464,     0,   300,   451,     0,     0,
       0,     0,     0,   874,   875,     0,   301,     0,     0,   452,
       0,     0,     0,     0,     0,     0,     0,   302,     0,     0,
       0,   465,     0,     0,     0,     0,   303,     0,     0,  1223,
       0,   304,   305,     0,     0,   306,     0,   307,     0,     0,
       0,     0,   453,   876,   877,   308,     0,     0,   454,     0,
     455,   466,     0,   456,     0,   457,   458,   459,   309,     0,
     445,   460,     0,   461,     0,     0,     0,     0,   462,     0,
       0,     0,     0,     0,     0,     0,   310,   447,     0,     0,
       0,   878,     0,   311,     0,     0,     0,   879,   312,     0,
       0,     0,   880,   467,     0,     0,     0,     0,     0,     0,
     881,     0,   463,     0,     0,   468,   469,   882,     0,     0,
       0,     0,   883,   445,     0,     0,   313,     0,     0,     0,
       0,   464,     0,     0,     0,     0,     0,     0,     0,   443,
     447,   884,   444,     0,     0,   867,   868,   869,     0,   470,
       0,     0,     0,   870,     0,     0,     0,     0,   465,   471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     472,     0,   448,   449,   450,   473,     0,     0,   466,     0,
       0,   451,     0,   474,   871,   437,     0,     0,     0,     0,
       0,   475,     0,   452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   445,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     467,     0,     0,     0,   447,   448,   449,   450,     0,     0,
       0,     0,   468,   469,   451,     0,     0,   456,     0,   457,
     458,   459,     0,     0,     0,   460,   452,   461,     0,     0,
       0,     0,     0,     0,   885,     0,   886,     0,   887,     0,
       0,   888,     0,   889,   890,   891,   470,     0,   892,   893,
       0,     0,     0,     0,     0,     0,   471,     0,     0,     0,
       0,     0,     0,     0,     0,   872,   463,     0,     0,     0,
     456,     0,   457,   458,   459,   873,     0,   472,   460,     0,
     461,     0,   473,     0,     0,     0,     0,     0,     0,     0,
     474,     0,   437,     0,     0,     0,     0,     0,   475,   448,
     449,   450,     0,     0,     0,     0,     0,     0,   451,     0,
       0,     0,   465,     0,   874,   875,     0,     0,     0,   463,
     452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   466,     0,     0,     0,   443,     0,     0,   444,
       0,     0,     0,   453,   876,   877,     0,     0,     0,   454,
       0,   455,     0,     0,   456,   465,   457,   458,   459,     0,
    -973,     0,   460,     0,   461,  -973,     0,     0,  -973,   462,
       0,     0,     0,     0,   467,  -973,  -973,     0,     0,     0,
       0,     0,   878,     0,     0,   466,   468,   469,   879,     0,
       0,     0,     0,   880,     0,     0,  -973,     0,  -973,     0,
       0,   881,     0,   463,     0,     0,     0,     0,   882,     0,
       0,     0,     0,   883,   445,     0,     0,     0,     0,     0,
     470,     0,   464,  -973,     0,  -905,     0,   467,  -905,     0,
     471,   447,   884,     0,     0,     0,     0,     0,     0,   468,
     469,     0,     0,     0,     0,     0,     0,     0,     0,   465,
       0,   472,     0,     0,     0,     0,   473,     0,     0,     0,
       0,     0,     0,     0,   474,     0,   437,     0,   427,     0,
       0,     0,   475,   470,     0,     0,     0,     0,     0,   466,
       0,     0,     0,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -973,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -905,   472,     0,     0,     0, -1852,   473,
       0,     0,     0,     0,     0,     0,     0,   474,     0,   437,
    -905,   467,     0,     0,  -973,   475,   448,   449,   450,     0,
       0,     0,     0,   468,   469,   451,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -973,   452,     0,     0,
       0,     0,     0,     0,     0,   885,     0,   886,     0,   887,
       0,     0,   888,     0,   889,   890,   891,   470,     0,   892,
     893,     0,     0,     0,     0,     0,     0,   471,     0,     0,
     453,     0,     0,     0,     0,     0,   454,  -973,   455,     0,
       0,   456,     0,   457,   458,   459,     0,     0,   472,   460,
    -973,   461,     0,   473,     0,     0,   462,  -973,     0,     0,
       0,   474,     0,   437,     0,  -905,  -905,  -905,     0,   475,
       0,     0,     0,     0,  -905,     0,     0,     0,     0,     0,
       0,   443,     0,     0,   444,  -973,  -905,     0,     0,     0,
     463,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -973,     0,     0,     0,   464,
       0,     0,     0,     0,     0,     0,     0,     0,  -973,  -905,
       0,     0,     0,     0,     0,  -905,     0,  -905,     0,     0,
    -905,     0,  -905,  -905,  -905,   445,   465,     0,  -905,     0,
    -905,     0,     0,     0,     0,  -905,     0,     0,     0,     0,
       0,     0,   447,     0,     0,     0,     0,     0,     0,   445,
       0,     0,     0,     0,     0,     0,   466,  -973,     0,     0,
       0,     0,     0,     0,     0,     0,   447,     0,     0,  -905,
       0,  -973,     0,     0,  -905,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -905,     0,
       0,     0,  -973,     0,     0,     0,     0,     0,   467,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     468,   469,     0,   443,     0,  -905,   444,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1852,     0,     0, -1957,
       0,     0,     0,     0,     0,     0,   600,   448,   449,   450,
       0,     0,     0,     0,   470,  -905,   451,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,  -973,     0,
       0,   448,   449,   450,     0,     0,     0,     0,     0,  -973,
     451,     0,   332,     0,     0,   472,     0,     0,     0,     0,
     473,  -905,   452,     0,     0,     0,     0,  -905,   474,  -973,
     437,   445,     0,     0,     0,     0,   475,     0,     0,  -905,
    -905,     0,   810,   589,   457,   458,   459,     0,   447,     0,
     460,     0,     0,     0,     0,   453,     0,     0,     0,     0,
       0,   454,     0,   455,     0,     0,   456,     0,   457,   458,
     459,     0,     0,  -905,   460,     0,   461,     0,     0,     0,
       0,   462,   443,  -905,     0,   444,     0,     0,     0,     0,
    -905,   463,     0,     0,     0,     0,     0,     0,     0, -1957,
       0,     0,     0,     0,  -905,     0,     0,     0,     0,  -905,
       0,     0, -1852,     0,     0,   463,     0,  -905,     0,  -905,
       0,     0,     0,     0,     0,  -905,     0,     0,     0,     0,
       0,     0,     0,     0,   464,     0,     0,     0,     0,     0,
       0,     0,     0,   448,   449,   450,     0,     0,     0,     0,
       0,     0,   451,     0,     0,     0,     0,     0,     0,     0,
     445,   465,     0,     0,   452,   443,     0,   466,   444,     0,
       0,     0,   653,     0,     0,     0,     0,   447,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     333,   466,     0,     0,     0,     0,     0,   453,     0,     0,
       0,     0,     0,   454,     0,   455,     0,     0,   456,     0,
     457,   458,   459,     0,     0,     0,   460,     0,   461,     0,
       0,   468,   469,   462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   467, -1957,     0,     0,     0,     0,     0,
       0,     0,     0,   445,     0,   468,   469,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   463,     0,     0,
     447,     0,     0,     0,     0,   471,     0,     0,     0,     0,
       0,     0,   448,   449,   450,     0,   464, -1183,     0,   470,
       0,   451,     0,     0,     0,     0,   472,     0,     0,   471,
       0,   473,     0,   452,   443,     0, -1183,   444,     0,   474,
     602,   437,     0,   465,     0,     0,     0,     0,     0,     0,
     472,     0,     0,     0,     0,   473,     0,     0,     0,     0,
       0,     0,     0,   474,   602,   437,   453,     0,     0,     0,
       0,   475,   454,   466,   455,     0,     0,   456,     0,   457,
     458,   459,     0,     0,     0,   460,     0,   461,     0,     0,
       0,     0,   462,     0,     0,   448,   449,   450,     0,     0,
       0,     0,     0,     0,   451,     0,     0,     0,     0,     0,
       0,     0,   445,     0,     0,   467,   452,   443,     0,     0,
     444,     0,     0,     0,     0,     0,   463,   468,   469,   447,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   464,     0,     0,     0,   453,
       0,     0,     0,     0,     0,   454,     0,   455,     0,     0,
     456,   470,   457,   458,   459,     0,     0,     0,   460,     0,
     461,   471,   465,     0,     0,   462,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   472,     0,     0,   445,     0,   473,     0,     0,
       0,     0,   466,     0,     0,   474,     0,   437,     0,   463,
       0,     0,   447,   475,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   448,   449,   450,     0,   464,     0,
       0,     0,     0,   451,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   467,   452,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   465,   468,   469,     0,   443,
       0,     0,   444,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   453,     0,
       0,     0,     0,     0,   454,   466,   455,     0,     0,   456,
     470,   457,   458,   459,     0,     0,     0,   460,     0,   461,
     471,     0,     0,     0,   462,     0,     0,   448,   449,   450,
       0,     0,     0,     0,     0,     0,   451,     0,     0,     0,
       0,   472,     0,     0,     0,     0,   473,   467,   452,     0,
       0,     0,     0,     0,   474,     0,   437,   445,   463,   468,
     469,     0,   475,   537,   986,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   447,     0,     0,   464,     0,   782,
       0,   453,     0,     0,     0,     0,     0,   454,     0,   455,
       0,     0,   456,   470,   457,   458,   459,     0,     0,     0,
     460,     0,   461,   471,   465,     0,     0,   462,   443,     0,
       0,   444,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   472,     0,     0,     0,     0,   473,
       0,     0,   443,     0,   466,   444,     0,   474,     0,   437,
       0,   463,     0,     0,     0,   475,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     464,     0,     0,     0,     0,     0,     0,     0,     0,   448,
     449,   450,     0,   993,     0,     0,   467,     0,   451,     0,
       0,     0,     0,     0,     0,     0,   445,   465,   468,   469,
     452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   447,     0,     0,     0,     0,     0,     0,
     445,     0,     0,     0,     0,     0,     0,   466,     0,     0,
       0,     0,   470,   453,     0,     0,     0,   447,     0,   454,
       0,   455,   471,     0,   456,     0,   457,   458,   459,     0,
       0,     0,   460,     0,   461,     0,     0,     0,     0,   462,
       0,     0,  1147,   472,     0,   444,     0,     0,   473,   467,
       0,     0,     0,     0,     0,     0,   474,     0,   437,     0,
       0,   468,   469,     0,   475,     0,     0,     0,     0,     0,
       0,     0,     0,   463,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   448,   449,
     450,     0,   464,     0,     0,   470,     0,   451,     0,     0,
       0,     0,     0,     0,     0,   471,     0,     0,     0,   452,
       0,     0,   448,   449,   450,     0,     0,     0,     0,   465,
     445,   451,     0,     0,     0,     0,   472,     0,     0,     0,
       0,   473,     0,   452,     0,     0,     0,   447,     0,   474,
       0,   437,   453,     0,     0,     0,     0,   475,   454,   466,
     455,     0,     0,   456,     0,   457,   458,   459,     0,     0,
       0,   460,     0,   461,     0,     0,   453,     0,   462,     0,
       0,     0,   454,     0,   455,     0,     0,   456,     0,   457,
     458,   459,     0,     0,     0,   460,     0,   461,     0,     0,
       0,   467,   462,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   463,   468,   469,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   464,     0,     0,     0,     0,   463,     0,     0,     0,
       0,     0,   448,   449,   450,     0,     0,   470,     0,     0,
       0,   451,     0,     0,     0,   464,     0,   471,   465,     0,
       0,     0,     0,   452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   472,     0,
       0,     0,   465,   473,     0,     0,     0,     0,   466,     0,
       0,   474,     0,   437,     0,     0,   453,     0,     0,   475,
       0,     0,   454,     0,   455,     0,     0,   456,     0,   457,
     458,   459,   466,  1243,     0,   460,     0,   461,     0,     0,
       0,     0,   462,     0,  1664,     0,     0,     0,     0,     0,
     467,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   468,   469,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   467,     0,   463,     0,     0,     0,
       0,     0,  1244,     0,     0,     0,   468,   469,     0,     0,
       0,     0,     0,     0,     0,   464,   470,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   471,     0, -1127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     470,     0,   465,     0,     0, -1127,     0,   472,     0,     0,
     471,     0,   473,     0,     0,     0,  2474,     0,     0,     0,
     474,     0,   437,     0,     0,     0,     0,     0,   475,     0,
       0,   472,   466,     0,     0,     0,   473,     0,     0,     0,
       0,     0,     0,     0,   474,     0,   437,  -377,     0,     0,
    -377,     0,   475,  -377,  -377,  -377,  -377,  -377,  -377,  -377,
    -377,  -377,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   467,     0,     0,     0,     0,     0,
       0,  -377,     0,  -377,     0,  1245,   468,   469,     0,     0,
    -377,     0,  -377,  -377,  -377,  -377,  -377,  -377,  -377,     0,
   -1127, -1127, -1127,     0,     0,     0,     0,     0,     0, -1127,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     470, -1127,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,     0,     0,     0,     0,  -377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1246,   472,     0,     0,     0,     0,   473,     0,     0,     0,
       0,     0,     0,     0,   474, -1127,   437, -1127, -1127, -1127,
       0,     0,   475, -1127,     0, -1127,     0,     0,  -377,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1020,     0,     0,     0,  -377,  -377,
    -377,  -377,  -377,     0,     0,  -377,  -377,     0,     0,  -377,
       0,     0,     0,     0, -1127,  -377,     0,  -377,     0,     0,
       0,     0,     0,  -377,     0,     0,     0,     0,  -377,     0,
       0,  -377,     0,     0,     0,     0,     0,     0,     0,  -377,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -377,     0,     0,  -377,     0,     0,     0,     0,
   -1127,  -377,     0,  -377,     0,     0,     0,     0,     0,     0,
       0,     0,  -377,     0,     0,     0,     0,     0,     0,  1019,
       0,     0,     0,     0,     0,  -377,     0,     0,     0,     0,
   -1127,     0,     0,     0,     0,     0,     0,  -377,     0,  -377,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -377,     0,     0,  -377,  -377,  -377,
    -377,  -377,  -377,  -377,     0,     0,     0,  -377,     0,     0,
       0,     0, -1127,     0,     0,     0,     0,     0,     0,     0,
    -377,  -377,     0,     0, -1127, -1127,     0,     0,     0,  -377,
       0,     0,  -377,  -377,     0,  -377,  -377,  -377,  -377,  -377,
    -377,  -377,     0,     0,     0,     0,     0,  -377,     0,  -377,
       0,     0,     0,     0,     0,     0,     0,     0, -1127,     0,
       0,     0,     0,     0,     0,     0,  1247,     0, -1127,     0,
       0,     0,     0,     0,     0,  -377,     0,     0,     0,  -377,
       0,     0,  -377,     0,     0,     0,     0,     0,     0, -1127,
       0,     0,     0,     0, -1127,     0,     0,     0,     0,     0,
    -377,     0, -1127,     0, -1127,     0,     0,     0,     0,     0,
   -1127,  -377,     0,  -377,  -377,  -377,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -377,     0,     0,     0,     0,  1020,     0,     0,
       0,  -377,  -377,  -377,  -377,  -377,     0,     0,  -377,  -377,
       0,     0,     0,     0,     0,  -377,     0,     0,     0,     0,
    -377,     0,     0,     0,     0,  -377,  -377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -377,     0,     0,
       0,     0,  -377,  -377,     0,     0,     0,  -377,  -377,  -377,
       0,     0,     0,     0,     0,  -377,     0,     0,  -377,  1930,
       0,  -377,     0,     0,  -377,     0,  -377,     0,  -377,     0,
       0,     0,     0,     0,  1931,  1013,     0,  1932,  1933,  1934,
    1935,  1936,  1937,  1938,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -377,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1939,     0,  1940,  1941,  1942,  1943,  1944,
    1945,  1946,     0,     0,     0,     0,     0,     0,     0,     0,
    -377,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -377,     0,     0,     0,     0,     0,
       0,     0,  -377,     0,     0,  -377,     0,     0,     0,  1947,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -377,     0,     0,     0,     0,     0,     0,  1087,     0, -1957,
       0,     0, -1957,  -377,     0, -1957,     0,     0,     0,     0,
       0,     0,     0, -1957,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1848,     0, -1848,     0,     0,     0,     0,
       0,  1948,  1949,  1950,  1951,  1952,     0,     0,  1953,  1954,
       0,     0,     0,     0,  -377,     0,  -377,  -377,  -377,     0,
   -1957,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -1957,
       0,     0,  1955,     0,  -377,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   407,     0,     0,  1956,     0,
       0,     0,     0,  -377, -1933,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -377,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -377,  -377,  -377,     0,     0,     0,     0,     0,     0,     0,
    1957,     0,     0,     0,  -377,     0,     0,     0,     0,     0,
       0,  -377,     0,     0,     0,     0,     0,     0,  1013,     0,
       0, -1957,     0,     0,     0,     0,     0,     0,  1088, -1957,
    1958,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1957,  1959,     0,     0,     0,     0,     0,
       0,     0,  1960,     0,     0,  1961,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1962,     0,     0,     0,     0, -1957,     0,     0,     0,     0,
       0,     0,     0,  1963, -1848,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -1957,     0, -1957,     0,     0,
       0,     0,     0,     0,  1089,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -1957,
   -1957,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1964,     0,  1965,  1966,  1967,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -1957,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1968, -1848,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -1957, -1957,  -374,   445,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1933,   447,     0,     0,     0,     0,     0, -1957,     0,     0,
    1969,  1970,  1971,     0, -1957,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1972,     0,     0,     0, -1957,     0,
       0,  1689, -1957,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2579,     0,     0,  2580,     0, -1957,
    2581,  1932,  1933,  1934,  1935,  1936,  1937,  2582,  2583,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1957,     0,  1437,     0,
    1438,     0,     0,     0,     0, -1957,     0,  1939,     0,  1940,
    1941,  1942,  1943,  1944,  1945,  1946,   448,   449,   450,  1932,
    1933,  1934,  1935,  1936,  1937,   451,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -1957,     0,   452,     0,     0,
       0,     0,     0,     0,     0, -1957,     0,     0,     0,     0,
       0, -1957,     0,  1947,     0,  1939,     0,  1940,  1941,  1942,
    1943,  1944,  1945,  1946,     0,     0,   602,     0,     0,     0,
     489,     0,     0,     0,     0,     0,   454,     0,   455,     0,
       0,   456,     0,   457,   458,   459,     0,     0,     0,   460,
       0,   461,     0,     0,     0,  2584,     0,     0,     0,     0,
       0,  1947,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1948,  1949,  1950,  1951,  1952,
       0,     0,  1953,  1954,     0,     0,  2585,     0,     0,     0,
     463,     0,  2586,     0,  2587,     0,     0,     0,     0,     0,
   -1883,     0,     0,     0,     0,  2588,     0,     0,  2589,   464,
       0,     0,     0,     0,     0,     0,  1955,     0,     0,     0,
       0,     0,   445,  1948,  1949,  1950,  1951,  1952,     0,   407,
    1953,  1954,  1956,     0,     0,     0,   465,     0,     0,   447,
    2590,     0,     0,     0,     0,     0,     0,     0,     0,  2591,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2592,     0,  1955,     0,   466,     0,     0,     0,
       0,     0,     0,     0,  1957,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2593,     0,     0,     0,   467,     0,
       0,     0,     0,     0,     0,     0,     0,  2594,  1959,     0,
     468,   469,  1957,     0,     0,     0,  1960,     0,     0,  1961,
     445,     0,     0,     0,   448,   449,   450,     0,     0,     0,
       0,     0,  1131,   451,  1962,     0,  2595,   447,     0,     0,
       0,     0,   490,     0,   470,   452,   491,   492,     0,     0,
       0,     0,     0,     0,   471,     0,  1959,     0,     0,     0,
       0,     0,  2596,     0,     0,     0,     0,  1961,     0,  2597,
       0,     0,     0,     0,     0,   472,     0,     0,   489,     0,
     473,     0,  1962,     0,   454,     0,   455,  2598,   474,   456,
     437,   457,   458,   459,     0,     0,   475,   460,  1964,   461,
    1965,  1966,  1967,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   445,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   447,   448,   449,   450,     0,     0,     0,   463,     0,
       0,   451,  2599,     0,     0,     0,     0,  -635,  1965,  1966,
    1967,     0,  2600,   452,     0,     0,     0,   464,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2601,     0,     0,     0,  1969,  1970,  1971,     0,     0,     0,
       0,     0,     0,     0,   465,     0,   489,     0,  1972,     0,
       0,     0,   454,  2602,   455,  1689,     0,   456,     0,   457,
     458,   459,     0,     0,     0,   460,     0,   461,     0,     0,
       0,     0,     0,     0,   466,     0,   445,     0,     0,     0,
       0,     0,  1969,  1970,  1971,     0,   448,   449,   450,     0,
       0,     0,     0,   447,     0,   451,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   463,   452,     0,     0,
       0,     0,     0,     0,     0,     0,   467,     0,     0,     0,
       0,     0,     0,     0,     0,   464,     0,     0,   468,   469,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     489,     0,     0,     0,     0,     0,   454,     0,   455,     0,
    1767,   456,   465,   457,   458,   459,     0,     0,     0,   460,
     490,   461,   470,     0,   491,   492,     0,     0,     0,     0,
     445,     0,   471,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   466,     0,     0,     0,     0,   447,   448,   449,
     450,     0,     0,   472,     0,     0,     0,   451,   473,     0,
     463,     0,     0,     0,     0,     0,   474,     0,   437,   452,
       0,     0,     0,     0,   475,     0,     0,     0,     0,   464,
       0,     0,     0,     0,   467,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   468,   469,     0,     0,
       0,     0,   489,     0,  1770,     0,   465,     0,   454,     0,
     455,     0,     0,   456,     0,   457,   458,   459,     0,     0,
       0,   460,     0,   461,     0,     0,     0,     0,   490,     0,
     470,     0,   491,   492,     0,     0,   466,     0,   445,     0,
     471,     0,   448,   449,   450,     0,     0,     0,     0,     0,
       0,   451,     0,     0,     0,   447,     0,     0,     0,     0,
       0,   472,   463,   452,     0,     0,   473,     0,     0,     0,
       0,     0,     0,     0,   474,     0,   437,     0,   467,     0,
       0,   464,   475,     0,     0,     0,     0,     0,     0,     0,
     468,   469,     0,     0,     0,     0,   489,     0,  1772,     0,
       0,     0,   454,     0,   455,     0,     0,   456,   465,   457,
     458,   459,     0,     0,     0,   460,     0,   461,     0,     0,
       0,     0,   490,     0,   470,     0,   491,   492,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,   466,     0,
       0,     0,     0,     0,     0,     0,     0,   445,     0,     0,
     448,   449,   450,     0,     0,   472,   463,     0,     0,   451,
     473,     0,     0,     0,   447,     0,     0,     0,   474,     0,
     437,   452,     0,     0,     0,   464,   475,     0,     0,     0,
     467,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   468,   469,     0,     0,     0,     0,     0,     0,
       0,     0,   465,     0,   489,     0,     0,     0,     0,     0,
     454,     0,   455,     0,  1791,   456,     0,   457,   458,   459,
       0,     0,     0,   460,   490,   461,   470,     0,   491,   492,
       0,     0,   466,     0,     0,     0,   471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   472,     0,   448,
     449,   450,   473,     0,   463,     0,     0,   445,   451,     0,
     474,     0,   437,     0,   467,     0,     0,     0,   475,     0,
     452,     0,     0,   464,   447,     0,   468,   469,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     465,     0,     0,   489,     0,     0,     0,     0,   490,   454,
     470,   455,   491,   492,   456,     0,   457,   458,   459,     0,
     471,     0,   460,     0,   461,     0,     0,     0,     0,     0,
     466,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   472,     0,     0,     0,     0,   473,     0,     0,     0,
       0,     0,     0,     0,   474,     0,   437,     0,     0,     0,
       0,     0,   475,   463,     0,     0,     0,     0,     0,   448,
     449,   450,   467,     0,     0,     0,     0,     0,   451,     0,
       0,     0,   464,     0,   468,   469,     0,     0,     0,     0,
     452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   465,
       0,     0,     0,     0,     0,     0,     0,     0,   470,     0,
     491,     0,     0,   489,     0,     0,     0,     0,   471,   454,
       0,   455,     0,     0,   456,  2829,   457,   458,   459,   466,
       0,     0,   460,     0,   461,     0,     0,     0,     0,   472,
       0,     0,     0,     0,   473,     0,     0,     0,     0,     0,
       0,     0,   474,     0,   437,     0,     0,     0,     0,     0,
     475,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   467,     0,   463,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   468,   469,     0,     0,     0,     0,     0,
       0,     0,   464,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   470,     0,   465,
       0,     0,     0,     0,     0,     0,     0,   471,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   472,   466,
       0,     0,     0,   473,     0,     0,     0,     0,     0,     0,
       0,   474,     0,   437,     0,     0,     0,     0,     0,   475,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   467,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   468,   469,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   470,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   471,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   472,     0,
       0,     0,     0,   473,     0,     0,     0,     0,     0,     0,
       0,   474,     0,   437,     0,     0,     0,     0,     0,   475
};

static const yytype_int16 yycheck[] =
{
     211,   212,   199,   408,   215,   410,   324,   444,   413,   364,
     524,   677,   678,   679,   934,   339,   207,    81,    42,   453,
    1491,  1263,   213,  1225,   351,  1689,   535,  1440,   329,   327,
    1443,   325,  1702,   315,  1368,   317,   337,   322,   320,   531,
     334,   639,   324,  1727,   872,  1691,   988,   341,   342,  1695,
     351,   782,   648,   337,  1284,   489,   626,   339,   318,  1285,
     778,  1626,  1004,   345,   862,   531,   360,   351,     1,  1177,
     788,    56,  1329,  2026,  2027,  1590,    22,     1,    17,    31,
    2011,     9,     1,   317,   366,  1311,   368,    58,   322,  2418,
      17,     9,     6,     9,    88,   329,  1764,  1346,     9,  1207,
       9,    21,   125,   337,   416,    65,    48,     6,  1098,   116,
     619,   179,   126,   841,   144,   132,  1106,   351,  1421,    49,
     399,  1310,   588,   125,   321,   856,   323,   162,  2025,   132,
     230,   249,   207,   317,   389,   127,    38,   261,   866,    49,
      27,    58,    74,    31,    46,  2407,   237,   267,  2019,   346,
     237,   348,   644,  1261,     0,   242,   353,   246,   756,    89,
     220,    30,   246,   112,   446,    98,    32,  2296,    58,   324,
     367,   180,   292,    57,   385,    26,    60,   239,   644,   907,
       1,    65,     1,   176,   478,   193,   397,  1909,   207,    89,
     168,   204,   403,   404,   925,   406,  1667,    66,   164,    68,
     411,   412,    98,    99,   932,   416,   361,   303,   490,   491,
     492,  1319,   337,    21,   204,   426,   207,   223,   734,   453,
    2642,   261,     0,   578,   267,   261,   237,   351,   218,   219,
     117,  2209,  2502,   515,  2707,   404,   518,     9,    58,   126,
     174,  2382,   174,   237,   523,   505,   112,  2302,   467,   543,
     192,   290,   422,    39,   261,   489,   490,   491,   492,   174,
     468,  2531,   993,   557,   565,   178,   758,  1209,   388,   267,
     317,     9,  2714,   108,   208,   576,   155,   205,    50,   361,
    2233,   363,   207,   118,   848,   109,  2728,     0,   180,   317,
    2012,   237,   526,   208,   292,   589,   465,   576,   517,   217,
     336,   656,   341,   217,   422,   218,   490,   491,   492,  2782,
     715,   339,   520,    94,   216,   321,   848,   345,   168,    72,
      58,   491,   604,   301,    72,   522,  2467,   761,   294,   115,
    1845,   565,  1847,   324,   233,   636,   533,   350,   830,  2317,
     641,   911,   576,   261,   626,   407,   248,   261,   785,   200,
     261,   394,   636,   271,   648,  2410,   650,   271,  2487,   419,
     350,   655,    72,   141,   372,   357,   385,   145,   270,   648,
    2054,   653,   218,   219,   465,    72,  2798,  2248,   207,   475,
     276,  2651,   203,   130,     6,   540,   461,   821,   257,   273,
     304,   703,   626,   482,   519,   261,   520,   296,   482,   519,
     520,   519,   636,  1231,    49,  1513,   130,   641,   277,   402,
    2672,   511,  1240,    72,   112,    49,   315,  2314,  2349,  1217,
     412,  1152,  1220,   325,   435,   453,   204,  1684,   998,   316,
     433,   466,   364,   520,   943,   465,  2158,   367,   356,  1742,
     218,   219,   356,   181,   491,   217,    72,   343,   465,  2778,
     966,   411,   520,   708,  1182,  1183,   242,   367,     9,  1648,
     279,   489,   490,   491,   492,   349,    72,   367,    72,   338,
     349,  1720,   130,   363,   497,   757,   755,   249,   250,  1721,
     520,   973,   764,   765,   766,   387,  1476,   515,   782,   261,
     701,   773,   774,   775,   776,   497,   778,   782,   709,   271,
    1228,   692,   713,   520,   786,   519,   788,   789,   246,   423,
     428,   520,   428,   520,   428,   323,   798,   799,   800,   801,
     802,   515,   406,   757,   476,   736,   511,   411,   349,   456,
     764,   765,   766,   744,   156,  1105,   815,   479,   470,   773,
     774,   775,   776,   460,   778,   827,   737,   467,   782,   498,
     521,   476,   786,   847,   788,   789,   474,  2488,   478,  1287,
     474,   855,   856,   861,   798,   799,   800,   801,   802,   848,
    2135,   856,   350,   757,   520,   474,   604,   519,   517,   398,
     764,   765,   766,   513,   356,  2307,   488,   520,   476,   773,
     774,   775,   520,   827,   375,   217,   520,  1195,   626,  2552,
     894,   520,   786,   513,  2119,   789,   520,   520,   526,   520,
     526,   520,   526,   513,   798,   799,   800,   801,   802,  1561,
     944,   922,   856,   245,   520,   475,   412,     6,   520,   911,
     499,   925,   519,  1147,  1198,   832,   520,   919,   922,   261,
     925,   520,     6,   827,   465,   905,   465,   476,   927,   271,
     519,   261,   472,   935,   276,   408,   428,   477,  1150,   467,
     408,  1769,   944,   132,   965,  1922,  1198,   414,    34,   450,
     478,    22,   951,   955,   421,   969,   936,   911,     6,   156,
    1250,   168,     6,   995,  1150,   919,   980,   465,   922,  1123,
     414,   925,  2376,    58,     9,   519,   520,   421,   408,   993,
     979,   124,   474,    49,   986,  1433,     6,  1435,   993,   462,
     757,   408,  1632,  2302,   462,  1823,   998,  1825,   257,   472,
      32,  1291,   367,  1035,   477,  2290,   773,   774,   775,   757,
     261,   965,   261,   367,   356,   919,   764,   765,   766,   786,
     385,   292,   520,     1,   482,   773,   774,   775,   776,   408,
     778,   385,   462,   279,   526,  1863,   414,   455,   786,   993,
     788,   789,   460,   421,   998,   462,   977,  1565,  1080,   315,
     798,   799,   800,   801,   802,  1682,   261,  1056,  1506,  1507,
     827,   337,   408,  1501,  1300,  1301,  1302,  1303,  1304,  1305,
    1306,  1307,   162,  1901,  1902,   221,   520,   125,   162,   827,
     179,   261,   408,   462,   408,    63,   428,   172,   359,    66,
     261,    68,  1091,  1024,   301,   241,   261,   261,   127,   315,
     465,  2410,  2546,  1105,   261,   249,  2546,  1558,   297,  1560,
     276,   465,   311,    91,    92,    33,   462,  1119,   217,   367,
    1568,   267,   459,  1125,  1572,  1752,  1753,  1754,  1755,  1756,
     203,  2546,   474,   217,   335,  1762,   462,  1671,   462,   476,
     399,  1295,   520,   228,  1165,   247,   292,  1152,   513,  2546,
    1081,  1105,   398,  2546,   267,  1606,  1067,  1068,  1089,   513,
    1761,   267,   261,   911,  2126,  1119,   237,   233,   364,   217,
     217,   919,   271,   217,  1622,  1623,   124,   261,   520,   292,
    2546,  2571,   217,  1810,   526,  2135,   292,   271,  1168,  2135,
     520,   261,  1819,   315,  1148,  1822,   944,   217,  1152,  1198,
     286,   287,   288,   275,   472,   295,   304,  1523,   276,   477,
     300,  1165,   262,   261,   261,  1119,   261,   261,  1669,  1820,
    1821,  2208,  2209,   271,   271,   465,   261,   271,   427,   261,
     208,   276,   261,    63,  2208,  2209,   271,  2627,  1259,  1260,
    2628,   261,  2296,   519,   520,   173,   465,  1178,  1250,  1251,
     998,   271,   170,  1207,   520,  1259,   257,   356,   517,   237,
     366,    91,    92,   261,   336,   513,  1265,   353,   475,   520,
     261,   520,   356,   236,   470,   175,   349,   526,   422,    57,
     257,  1283,   182,   328,  2728,  1299,   356,    65,  2728,  1291,
    1580,   393,     0,   291,   460,  1843,  1250,  1251,   257,  1511,
     422,  2689,   230,  1317,   520,  1259,  1260,  1261,   356,   356,
     238,   261,   356,  2728,  1848,   520,  1318,  1316,   519,  2281,
    2629,   356,   414,  1325,  1326,   423,   396,  1326,  1327,   428,
    2317,  2728,   223,  2721,  2722,  2728,   356,  1291,   336,   302,
     520,  2729,    27,  2317,   428,  2733,  2734,  2737,  1265,   520,
     328,  1505,  1119,   315,   272,   520,   520,  1105,   428,  2363,
     351,   338,  2728,   520,  1682,  1319,   434,   621,   436,   491,
     516,  1119,  1497,   124,  2762,   474,   466,  2767,    30,   464,
     428,   428,   466,   489,   428,  1675,  1418,   472,   473,   304,
     474,  2775,   477,   428,   480,   481,   324,   447,   399,   485,
     472,   351,    55,  2791,  1335,   477,   519,   237,   428,  1340,
    2794,  1342,   460,   519,  1345,  1346,   460,  1348,   460,   673,
     519,   520,   313,  2427,  2428,   200,   474,   526,   520,   132,
     474,   160,   117,   141,  1882,  1883,   520,   145,    91,   474,
     399,   279,   526,  2070,  2272,  2273,   417,  1895,  1896,   497,
     417,  1362,   465,   417,   474,   108,   526,   109,  2591,   509,
     119,   120,   121,  1911,  1912,   118,   361,  2514,   430,   511,
     520,    47,   520,  1517,     0,   520,   520,   129,   526,   526,
    2613,   526,   526,  1485,  1486,   520,   261,   465,    64,  1400,
    1492,   526,  1494,    86,  2541,   273,   204,   397,   328,  1501,
     520,   349,  1250,  1251,    11,   249,   526,   236,   423,   104,
     218,   219,  1514,   225,  1445,  1517,   127,   169,   426,  1450,
     491,   369,  2526,   361,   491,   363,     1,   491,   104,   424,
     425,  1485,  1486,   236,  1465,  1579,   165,   124,  1492,   168,
    1494,   193,   313,  1291,   417,   257,   237,  1501,  1566,   203,
    2472,   468,    59,  1558,    11,  1560,   208,   472,   466,  1513,
    1514,  2483,   477,   174,   308,   173,    57,    57,  1570,   298,
     261,   349,    47,   302,    65,    65,   347,  1579,  1580,  1813,
     417,  1485,  1486,  1604,   243,   404,  1588,   487,  1492,    64,
    1494,    98,   520,   100,   297,   102,   295,   368,   257,   302,
     261,  1606,    59,   110,  1558,  1619,  1560,   231,  1607,  2800,
    1514,  1542,   512,  1631,   514,   141,  1618,  1548,   491,   145,
     319,  1620,   230,   314,  1555,   316,  1580,  1671,   406,   104,
     238,   292,  2554,   411,  1588,   465,  2084,  2085,  2086,   181,
    2562,    98,   350,   100,   467,   102,  1660,  1661,  1650,   361,
    1604,   465,  1606,   110,   491,   478,  1658,   369,  2620,    12,
     167,   237,    15,    16,  1669,     8,  1597,  1699,   244,  1671,
     299,   181,   301,  1675,  1706,  1829,   330,   331,   204,   255,
     132,   267,   192,   342,   343,   344,  2787,   465,    11,   496,
    1607,   208,   218,   219,    37,   349,  2797,   349,   357,   174,
     242,     8,   205,   245,  1658,   215,   292,   159,   515,   161,
     167,    28,     8,  1760,  1761,  1669,   324,  1764,  1485,  1486,
     237,  1675,   800,   801,   802,  1492,   267,  1494,  1760,  1761,
      37,   234,   467,   208,  1859,   245,    59,  1485,  1486,   145,
     340,    37,   520,   478,  1492,  2385,  1494,  1514,   221,   222,
     404,   292,   352,  1501,  1658,   262,   164,   465,   211,   212,
     168,   192,   237,   314,  1695,   316,  1514,   464,   241,   244,
    1701,   454,   455,  1820,  1821,    98,   473,   100,  1709,   102,
     255,   464,   273,   273,   215,   361,   362,   110,  1820,  1821,
    1534,  2020,   451,   428,   267,  2780,  2781,   432,   251,   252,
     376,   308,   378,  1714,  1848,   262,   764,   765,   766,  1963,
     192,   128,   520,   192,  2446,   462,  2801,   253,   254,   292,
     346,  2453,  2454,   465,   350,   472,   509,  1758,  1759,   463,
     477,   789,  1580,   215,   465,  2820,   215,     1,   472,   462,
    1588,   318,   141,   477,   167,   322,  1848,   165,   124,   472,
     168,   308,   329,   330,   477,   361,  2090,   363,   349,   349,
     337,   404,  2847,   340,   500,   501,   502,   503,   345,  1823,
     347,  1825,   349,   350,   351,   352,    40,    41,    42,    43,
      44,  1925,   465,   299,   462,   301,   361,   362,  2550,   465,
     404,  1658,   369,   400,   472,   177,   257,   463,   259,   477,
     217,   376,   257,   378,   259,   465,   472,  2829,   465,  1863,
    1658,   477,   311,    77,    78,   406,   406,   429,    24,    25,
     411,   411,   217,   361,    26,   363,  1958,  1675,   427,   282,
     283,   408,   257,   410,   259,   465,   413,   513,   445,   262,
     465,    54,    55,   400,   261,    67,   465,    69,  1902,   465,
    1734,  1995,  1736,   385,   271,   308,   309,   490,   465,   492,
     465,    67,   409,    69,   465,    71,   261,   345,   285,  1900,
     306,   307,   522,  2207,   465,   465,   271,   435,    91,   500,
     501,   502,   503,    23,    24,   308,   798,   799,   445,   520,
     465,     9,   416,   217,    12,   108,  2028,    15,    16,   105,
     106,   107,   774,   775,   237,   118,   416,  2634,   465,    58,
     379,   223,  2639,   520,   261,   465,   469,   465,   471,   183,
     184,   185,   186,   187,   188,   189,   190,   191,   505,   261,
    2029,  2069,   500,   501,   502,   503,   520,   261,   513,   356,
     520,  1972,   460,   516,   520,   520,   519,   271,   404,   526,
     156,   266,   158,   465,  2208,  2209,   465,   411,   422,   165,
      65,   356,   168,   520,    60,   520,    30,  2069,    70,   465,
     133,    35,  2699,  1493,    38,  1495,  2703,   400,  1498,  1499,
    1500,    45,    46,  2014,  1504,   520,  2017,   476,   565,  2214,
     200,   313,   476,  2024,  2096,  1515,  1516,   134,   172,   576,
     476,  2106,    66,  2108,    68,   476,   476,  2121,  2122,   476,
     476,   428,   461,   476,   431,   432,   476,   476,  2132,   476,
    2051,   460,   445,   849,   850,   851,   852,   135,   476,    93,
    2174,  1126,   356,   428,  1129,  2052,  2053,   136,   395,  2779,
    1135,   361,   137,   511,  1139,   622,   138,  2149,   139,   103,
    1145,   257,  2106,   259,  2108,  2157,   140,   455,  2160,   636,
     476,   143,    49,  2317,   641,   415,    35,   460,   459,    38,
     276,   456,   459,   337,   453,   146,    45,    46,   200,   474,
    2212,  1934,  1935,  1936,  1937,   147,   149,  2219,   352,   148,
     168,   515,   276,    31,   150,   114,   151,   520,   114,   163,
     152,   154,   200,  2134,   428,   153,   465,   404,    32,   526,
     260,    35,   520,   465,    38,   328,   520,   411,   319,   332,
     333,  2346,    46,   261,    93,   465,   520,   109,   261,   261,
     194,   526,   422,   482,   465,   111,   319,   476,   715,   460,
    2365,  2366,   520,   520,  2369,   520,   207,   228,   385,   348,
     474,   261,   216,   277,   301,   168,   517,   517,   131,   179,
    2274,  2305,   460,   376,   282,   283,   375,   171,  2270,    93,
     132,   519,   460,   233,   460,   752,    49,  2309,   391,   392,
      49,   465,   200,   233,  2286,   404,  2672,   376,   112,   465,
     308,   309,   373,   257,   163,   465,    87,  2299,    45,   468,
      23,  2232,   526,   465,   277,   782,   270,   456,  2239,  2240,
    2241,  2242,   410,   277,   261,   237,  2247,  2442,   349,    66,
      67,    68,    69,    74,   520,   194,   465,   242,   435,   524,
    2405,   306,   525,   462,   462,  2252,   281,   462,   462,   462,
     460,   305,  2374,   389,   462,   462,   462,   216,  2279,  2348,
     236,   457,   460,   372,   207,   207,   520,    17,   302,   456,
     131,   325,   142,  2280,   375,   316,   460,    49,   126,   144,
     194,   207,     8,   200,   338,   517,   517,  2502,   202,   856,
     314,   435,   207,   465,   460,  2417,     9,     7,   465,   404,
     399,    88,   216,   277,    22,   451,   313,   193,  2523,   513,
       6,   270,   508,     9,   335,  2437,  2531,  2532,   520,    47,
     306,    57,     8,   304,   520,   422,    49,   423,   207,   193,
     513,   513,   411,   387,   248,   193,  2357,   242,   905,   321,
     323,   297,   317,  2558,   132,   267,   305,   401,   337,  2370,
     320,   115,   465,   208,   268,   922,   270,   450,   925,   208,
     520,   520,   456,  2384,   515,   404,   325,   261,   422,   936,
     371,   207,   234,   207,   465,   371,  2383,   214,   292,   293,
     104,     8,    37,   470,    49,   389,  2520,   267,    84,   242,
     303,   475,   507,   223,    97,   298,   465,   160,   965,   517,
     393,   520,  2617,   240,   237,   101,  2653,   193,   259,    57,
     412,   325,   460,   435,    39,   267,  2508,   301,  2742,    49,
     257,   112,   259,   346,   267,   267,   993,   267,   387,   465,
     456,    53,   460,    26,   488,   422,  2651,   410,   338,    17,
     354,   355,   401,   526,   412,   499,   358,   111,   346,   496,
     261,   200,  2586,   290,   465,   109,   349,   116,  2479,   155,
     459,   520,   431,   422,   413,   519,   380,   465,  2683,     7,
     465,  2593,   192,   387,   228,   312,   467,   430,   377,    30,
    2501,   465,   460,   225,   116,   465,   519,   401,   517,   444,
     346,   405,   316,   313,  2586,  2623,   208,   334,   465,   180,
     196,   197,   198,   340,    57,   520,   208,  2599,   422,   205,
    2602,   215,   226,   261,   214,   121,   200,   517,   320,    49,
    1087,   217,   328,   520,  2545,  2546,   437,     7,    55,   488,
      58,  2623,  1073,    53,    60,   449,    52,   204,   395,  1409,
     219,  2633,  1389,  1070,   458,   725,  2251,  1052,  2569,  2570,
    2505,  2238,  2680,  2442,   250,  2246,  2252,  2436,   696,  2580,
     256,  2231,   258,   680,  2585,   261,  1695,   263,   264,   265,
    2546,  2827,  2522,   269,   488,   271,  2202,  2525,  2548,   416,
     276,  1148,  2674,  2649,   498,  1152,  2814,  2174,  2680,   426,
     504,  2839,  2791,  2584,  2584,  2616,  2410,  2681,  1165,  2681,
     241,  1168,  1563,    65,  1091,  2626,   443,  1456,  1159,   505,
    1170,  1561,  2089,  2088,   310,   540,   518,  2739,   861,  1198,
    2641,   815,  2643,   864,  1565,   561,  2647,   908,   465,  1839,
    1220,  1588,  1853,   329,  1248,  1852,   915,   598,  1607,   604,
    1207,  1618,  1278,  1327,   926,  1634,  1308,   956,   636,   486,
    1661,   352,  1326,  1905,  2746,   634,  1326,  2541,   650,  2666,
     356,  1509,  1008,  1434,  2786,  1432,   844,   843,  2790,   506,
    1510,  1910,  1899,  1898,  2289,  1916,   513,  1915,   515,  1680,
    1187,  1654,  1186,   894,  2705,  2706,  1654,  1654,  2780,  2781,
     386,   622,  1259,  1260,  1261,  1654,   807,   727,  1039,  2720,
    2003,  1829,     4,  1295,  2725,  2726,  1517,   515,   473,  2801,
    2586,  1398,  1393,  2205,  2396,  2244,  2495,    19,  2766,  1850,
    2741,  2813,  2814,  2744,  2745,  1609,  2848,    29,  2820,  1064,
    1315,  1577,   428,  2657,  1718,  2490,  2780,  2781,    -1,  1344,
    2782,    -1,    -1,    -1,   440,   441,    -1,    -1,  2586,    -1,
      -1,  2843,  1319,    -1,    -1,  2847,  2777,  2801,    -1,    -1,
      -1,    -1,    -1,    65,    -1,    -1,    -1,    -1,     1,  1336,
       3,   467,     5,    -1,    -1,    -1,  2820,    10,   474,    -1,
      -1,    -1,   478,    -1,    -1,    18,  2780,  2781,   484,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2847,    -1,    -1,    -1,  2801,    -1,   505,
      -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,    51,    52,
      -1,    -1,   518,    -1,   520,    -1,  2820,    -1,    -1,    62,
     526,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    76,    -1,    -1,    -1,    84,    -1,    -1,
      -1,    -1,    -1,  2847,    -1,    -1,    -1,    90,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,    -1,   142,
     143,   144,    -1,   146,   147,   148,   149,   150,   151,   152,
     153,   154,   155,   225,    -1,    -1,    -1,   160,    -1,    -1,
    1497,    -1,   165,   166,    -1,   168,    -1,    -1,   171,    -1,
      -1,    -1,  2780,  2781,    -1,    -1,  1513,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   257,    -1,    -1,    -1,   196,
     197,   198,    -1,  2801,    -1,   267,   199,    -1,   205,    -1,
      -1,    -1,    -1,   206,    -1,    -1,   209,   210,    -1,    -1,
      -1,    -1,  2820,    -1,    -1,    -1,    -1,   220,    -1,    -1,
      -1,  1558,    -1,  1560,   227,    -1,   229,    -1,    -1,   232,
      -1,    -1,    -1,   236,    -1,    -1,    -1,    -1,    -1,  2847,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     322,    -1,    -1,    -1,   261,   327,   263,   264,   265,    -1,
      -1,    -1,   269,    -1,    -1,    -1,    -1,  1604,    -1,  1606,
      -1,   274,    -1,    -1,    -1,   278,    -1,   280,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   290,    -1,   361,
      -1,    -1,    -1,    -1,   297,   298,   299,   369,   301,   302,
     303,   304,    -1,   310,    -1,    -1,    -1,    -1,    -1,   381,
      -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   331,    -1,
      -1,    -1,  1669,    -1,    -1,    -1,    -1,    -1,    -1,   411,
      -1,    -1,    -1,   415,   351,  1682,    -1,    -1,    -1,    -1,
      -1,    -1,   424,    -1,    -1,    -1,   359,   360,    -1,    -1,
      -1,    -1,    -1,   435,    -1,    -1,    -1,   370,  1705,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   386,
     383,   384,    -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,
      -1,   394,    -1,   465,    -1,    -1,   468,    -1,    -1,    -1,
     403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     413,    -1,    -1,    -1,    -1,  1752,  1753,  1754,  1755,  1756,
     423,   424,    -1,  1760,  1761,  1762,    -1,  1764,    -1,    -1,
     433,    -1,    -1,   440,   441,   438,   439,    -1,    -1,   442,
      -1,   444,    -1,    -1,    -1,    -1,     1,    -1,     3,   452,
       5,    -1,    -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,
      -1,    -1,   465,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   475,  1810,    -1,    -1,    -1,   484,    -1,    -1,
     483,    -1,  1819,  1820,  1821,  1822,  1823,   490,  1825,    -1,
      -1,    -1,   495,    -1,    -1,    -1,    51,    52,   505,    -1,
      -1,    -1,    -1,   510,    -1,    -1,    -1,    62,    -1,    -1,
      -1,   518,    -1,   520,   517,    -1,    -1,    -1,    73,    -1,
     523,    76,  1859,    -1,    -1,    -1,  1863,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
      -1,    -1,    -1,    -1,    -1,  1902,   121,    -1,   123,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1928,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,     3,    -1,     5,
      -1,    -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,   199,    -1,    -1,    -1,    -1,    -1,
      -1,   206,    -1,    -1,   209,   210,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   220,    -1,    -1,    -1,    -1,
      -1,    -1,   227,    -1,   229,    51,    52,   232,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    62,    -1,    -1,     6,
      -1,    -1,     9,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,   274,
      -1,    -1,    -1,   278,    -1,   280,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2070,    -1,   290,    -1,   113,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   121,    -1,   123,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   318,    -1,    -1,    -1,    84,    -1,  2106,
      -1,  2108,    -1,    -1,    -1,    -1,   331,    -1,    -1,   155,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
     166,    -1,  2129,    -1,    -1,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   359,   360,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   370,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   199,    -1,    -1,    -1,    -1,   383,   384,
     206,    -1,    -1,   209,   210,   390,    -1,    -1,    -1,   394,
      -1,    -1,    -1,    -1,   220,    -1,    -1,    -1,   403,    -1,
      -1,   227,    -1,   229,    -1,    -1,   232,    -1,   413,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   424,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2214,   433,   196,
     197,   198,    -1,   438,   439,    -1,    -1,   442,   205,   444,
      -1,    -1,    -1,  2230,  2231,    -1,     9,   452,   274,    -1,
     217,    -1,   278,    -1,   280,    -1,    -1,    -1,    -1,    -1,
     465,    -1,    -1,    -1,   290,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   483,    -1,
      -1,    -1,    -1,   250,    -1,   490,    -1,    -1,    -1,   256,
     495,   258,   318,    -1,   261,    58,   263,   264,   265,    -1,
      -1,    -1,   269,    -1,   271,   331,    -1,    -1,    -1,   276,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   523,    -1,
      -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   359,   360,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,   310,   370,    -1,    -1,    -1,   315,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   383,   384,  2346,
      -1,    -1,   329,    -1,   390,    -1,    -1,    -1,   394,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2363,   403,  2365,  2366,
      -1,    -1,  2369,    -1,    -1,    -1,    -1,   413,    -1,   356,
      -1,    -1,    -1,    -1,    -1,  2382,    -1,    -1,   424,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   433,    -1,    -1,
      -1,    -1,   438,   439,    -1,    -1,   442,    -1,   444,   386,
       1,    -1,     3,    -1,     5,    -1,   452,    -1,    -1,    10,
      -1,    -1,    -1,   196,   197,   198,    -1,    18,    -1,   465,
    2427,  2428,   205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   217,  2442,    -1,   483,  2445,  2446,
      -1,   428,    -1,    -1,   490,    -1,  2453,  2454,  2455,   495,
      51,    52,    -1,   440,   441,    -1,    -1,    -1,    -1,    -1,
    2467,    62,    -1,   246,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    76,    -1,   523,   261,    -1,
     263,   264,   265,    -1,    -1,    -1,   269,   474,   271,    90,
      -1,    -1,    -1,    -1,    -1,  2502,    -1,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2514,    -1,    -1,
      -1,    -1,   113,    -1,    -1,    -1,  2523,    -1,   505,  2526,
     121,    -1,   123,   510,  2531,  2532,    -1,   310,    -1,    -1,
      -1,   518,    -1,   520,  2541,    -1,    -1,    -1,    -1,   526,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2558,    -1,    -1,   155,    -1,    -1,    -1,    -1,  2566,
      -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,
     171,    -1,    -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   199,    -1,
      -1,    -1,    -1,   386,    -1,   206,    -1,    -1,   209,   210,
    2617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   220,
      -1,    -1,    -1,    -1,    -1,    -1,   227,  2634,   229,    -1,
      -1,   232,  2639,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2651,   428,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    -1,    -1,   440,   441,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,   274,    -1,    -1,  2683,   278,    -1,   280,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   290,
      -1,   474,  2699,    -1,    -1,    -1,  2703,    -1,    -1,   482,
      -1,   484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,
      -1,    -1,   505,    -1,    -1,    -1,    -1,   510,    -1,    -1,
     331,    -1,    -1,    -1,    -1,   518,    -1,   520,    -1,    -1,
      -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   359,   360,
      -1,    -1,    -1,    -1,    -1,   195,   196,   197,   198,   370,
      -1,    -1,    -1,    -1,    -1,   205,    -1,    -1,    -1,    -1,
      -1,    -1,   383,   384,    -1,    -1,    -1,    -1,    -1,   390,
      -1,    -1,    -1,   394,    -1,    -1,     3,    -1,     5,    -1,
      -1,    -1,   403,    10,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,   413,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   424,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   261,   433,   263,   264,   265,    -1,   438,   439,   269,
      -1,   442,    -1,   444,    51,    52,    -1,    -1,    -1,    -1,
      -1,   452,    -1,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    76,
      -1,     6,    -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,
     310,    -1,   483,    90,    -1,    -1,    -1,    -1,    -1,   490,
      -1,    -1,    -1,    -1,   495,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   121,    -1,   123,    -1,    -1,    -1,
      -1,    -1,   523,    -1,   131,    -1,   133,   134,   135,   136,
     137,   138,   139,   140,    -1,   142,   143,   144,    -1,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,    84,
      -1,    -1,    -1,    -1,    -1,    -1,   386,    -1,    -1,   166,
      -1,    96,    -1,    -1,   171,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   199,    -1,    -1,    -1,    -1,    -1,     6,   206,
      -1,     9,   209,   210,    12,    13,    14,    -1,    -1,    -1,
     440,   441,    20,   220,    -1,    -1,    -1,    -1,    -1,    -1,
     227,    -1,   229,   453,    -1,   232,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    61,   484,    -1,    -1,    -1,    -1,    -1,
      -1,   196,   197,   198,    -1,    -1,   496,   274,    -1,    -1,
     205,   278,    -1,   280,    -1,   505,    84,    -1,    -1,    -1,
     510,    -1,   217,   290,    -1,   515,    -1,    -1,   518,   519,
     520,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   318,    -1,    -1,    -1,   250,    -1,    -1,    -1,    -1,
      -1,   256,    -1,   258,   331,    -1,   261,    -1,   263,   264,
     265,    -1,    -1,    -1,   269,    -1,   271,    -1,    -1,    -1,
      -1,   276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   359,    -1,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   370,   172,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   310,    -1,   384,    -1,    -1,
      -1,    -1,    -1,   390,    -1,     9,    -1,   394,   196,   197,
     198,    -1,    -1,    -1,   329,    -1,   403,   205,    -1,    -1,
      -1,    -1,    -1,   211,   212,    -1,   413,    -1,    -1,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   424,    -1,    -1,
      -1,   356,    -1,    -1,    -1,    -1,   433,    -1,    -1,   237,
      -1,   438,   439,    -1,    -1,   442,    -1,   444,    -1,    -1,
      -1,    -1,   250,   251,   252,   452,    -1,    -1,   256,    -1,
     258,   386,    -1,   261,    -1,   263,   264,   265,   465,    -1,
      84,   269,    -1,   271,    -1,    -1,    -1,    -1,   276,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   483,   101,    -1,    -1,
      -1,   289,    -1,   490,    -1,    -1,    -1,   295,   495,    -1,
      -1,    -1,   300,   428,    -1,    -1,    -1,    -1,    -1,    -1,
     308,    -1,   310,    -1,    -1,   440,   441,   315,    -1,    -1,
      -1,    -1,   320,    84,    -1,    -1,   523,    -1,    -1,    -1,
      -1,   329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     6,
     101,   339,     9,    -1,    -1,    12,    13,    14,    -1,   474,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    -1,   356,   484,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     505,    -1,   196,   197,   198,   510,    -1,    -1,   386,    -1,
      -1,   205,    -1,   518,    61,   520,    -1,    -1,    -1,    -1,
      -1,   526,    -1,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     428,    -1,    -1,    -1,   101,   196,   197,   198,    -1,    -1,
      -1,    -1,   440,   441,   205,    -1,    -1,   261,    -1,   263,
     264,   265,    -1,    -1,    -1,   269,   217,   271,    -1,    -1,
      -1,    -1,    -1,    -1,   462,    -1,   464,    -1,   466,    -1,
      -1,   469,    -1,   471,   472,   473,   474,    -1,   476,   477,
      -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   310,    -1,    -1,    -1,
     261,    -1,   263,   264,   265,   172,    -1,   505,   269,    -1,
     271,    -1,   510,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     518,    -1,   520,    -1,    -1,    -1,    -1,    -1,   526,   196,
     197,   198,    -1,    -1,    -1,    -1,    -1,    -1,   205,    -1,
      -1,    -1,   356,    -1,   211,   212,    -1,    -1,    -1,   310,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   386,    -1,    -1,    -1,     6,    -1,    -1,     9,
      -1,    -1,    -1,   250,   251,   252,    -1,    -1,    -1,   256,
      -1,   258,    -1,    -1,   261,   356,   263,   264,   265,    -1,
      30,    -1,   269,    -1,   271,    35,    -1,    -1,    38,   276,
      -1,    -1,    -1,    -1,   428,    45,    46,    -1,    -1,    -1,
      -1,    -1,   289,    -1,    -1,   386,   440,   441,   295,    -1,
      -1,    -1,    -1,   300,    -1,    -1,    66,    -1,    68,    -1,
      -1,   308,    -1,   310,    -1,    -1,    -1,    -1,   315,    -1,
      -1,    -1,    -1,   320,    84,    -1,    -1,    -1,    -1,    -1,
     474,    -1,   329,    93,    -1,     6,    -1,   428,     9,    -1,
     484,   101,   339,    -1,    -1,    -1,    -1,    -1,    -1,   440,
     441,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   356,
      -1,   505,    -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   518,    -1,   520,    -1,    49,    -1,
      -1,    -1,   526,   474,    -1,    -1,    -1,    -1,    -1,   386,
      -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,   505,    -1,    -1,    -1,    89,   510,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   518,    -1,   520,
     101,   428,    -1,    -1,   194,   526,   196,   197,   198,    -1,
      -1,    -1,    -1,   440,   441,   205,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   462,    -1,   464,    -1,   466,
      -1,    -1,   469,    -1,   471,   472,   473,   474,    -1,   476,
     477,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,   256,   257,   258,    -1,
      -1,   261,    -1,   263,   264,   265,    -1,    -1,   505,   269,
     270,   271,    -1,   510,    -1,    -1,   276,   277,    -1,    -1,
      -1,   518,    -1,   520,    -1,   196,   197,   198,    -1,   526,
      -1,    -1,    -1,    -1,   205,    -1,    -1,    -1,    -1,    -1,
      -1,     6,    -1,    -1,     9,   305,   217,    -1,    -1,    -1,
     310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   325,    -1,    -1,    -1,   329,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   338,   250,
      -1,    -1,    -1,    -1,    -1,   256,    -1,   258,    -1,    -1,
     261,    -1,   263,   264,   265,    84,   356,    -1,   269,    -1,
     271,    -1,    -1,    -1,    -1,   276,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    -1,   386,   387,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   310,
      -1,   401,    -1,    -1,   315,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,    -1,
      -1,    -1,   422,    -1,    -1,    -1,    -1,    -1,   428,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     440,   441,    -1,     6,    -1,   356,     9,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   367,    -1,    -1,   164,
      -1,    -1,    -1,    -1,    -1,    -1,   195,   196,   197,   198,
      -1,    -1,    -1,    -1,   474,   386,   205,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,   488,    -1,
      -1,   196,   197,   198,    -1,    -1,    -1,    -1,    -1,   499,
     205,    -1,   207,    -1,    -1,   505,    -1,    -1,    -1,    -1,
     510,   422,   217,    -1,    -1,    -1,    -1,   428,   518,   519,
     520,    84,    -1,    -1,    -1,    -1,   526,    -1,    -1,   440,
     441,    -1,   261,    96,   263,   264,   265,    -1,   101,    -1,
     269,    -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,    -1,
      -1,   256,    -1,   258,    -1,    -1,   261,    -1,   263,   264,
     265,    -1,    -1,   474,   269,    -1,   271,    -1,    -1,    -1,
      -1,   276,     6,   484,    -1,     9,    -1,    -1,    -1,    -1,
     491,   310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,
      -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,    -1,   510,
      -1,    -1,   513,    -1,    -1,   310,    -1,   518,    -1,   520,
      -1,    -1,    -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   329,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   196,   197,   198,    -1,    -1,    -1,    -1,
      -1,    -1,   205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,   356,    -1,    -1,   217,     6,    -1,   386,     9,    -1,
      -1,    -1,    96,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     385,   386,    -1,    -1,    -1,    -1,    -1,   250,    -1,    -1,
      -1,    -1,    -1,   256,    -1,   258,    -1,    -1,   261,    -1,
     263,   264,   265,    -1,    -1,    -1,   269,    -1,   271,    -1,
      -1,   440,   441,   276,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   428,   453,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    -1,   440,   441,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   310,    -1,    -1,
     101,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,
      -1,    -1,   196,   197,   198,    -1,   329,   496,    -1,   474,
      -1,   205,    -1,    -1,    -1,    -1,   505,    -1,    -1,   484,
      -1,   510,    -1,   217,     6,    -1,   515,     9,    -1,   518,
     519,   520,    -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,
     505,    -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   518,   519,   520,   250,    -1,    -1,    -1,
      -1,   526,   256,   386,   258,    -1,    -1,   261,    -1,   263,
     264,   265,    -1,    -1,    -1,   269,    -1,   271,    -1,    -1,
      -1,    -1,   276,    -1,    -1,   196,   197,   198,    -1,    -1,
      -1,    -1,    -1,    -1,   205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    -1,    -1,   428,   217,     6,    -1,    -1,
       9,    -1,    -1,    -1,    -1,    -1,   310,   440,   441,   101,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   329,    -1,    -1,    -1,   250,
      -1,    -1,    -1,    -1,    -1,   256,    -1,   258,    -1,    -1,
     261,   474,   263,   264,   265,    -1,    -1,    -1,   269,    -1,
     271,   484,   356,    -1,    -1,   276,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   505,    -1,    -1,    84,    -1,   510,    -1,    -1,
      -1,    -1,   386,    -1,    -1,   518,    -1,   520,    -1,   310,
      -1,    -1,   101,   526,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   196,   197,   198,    -1,   329,    -1,
      -1,    -1,    -1,   205,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   428,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   356,   440,   441,    -1,     6,
      -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   250,    -1,
      -1,    -1,    -1,    -1,   256,   386,   258,    -1,    -1,   261,
     474,   263,   264,   265,    -1,    -1,    -1,   269,    -1,   271,
     484,    -1,    -1,    -1,   276,    -1,    -1,   196,   197,   198,
      -1,    -1,    -1,    -1,    -1,    -1,   205,    -1,    -1,    -1,
      -1,   505,    -1,    -1,    -1,    -1,   510,   428,   217,    -1,
      -1,    -1,    -1,    -1,   518,    -1,   520,    84,   310,   440,
     441,    -1,   526,   315,   233,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   329,    -1,   460,
      -1,   250,    -1,    -1,    -1,    -1,    -1,   256,    -1,   258,
      -1,    -1,   261,   474,   263,   264,   265,    -1,    -1,    -1,
     269,    -1,   271,   484,   356,    -1,    -1,   276,     6,    -1,
      -1,     9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,    -1,   510,
      -1,    -1,     6,    -1,   386,     9,    -1,   518,    -1,   520,
      -1,   310,    -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   196,
     197,   198,    -1,   200,    -1,    -1,   428,    -1,   205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,   356,   440,   441,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    -1,    -1,    -1,    -1,    -1,    -1,   386,    -1,    -1,
      -1,    -1,   474,   250,    -1,    -1,    -1,   101,    -1,   256,
      -1,   258,   484,    -1,   261,    -1,   263,   264,   265,    -1,
      -1,    -1,   269,    -1,   271,    -1,    -1,    -1,    -1,   276,
      -1,    -1,     6,   505,    -1,     9,    -1,    -1,   510,   428,
      -1,    -1,    -1,    -1,    -1,    -1,   518,    -1,   520,    -1,
      -1,   440,   441,    -1,   526,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   196,   197,
     198,    -1,   329,    -1,    -1,   474,    -1,   205,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,   217,
      -1,    -1,   196,   197,   198,    -1,    -1,    -1,    -1,   356,
      84,   205,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,
      -1,   510,    -1,   217,    -1,    -1,    -1,   101,    -1,   518,
      -1,   520,   250,    -1,    -1,    -1,    -1,   526,   256,   386,
     258,    -1,    -1,   261,    -1,   263,   264,   265,    -1,    -1,
      -1,   269,    -1,   271,    -1,    -1,   250,    -1,   276,    -1,
      -1,    -1,   256,    -1,   258,    -1,    -1,   261,    -1,   263,
     264,   265,    -1,    -1,    -1,   269,    -1,   271,    -1,    -1,
      -1,   428,   276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   310,   440,   441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   329,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,
      -1,    -1,   196,   197,   198,    -1,    -1,   474,    -1,    -1,
      -1,   205,    -1,    -1,    -1,   329,    -1,   484,   356,    -1,
      -1,    -1,    -1,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,    -1,
      -1,    -1,   356,   510,    -1,    -1,    -1,    -1,   386,    -1,
      -1,   518,    -1,   520,    -1,    -1,   250,    -1,    -1,   526,
      -1,    -1,   256,    -1,   258,    -1,    -1,   261,    -1,   263,
     264,   265,   386,     9,    -1,   269,    -1,   271,    -1,    -1,
      -1,    -1,   276,    -1,   422,    -1,    -1,    -1,    -1,    -1,
     428,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   440,   441,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   428,    -1,   310,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,    -1,   440,   441,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   329,   474,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    84,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     474,    -1,   356,    -1,    -1,   101,    -1,   505,    -1,    -1,
     484,    -1,   510,    -1,    -1,    -1,     1,    -1,    -1,    -1,
     518,    -1,   520,    -1,    -1,    -1,    -1,    -1,   526,    -1,
      -1,   505,   386,    -1,    -1,    -1,   510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   518,    -1,   520,    32,    -1,    -1,
      35,    -1,   526,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,    -1,
      -1,    66,    -1,    68,    -1,   181,   440,   441,    -1,    -1,
      75,    -1,    77,    78,    79,    80,    81,    82,    83,    -1,
     196,   197,   198,    -1,    -1,    -1,    -1,    -1,    -1,   205,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     474,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     484,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     246,   505,    -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   518,   261,   520,   263,   264,   265,
      -1,    -1,   526,   269,    -1,   271,    -1,    -1,   163,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,   183,   184,
     185,   186,   187,    -1,    -1,   190,   191,    -1,    -1,   194,
      -1,    -1,    -1,    -1,   310,   200,    -1,   202,    -1,    -1,
      -1,    -1,    -1,   208,    -1,    -1,    -1,    -1,   213,    -1,
      -1,   216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   224,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   237,    -1,    -1,   240,    -1,    -1,    -1,    -1,
     356,   246,    -1,   248,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   257,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,   270,    -1,    -1,    -1,    -1,
     386,    -1,    -1,    -1,    -1,    -1,    -1,   282,    -1,    21,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    36,    -1,    -1,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,   428,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     325,   326,    -1,    -1,   440,   441,    -1,    -1,    -1,   334,
      -1,    -1,   337,    75,    -1,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    -1,   352,    -1,   354,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   482,    -1,   484,    -1,
      -1,    -1,    -1,    -1,    -1,   380,    -1,    -1,    -1,   121,
      -1,    -1,   387,    -1,    -1,    -1,    -1,    -1,    -1,   505,
      -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,    -1,
     405,    -1,   518,    -1,   520,    -1,    -1,    -1,    -1,    -1,
     526,   416,    -1,   418,   419,   420,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   174,    -1,    -1,    -1,    -1,   179,    -1,    -1,
      -1,   183,   184,   185,   186,   187,    -1,    -1,   190,   191,
      -1,    -1,    -1,    -1,    -1,   460,    -1,    -1,    -1,    -1,
     465,    -1,    -1,    -1,    -1,   470,   208,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   482,    -1,    -1,
      -1,    -1,   224,   488,    -1,    -1,    -1,   492,   493,   494,
      -1,    -1,    -1,    -1,    -1,   237,    -1,    -1,   240,    21,
      -1,   506,    -1,    -1,   246,    -1,   511,    -1,   513,    -1,
      -1,    -1,    -1,    -1,    36,   520,    -1,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     282,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    -1,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   326,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   334,    -1,    -1,   337,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     352,    -1,    -1,    -1,    -1,    -1,    -1,    30,    -1,    32,
      -1,    -1,    35,   365,    -1,    38,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    66,    -1,    68,    -1,    -1,    -1,    -1,
      -1,   183,   184,   185,   186,   187,    -1,    -1,   190,   191,
      -1,    -1,    -1,    -1,   416,    -1,   418,   419,   420,    -1,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   224,    -1,   446,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   237,    -1,    -1,   240,    -1,
      -1,    -1,    -1,   465,   246,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     492,   493,   494,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     282,    -1,    -1,    -1,   506,    -1,    -1,    -1,    -1,    -1,
      -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,   520,    -1,
      -1,   194,    -1,    -1,    -1,    -1,    -1,    -1,   201,   202,
     312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   326,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   334,    -1,    -1,   337,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     352,    -1,    -1,    -1,    -1,   248,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   365,   257,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   268,    -1,   270,    -1,    -1,
      -1,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   292,
     293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   416,    -1,   418,   419,   420,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   325,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   446,   338,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   354,   355,   465,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     482,   101,    -1,    -1,    -1,    -1,    -1,   380,    -1,    -1,
     492,   493,   494,    -1,   387,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   506,    -1,    -1,    -1,   401,    -1,
      -1,   513,   405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    32,    -1,    -1,    35,    -1,   422,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   449,    -1,    66,    -1,
      68,    -1,    -1,    -1,    -1,   458,    -1,    75,    -1,    77,
      78,    79,    80,    81,    82,    83,   196,   197,   198,    39,
      40,    41,    42,    43,    44,   205,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   488,    -1,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,
      -1,   504,    -1,   121,    -1,    75,    -1,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,   519,    -1,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,   256,    -1,   258,    -1,
      -1,   261,    -1,   263,   264,   265,    -1,    -1,    -1,   269,
      -1,   271,    -1,    -1,    -1,   163,    -1,    -1,    -1,    -1,
      -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   183,   184,   185,   186,   187,
      -1,    -1,   190,   191,    -1,    -1,   194,    -1,    -1,    -1,
     310,    -1,   200,    -1,   202,    -1,    -1,    -1,    -1,    -1,
     208,    -1,    -1,    -1,    -1,   213,    -1,    -1,   216,   329,
      -1,    -1,    -1,    -1,    -1,    -1,   224,    -1,    -1,    -1,
      -1,    -1,    84,   183,   184,   185,   186,   187,    -1,   237,
     190,   191,   240,    -1,    -1,    -1,   356,    -1,    -1,   101,
     248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   270,    -1,   224,    -1,   386,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   282,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,   428,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   325,   326,    -1,
     440,   441,   282,    -1,    -1,    -1,   334,    -1,    -1,   337,
      84,    -1,    -1,    -1,   196,   197,   198,    -1,    -1,    -1,
      -1,    -1,   462,   205,   352,    -1,   354,   101,    -1,    -1,
      -1,    -1,   472,    -1,   474,   217,   476,   477,    -1,    -1,
      -1,    -1,    -1,    -1,   484,    -1,   326,    -1,    -1,    -1,
      -1,    -1,   380,    -1,    -1,    -1,    -1,   337,    -1,   387,
      -1,    -1,    -1,    -1,    -1,   505,    -1,    -1,   250,    -1,
     510,    -1,   352,    -1,   256,    -1,   258,   405,   518,   261,
     520,   263,   264,   265,    -1,    -1,   526,   269,   416,   271,
     418,   419,   420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   196,   197,   198,    -1,    -1,    -1,   310,    -1,
      -1,   205,   460,    -1,    -1,    -1,    -1,   465,   418,   419,
     420,    -1,   470,   217,    -1,    -1,    -1,   329,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     488,    -1,    -1,    -1,   492,   493,   494,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   356,    -1,   250,    -1,   506,    -1,
      -1,    -1,   256,   511,   258,   513,    -1,   261,    -1,   263,
     264,   265,    -1,    -1,    -1,   269,    -1,   271,    -1,    -1,
      -1,    -1,    -1,    -1,   386,    -1,    84,    -1,    -1,    -1,
      -1,    -1,   492,   493,   494,    -1,   196,   197,   198,    -1,
      -1,    -1,    -1,   101,    -1,   205,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   310,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   329,    -1,    -1,   440,   441,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,   256,    -1,   258,    -1,
     462,   261,   356,   263,   264,   265,    -1,    -1,    -1,   269,
     472,   271,   474,    -1,   476,   477,    -1,    -1,    -1,    -1,
      84,    -1,   484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   386,    -1,    -1,    -1,    -1,   101,   196,   197,
     198,    -1,    -1,   505,    -1,    -1,    -1,   205,   510,    -1,
     310,    -1,    -1,    -1,    -1,    -1,   518,    -1,   520,   217,
      -1,    -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,   329,
      -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   440,   441,    -1,    -1,
      -1,    -1,   250,    -1,   448,    -1,   356,    -1,   256,    -1,
     258,    -1,    -1,   261,    -1,   263,   264,   265,    -1,    -1,
      -1,   269,    -1,   271,    -1,    -1,    -1,    -1,   472,    -1,
     474,    -1,   476,   477,    -1,    -1,   386,    -1,    84,    -1,
     484,    -1,   196,   197,   198,    -1,    -1,    -1,    -1,    -1,
      -1,   205,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,   505,   310,   217,    -1,    -1,   510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   518,    -1,   520,    -1,   428,    -1,
      -1,   329,   526,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     440,   441,    -1,    -1,    -1,    -1,   250,    -1,   448,    -1,
      -1,    -1,   256,    -1,   258,    -1,    -1,   261,   356,   263,
     264,   265,    -1,    -1,    -1,   269,    -1,   271,    -1,    -1,
      -1,    -1,   472,    -1,   474,    -1,   476,   477,    -1,    -1,
      -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,   386,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
     196,   197,   198,    -1,    -1,   505,   310,    -1,    -1,   205,
     510,    -1,    -1,    -1,   101,    -1,    -1,    -1,   518,    -1,
     520,   217,    -1,    -1,    -1,   329,   526,    -1,    -1,    -1,
     428,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   440,   441,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   356,    -1,   250,    -1,    -1,    -1,    -1,    -1,
     256,    -1,   258,    -1,   462,   261,    -1,   263,   264,   265,
      -1,    -1,    -1,   269,   472,   271,   474,    -1,   476,   477,
      -1,    -1,   386,    -1,    -1,    -1,   484,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,    -1,   196,
     197,   198,   510,    -1,   310,    -1,    -1,    84,   205,    -1,
     518,    -1,   520,    -1,   428,    -1,    -1,    -1,   526,    -1,
     217,    -1,    -1,   329,   101,    -1,   440,   441,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     356,    -1,    -1,   250,    -1,    -1,    -1,    -1,   472,   256,
     474,   258,   476,   477,   261,    -1,   263,   264,   265,    -1,
     484,    -1,   269,    -1,   271,    -1,    -1,    -1,    -1,    -1,
     386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   505,    -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   518,    -1,   520,    -1,    -1,    -1,
      -1,    -1,   526,   310,    -1,    -1,    -1,    -1,    -1,   196,
     197,   198,   428,    -1,    -1,    -1,    -1,    -1,   205,    -1,
      -1,    -1,   329,    -1,   440,   441,    -1,    -1,    -1,    -1,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   356,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,
     476,    -1,    -1,   250,    -1,    -1,    -1,    -1,   484,   256,
      -1,   258,    -1,    -1,   261,   382,   263,   264,   265,   386,
      -1,    -1,   269,    -1,   271,    -1,    -1,    -1,    -1,   505,
      -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   518,    -1,   520,    -1,    -1,    -1,    -1,    -1,
     526,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   428,    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   440,   441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,   356,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,   386,
      -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   518,    -1,   520,    -1,    -1,    -1,    -1,    -1,   526,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   428,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   440,   441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,    -1,
      -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   518,    -1,   520,    -1,    -1,    -1,    -1,    -1,   526
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   530,   531,     0,   218,   219,   532,   533,   534,   535,
     536,   537,   538,   544,   124,   124,   534,   156,   543,   557,
     558,   204,   350,   545,   548,   465,   465,   124,   104,   671,
     673,    86,   559,   560,   549,   546,   543,   543,   465,   124,
     346,   833,   836,   468,   674,   404,   231,   622,   623,   311,
     427,   561,   562,   566,   465,   465,   145,   539,   540,   541,
     141,   542,   465,   124,   859,   860,   404,   675,   465,   404,
     177,   624,   465,   465,   429,   583,   566,   562,   261,   351,
     550,   550,   261,   351,   551,   541,   551,    56,   511,   837,
       1,     3,     5,    10,    18,    51,    52,    62,    73,    76,
      90,   113,   121,   123,   155,   166,   171,   199,   206,   209,
     210,   220,   227,   229,   232,   274,   278,   280,   290,   318,
     331,   359,   360,   370,   383,   384,   390,   394,   403,   413,
     424,   433,   438,   439,   442,   444,   452,   465,   483,   490,
     495,   523,   861,   862,   878,   883,   887,   892,   912,   916,
     920,   924,   925,   926,   931,   945,   949,   952,   966,   970,
     973,   976,   980,   981,   985,   995,   998,  1016,  1018,  1021,
    1025,  1031,  1043,  1051,  1052,  1055,  1056,  1060,  1065,  1066,
    1074,  1090,  1100,  1109,  1114,  1123,  1127,  1129,  1132,  1135,
    1138,  1165,   861,   465,   176,   402,   672,   676,   677,   679,
     465,   465,   626,   567,   563,   465,    11,    59,    98,   100,
     102,   110,   167,   262,   308,   400,   445,   520,   584,   585,
     586,   587,   588,   594,   603,   605,   610,   613,   614,   616,
     617,   618,   619,   620,   621,    26,   552,   552,   465,   465,
     839,   838,   385,   845,     3,     5,    10,    18,    51,    52,
      62,    73,    76,    90,   113,   121,   123,   131,   133,   134,
     135,   136,   137,   138,   139,   140,   142,   143,   144,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,   166,
     171,   199,   206,   209,   210,   220,   227,   229,   232,   274,
     278,   280,   290,   318,   331,   359,   370,   384,   390,   394,
     403,   413,   424,   433,   438,   439,   442,   444,   452,   465,
     483,   490,   495,   523,  1319,   863,   879,   884,   888,   893,
     913,   917,   921,   927,   932,   946,   950,   953,   967,   971,
     974,   977,   207,   385,   904,   969,   982,   986,   996,   999,
    1017,  1019,  1022,   409,  1026,  1032,  1044,  1053,  1057,  1061,
    1067,  1075,  1091,  1101,   261,   356,   396,   428,   526,  1113,
    1115,  1124,   345,  1128,  1130,   848,  1133,  1136,  1139,  1166,
     522,   707,   709,   710,     1,   520,  1237,   239,   407,   625,
     627,    57,    65,   273,   349,   406,   411,   520,   568,   569,
     570,   571,   572,   573,   574,   576,  1328,  1390,   564,   576,
       1,   520,  1251,  1251,   435,   416,  1361,   237,  1342,  1342,
    1342,  1251,   416,  1342,    58,  1329,   589,   379,   577,   586,
     465,   587,   223,   604,   261,   465,   547,    49,   840,   841,
     842,  1327,   840,   315,   520,   465,   315,   520,   864,   866,
    1281,  1282,  1285,     6,     9,    84,    96,   101,   196,   197,
     198,   205,   217,   250,   256,   258,   261,   263,   264,   265,
     269,   271,   276,   310,   329,   356,   386,   428,   440,   441,
     474,   484,   505,   510,   518,   526,   880,  1231,  1256,  1257,
    1259,  1281,  1292,  1293,  1294,  1295,  1296,  1297,  1298,   250,
     472,   476,   477,   885,  1226,  1227,  1228,  1229,  1230,  1231,
    1262,  1281,  1293,  1295,   261,   889,   890,  1242,  1243,  1244,
    1285,   276,   434,   436,   894,   896,   261,   351,   914,   915,
    1269,  1281,   918,  1237,     6,   922,  1232,  1233,  1254,  1283,
    1284,  1285,  1293,   468,   928,  1237,   261,   315,   933,   934,
     935,   936,   938,  1256,  1269,  1281,   947,  1257,   261,   951,
     467,   478,   954,   955,   956,  1214,  1215,  1216,   203,   330,
     331,   349,   404,   968,   972,  1253,  1254,   975,  1285,   460,
     978,  1370,  1257,  1213,  1214,   987,  1253,   520,   997,  1238,
    1000,  1001,  1281,  1292,  1295,  1092,  1276,  1277,  1285,    96,
    1020,  1257,  1023,  1257,   173,   230,   238,   324,  1027,  1028,
     195,   261,   519,  1033,  1037,  1038,  1039,  1242,  1270,  1281,
    1285,  1295,  1374,  1045,  1237,  1054,  1234,  1285,  1058,  1237,
    1062,  1234,     9,  1068,  1235,  1285,   156,   245,   276,  1076,
    1079,  1080,  1083,  1084,  1085,  1086,  1087,  1088,  1089,  1239,
    1240,  1253,  1275,  1277,  1285,  1092,  1102,  1237,  1110,  1116,
    1117,  1118,  1257,    96,  1125,  1256,  1131,  1238,   465,   520,
     849,   850,   853,   854,   859,  1134,  1278,  1281,  1137,  1237,
    1140,  1281,  1167,  1234,   404,   266,   768,   130,   414,   421,
     711,   712,   714,   724,   726,   728,  1306,   465,   678,   465,
     295,   319,  1314,   279,   398,   661,   662,   663,   664,   666,
     411,   422,    65,  1342,   465,   570,   465,   520,   569,    60,
    1342,   565,  1374,   595,  1342,  1342,  1342,  1246,  1285,    70,
    1246,  1342,  1342,  1246,   520,   606,   607,   608,  1252,   261,
     314,   316,   590,   592,   593,  1077,  1288,  1342,   465,   465,
     520,   553,  1342,   841,   422,   491,   843,   367,   513,   834,
     223,   313,  1380,   133,   877,   865,   200,   476,  1286,  1287,
     313,  1352,  1294,  1281,   476,   476,   476,  1300,  1282,  1293,
    1295,  1380,  1380,   476,   476,   476,   476,  1380,   476,  1300,
     134,   882,   460,   881,  1257,   461,   476,  1299,   476,   476,
    1282,  1293,  1295,  1230,  1281,  1226,  1230,    58,   472,   477,
     464,   473,   172,   228,  1309,   890,   460,  1380,   135,   911,
     261,   351,   897,  1270,   915,  1237,   366,   489,   919,  1374,
    1386,  1352,   136,   923,   162,   466,  1233,  1378,   395,  1315,
    1286,  1287,   929,  1237,   137,   930,   361,  1358,   138,   944,
     168,   301,  1180,  1182,  1184,   936,  1255,  1256,   937,   500,
     501,   502,   503,   139,   948,    49,   233,   511,   898,   140,
     965,    17,   517,   957,   958,   959,   961,    12,    13,    14,
      20,    61,   162,   172,   211,   212,   251,   252,   289,   295,
     300,   308,   315,   320,   339,   462,   464,   466,   469,   471,
     472,   473,   476,   477,  1217,  1218,  1219,  1220,  1221,  1222,
    1223,  1257,   103,   969,  1254,  1241,   455,  1368,   988,  1374,
    1238,    94,   375,   450,  1002,  1003,  1005,  1006,  1094,   476,
    1286,  1257,   460,   143,  1024,    49,  1028,   415,  1029,  1038,
     144,   465,  1034,  1036,   496,   515,   456,   459,   453,   146,
    1050,   290,   341,  1312,   200,  1168,   147,  1059,  1358,   148,
    1064,  1168,  1235,   149,  1073,   515,  1069,  1265,  1267,  1281,
    1293,  1295,   168,  1086,  1088,  1253,   460,  1240,   125,   460,
     497,  1078,    31,  1286,   150,  1108,   181,   242,   245,  1104,
     904,  1111,  1257,  1374,   151,  1122,   233,  1118,   114,  1119,
    1281,   152,  1126,   200,  1238,   404,   465,   465,   200,   361,
     363,  1359,   153,  1149,   114,  1141,   154,  1172,  1168,   465,
     404,   260,   770,   520,   716,   716,   716,   712,   465,     1,
     179,   715,   716,   520,   680,   319,  1251,   667,   361,   424,
     425,   665,     1,   465,   663,  1342,   411,  1288,   465,  1342,
     520,  1247,   465,   109,  1342,   217,   261,   271,   356,   428,
     474,   526,   611,   612,  1291,  1246,   261,   261,   482,   607,
      22,   237,  1252,  1343,  1077,   237,   435,  1354,  1342,    98,
    1251,   578,   465,    74,   174,   364,   470,   554,   555,   556,
    1342,   422,   319,   844,   111,   846,  1285,    30,   201,   277,
     867,   868,   869,   871,   874,  1325,  1374,    24,    25,    67,
      69,    71,   105,   106,   107,   156,   158,   165,   168,   257,
     259,   457,   508,   520,   870,  1240,  1377,  1224,  1226,   476,
    1287,   155,   349,  1263,  1282,   460,  1224,  1226,  1304,  1224,
    1305,   462,  1224,   520,   520,  1226,  1303,  1303,  1303,  1261,
    1281,  1293,  1295,  1302,   520,  1261,  1301,     6,  1232,  1257,
    1285,  1293,   207,  1294,  1226,  1261,  1224,   462,   228,  1310,
    1227,  1227,  1228,  1228,  1228,   385,   886,   348,   891,  1244,
     895,   919,   267,   292,   193,  1335,  1282,  1226,   277,  1316,
    1287,  1237,   236,   302,  1206,  1207,  1209,  1211,   856,   857,
     856,  1183,  1184,  1181,  1182,   499,   871,   874,   939,   940,
     941,  1374,  1180,  1180,  1180,  1180,  1257,  1232,  1257,   899,
     956,    21,   467,   478,   962,   963,  1215,   517,   959,   960,
     517,   856,  1370,   237,  1218,   116,   979,  1242,   131,   856,
     983,     9,    12,    15,    16,   282,   283,   308,   309,   989,
     993,   179,  1265,     9,    58,   181,   246,   482,  1009,  1010,
    1011,  1004,  1005,   126,   316,   519,  1096,  1353,  1389,   460,
    1253,  1232,  1257,  1029,  1374,  1236,  1237,   856,   171,  1040,
    1213,  1041,  1042,  1281,  1242,     8,    37,  1170,  1358,  1274,
    1281,  1292,  1295,   233,  1046,  1063,  1374,   132,  1070,  1281,
    1070,   460,   460,   460,  1077,   155,   467,   478,  1257,    49,
      38,    46,   216,   248,   270,   325,   387,   488,  1081,  1082,
    1342,  1103,  1374,  1257,   164,   294,  1281,  1327,   200,  1232,
    1257,   855,  1288,  1265,  1327,   233,  1144,  1169,  1170,   708,
     465,   404,   376,   772,   727,   729,   373,   465,   465,   713,
      87,    47,    64,   104,   244,   255,   361,   362,   376,   378,
     465,   513,   681,   682,   684,   688,   689,   692,   693,   699,
     702,   704,   705,  1342,   628,   468,  1333,    23,  1323,   465,
    1288,   262,   447,   509,   575,  1247,   277,    28,   128,   217,
     261,   271,   285,   356,   428,   431,   432,   526,   596,   597,
     598,   601,   612,   456,   615,  1374,   410,   261,   609,  1289,
    1354,   237,  1251,  1251,   591,   592,   203,   349,   579,   580,
     581,   556,   349,  1357,    74,    32,   112,  1288,  1342,   520,
     465,   835,   526,  1271,  1275,  1288,  1342,   165,   168,   299,
     301,  1173,  1175,  1176,  1178,  1179,   869,    66,    68,   257,
     338,   872,   873,  1376,    32,    35,    38,    46,    93,   112,
     194,   202,   216,   248,   268,   270,   292,   293,   325,   354,
     355,   380,   387,   401,   405,   422,   449,   458,   488,   498,
     504,   875,   876,  1173,   525,   524,  1265,  1173,   242,   435,
     306,   281,    72,   408,   462,  1225,   463,  1226,   261,  1264,
    1282,  1281,  1225,   462,  1225,   462,   462,  1225,   462,   462,
     462,  1225,   462,  1225,   462,  1352,   304,   423,  1185,  1187,
    1189,  1286,  1287,  1232,   463,   462,   462,   460,  1311,   886,
    1254,   460,  1242,   898,   389,   372,  1185,  1342,   856,   856,
    1210,  1211,  1208,  1209,   858,    98,    99,   343,   520,   942,
    1240,   940,    35,    38,    45,    46,    93,   163,   194,   216,
     270,   305,   325,   387,   401,   422,   488,   943,   207,  1185,
     207,   900,   901,   902,  1327,    17,   456,   964,   323,   962,
    1353,   856,   131,   142,   984,  1370,   375,   990,  1370,   460,
      49,  1010,  1012,  1265,     9,    58,   246,   482,  1007,  1008,
    1265,  1279,  1281,   126,    65,   411,  1097,  1375,    27,   117,
     754,   223,   321,  1338,  1253,  1185,   207,  1236,     9,   292,
     359,   660,   388,  1030,  1237,  1374,   144,  1035,     8,   200,
    1046,  1281,   132,   297,  1195,  1198,  1200,  1206,   267,   292,
     856,   517,   517,  1071,  1072,  1265,   314,  1264,  1257,  1077,
    1077,  1077,  1077,  1077,  1077,  1077,  1077,  1082,   295,   300,
    1105,  1106,  1107,  1219,  1313,  1206,   249,   422,  1388,   435,
    1366,  1366,  1121,  1374,   422,  1120,  1257,  1281,  1185,   207,
     465,   460,     9,  1142,  1143,  1307,  1145,  1281,  1121,  1145,
    1063,     7,  1320,   709,   769,   465,   404,   399,   817,   513,
     762,   736,   737,  1342,  1285,   731,   717,  1342,    88,  1330,
    1342,   361,   363,  1385,  1385,  1342,  1330,  1342,   277,  1349,
    1342,    22,  1322,   313,   706,  1251,   174,   208,   629,   451,
    1367,  1335,    58,   521,  1384,   598,    17,   456,  1291,   335,
    1289,  1251,     9,   205,   520,   582,   520,     1,   465,   581,
      32,  1288,   847,   848,    47,  1177,  1178,   856,  1174,  1175,
     856,   306,  1350,  1350,  1350,  1342,  1342,   876,    57,   422,
     125,   497,  1342,     8,  1321,  1173,  1226,   462,  1226,  1315,
     448,  1299,   448,  1299,  1246,  1299,  1299,  1299,  1261,   246,
     482,  1299,  1282,   856,   856,  1188,  1189,  1186,  1187,  1287,
    1185,   462,  1226,  1299,  1299,  1268,  1281,  1292,   903,   904,
      34,   286,   287,   288,   353,   480,   481,   485,  1317,   859,
    1342,   257,   399,   132,   159,   161,   825,   826,  1332,  1342,
     125,   497,  1342,  1232,  1233,  1232,  1233,   901,   315,   843,
      89,   367,   513,   963,  1214,   856,  1281,   856,   513,   991,
     992,   993,   994,  1368,   513,  1266,  1268,  1265,    49,     8,
      37,  1013,  1014,  1015,  1008,  1013,   193,   411,  1093,  1342,
     242,  1344,   321,  1232,  1030,   323,  1355,  1355,   317,   267,
     292,  1042,  1257,   222,  1047,  1374,   856,   856,  1199,  1200,
    1198,   267,  1214,  1213,  1071,  1219,  1281,  1220,  1221,  1222,
    1223,  1226,  1112,  1257,  1112,   303,   475,  1190,  1192,  1194,
     337,  1315,  1232,   851,  1266,   320,  1265,   115,  1146,   450,
    1148,   160,   298,  1171,  1201,  1203,  1205,  1207,   328,  1240,
    1271,   709,   771,   465,   404,  1343,   762,   208,   456,   725,
      21,    36,    39,    40,    41,    42,    43,    44,    45,    75,
      77,    78,    79,    80,    81,    82,    83,   121,   183,   184,
     185,   186,   187,   190,   191,   224,   240,   282,   312,   326,
     334,   337,   352,   365,   416,   418,   419,   420,   446,   492,
     493,   494,   506,   732,   733,   734,   737,   738,   739,   740,
     741,   742,   743,   746,   758,   759,   760,   761,   762,   767,
    1342,  1363,    26,   200,   730,  1324,   208,  1288,   520,   643,
    1342,  1322,   520,  1248,  1249,   315,   430,  1381,   261,  1246,
    1250,  1288,   515,  1342,   178,   218,   520,   690,  1251,     4,
      19,    29,   225,   257,   322,   327,   361,   369,   381,   415,
     424,   465,   468,   630,   631,   638,   640,   642,   644,   645,
     646,   647,   650,   651,   652,   653,   654,   656,   657,   659,
    1358,  1375,  1330,  1236,   599,   601,   261,   234,   552,   205,
     234,   552,   465,   848,  1271,  1271,  1271,  1271,  1271,  1342,
    1342,  1212,  1273,  1275,  1288,  1212,  1271,   261,  1272,  1275,
    1290,   462,  1185,   462,   168,   301,   475,   906,   908,   910,
       6,   233,   296,   315,   474,   905,  1341,  1271,  1352,   257,
     399,  1271,  1212,  1212,  1271,  1185,   371,  1185,   371,  1258,
    1259,  1280,  1282,   992,   104,  1331,  1370,  1013,  1013,  1266,
     470,  1340,  1340,  1015,  1014,   230,   511,  1098,  1246,  1095,
    1185,   389,    49,   267,   242,  1048,   221,   241,   267,   292,
     516,   856,   856,   856,   856,  1193,  1194,  1191,  1192,  1342,
    1185,  1185,   507,   852,  1150,  1143,   223,  1337,    97,  1147,
    1337,  1190,   856,   856,  1204,  1205,  1202,  1203,   257,   259,
    1346,   709,   773,   465,   764,   765,  1292,  1285,   249,   308,
     417,   491,  1362,   491,  1362,   491,  1362,   491,  1362,   491,
    1362,   517,  1372,   393,  1360,   127,  1288,  1282,   237,   247,
     393,  1345,  1342,   174,   246,   482,   520,    50,   249,   250,
     718,  1292,   460,   687,   193,   703,  1249,   259,  1348,   460,
    1329,  1337,   175,   182,   397,   487,   512,   514,   700,   701,
    1342,  1342,  1349,  1358,   460,   511,  1371,   412,  1342,  1328,
     115,  1344,  1344,   292,   658,  1288,  1374,   435,   267,    39,
    1326,  1342,   668,   669,  1237,   600,   601,   132,  1269,  1271,
     257,   259,  1387,   856,   856,   856,   909,   910,   907,   908,
    1352,  1281,  1233,  1233,    49,   112,  1013,  1257,  1257,   346,
    1236,   207,   324,  1099,  1285,  1257,  1342,  1049,  1196,  1198,
    1200,  1206,   267,   267,   267,  1281,  1151,   465,  1281,  1337,
    1281,   774,   818,   763,   765,   456,   526,    53,   750,   460,
     747,   740,    26,   735,   410,  1318,  1318,  1352,     1,    40,
      41,    42,    43,    44,   183,   184,   185,   186,   187,   188,
     189,   337,   352,   719,   720,   721,   722,   723,   741,   742,
    1282,   719,  1288,    58,   363,   683,  1245,  1246,   694,  1288,
     422,  1364,   261,   691,  1285,   691,  1342,  1344,   127,   174,
     635,   369,   651,  1342,  1342,  1342,  1342,  1323,   660,  1342,
    1349,   412,   643,   669,   338,   670,    17,   111,  1281,  1185,
    1185,  1257,  1342,  1236,   346,   496,  1281,  1199,  1197,  1198,
      30,   129,   169,   208,  1152,  1153,  1154,  1156,  1160,  1162,
    1163,  1164,  1325,  1335,  1281,   358,   775,   714,   728,   819,
     820,   821,   517,   766,  1373,  1292,  1337,   200,   748,  1288,
     459,  1369,   261,  1328,   719,   465,  1246,    48,   479,   695,
     696,   697,   698,  1374,  1329,   200,   686,  1336,   127,   357,
     412,   639,  1342,   119,   120,   121,   243,   257,   342,   343,
     344,   357,   451,   632,   633,   634,  1250,   431,   655,  1246,
    1246,  1246,  1342,  1288,   601,   465,  1037,  1342,  1213,    37,
    1321,   349,   109,  1238,     1,   715,   821,   465,   413,   467,
     520,  1288,   747,   116,   749,  1250,  1250,   192,   687,  1288,
     655,   261,   637,  1285,   637,     7,   637,   637,   261,   636,
    1285,   426,   466,    33,   170,   272,   648,  1037,   377,   430,
    1365,   132,   433,  1161,  1353,   776,   465,   822,   465,   460,
    1342,   228,   751,  1353,   752,   753,  1325,  1329,  1308,  1389,
    1333,  1342,  1245,   519,   649,   649,  1281,   164,   168,  1379,
       9,  1157,  1158,  1243,     1,   777,   823,  1292,   752,  1246,
     225,   755,   754,  1250,   116,   685,   444,   641,  1245,   267,
     394,   346,  1356,   313,   347,   368,  1159,  1158,   465,    63,
      91,    92,   328,   465,   778,   779,   782,  1342,  1398,    32,
      35,    38,    45,    46,   163,   194,   200,   202,   213,   216,
     248,   257,   270,   312,   325,   354,   380,   387,   405,   460,
     470,   488,   511,   738,   739,   743,   758,   760,   762,   824,
     831,   832,  1342,  1376,   755,  1327,  1344,  1353,   517,   316,
    1353,   313,  1285,  1342,  1342,  1322,   253,   254,  1347,   791,
     208,   180,   780,  1334,  1342,   257,   399,   825,   826,  1342,
    1274,  1350,  1288,    57,  1281,  1281,   208,  1350,   520,   756,
     757,  1342,  1246,     9,   428,   526,   602,   279,   361,   363,
    1383,   173,   230,   238,   324,  1155,  1236,  1269,  1342,  1322,
     783,  1290,   714,   792,   781,  1281,  1271,  1271,  1342,  1369,
    1342,  1342,   757,  1245,  1294,  1383,   784,   257,   259,  1382,
     715,   716,  1281,   275,   336,   472,   477,   827,   828,   829,
    1269,   827,   828,   830,   181,   192,   215,   245,   785,   786,
     787,   788,   789,   790,  1290,   793,  1271,  1271,   108,   118,
    1391,  1342,  1342,    55,    91,  1391,  1392,  1377,   794,  1342,
    1290,  1290,   215,  1342,  1342,   214,   257,   259,   290,   312,
     340,   426,   443,   465,   486,   506,   515,   738,   743,   744,
     758,   760,   762,   795,   796,   800,   801,   804,   805,   806,
     807,   808,   809,   814,   815,   816,  1376,  1377,  1290,  1290,
    1290,   226,  1339,   306,   307,  1351,  1322,   214,  1288,   517,
    1342,  1352,  1342,  1342,  1281,   291,   336,   810,   811,  1290,
     336,   812,   813,  1290,  1351,  1322,  1343,  1342,   747,  1213,
    1262,  1260,  1262,    54,    91,   328,   332,   333,   376,   391,
     392,   797,  1391,  1392,  1393,  1394,  1395,  1396,  1397,   121,
     200,  1288,   811,  1288,   813,  1343,   811,  1369,  1315,   382,
     802,  1262,   192,   192,   215,   192,   215,   180,   798,  1281,
     798,  1262,   749,  1353,   320,   799,   799,    49,   437,   745,
     180,   803,  1281,   328,  1262,  1288
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   529,   531,   530,   532,   532,   533,   533,   534,   534,
     536,   535,   537,   538,   539,   539,   540,   540,   541,   542,
     543,   544,   544,   544,   546,   547,   545,   549,   548,   550,
     550,   551,   551,   552,   552,   553,   553,   554,   554,   554,
     554,   555,   555,   556,   556,   557,   558,   558,   559,   560,
     560,   561,   561,   561,   561,   561,   563,   562,   564,   564,
     565,   565,   567,   566,   568,   568,   568,   568,   569,   569,
     570,   570,   570,   570,   571,   572,   573,   574,   575,   575,
     575,   575,   576,   576,   577,   578,   577,   579,   579,   579,
     580,   580,   581,   581,   581,   581,   582,   582,   583,   583,
     584,   584,   585,   585,   586,   586,   587,   587,   587,   587,
     587,   587,   587,   587,   587,   587,   587,   587,   589,   588,
     590,   590,   590,   590,   591,   591,   592,   593,   593,   595,
     594,   596,   596,   596,   596,   596,   596,   597,   597,   598,
     598,   599,   598,   600,   600,   601,   601,   601,   601,   601,
     601,   602,   602,   603,   604,   604,   605,   606,   606,   607,
     608,   608,   609,   609,   610,   611,   611,   612,   612,   613,
     614,   615,   615,   616,   617,   618,   619,   620,   621,   622,
     623,   623,   624,   624,   625,   625,   626,   626,   628,   627,
     629,   629,   630,   630,   630,   630,   630,   630,   630,   630,
     630,   630,   630,   630,   630,   631,   631,   631,   631,   631,
     632,   632,   632,   633,   633,   633,   633,   634,   634,   635,
     635,   635,   636,   636,   637,   637,   637,   638,   639,   639,
     639,   640,   641,   641,   641,   642,   643,   644,   645,   645,
     645,   647,   646,   648,   648,   648,   649,   649,   649,   649,
     650,   650,   651,   651,   651,   651,   652,   653,   654,   655,
     655,   655,   656,   657,   658,   658,   659,   660,   660,   660,
     661,   661,   661,   662,   662,   663,   663,   664,   665,   665,
     665,   665,   667,   666,   668,   668,   669,   670,   670,   672,
     671,   673,   673,   674,   674,   675,   675,   676,   678,   677,
     677,   679,   679,   680,   680,   681,   681,   681,   681,   681,
     681,   681,   681,   681,   681,   681,   682,   683,   683,   683,
     684,   684,   684,   685,   685,   686,   686,   687,   687,   688,
     689,   689,   690,   690,   691,   691,   692,   693,   694,   694,
     695,   695,   695,   696,   697,   698,   699,   700,   700,   700,
     700,   700,   701,   701,   702,   703,   703,   704,   705,   705,
     706,   706,   707,   708,   707,   709,   710,   709,   711,   711,
     712,   712,   712,   713,   712,   712,   714,   715,   715,   715,
     716,   717,   717,   718,   718,   718,   718,   719,   719,   719,
     719,   719,   719,   719,   719,   719,   719,   719,   719,   719,
     720,   720,   721,   721,   722,   722,   722,   723,   723,   724,
     725,   725,   727,   726,   728,   729,   728,   730,   730,   731,
     731,   732,   732,   732,   732,   732,   732,   732,   732,   732,
     732,   732,   732,   732,   733,   734,   735,   735,   736,   736,
     737,   738,   739,   739,   740,   740,   740,   740,   740,   740,
     740,   740,   740,   740,   740,   740,   740,   740,   740,   740,
     740,   740,   740,   740,   740,   740,   740,   740,   740,   740,
     740,   740,   740,   740,   740,   740,   740,   740,   740,   740,
     741,   741,   742,   742,   743,   743,   744,   745,   745,   746,
     746,   747,   747,   748,   748,   749,   749,   750,   750,   751,
     751,   752,   753,   753,   754,   754,   755,   755,   756,   756,
     757,   758,   759,   760,   761,   763,   762,   764,   764,   765,
     765,   766,   766,   767,   767,   768,   769,   768,   770,   771,
     770,   772,   773,   772,   774,   774,   776,   775,   777,   777,
     777,   778,   778,   778,   778,   779,   780,   781,   781,   782,
     783,   783,   783,   784,   784,   785,   785,   785,   785,   785,
     786,   787,   788,   789,   790,   791,   791,   793,   792,   794,
     794,   795,   795,   795,   795,   795,   795,   795,   795,   795,
     795,   795,   795,   795,   795,   795,   795,   796,   797,   797,
     797,   797,   797,   797,   797,   798,   798,   798,   799,   799,
     800,   801,   802,   802,   803,   803,   804,   805,   806,   807,
     807,   808,   809,   809,   810,   810,   811,   811,   811,   812,
     812,   813,   813,   814,   815,   816,   817,   818,   817,   819,
     819,   820,   820,   821,   822,   821,   821,   823,   823,   824,
     824,   824,   824,   824,   824,   824,   824,   824,   824,   824,
     824,   824,   824,   824,   824,   824,   824,   824,   824,   824,
     824,   824,   824,   824,   824,   824,   824,   824,   824,   824,
     824,   824,   824,   824,   825,   825,   826,   826,   827,   827,
     828,   828,   829,   829,   829,   830,   830,   830,   831,   832,
     833,   834,   835,   833,   836,   833,   837,   838,   837,   839,
     837,   840,   840,   841,   842,   842,   842,   843,   843,   843,
     843,   843,   843,   844,   844,   845,   845,   845,   846,   847,
     846,   848,   848,   849,   849,   849,   849,   849,   851,   850,
     852,   852,   853,   854,   855,   855,   857,   858,   856,   860,
     859,   859,   861,   861,   861,   861,   861,   861,   861,   861,
     861,   861,   861,   861,   861,   861,   861,   861,   861,   861,
     861,   861,   861,   861,   861,   861,   861,   861,   861,   861,
     861,   861,   861,   861,   861,   861,   861,   861,   861,   861,
     861,   861,   861,   861,   861,   861,   861,   861,   861,   861,
     861,   861,   861,   863,   862,   865,   864,   864,   864,   864,
     864,   864,   864,   864,   864,   864,   864,   864,   864,   864,
     864,   864,   864,   864,   864,   866,   866,   867,   867,   868,
     868,   869,   869,   869,   869,   870,   870,   871,   871,   871,
     872,   873,   873,   874,   875,   875,   875,   875,   875,   875,
     875,   875,   875,   875,   875,   875,   875,   875,   875,   875,
     875,   875,   875,   875,   875,   875,   875,   875,   875,   875,
     875,   875,   876,   876,   877,   877,   879,   878,   880,   880,
     880,   881,   881,   882,   882,   884,   883,   885,   885,   886,
     886,   888,   887,   889,   889,   890,   891,   891,   893,   892,
     895,   894,   896,   896,   896,   896,   897,   897,   898,   899,
     898,   900,   900,   901,   901,   902,   902,   902,   902,   903,
     903,   903,   903,   903,   904,   904,   905,   905,   906,   906,
     906,   907,   907,   908,   908,   909,   909,   910,   911,   911,
     913,   912,   914,   914,   915,   915,   917,   916,   918,   918,
     919,   919,   919,   919,   919,   921,   920,   922,   923,   923,
     924,   925,   927,   926,   928,   928,   929,   929,   930,   930,
     932,   931,   933,   933,   933,   933,   933,   934,   934,   935,
     935,   937,   936,   938,   938,   939,   939,   940,   940,   940,
     940,   940,   941,   941,   941,   941,   942,   942,   943,   943,
     943,   943,   943,   943,   943,   943,   943,   943,   943,   943,
     943,   943,   943,   943,   943,   944,   944,   946,   945,   947,
     947,   947,   947,   947,   948,   948,   950,   949,   951,   953,
     952,   954,   955,   955,   956,   956,   956,   957,   957,   958,
     958,   959,   960,   961,   961,   962,   962,   963,   963,   963,
     963,   964,   964,   965,   965,   967,   966,   968,   968,   968,
     968,   968,   968,   968,   969,   969,   971,   970,   972,   974,
     973,   975,   977,   976,   978,   979,   979,   980,   982,   981,
     983,   983,   983,   984,   984,   986,   985,   987,   988,   988,
     989,   989,   989,   990,   990,   991,   991,   992,   993,   993,
     993,   993,   993,   993,   993,   994,   994,   996,   995,   997,
     997,   999,   998,  1000,  1001,  1001,  1001,  1002,  1002,  1002,
    1002,  1004,  1003,  1005,  1006,  1007,  1007,  1008,  1008,  1008,
    1008,  1008,  1008,  1009,  1009,  1010,  1010,  1011,  1011,  1011,
    1011,  1011,  1012,  1013,  1013,  1013,  1013,  1013,  1014,  1015,
    1017,  1016,  1019,  1018,  1020,  1020,  1022,  1021,  1023,  1023,
    1024,  1024,  1026,  1025,  1027,  1027,  1028,  1028,  1028,  1028,
    1029,  1029,  1030,  1030,  1030,  1030,  1032,  1031,  1033,  1034,
    1033,  1033,  1035,  1035,  1036,  1036,  1037,  1037,  1038,  1038,
    1038,  1038,  1038,  1039,  1039,  1040,  1040,  1041,  1041,  1042,
    1044,  1043,  1045,  1046,  1046,  1047,  1047,  1047,  1047,  1047,
    1047,  1047,  1048,  1048,  1049,  1049,  1050,  1050,  1051,  1053,
    1052,  1054,  1055,  1057,  1056,  1058,  1059,  1059,  1061,  1060,
    1062,  1063,  1063,  1063,  1064,  1064,  1065,  1067,  1066,  1068,
    1068,  1069,  1069,  1070,  1070,  1071,  1071,  1072,  1073,  1073,
    1075,  1074,  1076,  1076,  1076,  1076,  1076,  1076,  1076,  1077,
    1077,  1078,  1078,  1079,  1080,  1081,  1081,  1082,  1082,  1082,
    1082,  1082,  1082,  1082,  1082,  1083,  1083,  1084,  1085,  1085,
    1086,  1087,  1087,  1088,  1088,  1089,  1091,  1090,  1093,  1092,
    1094,  1094,  1095,  1095,  1096,  1096,  1097,  1097,  1098,  1098,
    1098,  1099,  1099,  1099,  1101,  1100,  1102,  1103,  1103,  1104,
    1104,  1104,  1104,  1105,  1105,  1105,  1105,  1105,  1105,  1106,
    1107,  1107,  1108,  1108,  1110,  1109,  1109,  1111,  1111,  1111,
    1111,  1111,  1112,  1112,  1113,  1113,  1113,  1113,  1115,  1114,
    1116,  1117,  1117,  1118,  1119,  1119,  1120,  1120,  1121,  1121,
    1122,  1122,  1124,  1123,  1125,  1125,  1125,  1126,  1126,  1127,
    1128,  1128,  1130,  1129,  1131,  1131,  1133,  1132,  1134,  1136,
    1135,  1137,  1139,  1138,  1140,  1141,  1141,  1142,  1142,  1143,
    1144,  1144,  1145,  1146,  1146,  1147,  1147,  1148,  1148,  1149,
    1149,  1151,  1150,  1152,  1152,  1152,  1152,  1152,  1153,  1154,
    1154,  1155,  1155,  1155,  1155,  1155,  1156,  1157,  1157,  1158,
    1158,  1158,  1159,  1159,  1159,  1159,  1160,  1161,  1161,  1162,
    1163,  1164,  1164,  1166,  1165,  1167,  1168,  1168,  1169,  1169,
    1169,  1169,  1170,  1170,  1171,  1171,  1171,  1172,  1172,  1173,
    1173,  1173,  1174,  1174,  1175,  1176,  1176,  1177,  1177,  1178,
    1179,  1179,  1180,  1180,  1180,  1181,  1181,  1182,  1183,  1183,
    1184,  1185,  1185,  1185,  1186,  1186,  1187,  1188,  1188,  1189,
    1190,  1190,  1190,  1191,  1191,  1192,  1193,  1193,  1194,  1195,
    1195,  1196,  1196,  1197,  1197,  1198,  1199,  1199,  1200,  1201,
    1201,  1202,  1202,  1203,  1204,  1204,  1205,  1206,  1206,  1207,
    1207,  1208,  1208,  1209,  1210,  1210,  1211,  1212,  1212,  1213,
    1214,  1216,  1215,  1217,  1217,  1217,  1218,  1218,  1218,  1218,
    1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,
    1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,
    1218,  1219,  1219,  1220,  1220,  1221,  1221,  1222,  1223,  1224,
    1224,  1225,  1225,  1225,  1226,  1226,  1226,  1227,  1227,  1227,
    1228,  1228,  1229,  1229,  1229,  1230,  1230,  1231,  1231,  1231,
    1231,  1231,  1231,  1232,  1232,  1233,  1234,  1235,  1236,  1236,
    1237,  1238,  1239,  1239,  1240,  1241,  1241,  1242,  1243,  1243,
    1243,  1244,  1245,  1245,  1246,  1247,  1248,  1248,  1249,  1250,
    1250,  1251,  1251,  1252,  1253,  1253,  1254,  1254,  1254,  1255,
    1255,  1256,  1256,  1257,  1257,  1258,  1258,  1259,  1259,  1259,
    1259,  1259,  1259,  1259,  1259,  1259,  1260,  1260,  1261,  1261,
    1261,  1262,  1262,  1262,  1262,  1262,  1262,  1262,  1263,  1263,
    1264,  1264,  1265,  1266,  1267,  1267,  1267,  1268,  1268,  1269,
    1269,  1270,  1270,  1270,  1271,  1271,  1271,  1272,  1272,  1273,
    1273,  1274,  1274,  1274,  1275,  1276,  1277,  1277,  1278,  1279,
    1280,  1281,  1282,  1282,  1282,  1282,  1283,  1284,  1284,  1284,
    1284,  1285,  1285,  1286,  1287,  1287,  1288,  1289,  1290,  1291,
    1291,  1291,  1291,  1291,  1291,  1291,  1292,  1292,  1293,  1293,
    1294,  1294,  1294,  1294,  1294,  1294,  1294,  1295,  1295,  1295,
    1295,  1295,  1295,  1295,  1295,  1295,  1295,  1295,  1295,  1296,
    1296,  1297,  1297,  1297,  1298,  1298,  1298,  1298,  1299,  1299,
    1299,  1300,  1300,  1300,  1301,  1301,  1301,  1302,  1302,  1303,
    1303,  1304,  1304,  1305,  1305,  1306,  1307,  1307,  1308,  1308,
    1309,  1309,  1310,  1310,  1311,  1311,  1312,  1312,  1312,  1313,
    1313,  1314,  1314,  1314,  1315,  1315,  1316,  1316,  1317,  1317,
    1317,  1317,  1317,  1317,  1317,  1317,  1318,  1318,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1320,  1320,
    1321,  1321,  1322,  1322,  1323,  1323,  1324,  1324,  1325,  1325,
    1326,  1326,  1327,  1327,  1328,  1328,  1329,  1329,  1330,  1330,
    1331,  1331,  1332,  1332,  1333,  1333,  1334,  1334,  1335,  1335,
    1336,  1336,  1337,  1337,  1338,  1338,  1338,  1339,  1339,  1340,
    1340,  1341,  1341,  1342,  1342,  1343,  1343,  1343,  1344,  1344,
    1345,  1345,  1345,  1346,  1346,  1346,  1347,  1347,  1347,  1348,
    1348,  1349,  1349,  1350,  1350,  1351,  1351,  1351,  1352,  1352,
    1353,  1353,  1354,  1354,  1354,  1354,  1355,  1355,  1356,  1356,
    1357,  1357,  1358,  1358,  1359,  1359,  1359,  1360,  1360,  1361,
    1361,  1362,  1362,  1363,  1363,  1363,  1364,  1364,  1365,  1365,
    1366,  1366,  1367,  1367,  1368,  1368,  1369,  1369,  1370,  1370,
    1371,  1371,  1371,  1372,  1372,  1373,  1373,  1374,  1374,  1375,
    1375,  1376,  1376,  1377,  1377,  1378,  1378,  1379,  1379,  1380,
    1380,  1381,  1381,  1382,  1382,  1383,  1383,  1384,  1384,  1385,
    1385,  1386,  1386,  1387,  1387,  1388,  1388,  1389,  1389,  1390,
    1390,  1390,  1391,  1391,  1392,  1392,  1393,  1393,  1394,  1394,
    1395,  1395,  1396,  1396,  1397,  1397,  1398,  1398
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     2,     1,     1,
       0,     2,     4,     4,     0,     1,     1,     2,     3,     3,
       3,     0,     3,     3,     0,     0,     8,     0,     6,     1,
       1,     1,     1,     0,     2,     0,     3,     1,     1,     1,
       1,     2,     2,     1,     1,     3,     0,     3,     5,     0,
       3,     0,     1,     1,     2,     2,     0,     4,     0,     3,
       0,     3,     0,     4,     0,     2,     3,     2,     1,     2,
       1,     1,     1,     1,     5,     3,     3,     4,     1,     1,
       1,     1,     1,     2,     0,     0,     4,     0,     2,     3,
       1,     2,     3,     3,     3,     3,     1,     2,     0,     2,
       0,     1,     2,     3,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     3,
       2,     3,     3,     1,     0,     1,     1,     3,     4,     0,
       5,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       3,     0,     4,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     0,     2,     3,     1,     2,     3,
       1,     2,     1,     2,     4,     1,     2,     1,     3,     4,
       5,     0,     3,     3,     5,     3,     4,     3,     3,     5,
       0,     3,     0,     2,     0,     2,     0,     2,     0,     6,
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     5,     5,     5,     5,     5,
       1,     1,     1,     1,     1,     1,     1,     0,     3,     0,
       1,     1,     1,     1,     0,     1,     1,     4,     1,     1,
       1,     7,     0,     4,     3,     3,     1,     4,     0,     1,
       1,     0,     5,     2,     2,     1,     0,     4,     5,     2,
       3,     1,     1,     3,     1,     2,     4,     4,     4,     1,
       3,     4,     4,     3,     1,     1,     3,     2,     2,     2,
       0,     2,     3,     1,     2,     1,     1,     5,     0,     1,
       1,     1,     0,     6,     1,     2,     2,     0,     2,     0,
       9,     0,     3,     0,     3,     0,     2,     2,     0,     5,
       3,     1,     1,     0,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     0,     1,     1,
       4,     6,     9,     0,     3,     0,     2,     0,     2,     3,
       5,     5,     1,     1,     1,     1,     3,     5,     0,     2,
       1,     1,     1,     4,     2,     2,     4,     1,     1,     1,
       1,     1,     1,     1,     4,     0,     2,     2,     2,     2,
       1,     2,     0,     0,     5,     0,     0,     2,     2,     3,
       1,     1,     1,     0,     4,     3,     2,     0,     1,     1,
       1,     0,     2,     1,     2,     2,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     5,
       0,     2,     0,     4,     5,     0,     5,     2,     2,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     3,     0,     2,     0,     1,
       2,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     6,     0,     2,     7,
       8,     0,     2,     0,     2,     0,     3,     0,     3,     0,
       1,     1,     0,     5,     1,     1,     0,     3,     1,     2,
       1,     2,     2,     3,     1,     0,     5,     1,     2,     1,
       3,     0,     4,     2,     2,     0,     0,     5,     0,     0,
       5,     0,     0,     5,     0,     2,     0,     6,     0,     2,
       2,     2,     3,     1,     1,     2,     2,     1,     2,     4,
       1,     4,     2,     0,     2,     1,     1,     1,     1,     1,
       3,     4,     4,     4,     3,     0,     2,     0,     5,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       2,     1,     2,     1,     1,     0,     2,     2,     0,     2,
       4,     4,     0,     3,     1,     1,     3,     6,     2,     3,
       2,     2,     3,     2,     1,     2,     2,     1,     1,     1,
       2,     2,     1,     4,     2,     3,     0,     0,     5,     0,
       1,     2,     3,     1,     0,     4,     3,     0,     2,     2,
       2,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     1,     5,
       5,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     2,     1,     2,     1,     2,     1,     1,
       1,     1,     0,     1,     1,     0,     1,     1,     3,     2,
       0,     0,     0,     9,     0,     4,     0,     0,     3,     0,
       3,     1,     2,     4,     0,     2,     2,     0,     3,     3,
       4,     4,     3,     0,     1,     0,     2,     2,     0,     0,
       7,     0,     2,     1,     1,     2,     1,     1,     0,     6,
       0,     2,     2,     1,     0,     1,     0,     0,     3,     0,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     0,     4,     0,     4,     3,     3,     4,
       3,     4,     3,     3,     4,     4,     3,     4,     3,     4,
       5,     3,     4,     3,     3,     1,     1,     0,     1,     1,
       2,     1,     1,     1,     2,     1,     2,     2,     2,     2,
       3,     3,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     1,     1,
       1,     4,     3,     1,     2,     1,     1,     3,     3,     3,
       3,     3,     1,     1,     0,     1,     0,     4,     4,     5,
       6,     0,     2,     0,     1,     0,     3,     3,     4,     0,
       2,     0,     3,     1,     2,     4,     0,     2,     0,     4,
       0,     6,     0,     1,     1,     1,     1,     1,     0,     0,
       3,     1,     2,     2,     3,     0,     2,     2,     2,     0,
       3,     2,     2,     4,     1,     1,     1,     1,     0,     2,
       2,     0,     1,     2,     2,     0,     1,     2,     0,     1,
       0,     3,     1,     2,     1,     1,     0,     3,     2,     3,
       0,     1,     3,     3,     2,     0,     4,     4,     0,     1,
       1,     1,     0,     4,     3,     2,     1,     2,     0,     1,
       0,     4,     3,     3,     3,     3,     2,     2,     1,     1,
       2,     0,     3,     1,     1,     1,     2,     1,     2,     1,
       1,     2,     2,     2,     2,     2,     1,     1,     1,     2,
       2,     1,     1,     2,     2,     1,     1,     1,     1,     3,
       1,     3,     3,     3,     3,     0,     1,     0,     4,     4,
       6,     6,     8,     8,     0,     1,     0,     3,     2,     0,
       4,     2,     1,     3,     1,     1,     1,     2,     1,     1,
       2,     2,     3,     2,     3,     1,     3,     2,     1,     1,
       1,     0,     2,     0,     1,     0,     3,     0,     2,     1,
       2,     1,     1,     1,     0,     2,     0,     3,     1,     0,
       3,     1,     0,     3,     3,     0,     3,     2,     0,     6,
       3,     2,     1,     0,     1,     0,     3,     5,     0,     2,
       0,     3,     3,     0,     2,     1,     2,     4,     1,     1,
       1,     1,     1,     1,     1,     0,     3,     0,     3,     1,
       2,     0,     3,     2,     1,     1,     1,     2,     1,     1,
       1,     0,     3,     2,     5,     1,     2,     2,     2,     1,
       1,     1,     2,     1,     2,     4,     2,     0,     1,     1,
       1,     1,     4,     0,     1,     1,     2,     2,     3,     3,
       0,     3,     0,     3,     3,     4,     0,     4,     4,     6,
       0,     1,     0,     3,     4,     5,     1,     1,     1,     1,
       0,     3,     0,     3,     2,     1,     0,     3,     2,     0,
       4,     2,     0,     1,     1,     1,     1,     3,     0,     2,
       1,     3,     3,     0,     3,     1,     1,     1,     3,     7,
       0,     4,     7,     0,     2,     0,     2,     2,     3,     3,
       3,     2,     0,     3,     1,     1,     0,     1,     1,     0,
       3,     2,     1,     0,     4,     4,     0,     1,     0,     4,
       4,     0,     2,     3,     0,     1,     1,     0,     4,     4,
       6,     0,     2,     0,     2,     1,     2,     3,     0,     1,
       0,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     3,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     4,     3,     4,     1,     2,
       3,     1,     2,     3,     3,     4,     0,     3,     0,     7,
       0,     5,     0,     2,     0,     2,     0,     3,     0,     2,
       4,     0,     2,     4,     0,     4,     4,     0,     3,     0,
       4,     1,     1,     1,     2,     2,     2,     2,     1,     1,
       2,     1,     0,     1,     0,     4,     2,     0,     2,     1,
       4,     4,     0,     1,     1,     1,     1,     1,     0,     4,
       5,     1,     2,     2,     0,     3,     1,     1,     0,     4,
       0,     1,     0,     4,     4,     6,     6,     0,     1,     2,
       0,     1,     0,     3,     1,     2,     0,     3,     5,     0,
       3,     2,     0,     4,     6,     0,     3,     1,     3,     2,
       2,     2,     3,     0,     3,     0,     3,     0,     3,     0,
       1,     0,     3,     1,     1,     1,     1,     1,     7,     0,
       1,     1,     1,     1,     1,     1,     4,     1,     2,     1,
       2,     3,     0,     1,     2,     1,     3,     1,     1,     4,
       1,     1,     1,     0,     4,     5,     0,     2,     0,     4,
       3,     3,     1,     1,     0,     1,     1,     0,     1,     0,
       2,     2,     0,     1,     2,     1,     1,     0,     1,     2,
       1,     1,     0,     2,     2,     0,     1,     2,     0,     1,
       2,     0,     2,     2,     0,     1,     2,     0,     1,     2,
       0,     2,     2,     0,     1,     2,     0,     1,     2,     2,
       2,     2,     2,     0,     1,     2,     0,     1,     2,     2,
       2,     0,     1,     2,     0,     1,     2,     0,     1,     2,
       2,     0,     1,     2,     0,     1,     2,     0,     2,     1,
       1,     0,     2,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       3,     0,     1,     1,     3,     3,     1,     3,     3,     1,
       3,     1,     2,     2,     1,     3,     1,     1,     3,     1,
       3,     1,     3,     1,     2,     2,     1,     1,     1,     2,
       1,     1,     1,     2,     1,     0,     2,     1,     1,     1,
       3,     1,     1,     2,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     3,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     4,     3,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     3,     2,     2,     1,     1,     3,     2,     2,
       1,     1,     3,     3,     4,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     2,     5,     5,
       5,     4,     5,     5,     5,     5,     5,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       5,     0,     3,     2,     1,     3,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     0,     0,     1,     0,     1,
       0,     1,     0,     2,     0,     2,     0,     1,     1,     0,
       1,     0,     1,     2,     0,     2,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     2,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     2,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     1,     0,     1,
       0,     1,     1,     0,     1,     1,     0,     2,     2,     0,
       1,     0,     1,     0,     1,     0,     1,     1,     0,     1,
       0,     1,     0,     2,     1,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     2,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     1,     0,     1,     0,     3,     0,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     1,     1,     1,     1,     1,     1,     2,     1,     3,
       2,     1,     1,     1,     2,     1,     2,     1,     2,     1,
       2,     1,     2,     1,     2,     1,     2,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 2081 "parser.y" /* yacc.c:1646  */
    {
	clear_initial_values ();
	current_program = NULL;
	defined_prog_list = NULL;
	cobc_cs_check = 0;
	prog_end = 0;
	depth = 0;
	main_flag_set = 0;
	current_program = cb_build_program (NULL, 0);
	cb_build_registers ();
  }
#line 6976 "parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 2093 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
	if (depth > 1) {
		cb_error (_("multiple PROGRAM-ID's without matching END PROGRAM"));
	}
	if (cobc_flag_main && !main_flag_set) {
		cb_error (_("executable requested but no program found"));
	}
	if (errorcount > 0) {
		YYABORT;
	}
	if (!current_program->entry_list) {
		emit_entry (current_program->program_id, 0, NULL);
	}
  }
#line 6999 "parser.c" /* yacc.c:1646  */
    break;

  case 10:
#line 2129 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	current_section = NULL;
	current_paragraph = NULL;
	prog_end = 1;
	if (increment_depth ()) {
		YYABORT;
	}
	l = cb_build_alphanumeric_literal (demangle_name,
					   strlen (demangle_name));
	current_program->program_id = cb_build_program_id (l, NULL, 0);
	current_program->prog_type = CB_PROGRAM_TYPE;
	if (!main_flag_set) {
		main_flag_set = 1;
		current_program->flag_main = cobc_flag_main;
	}
	check_relaxed_syntax (COBC_HD_PROGRAM_ID);
  }
#line 7023 "parser.c" /* yacc.c:1646  */
    break;

  case 18:
#line 2180 "parser.y" /* yacc.c:1646  */
    {
	first_nested_program = 0;
	clean_up_program ((yyvsp[-1]), CB_PROGRAM_TYPE);
  }
#line 7032 "parser.c" /* yacc.c:1646  */
    break;

  case 19:
#line 2188 "parser.y" /* yacc.c:1646  */
    {
	  clean_up_program ((yyvsp[-1]), CB_FUNCTION_TYPE);
  }
#line 7040 "parser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 2211 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 7048 "parser.c" /* yacc.c:1646  */
    break;

  case 25:
#line 2215 "parser.y" /* yacc.c:1646  */
    {
	if (set_up_program ((yyvsp[-1]), (yyvsp[0]), CB_PROGRAM_TYPE)) {
		YYABORT;
	}
	
	set_up_prototype ((yyvsp[-1]), (yyvsp[0]), CB_PROGRAM_TYPE, 1);
  }
#line 7060 "parser.c" /* yacc.c:1646  */
    break;

  case 26:
#line 2223 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 7069 "parser.c" /* yacc.c:1646  */
    break;

  case 27:
#line 2231 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 7077 "parser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 2235 "parser.y" /* yacc.c:1646  */
    {
	if (set_up_program ((yyvsp[-2]), (yyvsp[-1]), CB_FUNCTION_TYPE)) {
		YYABORT;
	}
	set_up_prototype ((yyvsp[-2]), (yyvsp[-1]), CB_FUNCTION_TYPE, 1);
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 7090 "parser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 2247 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE_P ((yyvsp[0])) && CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
	}
	/*
	  The program name is a key part of defining the current_program, so we
	  mustn't lose it (unlike in undefined_word).
	*/
	(yyval) = (yyvsp[0]);
  }
#line 7105 "parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 2266 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 7111 "parser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 2267 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 7117 "parser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 2276 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 7130 "parser.c" /* yacc.c:1646  */
    break;

  case 38:
#line 2285 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 7143 "parser.c" /* yacc.c:1646  */
    break;

  case 40:
#line 2295 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("CALL prototypes"));
  }
#line 7151 "parser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 2307 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_initial = 1;
  }
#line 7159 "parser.c" /* yacc.c:1646  */
    break;

  case 44:
#line 2311 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_recursive = 1;
  }
#line 7167 "parser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 2327 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_ENVIRONMENT_DIVISION;
  }
#line 7175 "parser.c" /* yacc.c:1646  */
    break;

  case 50:
#line 2344 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_CONFIGURATION_SECTION;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "CONFIGURATION SECTION");
	}
  }
#line 7187 "parser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 2358 "parser.y" /* yacc.c:1646  */
    {
	if (warningopt && (check_comp_duplicate & SYN_CLAUSE_2)) {
		cb_warning (_("phrases in non-standard order"));
	}
  }
#line 7197 "parser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 2370 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_repeated ("SOURCE-COMPUTER", SYN_CLAUSE_1, &check_comp_duplicate);
  }
#line 7207 "parser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 2385 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_debugging_line, "DEBUGGING MODE");
	current_program->flag_debugging = 1;
	needs_debug_item = 1;
	cobc_cs_check = 0;
	cb_build_debug_item ();
  }
#line 7219 "parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 2398 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_repeated ("OBJECT-COMPUTER", SYN_CLAUSE_2, &check_comp_duplicate);
  }
#line 7229 "parser.c" /* yacc.c:1646  */
    break;

  case 74:
#line 2427 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
#line 7237 "parser.c" /* yacc.c:1646  */
    break;

  case 75:
#line 2435 "parser.y" /* yacc.c:1646  */
    {
	current_program->collating_sequence = (yyvsp[0]);
  }
#line 7245 "parser.c" /* yacc.c:1646  */
    break;

  case 76:
#line 2442 "parser.y" /* yacc.c:1646  */
    {
	/* Ignore */
  }
#line 7253 "parser.c" /* yacc.c:1646  */
    break;

  case 77:
#line 2449 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->classification) {
		cb_error (_("duplicate CLASSIFICATION clause"));
	} else {
		current_program->classification = (yyvsp[0]);
	}
  }
#line 7265 "parser.c" /* yacc.c:1646  */
    break;

  case 78:
#line 2460 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7273 "parser.c" /* yacc.c:1646  */
    break;

  case 79:
#line 2464 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 7281 "parser.c" /* yacc.c:1646  */
    break;

  case 80:
#line 2468 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 7289 "parser.c" /* yacc.c:1646  */
    break;

  case 81:
#line 2472 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 7297 "parser.c" /* yacc.c:1646  */
    break;

  case 85:
#line 2486 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
  }
#line 7306 "parser.c" /* yacc.c:1646  */
    break;

  case 86:
#line 2491 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_repository = 0;
  }
#line 7314 "parser.c" /* yacc.c:1646  */
    break;

  case 89:
#line 2499 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 7322 "parser.c" /* yacc.c:1646  */
    break;

  case 92:
#line 2511 "parser.y" /* yacc.c:1646  */
    {
	functions_are_all = 1;
  }
#line 7330 "parser.c" /* yacc.c:1646  */
    break;

  case 93:
#line 2515 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) != cb_error_node) {
		set_up_prototype ((yyvsp[-1]), (yyvsp[0]), CB_FUNCTION_TYPE, 0);
	}
  }
#line 7340 "parser.c" /* yacc.c:1646  */
    break;

  case 95:
#line 2522 "parser.y" /* yacc.c:1646  */
    {
	  if ((yyvsp[-1]) != cb_error_node
	      && cb_verify (cb_program_prototypes, _("PROGRAM phrase"))) {
		set_up_prototype ((yyvsp[-1]), (yyvsp[0]), CB_PROGRAM_TYPE, 0);
  }
  }
#line 7351 "parser.c" /* yacc.c:1646  */
    break;

  case 96:
#line 2532 "parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 7360 "parser.c" /* yacc.c:1646  */
    break;

  case 97:
#line 2537 "parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 7369 "parser.c" /* yacc.c:1646  */
    break;

  case 99:
#line 2548 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	header_check |= COBC_HD_SPECIAL_NAMES;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
  }
#line 7383 "parser.c" /* yacc.c:1646  */
    break;

  case 118:
#line 2593 "parser.y" /* yacc.c:1646  */
    {
	char system_name[16];
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	check_duplicate = 0;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		save_tree = NULL;
	} else {
		/* get system name and revert word-combination of scanner.l,
		   if necessary (e.g. SWITCH A <--> SWITCH_A) */
		strncpy(system_name, CB_NAME ((yyvsp[0])), 15);
		if (system_name [6] == '_') {
			system_name [6] = ' ';
		}
		/* lookup system name */
		save_tree = lookup_system_name (system_name);
		if (!save_tree) {
			cb_error_x ((yyvsp[0]), _("invalid system-name '%s'"), system_name);
		}
	}
  }
#line 7411 "parser.c" /* yacc.c:1646  */
    break;

  case 120:
#line 2621 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_DEVICE_CONSOLE) {
			cb_error_x (save_tree, _("invalid %s clause"), "");
		} else {
			current_program->flag_console_is_crt = 1;
		}
	}
  }
#line 7425 "parser.c" /* yacc.c:1646  */
    break;

  case 121:
#line 2631 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (save_tree, _("invalid %s clause"), "SPECIAL NAMES");
		} else if (CB_VALID_TREE ((yyvsp[0]))) {
			CB_SYSTEM_NAME(save_tree)->value = (yyvsp[-2]);
			cb_define ((yyvsp[0]), save_tree);
			CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
					(yyvsp[0]), save_tree);
		}
	}
  }
#line 7442 "parser.c" /* yacc.c:1646  */
    break;

  case 122:
#line 2644 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree && CB_VALID_TREE ((yyvsp[-1]))) {
		cb_define ((yyvsp[-1]), save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				(yyvsp[-1]), save_tree);
	}
  }
#line 7454 "parser.c" /* yacc.c:1646  */
    break;

  case 126:
#line 2660 "parser.y" /* yacc.c:1646  */
    {
	  check_on_off_duplicate = 0;
  }
#line 7462 "parser.c" /* yacc.c:1646  */
    break;

  case 127:
#line 2667 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 7481 "parser.c" /* yacc.c:1646  */
    break;

  case 128:
#line 2682 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 7500 "parser.c" /* yacc.c:1646  */
    break;

  case 129:
#line 2702 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		(yyval) = NULL;
	} else {
		/* Returns null on error */
		(yyval) = cb_build_alphabet_name ((yyvsp[0]));
	}
  }
#line 7517 "parser.c" /* yacc.c:1646  */
    break;

  case 130:
#line 2715 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		current_program->alphabet_name_list =
			cb_list_add (current_program->alphabet_name_list, (yyvsp[-2]));
	}
	cobc_cs_check = 0;
  }
#line 7529 "parser.c" /* yacc.c:1646  */
    break;

  case 131:
#line 2726 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_NATIVE;
	}
  }
#line 7539 "parser.c" /* yacc.c:1646  */
    break;

  case 132:
#line 2732 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 7549 "parser.c" /* yacc.c:1646  */
    break;

  case 133:
#line 2738 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 7559 "parser.c" /* yacc.c:1646  */
    break;

  case 134:
#line 2744 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_EBCDIC;
	}
  }
#line 7569 "parser.c" /* yacc.c:1646  */
    break;

  case 135:
#line 2750 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 7579 "parser.c" /* yacc.c:1646  */
    break;

  case 136:
#line 2756 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_CUSTOM;
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->custom_list = (yyvsp[0]);
	}
  }
#line 7590 "parser.c" /* yacc.c:1646  */
    break;

  case 137:
#line 2766 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 7598 "parser.c" /* yacc.c:1646  */
    break;

  case 138:
#line 2770 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 7606 "parser.c" /* yacc.c:1646  */
    break;

  case 139:
#line 2777 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7614 "parser.c" /* yacc.c:1646  */
    break;

  case 140:
#line 2781 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 7622 "parser.c" /* yacc.c:1646  */
    break;

  case 141:
#line 2785 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[-1]));
  }
#line 7630 "parser.c" /* yacc.c:1646  */
    break;

  case 142:
#line 2789 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 7638 "parser.c" /* yacc.c:1646  */
    break;

  case 143:
#line 2796 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 7646 "parser.c" /* yacc.c:1646  */
    break;

  case 144:
#line 2800 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-3]), (yyvsp[0]));
  }
#line 7654 "parser.c" /* yacc.c:1646  */
    break;

  case 145:
#line 2806 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 7660 "parser.c" /* yacc.c:1646  */
    break;

  case 146:
#line 2807 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 7666 "parser.c" /* yacc.c:1646  */
    break;

  case 147:
#line 2808 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 7672 "parser.c" /* yacc.c:1646  */
    break;

  case 148:
#line 2809 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 7678 "parser.c" /* yacc.c:1646  */
    break;

  case 149:
#line 2810 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_high; }
#line 7684 "parser.c" /* yacc.c:1646  */
    break;

  case 150:
#line 2811 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_low; }
#line 7690 "parser.c" /* yacc.c:1646  */
    break;

  case 151:
#line 2815 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 7696 "parser.c" /* yacc.c:1646  */
    break;

  case 152:
#line 2816 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 7702 "parser.c" /* yacc.c:1646  */
    break;

  case 153:
#line 2824 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else if ((yyvsp[-1])) {
		CB_CHAIN_PAIR (current_program->symbolic_char_list, (yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 7717 "parser.c" /* yacc.c:1646  */
    break;

  case 154:
#line 2838 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 7725 "parser.c" /* yacc.c:1646  */
    break;

  case 155:
#line 2842 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7733 "parser.c" /* yacc.c:1646  */
    break;

  case 156:
#line 2850 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7741 "parser.c" /* yacc.c:1646  */
    break;

  case 157:
#line 2857 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7749 "parser.c" /* yacc.c:1646  */
    break;

  case 158:
#line 2861 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
	} else {
		(yyval) = (yyvsp[-1]);
	}
  }
#line 7761 "parser.c" /* yacc.c:1646  */
    break;

  case 159:
#line 2872 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l1;
	cb_tree		l2;

	if (cb_list_length ((yyvsp[-2])) != cb_list_length ((yyvsp[0]))) {
		cb_error (_("invalid %s clause"), "SYMBOLIC");
		(yyval) = NULL;
	} else {
		l1 = (yyvsp[-2]);
		l2 = (yyvsp[0]);
		for (; l1; l1 = CB_CHAIN (l1), l2 = CB_CHAIN (l2)) {
			CB_PURPOSE (l1) = CB_VALUE (l2);
		}
		(yyval) = (yyvsp[-2]);
	}
  }
#line 7782 "parser.c" /* yacc.c:1646  */
    break;

  case 160:
#line 2892 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = NULL;
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[0]));
	}
  }
#line 7794 "parser.c" /* yacc.c:1646  */
    break;

  case 161:
#line 2900 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = (yyvsp[-1]);
	} else {
		(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 7806 "parser.c" /* yacc.c:1646  */
    break;

  case 162:
#line 2910 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 7812 "parser.c" /* yacc.c:1646  */
    break;

  case 163:
#line 2911 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 7818 "parser.c" /* yacc.c:1646  */
    break;

  case 164:
#line 2918 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		x = cb_build_class_name ((yyvsp[-2]), (yyvsp[0]));
		if (x) {
			current_program->class_name_list =
				cb_list_add (current_program->class_name_list, x);
		}
	}
  }
#line 7840 "parser.c" /* yacc.c:1646  */
    break;

  case 165:
#line 2938 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 7846 "parser.c" /* yacc.c:1646  */
    break;

  case 166:
#line 2939 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 7852 "parser.c" /* yacc.c:1646  */
    break;

  case 167:
#line 2944 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7860 "parser.c" /* yacc.c:1646  */
    break;

  case 168:
#line 2948 "parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CLASS ((yyvsp[-2])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[-2])) && CB_LITERAL ((yyvsp[-2]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (CB_TREE_CLASS ((yyvsp[0])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (literal_value ((yyvsp[-2])) <= literal_value ((yyvsp[0]))) {
		(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
	} else {
		(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-2]));
	}
  }
#line 7880 "parser.c" /* yacc.c:1646  */
    break;

  case 169:
#line 2969 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		l = cb_build_locale_name ((yyvsp[-2]), (yyvsp[0]));
		if (l) {
			current_program->locale_list =
				cb_list_add (current_program->locale_list, l);
		}
	}
  }
#line 7902 "parser.c" /* yacc.c:1646  */
    break;

  case 170:
#line 2992 "parser.y" /* yacc.c:1646  */
    {
	unsigned char	*s = CB_LITERAL ((yyvsp[-1]))->data;
	unsigned int	error_ind = 0;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		error_ind = 1;
	}
	check_repeated ("CURRENCY", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		CB_PENDING ("PICTURE SYMBOL");
	}
	if (CB_LITERAL ((yyvsp[-1]))->size != 1) {
		cb_error_x ((yyvsp[-1]), _("invalid currency sign '%s'"), (char *)s);
		error_ind = 1;
	}
	switch (*s) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'N':
	case 'P':
	case 'R':
	case 'S':
	case 'V':
	case 'X':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'n':
	case 'p':
	case 'r':
	case 's':
	case 'v':
	case 'x':
	case 'z':
	case '+':
	case '-':
	case ',':
	case '.':
	case '*':
	case '/':
	case ';':
	case '(':
	case ')':
	case '=':
	case '\'':
	case '"':
	case ' ':
		cb_error_x ((yyvsp[-1]), _("invalid currency sign '%s'"), (char *)s);
		break;
	default:
		if (!error_ind) {
			current_program->currency_symbol = s[0];
		}
		break;
	}
  }
#line 7983 "parser.c" /* yacc.c:1646  */
    break;

  case 171:
#line 3073 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 7991 "parser.c" /* yacc.c:1646  */
    break;

  case 172:
#line 3077 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 7999 "parser.c" /* yacc.c:1646  */
    break;

  case 173:
#line 3086 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("DECIMAL-POINT", SYN_CLAUSE_2, &check_duplicate);
		current_program->decimal_point = ',';
		current_program->numeric_separator = '.';
	}
  }
#line 8016 "parser.c" /* yacc.c:1646  */
    break;

  case 174:
#line 3105 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		current_program->flag_trailing_separate = 1;
	}
  }
#line 8031 "parser.c" /* yacc.c:1646  */
    break;

  case 175:
#line 3121 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURSOR", SYN_CLAUSE_3, &check_duplicate);
		current_program->cursor_pos = (yyvsp[0]);
	}
  }
#line 8047 "parser.c" /* yacc.c:1646  */
    break;

  case 176:
#line 3139 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CRT STATUS", SYN_CLAUSE_4, &check_duplicate);
		current_program->crt_status = (yyvsp[0]);
	}
  }
#line 8063 "parser.c" /* yacc.c:1646  */
    break;

  case 177:
#line 3157 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("SCREEN CONTROL", SYN_CLAUSE_5, &check_duplicate);
		CB_PENDING ("SCREEN CONTROL");
	}
  }
#line 8079 "parser.c" /* yacc.c:1646  */
    break;

  case 178:
#line 3174 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("EVENT STATUS", SYN_CLAUSE_6, &check_duplicate);
		CB_PENDING ("EVENT STATUS");
	}
  }
#line 8095 "parser.c" /* yacc.c:1646  */
    break;

  case 179:
#line 3195 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_environment (current_program);
  }
#line 8103 "parser.c" /* yacc.c:1646  */
    break;

  case 181:
#line 3202 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_INPUT_OUTPUT_SECTION;
  }
#line 8112 "parser.c" /* yacc.c:1646  */
    break;

  case 183:
#line 3210 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_FILE_CONTROL;
  }
#line 8122 "parser.c" /* yacc.c:1646  */
    break;

  case 185:
#line 3219 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_I_O_CONTROL;
  }
#line 8132 "parser.c" /* yacc.c:1646  */
    break;

  case 188:
#line 3234 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_FILE_CONTROL, 0);
	check_duplicate = 0;
	if (CB_VALID_TREE ((yyvsp[0]))) {
		/* Build new file */
		current_file = build_file ((yyvsp[0]));
		current_file->optional = CB_INTEGER ((yyvsp[-1]))->val;

		/* Add file to current program list */
		CB_ADD_TO_CHAIN (CB_TREE (current_file),
				 current_program->file_list);
	} else {
		current_file = NULL;
		if (current_program->file_list) {
			current_program->file_list
				= CB_CHAIN (current_program->file_list);
		}
	}
  }
#line 8158 "parser.c" /* yacc.c:1646  */
    break;

  case 189:
#line 3256 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-3]))) {
		validate_file (current_file, (yyvsp[-3]));
	}
  }
#line 8168 "parser.c" /* yacc.c:1646  */
    break;

  case 205:
#line 3288 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
  }
#line 8178 "parser.c" /* yacc.c:1646  */
    break;

  case 206:
#line 3294 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 8192 "parser.c" /* yacc.c:1646  */
    break;

  case 207:
#line 3304 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdout", (size_t)6);
		current_file->special = COB_SELECT_STDOUT;
	}
  }
#line 8209 "parser.c" /* yacc.c:1646  */
    break;

  case 208:
#line 3317 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdin", (size_t)5);
		current_file->special = COB_SELECT_STDIN;
	}
  }
#line 8226 "parser.c" /* yacc.c:1646  */
    break;

  case 209:
#line 3330 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		/* RM/COBOL always expects an assignment name here - we ignore this
		   for PRINTER + PRINTER-1 as ACUCOBOL allows this for using as alias */
		current_file->flag_ext_assign = 0;
		if ((yyvsp[-1]) == cb_int0) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER",	(size_t)7);
		} else if ((yyvsp[-1]) == cb_int1) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER-1",	(size_t)9);
		} else {
			current_file->assign =
				cb_build_alphanumeric_literal ("LPT1",	(size_t)4);
		}

	}
  }
#line 8254 "parser.c" /* yacc.c:1646  */
    break;

  case 210:
#line 3356 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 8260 "parser.c" /* yacc.c:1646  */
    break;

  case 211:
#line 3357 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 8266 "parser.c" /* yacc.c:1646  */
    break;

  case 212:
#line 3358 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int4; }
#line 8272 "parser.c" /* yacc.c:1646  */
    break;

  case 218:
#line 3370 "parser.y" /* yacc.c:1646  */
    {
	current_file->flag_line_adv = 1;
  }
#line 8280 "parser.c" /* yacc.c:1646  */
    break;

  case 220:
#line 3377 "parser.y" /* yacc.c:1646  */
    {
	current_file->flag_ext_assign = 1;
  }
#line 8288 "parser.c" /* yacc.c:1646  */
    break;

  case 224:
#line 3390 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 8296 "parser.c" /* yacc.c:1646  */
    break;

  case 227:
#line 3402 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	check_repeated ("ACCESS", SYN_CLAUSE_2, &check_duplicate);
  }
#line 8305 "parser.c" /* yacc.c:1646  */
    break;

  case 228:
#line 3409 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
#line 8311 "parser.c" /* yacc.c:1646  */
    break;

  case 229:
#line 3410 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_DYNAMIC; }
#line 8317 "parser.c" /* yacc.c:1646  */
    break;

  case 230:
#line 3411 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_RANDOM; }
#line 8323 "parser.c" /* yacc.c:1646  */
    break;

  case 231:
#line 3419 "parser.y" /* yacc.c:1646  */
    {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	p = cobc_parse_malloc (sizeof (struct cb_alt_key));
	p->key = (yyvsp[-2]);
	p->duplicates = CB_INTEGER ((yyvsp[-1]))->val;
	p->next = NULL;

	/* Add to the end of list */
	if (current_file->alt_key_list == NULL) {
		current_file->alt_key_list = p;
	} else {
		l = current_file->alt_key_list;
		for (; l->next; l = l->next) {
			;
		}
		l->next = p;
	}
  }
#line 8348 "parser.c" /* yacc.c:1646  */
    break;

  case 232:
#line 3442 "parser.y" /* yacc.c:1646  */
    { }
#line 8354 "parser.c" /* yacc.c:1646  */
    break;

  case 233:
#line 3445 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUPPRESS WHEN ALL");
  }
#line 8362 "parser.c" /* yacc.c:1646  */
    break;

  case 234:
#line 3450 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUPPRESS WHEN SPACE/ZERO");
  }
#line 8370 "parser.c" /* yacc.c:1646  */
    break;

  case 235:
#line 3460 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLLATING", SYN_CLAUSE_3, &check_duplicate);
	CB_PENDING ("COLLATING SEQUENCE");
  }
#line 8379 "parser.c" /* yacc.c:1646  */
    break;

  case 236:
#line 3468 "parser.y" /* yacc.c:1646  */
    {
	  if (CB_ALPHABET_NAME_P (cb_ref ((yyvsp[0])))) {
		  (yyval) = (yyvsp[0]);
	  } else {
		  cb_error_x ((yyvsp[0]), _("'%s' is not an alphabet-name"),
			      cb_name ((yyvsp[0])));
		  (yyval) = cb_error_node;
	  }
  }
#line 8393 "parser.c" /* yacc.c:1646  */
    break;

  case 237:
#line 3483 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("STATUS", SYN_CLAUSE_4, &check_duplicate);
	current_file->file_status = (yyvsp[0]);
  }
#line 8402 "parser.c" /* yacc.c:1646  */
    break;

  case 241:
#line 3498 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOCK", SYN_CLAUSE_5, &check_duplicate);
  }
#line 8410 "parser.c" /* yacc.c:1646  */
    break;

  case 243:
#line 3506 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode = COB_LOCK_MANUAL;
	cobc_cs_check = 0;
  }
#line 8419 "parser.c" /* yacc.c:1646  */
    break;

  case 244:
#line 3511 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode = COB_LOCK_AUTOMATIC;
	cobc_cs_check = 0;
  }
#line 8428 "parser.c" /* yacc.c:1646  */
    break;

  case 245:
#line 3516 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode = COB_LOCK_EXCLUSIVE;
	cobc_cs_check = 0;
  }
#line 8437 "parser.c" /* yacc.c:1646  */
    break;

  case 248:
#line 3525 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 8445 "parser.c" /* yacc.c:1646  */
    break;

  case 249:
#line 3529 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
	CB_PENDING ("WITH ROLLBACK");
  }
#line 8454 "parser.c" /* yacc.c:1646  */
    break;

  case 252:
#line 3545 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_INDEXED;
  }
#line 8463 "parser.c" /* yacc.c:1646  */
    break;

  case 253:
#line 3550 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_SEQUENTIAL;
  }
#line 8472 "parser.c" /* yacc.c:1646  */
    break;

  case 254:
#line 3555 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_RELATIVE;
  }
#line 8481 "parser.c" /* yacc.c:1646  */
    break;

  case 255:
#line 3560 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
  }
#line 8490 "parser.c" /* yacc.c:1646  */
    break;

  case 256:
#line 3571 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PADDING", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
#line 8499 "parser.c" /* yacc.c:1646  */
    break;

  case 257:
#line 3582 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD DELIMITER", SYN_CLAUSE_8, &check_duplicate);
  }
#line 8507 "parser.c" /* yacc.c:1646  */
    break;

  case 258:
#line 3592 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD KEY", SYN_CLAUSE_9, &check_duplicate);
	current_file->key = (yyvsp[0]);
  }
#line 8516 "parser.c" /* yacc.c:1646  */
    break;

  case 259:
#line 3599 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 8522 "parser.c" /* yacc.c:1646  */
    break;

  case 260:
#line 3600 "parser.y" /* yacc.c:1646  */
    { CB_PENDING ("SPLIT KEYS"); }
#line 8528 "parser.c" /* yacc.c:1646  */
    break;

  case 261:
#line 3601 "parser.y" /* yacc.c:1646  */
    { CB_PENDING ("SPLIT KEYS"); }
#line 8534 "parser.c" /* yacc.c:1646  */
    break;

  case 262:
#line 3608 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RELATIVE KEY", SYN_CLAUSE_10, &check_duplicate);
	current_file->key = (yyvsp[0]);
  }
#line 8543 "parser.c" /* yacc.c:1646  */
    break;

  case 263:
#line 3619 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RESERVE", SYN_CLAUSE_11, &check_duplicate);
  }
#line 8551 "parser.c" /* yacc.c:1646  */
    break;

  case 266:
#line 3633 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SHARING", SYN_CLAUSE_12, &check_duplicate);
	current_file->sharing = (yyvsp[0]);
  }
#line 8560 "parser.c" /* yacc.c:1646  */
    break;

  case 267:
#line 3640 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 8566 "parser.c" /* yacc.c:1646  */
    break;

  case 268:
#line 3641 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 8572 "parser.c" /* yacc.c:1646  */
    break;

  case 269:
#line 3642 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 8578 "parser.c" /* yacc.c:1646  */
    break;

  case 272:
#line 3651 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 8586 "parser.c" /* yacc.c:1646  */
    break;

  case 277:
#line 3670 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	switch (CB_INTEGER ((yyvsp[-3]))->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l))) {
				CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = samearea;
			}
		}
		samearea++;
		break;
	case 2:
		/* SAME SORT-MERGE */
		break;
	}
  }
#line 8615 "parser.c" /* yacc.c:1646  */
    break;

  case 278:
#line 3697 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 8621 "parser.c" /* yacc.c:1646  */
    break;

  case 279:
#line 3698 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 8627 "parser.c" /* yacc.c:1646  */
    break;

  case 280:
#line 3699 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 8633 "parser.c" /* yacc.c:1646  */
    break;

  case 281:
#line 3700 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 8639 "parser.c" /* yacc.c:1646  */
    break;

  case 282:
#line 3707 "parser.y" /* yacc.c:1646  */
    {
	/* Fake for TAPE */
	cobc_cs_check = CB_CS_ASSIGN;
  }
#line 8648 "parser.c" /* yacc.c:1646  */
    break;

  case 283:
#line 3712 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
	cobc_cs_check = 0;
  }
#line 8660 "parser.c" /* yacc.c:1646  */
    break;

  case 289:
#line 3741 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_WORKING;
  }
#line 8668 "parser.c" /* yacc.c:1646  */
    break;

  case 290:
#line 3749 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_data (current_program);
  }
#line 8676 "parser.c" /* yacc.c:1646  */
    break;

  case 292:
#line 3756 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_DATA_DIVISION;
  }
#line 8684 "parser.c" /* yacc.c:1646  */
    break;

  case 294:
#line 3765 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_FILE_SECTION;
  }
#line 8694 "parser.c" /* yacc.c:1646  */
    break;

  case 297:
#line 3779 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_file)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			if (current_file->reports) {
				cb_error (_("RECORD description invalid with REPORT"));
			} else {
				finalize_file (current_file, CB_FIELD ((yyvsp[0])));
			}
		} else if (!current_file->reports) {
			cb_error (_("RECORD description missing or invalid"));
		}
	}
  }
#line 8712 "parser.c" /* yacc.c:1646  */
    break;

  case 298:
#line 3798 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION,
			       COBC_HD_FILE_SECTION, 0, 0);
	check_duplicate = 0;
	if (CB_INVALID_TREE ((yyvsp[0])) || cb_ref ((yyvsp[0])) == cb_error_node) {
		YYERROR;
	}
	current_file = CB_FILE (cb_ref ((yyvsp[0])));
	if (CB_VALID_TREE (current_file)) {
		if ((yyvsp[-1])) {
			current_file->organization = COB_ORG_SORT;
		}
	}
  }
#line 8732 "parser.c" /* yacc.c:1646  */
    break;

  case 300:
#line 3815 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 8740 "parser.c" /* yacc.c:1646  */
    break;

  case 301:
#line 3822 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 8748 "parser.c" /* yacc.c:1646  */
    break;

  case 302:
#line 3826 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 8756 "parser.c" /* yacc.c:1646  */
    break;

  case 305:
#line 3837 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_1, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_global) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	current_file->flag_external = 1;
  }
#line 8770 "parser.c" /* yacc.c:1646  */
    break;

  case 306:
#line 3847 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_2, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_external) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		current_file->flag_global = 1;
		current_program->flag_file_global = 1;
	}
  }
#line 8789 "parser.c" /* yacc.c:1646  */
    break;

  case 316:
#line 3877 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLOCK", SYN_CLAUSE_3, &check_duplicate);
	/* ignore */
  }
#line 8798 "parser.c" /* yacc.c:1646  */
    break;

  case 320:
#line 3890 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		if (warningopt) {
			cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
		}
	} else {
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			cb_error (_("RECORD clause invalid"));
		}
		if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
		}
	}
  }
#line 8822 "parser.c" /* yacc.c:1646  */
    break;

  case 321:
#line 3910 "parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		if (warningopt) {
			cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
		}
	} else {
		current_file->record_min = cb_get_int ((yyvsp[-3]));
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_min < 0)  {
			current_file->record_min = 0;
			error_ind = 1;
		}
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			error_ind = 1;
		}
		if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
		if (current_file->record_max <= current_file->record_min)  {
			error_ind = 1;
		}
		if (error_ind) {
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
#line 8860 "parser.c" /* yacc.c:1646  */
    break;

  case 322:
#line 3945 "parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	current_file->record_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	current_file->record_max = (yyvsp[-2]) ? cb_get_int ((yyvsp[-2])) : 0;
	if ((yyvsp[-3]) && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if ((yyvsp[-2]) && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if ((yyvsp[-2]) && current_file->record_max > MAX_FD_RECORD)  {
		current_file->record_max = MAX_FD_RECORD;
		cb_error (_("RECORD size exceeds maximum allowed (%d)"),
			  MAX_FD_RECORD);
		error_ind = 1;
	}
	if (((yyvsp[-3]) || (yyvsp[-2])) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
#line 8892 "parser.c" /* yacc.c:1646  */
    break;

  case 324:
#line 3976 "parser.y" /* yacc.c:1646  */
    {
	current_file->record_depending = (yyvsp[0]);
  }
#line 8900 "parser.c" /* yacc.c:1646  */
    break;

  case 325:
#line 3982 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 8906 "parser.c" /* yacc.c:1646  */
    break;

  case 326:
#line 3983 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 8912 "parser.c" /* yacc.c:1646  */
    break;

  case 327:
#line 3987 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 8918 "parser.c" /* yacc.c:1646  */
    break;

  case 328:
#line 3988 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 8924 "parser.c" /* yacc.c:1646  */
    break;

  case 329:
#line 3996 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LABEL", SYN_CLAUSE_5, &check_duplicate);
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
#line 8933 "parser.c" /* yacc.c:1646  */
    break;

  case 330:
#line 4007 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
#line 8942 "parser.c" /* yacc.c:1646  */
    break;

  case 331:
#line 4012 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	}
  }
#line 8954 "parser.c" /* yacc.c:1646  */
    break;

  case 336:
#line 4035 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("DATA", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
#line 8963 "parser.c" /* yacc.c:1646  */
    break;

  case 337:
#line 4047 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINAGE", SYN_CLAUSE_8, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
	} else {
		current_file->linage = (yyvsp[-2]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
#line 8982 "parser.c" /* yacc.c:1646  */
    break;

  case 343:
#line 4075 "parser.y" /* yacc.c:1646  */
    {
	current_file->latfoot = (yyvsp[0]);
  }
#line 8990 "parser.c" /* yacc.c:1646  */
    break;

  case 344:
#line 4082 "parser.y" /* yacc.c:1646  */
    {
	current_file->lattop = (yyvsp[0]);
  }
#line 8998 "parser.c" /* yacc.c:1646  */
    break;

  case 345:
#line 4089 "parser.y" /* yacc.c:1646  */
    {
	current_file->latbot = (yyvsp[0]);
  }
#line 9006 "parser.c" /* yacc.c:1646  */
    break;

  case 346:
#line 4098 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	check_repeated ("RECORDING", SYN_CLAUSE_9, &check_duplicate);
	/* ignore */
  }
#line 9016 "parser.c" /* yacc.c:1646  */
    break;

  case 351:
#line 4111 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORDING MODE U or S can only be used with RECORD SEQUENTIAL files"));
	}
  }
#line 9026 "parser.c" /* yacc.c:1646  */
    break;

  case 354:
#line 4127 "parser.y" /* yacc.c:1646  */
    {
	struct cb_alphabet_name	*al;

	check_repeated ("CODE SET", SYN_CLAUSE_10, &check_duplicate);

	al = CB_ALPHABET_NAME (cb_ref ((yyvsp[-1])));
	switch (al->alphabet_type) {
#ifdef	COB_EBCDIC_MACHINE
	case CB_ALPHABET_ASCII:
#else
	case CB_ALPHABET_EBCDIC:
#endif
	case CB_ALPHABET_CUSTOM:
		current_file->code_set = al;
		break;
	default:
		if (warningopt && CB_VALID_TREE ((yyvsp[-1]))) {
			cb_warning_x ((yyvsp[-1]), _("ignoring CODE-SET '%s'"),
				      cb_name ((yyvsp[-1])));
		}
		break;
	}

	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("CODE-SET clause invalid for file type"));
	}

	if (warningopt) {
		CB_PENDING ("CODE-SET");
	}
  }
#line 9063 "parser.c" /* yacc.c:1646  */
    break;

  case 356:
#line 4163 "parser.y" /* yacc.c:1646  */
    {
	  if (warningopt) {
		  CB_PENDING ("FOR sub-records");
	  }

	  current_file->code_set_items = CB_LIST ((yyvsp[0]));
  }
#line 9075 "parser.c" /* yacc.c:1646  */
    break;

  case 357:
#line 4176 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REPORT", SYN_CLAUSE_11, &check_duplicate);
	CB_PENDING("REPORT WRITER");
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("REPORT clause with wrong file type"));
	} else {
		current_file->reports = (yyvsp[0]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	}
  }
#line 9091 "parser.c" /* yacc.c:1646  */
    break;

  case 360:
#line 4196 "parser.y" /* yacc.c:1646  */
    {
	current_report = build_report ((yyvsp[0]));
	current_report->file = current_file;
	CB_ADD_TO_CHAIN (CB_TREE (current_report), current_program->report_list);
	if (report_count == 0) {
		report_instance = current_report;
	}
	report_count++;
  }
#line 9105 "parser.c" /* yacc.c:1646  */
    break;

  case 361:
#line 4206 "parser.y" /* yacc.c:1646  */
    {
	current_report = build_report ((yyvsp[0]));
	CB_ADD_TO_CHAIN (CB_TREE (current_report), current_program->report_list);
	if (report_count == 0) {
		report_instance = current_report;
	}
	report_count++;
  }
#line 9118 "parser.c" /* yacc.c:1646  */
    break;

  case 363:
#line 4221 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_WORKING_STORAGE_SECTION;
	current_storage = CB_STORAGE_WORKING;
  }
#line 9128 "parser.c" /* yacc.c:1646  */
    break;

  case 364:
#line 4227 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD ((yyvsp[0])));
	}
  }
#line 9138 "parser.c" /* yacc.c:1646  */
    break;

  case 365:
#line 4236 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 9146 "parser.c" /* yacc.c:1646  */
    break;

  case 366:
#line 4239 "parser.y" /* yacc.c:1646  */
    {
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 9156 "parser.c" /* yacc.c:1646  */
    break;

  case 367:
#line 4245 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	(yyval) = CB_TREE (description_field);
  }
#line 9169 "parser.c" /* yacc.c:1646  */
    break;

  case 373:
#line 4265 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 9179 "parser.c" /* yacc.c:1646  */
    break;

  case 374:
#line 4271 "parser.y" /* yacc.c:1646  */
    {
	if (!qualifier) {
		current_field->flag_filler = 1;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 9192 "parser.c" /* yacc.c:1646  */
    break;

  case 375:
#line 4280 "parser.y" /* yacc.c:1646  */
    {
	/* Free tree assocated with level number */
	cobc_parse_free ((yyvsp[-2]));
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 9206 "parser.c" /* yacc.c:1646  */
    break;

  case 376:
#line 4293 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9214 "parser.c" /* yacc.c:1646  */
    break;

  case 377:
#line 4300 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
#line 9224 "parser.c" /* yacc.c:1646  */
    break;

  case 378:
#line 4306 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
#line 9234 "parser.c" /* yacc.c:1646  */
    break;

  case 380:
#line 4316 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	qualifier = (yyvsp[0]);
	non_const_word = 0;
  }
#line 9244 "parser.c" /* yacc.c:1646  */
    break;

  case 381:
#line 4325 "parser.y" /* yacc.c:1646  */
    {
	(yyval)= NULL;
  }
#line 9252 "parser.c" /* yacc.c:1646  */
    break;

  case 382:
#line 4329 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
		(yyval)= NULL;
	} else {
		(yyval) = cb_null;
	}
  }
#line 9265 "parser.c" /* yacc.c:1646  */
    break;

  case 383:
#line 4340 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 9271 "parser.c" /* yacc.c:1646  */
    break;

  case 384:
#line 4341 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 9277 "parser.c" /* yacc.c:1646  */
    break;

  case 385:
#line 4342 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 9283 "parser.c" /* yacc.c:1646  */
    break;

  case 386:
#line 4343 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 9289 "parser.c" /* yacc.c:1646  */
    break;

  case 387:
#line 4348 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9297 "parser.c" /* yacc.c:1646  */
    break;

  case 388:
#line 4352 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 9305 "parser.c" /* yacc.c:1646  */
    break;

  case 389:
#line 4356 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 9313 "parser.c" /* yacc.c:1646  */
    break;

  case 390:
#line 4360 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int4;
  }
#line 9321 "parser.c" /* yacc.c:1646  */
    break;

  case 391:
#line 4364 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 9329 "parser.c" /* yacc.c:1646  */
    break;

  case 392:
#line 4368 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(long));
  }
#line 9337 "parser.c" /* yacc.c:1646  */
    break;

  case 393:
#line 4372 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(void *));
  }
#line 9345 "parser.c" /* yacc.c:1646  */
    break;

  case 394:
#line 4376 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(float));
  }
#line 9353 "parser.c" /* yacc.c:1646  */
    break;

  case 395:
#line 4380 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(double));
  }
#line 9361 "parser.c" /* yacc.c:1646  */
    break;

  case 396:
#line 4384 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (4);
  }
#line 9369 "parser.c" /* yacc.c:1646  */
    break;

  case 397:
#line 4388 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 9377 "parser.c" /* yacc.c:1646  */
    break;

  case 398:
#line 4392 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (16);
  }
#line 9385 "parser.c" /* yacc.c:1646  */
    break;

  case 399:
#line 4396 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 9397 "parser.c" /* yacc.c:1646  */
    break;

  case 409:
#line 4428 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-4]), (yyvsp[-3]))) {
		YYERROR;
	}

	if (cb_ref ((yyvsp[-1])) != cb_error_node) {
		error_if_invalid_level_for_renames ((yyvsp[-1]));
		current_field->redefines = CB_FIELD (cb_ref ((yyvsp[-1])));
	}

	if ((yyvsp[0])) {
		error_if_invalid_level_for_renames ((yyvsp[0]));
		current_field->rename_thru = CB_FIELD (cb_ref ((yyvsp[0])));
	} else {
		/* If there is no THRU clause, RENAMES acts like REDEFINES. */
		current_field->pic = current_field->redefines->pic;
	}

	cb_validate_renames_item (current_field);
  }
#line 9422 "parser.c" /* yacc.c:1646  */
    break;

  case 410:
#line 4452 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 9430 "parser.c" /* yacc.c:1646  */
    break;

  case 411:
#line 4456 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]) == cb_error_node ? NULL : (yyvsp[0]);
  }
#line 9438 "parser.c" /* yacc.c:1646  */
    break;

  case 412:
#line 4463 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 9448 "parser.c" /* yacc.c:1646  */
    break;

  case 413:
#line 4469 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_88_item (current_field);
  }
#line 9456 "parser.c" /* yacc.c:1646  */
    break;

  case 414:
#line 4476 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x;
	int	level;

	cobc_cs_check = 0;
	level = cb_get_level ((yyvsp[-4]));
	/* Free tree assocated with level number */
	cobc_parse_free ((yyvsp[-4]));
	if (level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	} else if ((yyvsp[0])) {
		x = cb_build_constant ((yyvsp[-3]), (yyvsp[0]));
		CB_FIELD (x)->flag_item_78 = 1;
		CB_FIELD (x)->level = 1;
		cb_needs_01 = 1;
		if ((yyvsp[-1])) {
			CB_FIELD (x)->flag_is_global = 1;
		}
		/* Ignore return value */
		(void)cb_validate_78_item (CB_FIELD (x), 0);
	}
  }
#line 9483 "parser.c" /* yacc.c:1646  */
    break;

  case 415:
#line 4499 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 9493 "parser.c" /* yacc.c:1646  */
    break;

  case 416:
#line 4505 "parser.y" /* yacc.c:1646  */
    {
	/* Reset to last non-78 item */
	current_field = cb_validate_78_item (current_field, 0);
  }
#line 9502 "parser.c" /* yacc.c:1646  */
    break;

  case 417:
#line 4513 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9510 "parser.c" /* yacc.c:1646  */
    break;

  case 418:
#line 4517 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("CONSTANT FROM");
	(yyval) = NULL;
  }
#line 9519 "parser.c" /* yacc.c:1646  */
    break;

  case 419:
#line 4525 "parser.y" /* yacc.c:1646  */
    {
	/* Required to check redefines */
	(yyval) = NULL;
  }
#line 9528 "parser.c" /* yacc.c:1646  */
    break;

  case 420:
#line 4531 "parser.y" /* yacc.c:1646  */
    {
	/* Required to check redefines */
	(yyval) = cb_true;
  }
#line 9537 "parser.c" /* yacc.c:1646  */
    break;

  case 434:
#line 4558 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REDEFINES", SYN_CLAUSE_1, &check_pic_duplicate);
	if ((yyvsp[-2]) != NULL) {
		if (cb_relaxed_syntax_checks) {
			cb_warning_x ((yyvsp[0]), _("REDEFINES clause should follow entry-name"));
		} else {
			cb_error_x ((yyvsp[0]), _("REDEFINES clause must follow entry-name"));
		}
	}

	current_field->redefines = cb_resolve_redefines (current_field, (yyvsp[0]));
	if (current_field->redefines == NULL) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		YYERROR;
	}
  }
#line 9559 "parser.c" /* yacc.c:1646  */
    break;

  case 435:
#line 4582 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_2, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "EXTERNAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_is_global) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "REDEFINES");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	} else {
		current_field->flag_external = 1;
		current_program->flag_has_external = 1;
	}
  }
#line 9587 "parser.c" /* yacc.c:1646  */
    break;

  case 436:
#line 4609 "parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname (current_field->name);
  }
#line 9595 "parser.c" /* yacc.c:1646  */
    break;

  case 437:
#line 4613 "parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname ((const char *)CB_LITERAL ((yyvsp[0]))->data);
  }
#line 9603 "parser.c" /* yacc.c:1646  */
    break;

  case 440:
#line 4626 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_3, &check_pic_duplicate);
	if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "GLOBAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "GLOBAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else if (current_storage == CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "GLOBAL");
	} else {
		current_field->flag_is_global = 1;
	}
  }
#line 9626 "parser.c" /* yacc.c:1646  */
    break;

  case 441:
#line 4651 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PICTURE", SYN_CLAUSE_4, &check_pic_duplicate);
	current_field->pic = CB_PICTURE ((yyvsp[0]));
  }
#line 9635 "parser.c" /* yacc.c:1646  */
    break;

  case 444:
#line 4667 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_BINARY);
  }
#line 9643 "parser.c" /* yacc.c:1646  */
    break;

  case 445:
#line 4671 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_BINARY);
  }
#line 9651 "parser.c" /* yacc.c:1646  */
    break;

  case 446:
#line 4675 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_FLOAT);
  }
#line 9659 "parser.c" /* yacc.c:1646  */
    break;

  case 447:
#line 4679 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_DOUBLE);
  }
#line 9667 "parser.c" /* yacc.c:1646  */
    break;

  case 448:
#line 4683 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_PACKED);
  }
#line 9675 "parser.c" /* yacc.c:1646  */
    break;

  case 449:
#line 4687 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_BINARY);
  }
#line 9683 "parser.c" /* yacc.c:1646  */
    break;

  case 450:
#line 4691 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_COMP_5);
  }
#line 9691 "parser.c" /* yacc.c:1646  */
    break;

  case 451:
#line 4695 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_COMP_6);
  }
#line 9699 "parser.c" /* yacc.c:1646  */
    break;

  case 452:
#line 4699 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_COMP_X);
  }
#line 9707 "parser.c" /* yacc.c:1646  */
    break;

  case 453:
#line 4703 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_DISPLAY);
  }
#line 9715 "parser.c" /* yacc.c:1646  */
    break;

  case 454:
#line 4707 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_INDEX);
  }
#line 9723 "parser.c" /* yacc.c:1646  */
    break;

  case 455:
#line 4711 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_PACKED);
  }
#line 9731 "parser.c" /* yacc.c:1646  */
    break;

  case 456:
#line 4715 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 9740 "parser.c" /* yacc.c:1646  */
    break;

  case 457:
#line 4720 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_PROGRAM_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 9749 "parser.c" /* yacc.c:1646  */
    break;

  case 458:
#line 4725 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 9757 "parser.c" /* yacc.c:1646  */
    break;

  case 459:
#line 4729 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 9765 "parser.c" /* yacc.c:1646  */
    break;

  case 460:
#line 4733 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
		check_set_usage (CB_USAGE_SIGNED_INT);
#else
		check_set_usage (CB_USAGE_SIGNED_LONG);
#endif
	}
#line 9777 "parser.c" /* yacc.c:1646  */
    break;

  case 461:
#line 4741 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 9785 "parser.c" /* yacc.c:1646  */
    break;

  case 462:
#line 4745 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 9793 "parser.c" /* yacc.c:1646  */
    break;

  case 463:
#line 4749 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
		check_set_usage (CB_USAGE_UNSIGNED_INT);
#else
		check_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
	}
#line 9805 "parser.c" /* yacc.c:1646  */
    break;

  case 464:
#line 4757 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_SIGNED_CHAR);
  }
#line 9813 "parser.c" /* yacc.c:1646  */
    break;

  case 465:
#line 4761 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_UNSIGNED_CHAR);
  }
#line 9821 "parser.c" /* yacc.c:1646  */
    break;

  case 466:
#line 4765 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 9829 "parser.c" /* yacc.c:1646  */
    break;

  case 467:
#line 4769 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 9837 "parser.c" /* yacc.c:1646  */
    break;

  case 468:
#line 4773 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 9845 "parser.c" /* yacc.c:1646  */
    break;

  case 469:
#line 4777 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 9853 "parser.c" /* yacc.c:1646  */
    break;

  case 470:
#line 4781 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_SIGNED_LONG);
  }
#line 9861 "parser.c" /* yacc.c:1646  */
    break;

  case 471:
#line 4785 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_UNSIGNED_LONG);
  }
#line 9869 "parser.c" /* yacc.c:1646  */
    break;

  case 472:
#line 4789 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
		check_set_usage (CB_USAGE_SIGNED_INT);
#else
		check_set_usage (CB_USAGE_SIGNED_LONG);
#endif
	}
#line 9881 "parser.c" /* yacc.c:1646  */
    break;

  case 473:
#line 4797 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
		check_set_usage (CB_USAGE_UNSIGNED_INT);
#else
		check_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
	}
#line 9893 "parser.c" /* yacc.c:1646  */
    break;

  case 474:
#line 4805 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_FP_BIN32);
  }
#line 9901 "parser.c" /* yacc.c:1646  */
    break;

  case 475:
#line 4809 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_FP_BIN64);
  }
#line 9909 "parser.c" /* yacc.c:1646  */
    break;

  case 476:
#line 4813 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_FP_BIN128);
  }
#line 9917 "parser.c" /* yacc.c:1646  */
    break;

  case 477:
#line 4817 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_FP_DEC64);
  }
#line 9925 "parser.c" /* yacc.c:1646  */
    break;

  case 478:
#line 4821 "parser.y" /* yacc.c:1646  */
    {
	check_set_usage (CB_USAGE_FP_DEC128);
  }
#line 9933 "parser.c" /* yacc.c:1646  */
    break;

  case 479:
#line 4825 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	CB_PENDING ("USAGE NATIONAL");
  }
#line 9942 "parser.c" /* yacc.c:1646  */
    break;

  case 484:
#line 4845 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 1;
  }
#line 9952 "parser.c" /* yacc.c:1646  */
    break;

  case 485:
#line 4851 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 0;
  }
#line 9962 "parser.c" /* yacc.c:1646  */
    break;

  case 486:
#line 4864 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->depending && !((yyvsp[-3]))) {
		cb_verify (cb_odo_without_to, _("ODO without TO clause"));
	}
	current_field->occurs_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-4])) : 1;
	current_field->occurs_max = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : cb_get_int ((yyvsp[-4]));
	current_field->indexes++;
	if (current_field->indexes > COB_MAX_SUBSCRIPTS) {
		cb_error (_("maximum OCCURS depth exceeded (%d)"),
			  COB_MAX_SUBSCRIPTS);
	}
	current_field->flag_occurs = 1;
  }
#line 9981 "parser.c" /* yacc.c:1646  */
    break;

  case 488:
#line 4882 "parser.y" /* yacc.c:1646  */
    {
	current_field->step_count = cb_get_int ((yyvsp[0]));
  }
#line 9989 "parser.c" /* yacc.c:1646  */
    break;

  case 489:
#line 4892 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->indexes == COB_MAX_SUBSCRIPTS) {
		cb_error (_("maximum OCCURS depth exceeded (%d)"),
			  COB_MAX_SUBSCRIPTS);
	} else {
		current_field->indexes++;
	}
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	}
	if ((yyvsp[-4])) {
		current_field->occurs_min = cb_get_int ((yyvsp[-5]));
		current_field->occurs_max = cb_get_int ((yyvsp[-4]));
		if (current_field->depending &&
			current_field->occurs_max > 0 &&
			current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_min = 1;
		current_field->occurs_max = cb_get_int ((yyvsp[-5]));
		if (current_field->depending) {
			cb_verify (cb_odo_without_to, _("ODO without TO clause"));
		}
	}
	current_field->flag_occurs = 1;
  }
#line 10024 "parser.c" /* yacc.c:1646  */
    break;

  case 490:
#line 4924 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->indexes == COB_MAX_SUBSCRIPTS) {
		cb_error (_("maximum OCCURS depth exceeded (%d)"),
			  COB_MAX_SUBSCRIPTS);
	} else {
		current_field->indexes++;
	}
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	}
	current_field->occurs_min = (yyvsp[-4]) ? cb_get_int ((yyvsp[-4])) : 0;
	if ((yyvsp[-3])) {
		current_field->occurs_max = cb_get_int ((yyvsp[-3]));
		if (current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_max = 0;
	}
	CB_PENDING("OCCURS DYNAMIC");
	current_field->flag_occurs = 1;
  }
#line 10054 "parser.c" /* yacc.c:1646  */
    break;

  case 491:
#line 4952 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10060 "parser.c" /* yacc.c:1646  */
    break;

  case 492:
#line 4953 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 10066 "parser.c" /* yacc.c:1646  */
    break;

  case 493:
#line 4957 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10072 "parser.c" /* yacc.c:1646  */
    break;

  case 494:
#line 4958 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 10078 "parser.c" /* yacc.c:1646  */
    break;

  case 496:
#line 4963 "parser.y" /* yacc.c:1646  */
    {
	current_field->depending = (yyvsp[0]);
  }
#line 10086 "parser.c" /* yacc.c:1646  */
    break;

  case 498:
#line 4970 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_zero, 0, current_field);
	CB_FIELD_PTR ((yyval))->special_index = 1;
  }
#line 10095 "parser.c" /* yacc.c:1646  */
    break;

  case 500:
#line 4978 "parser.y" /* yacc.c:1646  */
    {
	/* current_field->initialized = 1; */
  }
#line 10103 "parser.c" /* yacc.c:1646  */
    break;

  case 501:
#line 4985 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = (yyvsp[0]);
		nkeys = cb_list_length ((yyvsp[0]));
		keys = cobc_parse_malloc (sizeof (struct cb_key) * nkeys);

		for (i = 0; i < nkeys; i++) {
			keys[i].dir = CB_PURPOSE_INT (l);
			keys[i].key = CB_VALUE (l);
			l = CB_CHAIN (l);
		}
		current_field->keys = keys;
		current_field->nkeys = nkeys;
	}
  }
#line 10128 "parser.c" /* yacc.c:1646  */
    break;

  case 502:
#line 5008 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10134 "parser.c" /* yacc.c:1646  */
    break;

  case 503:
#line 5011 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-3]);
		if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
		    strcasecmp (CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
			CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
		}
	}
	(yyval) = cb_list_append ((yyvsp[-4]), (yyvsp[0]));
  }
#line 10151 "parser.c" /* yacc.c:1646  */
    break;

  case 504:
#line 5026 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_ASCENDING); }
#line 10157 "parser.c" /* yacc.c:1646  */
    break;

  case 505:
#line 5027 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_DESCENDING); }
#line 10163 "parser.c" /* yacc.c:1646  */
    break;

  case 507:
#line 5032 "parser.y" /* yacc.c:1646  */
    {
	current_field->index_list = (yyvsp[0]);
  }
#line 10171 "parser.c" /* yacc.c:1646  */
    break;

  case 508:
#line 5038 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 10177 "parser.c" /* yacc.c:1646  */
    break;

  case 509:
#line 5040 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 10183 "parser.c" /* yacc.c:1646  */
    break;

  case 510:
#line 5045 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_int1, 1U, current_field);
	CB_FIELD_PTR ((yyval))->special_index = 1;
  }
#line 10192 "parser.c" /* yacc.c:1646  */
    break;

  case 511:
#line 5056 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("JUSTIFIED", SYN_CLAUSE_8, &check_pic_duplicate);
	current_field->flag_justified = 1;
  }
#line 10201 "parser.c" /* yacc.c:1646  */
    break;

  case 512:
#line 5067 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SYNCHRONIZED", SYN_CLAUSE_9, &check_pic_duplicate);
	current_field->flag_synchronized = 1;
  }
#line 10210 "parser.c" /* yacc.c:1646  */
    break;

  case 513:
#line 5078 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK", SYN_CLAUSE_10, &check_pic_duplicate);
	current_field->flag_blank_zero = 1;
  }
#line 10219 "parser.c" /* yacc.c:1646  */
    break;

  case 514:
#line 5089 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BASED", SYN_CLAUSE_11, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING &&
	    current_storage != CB_STORAGE_LINKAGE &&
	    current_storage != CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "BASED");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "BASED");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "BASED");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "REDEFINES");
	} else if (current_field->flag_any_length) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else {
		current_field->flag_item_based = 1;
	}
  }
#line 10246 "parser.c" /* yacc.c:1646  */
    break;

  case 515:
#line 5117 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE", SYN_CLAUSE_12, &check_pic_duplicate);
	current_field->values = (yyvsp[0]);
  }
#line 10255 "parser.c" /* yacc.c:1646  */
    break;

  case 517:
#line 5125 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 10261 "parser.c" /* yacc.c:1646  */
    break;

  case 518:
#line 5126 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 10267 "parser.c" /* yacc.c:1646  */
    break;

  case 519:
#line 5130 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 10273 "parser.c" /* yacc.c:1646  */
    break;

  case 520:
#line 5131 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0])); }
#line 10279 "parser.c" /* yacc.c:1646  */
    break;

  case 522:
#line 5136 "parser.y" /* yacc.c:1646  */
    {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = CB_LIST_INIT ((yyvsp[0]));
  }
#line 10290 "parser.c" /* yacc.c:1646  */
    break;

  case 523:
#line 5148 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else {
		current_field->flag_any_length = 1;
	}
  }
#line 10303 "parser.c" /* yacc.c:1646  */
    break;

  case 524:
#line 5157 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY NUMERIC");
	} else {
		current_field->flag_any_length = 1;
		current_field->flag_any_numeric = 1;
	}
  }
#line 10317 "parser.c" /* yacc.c:1646  */
    break;

  case 526:
#line 5172 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LOCAL_STORAGE_SECTION;
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "LOCAL-STORAGE");
	}
  }
#line 10330 "parser.c" /* yacc.c:1646  */
    break;

  case 527:
#line 5181 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->local_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 10340 "parser.c" /* yacc.c:1646  */
    break;

  case 529:
#line 5193 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LINKAGE_SECTION;
	current_storage = CB_STORAGE_LINKAGE;
  }
#line 10350 "parser.c" /* yacc.c:1646  */
    break;

  case 530:
#line 5199 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->linkage_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 10360 "parser.c" /* yacc.c:1646  */
    break;

  case 532:
#line 5210 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("REPORT SECTION");
	current_storage = CB_STORAGE_REPORT;
	cb_clear_real_field ();
  }
#line 10370 "parser.c" /* yacc.c:1646  */
    break;

  case 536:
#line 5226 "parser.y" /* yacc.c:1646  */
    {
	if (CB_INVALID_TREE ((yyvsp[0]))) {
		YYERROR;
	} else {
		current_report = CB_REPORT (cb_ref ((yyvsp[0])));
	}
	check_duplicate = 0;
  }
#line 10383 "parser.c" /* yacc.c:1646  */
    break;

  case 540:
#line 5241 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 10391 "parser.c" /* yacc.c:1646  */
    break;

  case 541:
#line 5248 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_1, &check_duplicate);
	cb_error (_("GLOBAL is not allowed with RD"));
  }
#line 10400 "parser.c" /* yacc.c:1646  */
    break;

  case 542:
#line 5253 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CODE", SYN_CLAUSE_2, &check_duplicate);
  }
#line 10408 "parser.c" /* yacc.c:1646  */
    break;

  case 545:
#line 5264 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONTROL", SYN_CLAUSE_3, &check_duplicate);
  }
#line 10416 "parser.c" /* yacc.c:1646  */
    break;

  case 549:
#line 5283 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PAGE", SYN_CLAUSE_4, &check_duplicate);
	if (!current_report->heading) {
		current_report->heading = 1;
	}
	if (!current_report->first_detail) {
		current_report->first_detail = current_report->heading;
	}
	if (!current_report->last_control) {
		if (current_report->last_detail) {
			current_report->last_control = current_report->last_detail;
		} else if (current_report->footing) {
			current_report->last_control = current_report->footing;
		} else {
			current_report->last_control = current_report->lines;
		}
	}
	if (!current_report->last_detail && !current_report->footing) {
		current_report->last_detail = current_report->lines;
		current_report->footing = current_report->lines;
	} else if (!current_report->last_detail) {
		current_report->last_detail = current_report->footing;
	} else if (!current_report->footing) {
		current_report->footing = current_report->last_detail;
	}
	if (current_report->heading > current_report->first_detail ||
	    current_report->first_detail > current_report->last_control ||
	    current_report->last_control > current_report->last_detail ||
	    current_report->last_detail > current_report->footing) {
		cb_error (_("invalid %s clause"), "PAGE");
	}
  }
#line 10453 "parser.c" /* yacc.c:1646  */
    break;

  case 550:
#line 5319 "parser.y" /* yacc.c:1646  */
    {
	current_report->lines = cb_get_int ((yyvsp[0]));
  }
#line 10461 "parser.c" /* yacc.c:1646  */
    break;

  case 551:
#line 5323 "parser.y" /* yacc.c:1646  */
    {
	current_report->lines = cb_get_int ((yyvsp[-3]));
	current_report->columns = cb_get_int ((yyvsp[-1]));
  }
#line 10470 "parser.c" /* yacc.c:1646  */
    break;

  case 552:
#line 5328 "parser.y" /* yacc.c:1646  */
    {
	current_report->lines = cb_get_int ((yyvsp[-1]));
  }
#line 10478 "parser.c" /* yacc.c:1646  */
    break;

  case 560:
#line 5348 "parser.y" /* yacc.c:1646  */
    {
	current_report->heading = cb_get_int ((yyvsp[0]));
  }
#line 10486 "parser.c" /* yacc.c:1646  */
    break;

  case 561:
#line 5355 "parser.y" /* yacc.c:1646  */
    {
	current_report->first_detail = cb_get_int ((yyvsp[0]));
  }
#line 10494 "parser.c" /* yacc.c:1646  */
    break;

  case 562:
#line 5362 "parser.y" /* yacc.c:1646  */
    {
	current_report->last_control = cb_get_int ((yyvsp[0]));
  }
#line 10502 "parser.c" /* yacc.c:1646  */
    break;

  case 563:
#line 5369 "parser.y" /* yacc.c:1646  */
    {
	current_report->last_detail = cb_get_int ((yyvsp[0]));
  }
#line 10510 "parser.c" /* yacc.c:1646  */
    break;

  case 564:
#line 5376 "parser.y" /* yacc.c:1646  */
    {
	current_report->footing = cb_get_int ((yyvsp[0]));
  }
#line 10518 "parser.c" /* yacc.c:1646  */
    break;

  case 567:
#line 5387 "parser.y" /* yacc.c:1646  */
    {
	check_pic_duplicate = 0;
  }
#line 10526 "parser.c" /* yacc.c:1646  */
    break;

  case 587:
#line 5418 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TYPE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 10534 "parser.c" /* yacc.c:1646  */
    break;

  case 600:
#line 5444 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NEXT GROUP", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 10542 "parser.c" /* yacc.c:1646  */
    break;

  case 601:
#line 5451 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SUM", SYN_CLAUSE_19, &check_pic_duplicate);
  }
#line 10550 "parser.c" /* yacc.c:1646  */
    break;

  case 606:
#line 5467 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
  }
#line 10558 "parser.c" /* yacc.c:1646  */
    break;

  case 608:
#line 5478 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_21, &check_pic_duplicate);
  }
#line 10566 "parser.c" /* yacc.c:1646  */
    break;

  case 611:
#line 5490 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_18, &check_pic_duplicate);
  }
#line 10574 "parser.c" /* yacc.c:1646  */
    break;

  case 623:
#line 5523 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SOURCE", SYN_CLAUSE_22, &check_pic_duplicate);
  }
#line 10582 "parser.c" /* yacc.c:1646  */
    break;

  case 624:
#line 5530 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GROUP", SYN_CLAUSE_23, &check_pic_duplicate);
  }
#line 10590 "parser.c" /* yacc.c:1646  */
    break;

  case 625:
#line 5537 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("USAGE", SYN_CLAUSE_24, &check_pic_duplicate);
  }
#line 10598 "parser.c" /* yacc.c:1646  */
    break;

  case 627:
#line 5546 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_SCREEN;
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 10609 "parser.c" /* yacc.c:1646  */
    break;

  case 628:
#line 5553 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	if (description_field) {
		for (p = description_field; p; p = p->sister) {
			cb_validate_field (p);
		}
		current_program->screen_storage = description_field;
		current_program->flag_screen = 1;
	}
  }
#line 10625 "parser.c" /* yacc.c:1646  */
    break;

  case 634:
#line 5578 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree assocated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 10649 "parser.c" /* yacc.c:1646  */
    break;

  case 635:
#line 5598 "parser.y" /* yacc.c:1646  */
    {
	int	flags;

	if (current_field->parent) {
		flags = current_field->parent->screen_flag;
		flags &= ~COB_SCREEN_BLANK_LINE;
		flags &= ~COB_SCREEN_BLANK_SCREEN;
		flags &= ~COB_SCREEN_ERASE_EOL;
		flags &= ~COB_SCREEN_ERASE_EOS;
		flags &= ~COB_SCREEN_LINE_PLUS;
		flags &= ~COB_SCREEN_LINE_MINUS;
		flags &= ~COB_SCREEN_COLUMN_PLUS;
		flags &= ~COB_SCREEN_COLUMN_MINUS;

		flags = zero_conflicting_flags (current_field->screen_flag,
						flags);

		current_field->screen_flag |= flags;
	}

	if (current_field->screen_flag & COB_SCREEN_INITIAL) {
		if (!(current_field->screen_flag & COB_SCREEN_INPUT)) {
			cb_error (_("INITIAL specified on non-input field"));
		}
	}
	if (!qualifier) {
		current_field->flag_filler = 1;
	}

	if (likely (current_field)) {
		if (!description_field) {
			description_field = current_field;
		}
		if (current_field->flag_occurs
		    && !has_relative_pos (current_field)) {
			cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
		}
	}
  }
#line 10693 "parser.c" /* yacc.c:1646  */
    break;

  case 636:
#line 5638 "parser.y" /* yacc.c:1646  */
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if	1	/* RXWRXW Screen field */
	if (current_field) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
	}
#endif
	current_field = cb_get_real_field ();
  }
#line 10713 "parser.c" /* yacc.c:1646  */
    break;

  case 639:
#line 5661 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
					 "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 10722 "parser.c" /* yacc.c:1646  */
    break;

  case 640:
#line 5666 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
					 "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 10731 "parser.c" /* yacc.c:1646  */
    break;

  case 641:
#line 5671 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("BELL", COB_SCREEN_BELL);
  }
#line 10739 "parser.c" /* yacc.c:1646  */
    break;

  case 642:
#line 5675 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("BLINK", COB_SCREEN_BLINK);
  }
#line 10747 "parser.c" /* yacc.c:1646  */
    break;

  case 643:
#line 5679 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
					 "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 10756 "parser.c" /* yacc.c:1646  */
    break;

  case 644:
#line 5684 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
					 "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 10765 "parser.c" /* yacc.c:1646  */
    break;

  case 645:
#line 5689 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
					 "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 10774 "parser.c" /* yacc.c:1646  */
    break;

  case 646:
#line 5694 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
					 "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 10783 "parser.c" /* yacc.c:1646  */
    break;

  case 647:
#line 5699 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("REVERSE-VIDEO", COB_SCREEN_REVERSE);
  }
#line 10791 "parser.c" /* yacc.c:1646  */
    break;

  case 648:
#line 5703 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("UNDERLINE", COB_SCREEN_UNDERLINE);
  }
#line 10799 "parser.c" /* yacc.c:1646  */
    break;

  case 649:
#line 5707 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("OVERLINE", COB_SCREEN_OVERLINE);
	CB_PENDING ("OVERLINE");
  }
#line 10808 "parser.c" /* yacc.c:1646  */
    break;

  case 650:
#line 5712 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("GRID", COB_SCREEN_GRID);
	CB_PENDING ("GRID");
  }
#line 10817 "parser.c" /* yacc.c:1646  */
    break;

  case 651:
#line 5717 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("LEFTLINE", COB_SCREEN_LEFTLINE);
	CB_PENDING ("LEFTLINE");
  }
#line 10826 "parser.c" /* yacc.c:1646  */
    break;

  case 652:
#line 5722 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("AUTO", COB_SCREEN_AUTO);
  }
#line 10834 "parser.c" /* yacc.c:1646  */
    break;

  case 653:
#line 5726 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("SECURE", COB_SCREEN_SECURE);
  }
#line 10842 "parser.c" /* yacc.c:1646  */
    break;

  case 654:
#line 5730 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("REQUIRED", COB_SCREEN_REQUIRED);
  }
#line 10850 "parser.c" /* yacc.c:1646  */
    break;

  case 655:
#line 5734 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("FULL", COB_SCREEN_FULL);
  }
#line 10858 "parser.c" /* yacc.c:1646  */
    break;

  case 656:
#line 5738 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
	current_field->screen_prompt = (yyvsp[0]);
  }
#line 10867 "parser.c" /* yacc.c:1646  */
    break;

  case 657:
#line 5743 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
  }
#line 10875 "parser.c" /* yacc.c:1646  */
    break;

  case 658:
#line 5747 "parser.y" /* yacc.c:1646  */
    {
	check_screen_attr ("INITIAL", COB_SCREEN_INITIAL);
  }
#line 10883 "parser.c" /* yacc.c:1646  */
    break;

  case 659:
#line 5751 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_16, &check_pic_duplicate);
	current_field->screen_line = (yyvsp[0]);
  }
#line 10892 "parser.c" /* yacc.c:1646  */
    break;

  case 660:
#line 5756 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_17, &check_pic_duplicate);
	current_field->screen_column = (yyvsp[0]);
  }
#line 10901 "parser.c" /* yacc.c:1646  */
    break;

  case 661:
#line 5761 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_18, &check_pic_duplicate);
	current_field->screen_foreg = (yyvsp[0]);
  }
#line 10910 "parser.c" /* yacc.c:1646  */
    break;

  case 662:
#line 5766 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->screen_backg = (yyvsp[0]);
  }
#line 10919 "parser.c" /* yacc.c:1646  */
    break;

  case 671:
#line 5779 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));

	check_repeated ("USING", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
	current_field->screen_to = (yyvsp[0]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 10932 "parser.c" /* yacc.c:1646  */
    break;

  case 672:
#line 5788 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FROM", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
  }
#line 10941 "parser.c" /* yacc.c:1646  */
    break;

  case 673:
#line 5793 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));

	check_repeated ("TO", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->screen_to = (yyvsp[0]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 10953 "parser.c" /* yacc.c:1646  */
    break;

  case 682:
#line 5824 "parser.y" /* yacc.c:1646  */
    {
	/* Nothing */
  }
#line 10961 "parser.c" /* yacc.c:1646  */
    break;

  case 683:
#line 5828 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
#line 10969 "parser.c" /* yacc.c:1646  */
    break;

  case 684:
#line 5832 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
#line 10977 "parser.c" /* yacc.c:1646  */
    break;

  case 685:
#line 5839 "parser.y" /* yacc.c:1646  */
    {
	/* Nothing */
  }
#line 10985 "parser.c" /* yacc.c:1646  */
    break;

  case 686:
#line 5843 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
#line 10993 "parser.c" /* yacc.c:1646  */
    break;

  case 687:
#line 5847 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
#line 11001 "parser.c" /* yacc.c:1646  */
    break;

  case 688:
#line 5855 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OCCURS", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->occurs_max = cb_get_int ((yyvsp[-1]));
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
#line 11013 "parser.c" /* yacc.c:1646  */
    break;

  case 689:
#line 5866 "parser.y" /* yacc.c:1646  */
    {
	cb_error (_("GLOBAL is not allowed with screen items"));
  }
#line 11021 "parser.c" /* yacc.c:1646  */
    break;

  case 691:
#line 5875 "parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	cb_set_system_names ();
	header_check |= COBC_HD_PROCEDURE_DIVISION;
  }
#line 11035 "parser.c" /* yacc.c:1646  */
    break;

  case 692:
#line 5885 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main && !current_program->flag_chained && (yyvsp[-4])) {
		cb_error (_("executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	/* Main entry point */
	emit_entry (current_program->program_id, 0, (yyvsp[-4]));
	current_program->num_proc_params = cb_list_length ((yyvsp[-4]));
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, (yyvsp[-4]));
	}
  }
#line 11051 "parser.c" /* yacc.c:1646  */
    break;

  case 693:
#line 5897 "parser.y" /* yacc.c:1646  */
    {
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
  }
#line 11070 "parser.c" /* yacc.c:1646  */
    break;

  case 694:
#line 5912 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	/* No PROCEDURE DIVISION header ! */
	/* Only a statement is allowed as first element */
	/* Thereafter, sections/paragraphs may be used */
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	label = cb_build_reference ("MAIN SECTION");
	current_section = CB_LABEL (cb_build_label (label, NULL));
	current_section->flag_section = 1;
	current_section->flag_dummy_section = 1;
	current_section->flag_skip_label = !!skip_statements;
	current_section->flag_declaratives = !!in_declaratives;
	CB_TREE (current_section)->source_file = cb_source_file;
	CB_TREE (current_section)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_section));
	label = cb_build_reference ("MAIN PARAGRAPH");
	current_paragraph = CB_LABEL (cb_build_label (label, NULL));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_dummy_paragraph = 1;
	CB_TREE (current_paragraph)->source_file = cb_source_file;
	CB_TREE (current_paragraph)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_paragraph));
	cb_set_system_names ();
  }
#line 11103 "parser.c" /* yacc.c:1646  */
    break;

  case 696:
#line 5945 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 11111 "parser.c" /* yacc.c:1646  */
    break;

  case 697:
#line 5949 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 11120 "parser.c" /* yacc.c:1646  */
    break;

  case 698:
#line 5954 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > COB_MAX_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  COB_MAX_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 11132 "parser.c" /* yacc.c:1646  */
    break;

  case 699:
#line 5962 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("CHAINING invalid in user FUNCTION"));
	} else {
		current_program->flag_chained = 1;
	}
  }
#line 11145 "parser.c" /* yacc.c:1646  */
    break;

  case 700:
#line 5971 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > COB_MAX_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  COB_MAX_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 11157 "parser.c" /* yacc.c:1646  */
    break;

  case 701:
#line 5981 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11163 "parser.c" /* yacc.c:1646  */
    break;

  case 702:
#line 5983 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 11169 "parser.c" /* yacc.c:1646  */
    break;

  case 703:
#line 5988 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	struct cb_field	*f;

	x = cb_build_identifier ((yyvsp[0]), 0);
	if ((yyvsp[-1]) == cb_int1 && CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
		f = CB_FIELD (cb_ref (x));
		f->flag_is_pdiv_opt = 1;
	}

	if (call_mode == CB_CALL_BY_VALUE
	    && CB_REFERENCE_P ((yyvsp[0]))
	    && CB_FIELD (cb_ref ((yyvsp[0])))->flag_any_length) {
		cb_error_x ((yyvsp[0]), _("ANY LENGTH items may only be BY REFERENCE formal parameters"));
	}

	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), x);
	CB_SIZES ((yyval)) = size_mode;
  }
#line 11193 "parser.c" /* yacc.c:1646  */
    break;

  case 705:
#line 6012 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 11201 "parser.c" /* yacc.c:1646  */
    break;

  case 706:
#line 6016 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error (_("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		CB_PENDING (_("parameters passed BY VALUE"));
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 11214 "parser.c" /* yacc.c:1646  */
    break;

  case 708:
#line 6029 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
#line 11226 "parser.c" /* yacc.c:1646  */
    break;

  case 709:
#line 6037 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
#line 11238 "parser.c" /* yacc.c:1646  */
    break;

  case 710:
#line 6045 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO | CB_SIZE_UNSIGNED;
	}
  }
#line 11250 "parser.c" /* yacc.c:1646  */
    break;

  case 711:
#line 6053 "parser.y" /* yacc.c:1646  */
    {
	unsigned char *s = CB_LITERAL ((yyvsp[0]))->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
	} else {
		size_mode = CB_SIZE_UNSIGNED;
		switch (*s) {
		case '1':
			size_mode |= CB_SIZE_1;
			break;
		case '2':
			size_mode |= CB_SIZE_2;
			break;
		case '4':
			size_mode |= CB_SIZE_4;
			break;
		case '8':
			size_mode |= CB_SIZE_8;
			break;
		default:
			cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
			break;
		}
	}
  }
#line 11283 "parser.c" /* yacc.c:1646  */
    break;

  case 712:
#line 6082 "parser.y" /* yacc.c:1646  */
    {
	unsigned char *s = CB_LITERAL ((yyvsp[0]))->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
	} else {
		size_mode = 0;
		switch (*s) {
		case '1':
			size_mode = CB_SIZE_1;
			break;
		case '2':
			size_mode = CB_SIZE_2;
			break;
		case '4':
			size_mode = CB_SIZE_4;
			break;
		case '8':
			size_mode = CB_SIZE_8;
			break;
		default:
			cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
			break;
		}
	}
  }
#line 11316 "parser.c" /* yacc.c:1646  */
    break;

  case 713:
#line 6114 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 11324 "parser.c" /* yacc.c:1646  */
    break;

  case 714:
#line 6118 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
		(yyval) = cb_int0;
	} else {
		(yyval) = cb_int1;
	}
  }
#line 11337 "parser.c" /* yacc.c:1646  */
    break;

  case 715:
#line 6130 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
#line 11347 "parser.c" /* yacc.c:1646  */
    break;

  case 716:
#line 6136 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main) {
		cb_error (_("RETURNING clause cannot be OMITTED for main program"));
	}
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause cannot be OMITTED for a FUNCTION"));
	}
	current_program->flag_void = 1;
  }
#line 11361 "parser.c" /* yacc.c:1646  */
    break;

  case 717:
#line 6146 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
/* RXWRXW
		if (f->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		} else if (f->level != 1 && f->level != 77) {
*/
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01"));
		} else if(f->flag_occurs) {
			cb_error(_("RETURNING item should not have OCCURS"));
		} else if(f->storage == CB_STORAGE_LOCAL) {
			cb_error (_("RETURNING item should not be in LOCAL-STORAGE"));
		} else {
			if (current_program->prog_type == CB_FUNCTION_TYPE) {
				if (f->flag_any_length) {
					cb_error (_("function RETURNING item may not be ANY LENGTH"));
				}

				f->flag_is_returning = 1;
			}
			current_program->returning = (yyvsp[0]);
		}
	}
  }
#line 11394 "parser.c" /* yacc.c:1646  */
    break;

  case 719:
#line 6178 "parser.y" /* yacc.c:1646  */
    {
	in_declaratives = 1;
	emit_statement (cb_build_comment ("DECLARATIVES"));
  }
#line 11403 "parser.c" /* yacc.c:1646  */
    break;

  case 720:
#line 6184 "parser.y" /* yacc.c:1646  */
    {
	if (needs_field_debug) {
		start_debug = 1;
	}
	in_declaratives = 0;
	in_debugging = 0;
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		current_paragraph = NULL;
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		current_section->flag_fatal_check = 1;
		emit_statement (cb_build_perform_exit (current_section));
		current_section = NULL;
	}
	skip_statements = 0;
	emit_statement (cb_build_comment ("END DECLARATIVES"));
	check_unreached = 0;
  }
#line 11433 "parser.c" /* yacc.c:1646  */
    break;

  case 725:
#line 6222 "parser.y" /* yacc.c:1646  */
    {
	if (next_label_list) {
		cb_tree	plabel;
		char	name[32];

		snprintf (name, sizeof(name), "L$%d", next_label_id);
		plabel = cb_build_label (cb_build_reference (name), NULL);
		CB_LABEL (plabel)->flag_next_sentence = 1;
		emit_statement (plabel);
		current_program->label_list =
			cb_list_append (current_program->label_list, next_label_list);
		next_label_list = NULL;
		next_label_id++;
	}
	/* check_unreached = 0; */
  }
#line 11454 "parser.c" /* yacc.c:1646  */
    break;

  case 727:
#line 6240 "parser.y" /* yacc.c:1646  */
    {
	/* check_unreached = 0; */
  }
#line 11462 "parser.c" /* yacc.c:1646  */
    break;

  case 728:
#line 6250 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-3]), 0) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph/section */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
	if (current_program->flag_debugging && !in_debugging) {
		if (current_paragraph || current_section) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new section */
	current_section = CB_LABEL (cb_build_label ((yyvsp[-3]), NULL));
	if ((yyvsp[-1])) {
		current_section->segment = cb_get_int ((yyvsp[-1]));
	}
	current_section->flag_section = 1;
	/* Careful here, one negation */
	current_section->flag_real_label = !in_debugging;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->flag_skip_label = !!skip_statements;
	CB_TREE (current_section)->source_file = cb_source_file;
	CB_TREE (current_section)->source_line = cb_source_line;
	current_paragraph = NULL;
  }
#line 11510 "parser.c" /* yacc.c:1646  */
    break;

  case 729:
#line 6294 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (CB_TREE (current_section));
  }
#line 11518 "parser.c" /* yacc.c:1646  */
    break;

  case 732:
#line 6305 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 1) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		if (current_program->flag_debugging && !in_debugging) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new paragraph */
	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->flag_skip_label = !!skip_statements;
		CB_TREE (current_section)->source_file = cb_source_file;
		CB_TREE (current_section)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_section));
	}
	current_paragraph = CB_LABEL (cb_build_label ((yyvsp[-1]), current_section));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_real_label = !in_debugging;
	current_paragraph->segment = current_section->segment;
	CB_TREE (current_paragraph)->source_file = cb_source_file;
	CB_TREE (current_paragraph)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_paragraph));
  }
#line 11567 "parser.c" /* yacc.c:1646  */
    break;

  case 733:
#line 6353 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[0]), 0) != cb_error_node) {
		if (is_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a statement"), CB_NAME ((yyvsp[0])));
		} else if (is_default_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'; it may exist in another dialect"),
				    CB_NAME ((yyvsp[0])));
		} else {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'"), CB_NAME ((yyvsp[0])));
		}
	}
	YYERROR;
  }
#line 11587 "parser.c" /* yacc.c:1646  */
    break;

  case 734:
#line 6372 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 11595 "parser.c" /* yacc.c:1646  */
    break;

  case 735:
#line 6376 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives) {
		cb_error (_("SECTION segment invalid within DECLARATIVE"));
	}
	if (cb_verify (cb_section_segments, _("SECTION segment"))) {
		current_program->flag_segments = 1;
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = NULL;
	}
  }
#line 11611 "parser.c" /* yacc.c:1646  */
    break;

  case 736:
#line 6394 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = current_program->exec_list;
	current_program->exec_list = NULL;
	check_unreached = 0;
  }
#line 11621 "parser.c" /* yacc.c:1646  */
    break;

  case 737:
#line 6399 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_TREE (current_statement);
	current_statement = NULL;
  }
#line 11630 "parser.c" /* yacc.c:1646  */
    break;

  case 738:
#line 6404 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = (yyvsp[-2]);
	current_statement = CB_STATEMENT ((yyvsp[-1]));
  }
#line 11640 "parser.c" /* yacc.c:1646  */
    break;

  case 739:
#line 6412 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_skip_label = !!skip_statements;
		current_section->flag_declaratives = !!in_declaratives;
		CB_TREE (current_section)->source_file = cb_source_file;
		CB_TREE (current_section)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_section));
	}
	if (!current_paragraph) {
		label = cb_build_reference ("MAIN PARAGRAPH");
		current_paragraph = CB_LABEL (cb_build_label (label, NULL));
		current_paragraph->flag_declaratives = !!in_declaratives;
		current_paragraph->flag_skip_label = !!skip_statements;
		current_paragraph->flag_dummy_paragraph = 1;
		CB_TREE (current_paragraph)->source_file = cb_source_file;
		CB_TREE (current_paragraph)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_paragraph));
	}
	check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0);
  }
#line 11671 "parser.c" /* yacc.c:1646  */
    break;

  case 740:
#line 6439 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 11679 "parser.c" /* yacc.c:1646  */
    break;

  case 741:
#line 6443 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 11687 "parser.c" /* yacc.c:1646  */
    break;

  case 791:
#line 6499 "parser.y" /* yacc.c:1646  */
    {
	if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE")) {
		cb_tree label;
		char	name[32];

		begin_statement ("NEXT SENTENCE", 0);
		sprintf (name, "L$%d", next_label_id);
		label = cb_build_reference (name);
		next_label_list = cb_list_add (next_label_list, label);
		emit_statement (cb_build_goto (label, NULL));
	}
	check_unreached = 0;
  }
#line 11705 "parser.c" /* yacc.c:1646  */
    break;

  case 792:
#line 6513 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cobc_cs_check = 0;
  }
#line 11714 "parser.c" /* yacc.c:1646  */
    break;

  case 793:
#line 6524 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ACCEPT", TERM_ACCEPT);
	cobc_cs_check = CB_CS_ACCEPT;
	if (cb_accept_update) {
		check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto) {
		check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_AUTO);
	}
  }
#line 11729 "parser.c" /* yacc.c:1646  */
    break;

  case 795:
#line 6540 "parser.y" /* yacc.c:1646  */
    {
	  check_duplicate = 0;
	  check_line_col_duplicate = 0;
	  line_column = NULL;
  }
#line 11739 "parser.c" /* yacc.c:1646  */
    break;

  case 796:
#line 6546 "parser.y" /* yacc.c:1646  */
    {
	/* Check for invalid use of screen clauses */
	  if (current_statement->attr_ptr
	      || (!is_screen_field ((yyvsp[-3])) && line_column)) {
		  cb_verify_x ((yyvsp[-3]), cb_accept_display_extensions,
			       _("non-standard ACCEPT"));
	  }

	cobc_cs_check = 0;
	cb_emit_accept ((yyvsp[-3]), line_column, current_statement->attr_ptr);
  }
#line 11755 "parser.c" /* yacc.c:1646  */
    break;

  case 797:
#line 6558 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 0);
  }
#line 11763 "parser.c" /* yacc.c:1646  */
    break;

  case 798:
#line 6562 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 1);
  }
#line 11771 "parser.c" /* yacc.c:1646  */
    break;

  case 799:
#line 6566 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date_yyyymmdd ((yyvsp[-3]));
  }
#line 11780 "parser.c" /* yacc.c:1646  */
    break;

  case 800:
#line 6571 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date ((yyvsp[-2]));
  }
#line 11789 "parser.c" /* yacc.c:1646  */
    break;

  case 801:
#line 6576 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day_yyyyddd ((yyvsp[-3]));
  }
#line 11798 "parser.c" /* yacc.c:1646  */
    break;

  case 802:
#line 6581 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day ((yyvsp[-2]));
  }
#line 11807 "parser.c" /* yacc.c:1646  */
    break;

  case 803:
#line 6586 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_day_of_week ((yyvsp[-2]));
  }
#line 11815 "parser.c" /* yacc.c:1646  */
    break;

  case 804:
#line 6590 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_escape_key ((yyvsp[-3]));
  }
#line 11823 "parser.c" /* yacc.c:1646  */
    break;

  case 805:
#line 6594 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_exception_status ((yyvsp[-3]));
  }
#line 11831 "parser.c" /* yacc.c:1646  */
    break;

  case 806:
#line 6598 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_time ((yyvsp[-2]));
  }
#line 11839 "parser.c" /* yacc.c:1646  */
    break;

  case 807:
#line 6602 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_user_name ((yyvsp[-3]));
  }
#line 11848 "parser.c" /* yacc.c:1646  */
    break;

  case 808:
#line 6607 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_command_line ((yyvsp[-2]));
  }
#line 11856 "parser.c" /* yacc.c:1646  */
    break;

  case 809:
#line 6611 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_environment ((yyvsp[-3]));
  }
#line 11864 "parser.c" /* yacc.c:1646  */
    break;

  case 810:
#line 6615 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_get_environment ((yyvsp[-1]), (yyvsp[-4]));
  }
#line 11872 "parser.c" /* yacc.c:1646  */
    break;

  case 811:
#line 6619 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_number ((yyvsp[-2]));
  }
#line 11880 "parser.c" /* yacc.c:1646  */
    break;

  case 812:
#line 6623 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_value ((yyvsp[-3]));
  }
#line 11888 "parser.c" /* yacc.c:1646  */
    break;

  case 813:
#line 6627 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 11896 "parser.c" /* yacc.c:1646  */
    break;

  case 814:
#line 6631 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_name ((yyvsp[-2]), (yyvsp[0]));
  }
#line 11904 "parser.c" /* yacc.c:1646  */
    break;

  case 816:
#line 6639 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 11912 "parser.c" /* yacc.c:1646  */
    break;

  case 822:
#line 6657 "parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("FROM CRT", SYN_CLAUSE_1, &check_duplicate);
  }
#line 11920 "parser.c" /* yacc.c:1646  */
    break;

  case 823:
#line 6661 "parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("MODE IS BLOCK", SYN_CLAUSE_2, &check_duplicate);
  }
#line 11928 "parser.c" /* yacc.c:1646  */
    break;

  case 827:
#line 6674 "parser.y" /* yacc.c:1646  */
    {
	check_attr_with_conflict ("LINE", SYN_CLAUSE_1,
				  _("AT screen-location"), SYN_CLAUSE_3,
				  &check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "LINE 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR ((yyvsp[0]), cb_int0);
	} else {
		CB_PAIR_X (line_column) = (yyvsp[0]);
	}
  }
#line 11948 "parser.c" /* yacc.c:1646  */
    break;

  case 828:
#line 6690 "parser.y" /* yacc.c:1646  */
    {
	check_attr_with_conflict ("COLUMN", SYN_CLAUSE_2,
				  _("AT screen-location"), SYN_CLAUSE_3,
				  &check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "COLUMN 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (cb_int0, (yyvsp[0]));
	} else {
		CB_PAIR_Y (line_column) = (yyvsp[0]);
	}
  }
#line 11968 "parser.c" /* yacc.c:1646  */
    break;

  case 829:
#line 6706 "parser.y" /* yacc.c:1646  */
    {
	check_attr_with_conflict (_("AT screen-location"), SYN_CLAUSE_3,
				  _("LINE or COLUMN"), SYN_CLAUSE_1 | SYN_CLAUSE_2,
				  &check_line_col_duplicate);

	cb_verify (cb_accept_display_extensions, "AT clause");

	line_column = (yyvsp[0]);
  }
#line 11982 "parser.c" /* yacc.c:1646  */
    break;

  case 830:
#line 6718 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11988 "parser.c" /* yacc.c:1646  */
    break;

  case 831:
#line 6722 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11994 "parser.c" /* yacc.c:1646  */
    break;

  case 832:
#line 6723 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 12000 "parser.c" /* yacc.c:1646  */
    break;

  case 833:
#line 6728 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 12008 "parser.c" /* yacc.c:1646  */
    break;

  case 834:
#line 6735 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_AUTO);
  }
#line 12016 "parser.c" /* yacc.c:1646  */
    break;

  case 835:
#line 6739 "parser.y" /* yacc.c:1646  */
    {
	if (cb_accept_auto) {
		remove_attrib (COB_SCREEN_AUTO);
	}
  }
#line 12026 "parser.c" /* yacc.c:1646  */
    break;

  case 836:
#line 6745 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_BELL);
  }
#line 12034 "parser.c" /* yacc.c:1646  */
    break;

  case 837:
#line 6749 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_BLINK);
  }
#line 12042 "parser.c" /* yacc.c:1646  */
    break;

  case 838:
#line 6753 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ACCEPT CONVERSION");
  }
#line 12050 "parser.c" /* yacc.c:1646  */
    break;

  case 839:
#line 6757 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_FULL);
  }
#line 12058 "parser.c" /* yacc.c:1646  */
    break;

  case 840:
#line 6761 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				     "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 12068 "parser.c" /* yacc.c:1646  */
    break;

  case 841:
#line 6767 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_LEFTLINE);
  }
#line 12076 "parser.c" /* yacc.c:1646  */
    break;

  case 842:
#line 6771 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_LOWER);
  }
#line 12084 "parser.c" /* yacc.c:1646  */
    break;

  case 843:
#line 6775 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "LOWLIGHT", COB_SCREEN_LOWLIGHT,
				     "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 12094 "parser.c" /* yacc.c:1646  */
    break;

  case 844:
#line 6781 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_NO_ECHO);
  }
#line 12102 "parser.c" /* yacc.c:1646  */
    break;

  case 845:
#line 6785 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_OVERLINE);
  }
#line 12110 "parser.c" /* yacc.c:1646  */
    break;

  case 846:
#line 6789 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, (yyvsp[0]), NULL, COB_SCREEN_PROMPT);
  }
#line 12118 "parser.c" /* yacc.c:1646  */
    break;

  case 847:
#line 6793 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_PROMPT);
  }
#line 12126 "parser.c" /* yacc.c:1646  */
    break;

  case 848:
#line 6797 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_REQUIRED);
  }
#line 12134 "parser.c" /* yacc.c:1646  */
    break;

  case 849:
#line 6801 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_REVERSE);
  }
#line 12142 "parser.c" /* yacc.c:1646  */
    break;

  case 850:
#line 6805 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_SECURE);
  }
#line 12150 "parser.c" /* yacc.c:1646  */
    break;

  case 851:
#line 6809 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 12158 "parser.c" /* yacc.c:1646  */
    break;

  case 852:
#line 6813 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 12166 "parser.c" /* yacc.c:1646  */
    break;

  case 853:
#line 6817 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_UNDERLINE);
  }
#line 12174 "parser.c" /* yacc.c:1646  */
    break;

  case 854:
#line 6821 "parser.y" /* yacc.c:1646  */
    {
	if (cb_accept_update) {
		remove_attrib (COB_SCREEN_UPDATE);
	}
  }
#line 12184 "parser.c" /* yacc.c:1646  */
    break;

  case 855:
#line 6827 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_UPDATE);
  }
#line 12192 "parser.c" /* yacc.c:1646  */
    break;

  case 856:
#line 6831 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_UPPER);
  }
#line 12200 "parser.c" /* yacc.c:1646  */
    break;

  case 857:
#line 6835 "parser.y" /* yacc.c:1646  */
    {
	check_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 12208 "parser.c" /* yacc.c:1646  */
    break;

  case 858:
#line 6839 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 12216 "parser.c" /* yacc.c:1646  */
    break;

  case 859:
#line 6843 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL, 0);
  }
#line 12224 "parser.c" /* yacc.c:1646  */
    break;

  case 860:
#line 6847 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL, COB_SCREEN_SCROLL_DOWN);
  }
#line 12232 "parser.c" /* yacc.c:1646  */
    break;

  case 861:
#line 6851 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 12240 "parser.c" /* yacc.c:1646  */
    break;

  case 864:
#line 6863 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ACCEPT);
  }
#line 12248 "parser.c" /* yacc.c:1646  */
    break;

  case 865:
#line 6867 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ACCEPT);
# if 0 /* activate only for debugging purposes for attribs */
	if (current_statement->attr_ptr) {
		printBits (current_statement->attr_ptr->dispattrs);
	} else {
		fprintf(stderr, "No Attribs\n");
	}
#endif
  }
#line 12263 "parser.c" /* yacc.c:1646  */
    break;

  case 866:
#line 6884 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ADD", TERM_ADD);
  }
#line 12271 "parser.c" /* yacc.c:1646  */
    break;

  case 868:
#line 6893 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '+', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 12279 "parser.c" /* yacc.c:1646  */
    break;

  case 869:
#line 6897 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list ((yyvsp[-4]), '+'));
  }
#line 12287 "parser.c" /* yacc.c:1646  */
    break;

  case 870:
#line 6901 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_add, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 12295 "parser.c" /* yacc.c:1646  */
    break;

  case 872:
#line 6908 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 12303 "parser.c" /* yacc.c:1646  */
    break;

  case 873:
#line 6915 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ADD);
  }
#line 12311 "parser.c" /* yacc.c:1646  */
    break;

  case 874:
#line 6919 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ADD);
  }
#line 12319 "parser.c" /* yacc.c:1646  */
    break;

  case 875:
#line 6929 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALLOCATE", 0);
	current_statement->flag_no_based = 1;
  }
#line 12328 "parser.c" /* yacc.c:1646  */
    break;

  case 877:
#line 6938 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_allocate ((yyvsp[-2]), (yyvsp[0]), NULL, (yyvsp[-1]));
  }
#line 12336 "parser.c" /* yacc.c:1646  */
    break;

  case 878:
#line 6942 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("ALLOCATE CHARACTERS requires RETURNING clause"));
	} else {
		cb_emit_allocate (NULL, (yyvsp[0]), (yyvsp[-3]), (yyvsp[-1]));
	}
  }
#line 12349 "parser.c" /* yacc.c:1646  */
    break;

  case 879:
#line 6953 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 12355 "parser.c" /* yacc.c:1646  */
    break;

  case 880:
#line 6954 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 12361 "parser.c" /* yacc.c:1646  */
    break;

  case 881:
#line 6962 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALTER", 0);
	cb_verify (cb_alter_statement, "ALTER");
  }
#line 12370 "parser.c" /* yacc.c:1646  */
    break;

  case 885:
#line 6976 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_alter ((yyvsp[-3]), (yyvsp[0]));
  }
#line 12378 "parser.c" /* yacc.c:1646  */
    break;

  case 888:
#line 6988 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CALL", TERM_CALL);
	cobc_cs_check = CB_CS_CALL;
	call_nothing = 0;
	cobc_allow_program_name = 1;
  }
#line 12389 "parser.c" /* yacc.c:1646  */
    break;

  case 890:
#line 7000 "parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 12397 "parser.c" /* yacc.c:1646  */
    break;

  case 891:
#line 7006 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_PROGRAM_TYPE
	    && !current_program->flag_recursive
	    && is_recursive_call ((yyvsp[-4]))) {
		cb_warning_x ((yyvsp[-4]), _("recursive program call - assuming RECURSIVE attribute"));
		current_program->flag_recursive = 1;
	}
	/* For CALL ... RETURNING NOTHING, set the call convention bit */
	if (call_nothing) {
		if ((yyvsp[-5]) && CB_INTEGER_P ((yyvsp[-5]))) {
			(yyvsp[-5]) = cb_int ((CB_INTEGER ((yyvsp[-5]))->val) | CB_CONV_NO_RET_UPD);
		} else {
			(yyvsp[-5]) = cb_int (CB_CONV_NO_RET_UPD);
		}
	}
	cb_emit_call ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[-1]), CB_PAIR_X ((yyvsp[0])), CB_PAIR_Y ((yyvsp[0])), (yyvsp[-5]));
  }
#line 12419 "parser.c" /* yacc.c:1646  */
    break;

  case 892:
#line 7027 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
#line 12428 "parser.c" /* yacc.c:1646  */
    break;

  case 893:
#line 7032 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STATIC_LINK);
	cobc_cs_check = 0;
  }
#line 12437 "parser.c" /* yacc.c:1646  */
    break;

  case 894:
#line 7037 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
	cobc_cs_check = 0;
  }
#line 12446 "parser.c" /* yacc.c:1646  */
    break;

  case 895:
#line 7042 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME(x)->token != CB_FEATURE_CONVENTION) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic name"));
			(yyval) = NULL;
		} else {
			(yyval) = CB_SYSTEM_NAME(x)->value;
		}
	} else {
		(yyval) = NULL;
	}
	cobc_cs_check = 0;
  }
#line 12467 "parser.c" /* yacc.c:1646  */
    break;

  case 897:
#line 7063 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
#line 12475 "parser.c" /* yacc.c:1646  */
    break;

  case 898:
#line 7070 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12483 "parser.c" /* yacc.c:1646  */
    break;

  case 899:
#line 7074 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 12492 "parser.c" /* yacc.c:1646  */
    break;

  case 900:
#line 7079 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > COB_MAX_FIELD_PARAMS) {
		cb_error_x (CB_TREE (current_statement),
			    _("number of parameters exceeds maximum %d"),
			    COB_MAX_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 12505 "parser.c" /* yacc.c:1646  */
    break;

  case 901:
#line 7090 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 12511 "parser.c" /* yacc.c:1646  */
    break;

  case 902:
#line 7092 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 12517 "parser.c" /* yacc.c:1646  */
    break;

  case 903:
#line 7097 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error_x (CB_TREE (current_statement),
			    _("OMITTED only allowed when parameters are passed BY REFERENCE"));
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), cb_null);
  }
#line 12529 "parser.c" /* yacc.c:1646  */
    break;

  case 904:
#line 7105 "parser.y" /* yacc.c:1646  */
    {
	int	save_mode;

	save_mode = call_mode;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P ((yyvsp[0])) || (CB_REFERENCE_P ((yyvsp[0])) &&
		    CB_FILE_P (CB_REFERENCE ((yyvsp[0]))->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			if (cb_category_is_alpha ((yyvsp[0]))) {
				cb_warning_x ((yyvsp[0]),
					      _("BY CONTENT assumed for alphanumeric item"));
				save_mode = CB_CALL_BY_CONTENT;
			}
		}
	}
	(yyval) = CB_BUILD_PAIR (cb_int (save_mode), (yyvsp[0]));
	CB_SIZES ((yyval)) = size_mode;
	call_mode = save_mode;
  }
#line 12555 "parser.c" /* yacc.c:1646  */
    break;

  case 906:
#line 7131 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 12563 "parser.c" /* yacc.c:1646  */
    break;

  case 907:
#line 7135 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY CONTENT");
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
#line 12576 "parser.c" /* yacc.c:1646  */
    break;

  case 908:
#line 7144 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 12589 "parser.c" /* yacc.c:1646  */
    break;

  case 909:
#line 7156 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12597 "parser.c" /* yacc.c:1646  */
    break;

  case 910:
#line 7160 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12605 "parser.c" /* yacc.c:1646  */
    break;

  case 911:
#line 7164 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 12613 "parser.c" /* yacc.c:1646  */
    break;

  case 912:
#line 7168 "parser.y" /* yacc.c:1646  */
    {
	call_nothing = CB_CONV_NO_RET_UPD;
	(yyval) = cb_null;
  }
#line 12622 "parser.c" /* yacc.c:1646  */
    break;

  case 913:
#line 7173 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01 or 77"));
			(yyval) = NULL;
		} else if (f->storage != CB_STORAGE_LINKAGE &&
			   !f->flag_item_based) {
			cb_error (_("RETURNING item must be a LINKAGE SECTION item or have BASED clause"));
			(yyval) = NULL;
		} else {
			(yyval) = cb_build_address ((yyvsp[0]));
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 12646 "parser.c" /* yacc.c:1646  */
    break;

  case 918:
#line 7206 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR (NULL, NULL);
  }
#line 12654 "parser.c" /* yacc.c:1646  */
    break;

  case 919:
#line 7210 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-1]), (yyvsp[0]));
  }
#line 12662 "parser.c" /* yacc.c:1646  */
    break;

  case 920:
#line 7214 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 12674 "parser.c" /* yacc.c:1646  */
    break;

  case 921:
#line 7225 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12682 "parser.c" /* yacc.c:1646  */
    break;

  case 922:
#line 7229 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12690 "parser.c" /* yacc.c:1646  */
    break;

  case 923:
#line 7236 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12698 "parser.c" /* yacc.c:1646  */
    break;

  case 924:
#line 7240 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_call_overflow, "ON OVERFLOW");
	(yyval) = (yyvsp[0]);
  }
#line 12707 "parser.c" /* yacc.c:1646  */
    break;

  case 925:
#line 7248 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12715 "parser.c" /* yacc.c:1646  */
    break;

  case 926:
#line 7252 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12723 "parser.c" /* yacc.c:1646  */
    break;

  case 927:
#line 7259 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12731 "parser.c" /* yacc.c:1646  */
    break;

  case 928:
#line 7266 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), CALL);
  }
#line 12739 "parser.c" /* yacc.c:1646  */
    break;

  case 929:
#line 7270 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), CALL);
  }
#line 12747 "parser.c" /* yacc.c:1646  */
    break;

  case 930:
#line 7280 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CANCEL", 0);
	cobc_allow_program_name = 1;
  }
#line 12756 "parser.c" /* yacc.c:1646  */
    break;

  case 931:
#line 7285 "parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 12764 "parser.c" /* yacc.c:1646  */
    break;

  case 932:
#line 7292 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 12772 "parser.c" /* yacc.c:1646  */
    break;

  case 933:
#line 7296 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 12780 "parser.c" /* yacc.c:1646  */
    break;

  case 935:
#line 7304 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
#line 12788 "parser.c" /* yacc.c:1646  */
    break;

  case 936:
#line 7313 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CLOSE", 0);
  }
#line 12796 "parser.c" /* yacc.c:1646  */
    break;

  case 938:
#line 7321 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 12805 "parser.c" /* yacc.c:1646  */
    break;

  case 939:
#line 7326 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 12814 "parser.c" /* yacc.c:1646  */
    break;

  case 940:
#line 7333 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NORMAL); }
#line 12820 "parser.c" /* yacc.c:1646  */
    break;

  case 941:
#line 7334 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT); }
#line 12826 "parser.c" /* yacc.c:1646  */
    break;

  case 942:
#line 7335 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT_REMOVAL); }
#line 12832 "parser.c" /* yacc.c:1646  */
    break;

  case 943:
#line 7336 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NO_REWIND); }
#line 12838 "parser.c" /* yacc.c:1646  */
    break;

  case 944:
#line 7337 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_LOCK); }
#line 12844 "parser.c" /* yacc.c:1646  */
    break;

  case 945:
#line 7345 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMPUTE", TERM_COMPUTE);
  }
#line 12852 "parser.c" /* yacc.c:1646  */
    break;

  case 947:
#line 7354 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-3]), 0, (yyvsp[-1]));
  }
#line 12860 "parser.c" /* yacc.c:1646  */
    break;

  case 948:
#line 7361 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), COMPUTE);
  }
#line 12868 "parser.c" /* yacc.c:1646  */
    break;

  case 949:
#line 7365 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), COMPUTE);
  }
#line 12876 "parser.c" /* yacc.c:1646  */
    break;

  case 950:
#line 7375 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMMIT", 0);
	cb_emit_commit ();
  }
#line 12885 "parser.c" /* yacc.c:1646  */
    break;

  case 951:
#line 7386 "parser.y" /* yacc.c:1646  */
    {
	size_t	save_unreached;

	/* Do not check unreached for CONTINUE */
	save_unreached = check_unreached;
	check_unreached = 0;
	begin_statement ("CONTINUE", 0);
	cb_emit_continue ();
	check_unreached = (unsigned int) save_unreached;
  }
#line 12900 "parser.c" /* yacc.c:1646  */
    break;

  case 952:
#line 7403 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DELETE", TERM_DELETE);
  }
#line 12908 "parser.c" /* yacc.c:1646  */
    break;

  case 954:
#line 7412 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_delete ((yyvsp[-2]));
  }
#line 12916 "parser.c" /* yacc.c:1646  */
    break;

  case 956:
#line 7420 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 12925 "parser.c" /* yacc.c:1646  */
    break;

  case 957:
#line 7425 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 12934 "parser.c" /* yacc.c:1646  */
    break;

  case 958:
#line 7433 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DELETE);
  }
#line 12942 "parser.c" /* yacc.c:1646  */
    break;

  case 959:
#line 7437 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DELETE);
  }
#line 12950 "parser.c" /* yacc.c:1646  */
    break;

  case 960:
#line 7447 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISPLAY", TERM_DISPLAY);
	cobc_cs_check = CB_CS_DISPLAY;
	display_type = UNKNOWN_DISPLAY;
	is_first_display_item = 1;
  }
#line 12961 "parser.c" /* yacc.c:1646  */
    break;

  case 962:
#line 7459 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_name ((yyvsp[-2]));
  }
#line 12969 "parser.c" /* yacc.c:1646  */
    break;

  case 963:
#line 7463 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_value ((yyvsp[-2]));
  }
#line 12977 "parser.c" /* yacc.c:1646  */
    break;

  case 964:
#line 7467 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arg_number ((yyvsp[-2]));
  }
#line 12985 "parser.c" /* yacc.c:1646  */
    break;

  case 965:
#line 7471 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_command_line ((yyvsp[-2]));
  }
#line 12993 "parser.c" /* yacc.c:1646  */
    break;

  case 967:
#line 7479 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != NULL) {
		error_if_different_display_type ((yyvsp[0]), NULL, NULL, NULL);
		cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 0,
				 display_type);
	}
  }
#line 13005 "parser.c" /* yacc.c:1646  */
    break;

  case 968:
#line 7487 "parser.y" /* yacc.c:1646  */
    {
	set_display_type ((yyvsp[0]), NULL, NULL, NULL);
	cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 1,
			 display_type);
  }
#line 13015 "parser.c" /* yacc.c:1646  */
    break;

  case 971:
#line 7501 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
  	advancing_value = cb_int1;
	upon_value = NULL;
	line_column = NULL;
  }
#line 13027 "parser.c" /* yacc.c:1646  */
    break;

  case 972:
#line 7509 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) == cb_null) {
		/* Emit DISPLAY OMITTED. */
		error_if_no_advancing_in_screen_display (advancing_value);
		cb_emit_display_omitted (line_column,
					 current_statement->attr_ptr);
	} else {
		/* Emit device or screen DISPLAY. */

		/*
		  Check that disp_list does not contain an invalid mix of fields.
		*/
		if (display_type == UNKNOWN_DISPLAY) {
			set_display_type ((yyvsp[-2]), upon_value, line_column,
					  current_statement->attr_ptr);
		} else {
		        error_if_different_display_type ((yyvsp[-2]), upon_value,
							 line_column,
							 current_statement->attr_ptr);
		}

		if (display_type == SCREEN_DISPLAY
		    || display_type == FIELD_ON_SCREEN_DISPLAY) {
			error_if_no_advancing_in_screen_display (advancing_value);
		}

		cb_emit_display ((yyvsp[-2]), upon_value, advancing_value, line_column,
				 current_statement->attr_ptr,
				 is_first_display_item, display_type);
	}

	is_first_display_item = 0;
  }
#line 13065 "parser.c" /* yacc.c:1646  */
    break;

  case 973:
#line 7546 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13073 "parser.c" /* yacc.c:1646  */
    break;

  case 974:
#line 7550 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("DISPLAY OMITTED");
	(yyval) = cb_null;
  }
#line 13082 "parser.c" /* yacc.c:1646  */
    break;

  case 977:
#line 7563 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPON", SYN_CLAUSE_1, &check_duplicate);
  }
#line 13090 "parser.c" /* yacc.c:1646  */
    break;

  case 978:
#line 7567 "parser.y" /* yacc.c:1646  */
    {
 	check_repeated ("NO ADVANCING", SYN_CLAUSE_2, &check_duplicate);
	advancing_value = cb_int0;
  }
#line 13099 "parser.c" /* yacc.c:1646  */
    break;

  case 979:
#line 7572 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 13107 "parser.c" /* yacc.c:1646  */
    break;

  case 982:
#line 7581 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_mnemonic ((yyvsp[0]));
  }
#line 13115 "parser.c" /* yacc.c:1646  */
    break;

  case 983:
#line 7585 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_name ((yyvsp[0]));
  }
#line 13123 "parser.c" /* yacc.c:1646  */
    break;

  case 984:
#line 7589 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_int0;
  }
#line 13131 "parser.c" /* yacc.c:1646  */
    break;

  case 985:
#line 7593 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_null;
  }
#line 13139 "parser.c" /* yacc.c:1646  */
    break;

  case 988:
#line 7605 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_BELL);
  }
#line 13147 "parser.c" /* yacc.c:1646  */
    break;

  case 989:
#line 7609 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "BLANK LINE", COB_SCREEN_BLANK_LINE,
				     "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 13157 "parser.c" /* yacc.c:1646  */
    break;

  case 990:
#line 7615 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				     "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 13167 "parser.c" /* yacc.c:1646  */
    break;

  case 991:
#line 7621 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_BLINK);
  }
#line 13175 "parser.c" /* yacc.c:1646  */
    break;

  case 992:
#line 7625 "parser.y" /* yacc.c:1646  */
    {
	cb_warning (_("ignoring CONVERSION"));
  }
#line 13183 "parser.c" /* yacc.c:1646  */
    break;

  case 993:
#line 7629 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "ERASE EOL", COB_SCREEN_ERASE_EOL,
				     "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 13193 "parser.c" /* yacc.c:1646  */
    break;

  case 994:
#line 7635 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "ERASE EOS", COB_SCREEN_ERASE_EOS,
				     "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 13203 "parser.c" /* yacc.c:1646  */
    break;

  case 995:
#line 7641 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				     "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 13213 "parser.c" /* yacc.c:1646  */
    break;

  case 996:
#line 7647 "parser.y" /* yacc.c:1646  */
    {
	check_attribs_with_conflict (NULL, NULL, NULL, NULL, NULL, NULL,
				     "LOWLIGHT", COB_SCREEN_LOWLIGHT,
				     "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 13223 "parser.c" /* yacc.c:1646  */
    break;

  case 997:
#line 7653 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_OVERLINE);
  }
#line 13231 "parser.c" /* yacc.c:1646  */
    break;

  case 998:
#line 7657 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_REVERSE);
  }
#line 13239 "parser.c" /* yacc.c:1646  */
    break;

  case 999:
#line 7661 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 13247 "parser.c" /* yacc.c:1646  */
    break;

  case 1000:
#line 7665 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, NULL, NULL, NULL, NULL, COB_SCREEN_UNDERLINE);
  }
#line 13255 "parser.c" /* yacc.c:1646  */
    break;

  case 1001:
#line 7669 "parser.y" /* yacc.c:1646  */
    {
	check_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 13263 "parser.c" /* yacc.c:1646  */
    break;

  case 1002:
#line 7673 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 13271 "parser.c" /* yacc.c:1646  */
    break;

  case 1003:
#line 7677 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL, 0);
  }
#line 13279 "parser.c" /* yacc.c:1646  */
    break;

  case 1004:
#line 7681 "parser.y" /* yacc.c:1646  */
    {
	check_attribs (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL, COB_SCREEN_SCROLL_DOWN);
  }
#line 13287 "parser.c" /* yacc.c:1646  */
    break;

  case 1005:
#line 7688 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DISPLAY);
  }
#line 13295 "parser.c" /* yacc.c:1646  */
    break;

  case 1006:
#line 7692 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DISPLAY);
  }
#line 13303 "parser.c" /* yacc.c:1646  */
    break;

  case 1007:
#line 7702 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DIVIDE", TERM_DIVIDE);
  }
#line 13311 "parser.c" /* yacc.c:1646  */
    break;

  case 1009:
#line 7711 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '/', (yyvsp[-3]));
  }
#line 13319 "parser.c" /* yacc.c:1646  */
    break;

  case 1010:
#line 7715 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-3]), '/', (yyvsp[-5])));
  }
#line 13327 "parser.c" /* yacc.c:1646  */
    break;

  case 1011:
#line 7719 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '/', (yyvsp[-3])));
  }
#line 13335 "parser.c" /* yacc.c:1646  */
    break;

  case 1012:
#line 7723 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-5]), (yyvsp[-7]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 13343 "parser.c" /* yacc.c:1646  */
    break;

  case 1013:
#line 7727 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-7]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 13351 "parser.c" /* yacc.c:1646  */
    break;

  case 1014:
#line 7734 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DIVIDE);
  }
#line 13359 "parser.c" /* yacc.c:1646  */
    break;

  case 1015:
#line 7738 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DIVIDE);
  }
#line 13367 "parser.c" /* yacc.c:1646  */
    break;

  case 1016:
#line 7748 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	begin_statement ("ENTRY", 0);
  }
#line 13376 "parser.c" /* yacc.c:1646  */
    break;

  case 1018:
#line 7757 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "ENTRY");
	} else if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "ENTRY");
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (!cobc_check_valid_name ((char *)(CB_LITERAL ((yyvsp[-1]))->data), 1U)) {
			emit_entry ((char *)(CB_LITERAL ((yyvsp[-1]))->data), 1, (yyvsp[0]));
		}
	}
  }
#line 13392 "parser.c" /* yacc.c:1646  */
    break;

  case 1019:
#line 7775 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EVALUATE", TERM_EVALUATE);
	eval_level++;
	if (eval_level >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_level = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		YYERROR;
	} else {
		for (eval_inc = 0; eval_inc < EVAL_DEPTH; ++eval_inc) {
			eval_check[eval_level][eval_inc] = NULL;
		}
		eval_inc = 0;
		eval_inc2 = 0;
	}
  }
#line 13415 "parser.c" /* yacc.c:1646  */
    break;

  case 1021:
#line 7799 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_evaluate ((yyvsp[-1]), (yyvsp[0]));
	eval_level--;
  }
#line 13424 "parser.c" /* yacc.c:1646  */
    break;

  case 1022:
#line 7806 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 13430 "parser.c" /* yacc.c:1646  */
    break;

  case 1023:
#line 7808 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 13436 "parser.c" /* yacc.c:1646  */
    break;

  case 1024:
#line 7813 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	eval_check[eval_level][eval_inc++] = (yyvsp[0]);
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 13451 "parser.c" /* yacc.c:1646  */
    break;

  case 1025:
#line 7824 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
	eval_check[eval_level][eval_inc++] = NULL;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 13466 "parser.c" /* yacc.c:1646  */
    break;

  case 1026:
#line 7835 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_false;
	eval_check[eval_level][eval_inc++] = NULL;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 13481 "parser.c" /* yacc.c:1646  */
    break;

  case 1027:
#line 7849 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 13489 "parser.c" /* yacc.c:1646  */
    break;

  case 1028:
#line 7853 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13497 "parser.c" /* yacc.c:1646  */
    break;

  case 1029:
#line 7859 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 13503 "parser.c" /* yacc.c:1646  */
    break;

  case 1030:
#line 7861 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 13509 "parser.c" /* yacc.c:1646  */
    break;

  case 1031:
#line 7867 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), (yyvsp[-1]));
	eval_inc2 = 0;
  }
#line 13518 "parser.c" /* yacc.c:1646  */
    break;

  case 1032:
#line 7876 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), NULL);
	eval_inc2 = 0;
  }
#line 13527 "parser.c" /* yacc.c:1646  */
    break;

  case 1033:
#line 7884 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
	eval_inc2 = 0;
  }
#line 13536 "parser.c" /* yacc.c:1646  */
    break;

  case 1034:
#line 7890 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
	eval_inc2 = 0;
  }
#line 13545 "parser.c" /* yacc.c:1646  */
    break;

  case 1035:
#line 7897 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 13551 "parser.c" /* yacc.c:1646  */
    break;

  case 1036:
#line 7899 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 13557 "parser.c" /* yacc.c:1646  */
    break;

  case 1037:
#line 7904 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	not0;
	cb_tree	e1;
	cb_tree	e2;
	cb_tree	x;
	cb_tree	parm1;

	not0 = cb_int0;
	e2 = (yyvsp[0]);
	x = NULL;
	parm1 = (yyvsp[-1]);
	if (eval_check[eval_level][eval_inc2]) {
		/* Check if the first token is NOT */
		/* It may belong to the EVALUATE, however see */
		/* below when it may be part of a partial expression */
		if (CB_PURPOSE_INT (parm1) == '!') {
			/* Pop stack if subject not TRUE / FALSE */
			not0 = cb_int1;
			x = parm1;
			parm1 = CB_CHAIN (parm1);
		}
		/* Partial expression handling */
		switch (CB_PURPOSE_INT (parm1)) {
		/* Relational conditions */
		case '<':
		case '>':
		case '[':
		case ']':
		case '~':
		case '=':
		/* Class conditions */
		case '9':
		case 'A':
		case 'L':
		case 'U':
		case 'P':
		case 'N':
		case 'O':
		case 'C':
			if (e2) {
				cb_error_x (e2, _("invalid THROUGH usage"));
				e2 = NULL;
			}
			not0 = CB_PURPOSE (parm1);
			if (x) {
				/* Rebind the NOT to the partial expression */
				parm1 = cb_build_list (cb_int ('!'), NULL, parm1);
			}
			/* Insert subject at head of list */
			parm1 = cb_build_list (cb_int ('x'),
					    eval_check[eval_level][eval_inc2], parm1);
			break;
		}
	}

	/* Build expr now */
	e1 = cb_build_expr (parm1);

	eval_inc2++;
	(yyval) = CB_BUILD_PAIR (not0, CB_BUILD_PAIR (e1, e2));
  }
#line 13623 "parser.c" /* yacc.c:1646  */
    break;

  case 1038:
#line 7965 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_any; eval_inc2++; }
#line 13629 "parser.c" /* yacc.c:1646  */
    break;

  case 1039:
#line 7966 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; eval_inc2++; }
#line 13635 "parser.c" /* yacc.c:1646  */
    break;

  case 1040:
#line 7967 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_false; eval_inc2++; }
#line 13641 "parser.c" /* yacc.c:1646  */
    break;

  case 1041:
#line 7971 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13647 "parser.c" /* yacc.c:1646  */
    break;

  case 1042:
#line 7972 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 13653 "parser.c" /* yacc.c:1646  */
    break;

  case 1043:
#line 7977 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), EVALUATE);
  }
#line 13661 "parser.c" /* yacc.c:1646  */
    break;

  case 1044:
#line 7981 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), EVALUATE);
  }
#line 13669 "parser.c" /* yacc.c:1646  */
    break;

  case 1045:
#line 7991 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EXIT", 0);
	cobc_cs_check = CB_CS_EXIT;
  }
#line 13678 "parser.c" /* yacc.c:1646  */
    break;

  case 1046:
#line 7996 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 13686 "parser.c" /* yacc.c:1646  */
    break;

  case 1048:
#line 8004 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != CB_PROGRAM_TYPE) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM not allowed within a FUNCTION"));
	}
	if (current_program->flag_main) {
		check_unreached = 0;
	} else {
		check_unreached = 1;
	}
	if ((yyvsp[0]) != NULL) {
		cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
	}
	current_statement->name = (const char *)"EXIT PROGRAM";
	cb_emit_exit (0);
  }
#line 13711 "parser.c" /* yacc.c:1646  */
    break;

  case 1049:
#line 8025 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != CB_FUNCTION_TYPE) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION only allowed within a FUNCTION"));
	}
	check_unreached = 1;
	current_statement->name = (const char *)"EXIT FUNCTION";
	cb_emit_exit (0);
  }
#line 13729 "parser.c" /* yacc.c:1646  */
    break;

  case 1050:
#line 8039 "parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->cycle_label) {
			sprintf (name, "EXIT PERFORM CYCLE %d", cb_id);
			p->cycle_label = cb_build_reference (name);
			plabel = cb_build_label (p->cycle_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM CYCLE";
		cb_emit_goto (CB_LIST_INIT (p->cycle_label), NULL);
	}
  }
#line 13755 "parser.c" /* yacc.c:1646  */
    break;

  case 1051:
#line 8061 "parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->exit_label) {
			sprintf (name, "EXIT PERFORM %d", cb_id);
			p->exit_label = cb_build_reference (name);
			plabel = cb_build_label (p->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM";
		cb_emit_goto (CB_LIST_INIT (p->exit_label), NULL);
	}
  }
#line 13781 "parser.c" /* yacc.c:1646  */
    break;

  case 1052:
#line 8083 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_section) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT SECTION is only valid with an active SECTION"));
	} else {
		if (!current_section->exit_label) {
			sprintf (name, "EXIT SECTION %d", cb_id);
			current_section->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_section->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT SECTION";
		cb_emit_goto (CB_LIST_INIT (current_section->exit_label), NULL);
	}
  }
#line 13805 "parser.c" /* yacc.c:1646  */
    break;

  case 1053:
#line 8103 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_paragraph) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PARAGRAPH is only valid with an active PARAGRAPH"));
	} else {
		if (!current_paragraph->exit_label) {
			sprintf (name, "EXIT PARAGRAPH %d", cb_id);
			current_paragraph->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_paragraph->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PARAGRAPH";
		cb_emit_goto (CB_LIST_INIT (current_paragraph->exit_label), NULL);
	}
  }
#line 13829 "parser.c" /* yacc.c:1646  */
    break;

  case 1054:
#line 8125 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13835 "parser.c" /* yacc.c:1646  */
    break;

  case 1055:
#line 8126 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 13841 "parser.c" /* yacc.c:1646  */
    break;

  case 1056:
#line 8134 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("FREE", 0);
	current_statement->flag_no_based = 1;
  }
#line 13850 "parser.c" /* yacc.c:1646  */
    break;

  case 1058:
#line 8143 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_free ((yyvsp[0]));
  }
#line 13858 "parser.c" /* yacc.c:1646  */
    break;

  case 1059:
#line 8153 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GENERATE", 0);
	CB_PENDING("GENERATE");
  }
#line 13867 "parser.c" /* yacc.c:1646  */
    break;

  case 1062:
#line 8169 "parser.y" /* yacc.c:1646  */
    {
	if (!current_paragraph->flag_statement) {
		current_paragraph->flag_first_is_goto = 1;
	}
	begin_statement ("GO TO", 0);
	save_debug = start_debug;
	start_debug = 0;
  }
#line 13880 "parser.c" /* yacc.c:1646  */
    break;

  case 1064:
#line 8182 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_goto ((yyvsp[-1]), (yyvsp[0]));
	start_debug = save_debug;
  }
#line 13889 "parser.c" /* yacc.c:1646  */
    break;

  case 1065:
#line 8190 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 1;
	(yyval) = NULL;
  }
#line 13898 "parser.c" /* yacc.c:1646  */
    break;

  case 1066:
#line 8195 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	(yyval) = (yyvsp[0]);
  }
#line 13907 "parser.c" /* yacc.c:1646  */
    break;

  case 1067:
#line 8206 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GOBACK", 0);
	check_unreached = 1;
	if ((yyvsp[0]) != NULL) {
		cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
	}
	cb_emit_exit (1U);
  }
#line 13920 "parser.c" /* yacc.c:1646  */
    break;

  case 1068:
#line 8221 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("IF", TERM_IF);
  }
#line 13928 "parser.c" /* yacc.c:1646  */
    break;

  case 1070:
#line 8230 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (3)]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 13936 "parser.c" /* yacc.c:1646  */
    break;

  case 1071:
#line 8234 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (2)]), NULL, (yyvsp[0]));
  }
#line 13944 "parser.c" /* yacc.c:1646  */
    break;

  case 1072:
#line 8238 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (1)]), (yyvsp[0]), NULL);
  }
#line 13952 "parser.c" /* yacc.c:1646  */
    break;

  case 1073:
#line 8245 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), IF);
  }
#line 13960 "parser.c" /* yacc.c:1646  */
    break;

  case 1074:
#line 8249 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), IF);
  }
#line 13968 "parser.c" /* yacc.c:1646  */
    break;

  case 1075:
#line 8259 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIALIZE", 0);
  }
#line 13976 "parser.c" /* yacc.c:1646  */
    break;

  case 1077:
#line 8268 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_initialize ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 13984 "parser.c" /* yacc.c:1646  */
    break;

  case 1078:
#line 8274 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13990 "parser.c" /* yacc.c:1646  */
    break;

  case 1079:
#line 8275 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 13996 "parser.c" /* yacc.c:1646  */
    break;

  case 1080:
#line 8279 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14002 "parser.c" /* yacc.c:1646  */
    break;

  case 1081:
#line 8280 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 14008 "parser.c" /* yacc.c:1646  */
    break;

  case 1082:
#line 8281 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-2]); }
#line 14014 "parser.c" /* yacc.c:1646  */
    break;

  case 1083:
#line 8286 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14022 "parser.c" /* yacc.c:1646  */
    break;

  case 1084:
#line 8290 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14030 "parser.c" /* yacc.c:1646  */
    break;

  case 1085:
#line 8297 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14038 "parser.c" /* yacc.c:1646  */
    break;

  case 1086:
#line 8302 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 14046 "parser.c" /* yacc.c:1646  */
    break;

  case 1087:
#line 8309 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[0]));
  }
#line 14054 "parser.c" /* yacc.c:1646  */
    break;

  case 1088:
#line 8315 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHABETIC); }
#line 14060 "parser.c" /* yacc.c:1646  */
    break;

  case 1089:
#line 8316 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC); }
#line 14066 "parser.c" /* yacc.c:1646  */
    break;

  case 1090:
#line 8317 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC); }
#line 14072 "parser.c" /* yacc.c:1646  */
    break;

  case 1091:
#line 8318 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
#line 14078 "parser.c" /* yacc.c:1646  */
    break;

  case 1092:
#line 8319 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
#line 14084 "parser.c" /* yacc.c:1646  */
    break;

  case 1093:
#line 8320 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL); }
#line 14090 "parser.c" /* yacc.c:1646  */
    break;

  case 1094:
#line 8321 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
#line 14096 "parser.c" /* yacc.c:1646  */
    break;

  case 1095:
#line 8326 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14104 "parser.c" /* yacc.c:1646  */
    break;

  case 1096:
#line 8330 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
  }
#line 14112 "parser.c" /* yacc.c:1646  */
    break;

  case 1097:
#line 8339 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIATE", 0);
	CB_PENDING("INITIATE");
  }
#line 14121 "parser.c" /* yacc.c:1646  */
    break;

  case 1099:
#line 8348 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 14131 "parser.c" /* yacc.c:1646  */
    break;

  case 1100:
#line 8354 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 14141 "parser.c" /* yacc.c:1646  */
    break;

  case 1101:
#line 8365 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INSPECT", 0);
	inspect_keyword = 0;
  }
#line 14150 "parser.c" /* yacc.c:1646  */
    break;

  case 1111:
#line 8393 "parser.y" /* yacc.c:1646  */
    {
	previous_tallying_phrase = NO_PHRASE;
	cb_init_tallying ();
  }
#line 14159 "parser.c" /* yacc.c:1646  */
    break;

  case 1112:
#line 8398 "parser.y" /* yacc.c:1646  */
    {
	if (!(previous_tallying_phrase == CHARACTERS_PHRASE
	      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
		cb_error (_("TALLYING clause is incomplete"));
	} else {
		cb_emit_inspect ((yyvsp[-3]), (yyvsp[0]), TALLYING_CLAUSE);
	}

	(yyval) = (yyvsp[-3]);
  }
#line 14174 "parser.c" /* yacc.c:1646  */
    break;

  case 1113:
#line 8414 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_inspect ((yyvsp[-2]), (yyvsp[0]), REPLACING_CLAUSE);
	inspect_keyword = 0;
  }
#line 14183 "parser.c" /* yacc.c:1646  */
    break;

  case 1114:
#line 8424 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	x = cb_build_converting ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
	cb_emit_inspect ((yyvsp[-5]), x, CONVERTING_CLAUSE);
  }
#line 14193 "parser.c" /* yacc.c:1646  */
    break;

  case 1115:
#line 8433 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14201 "parser.c" /* yacc.c:1646  */
    break;

  case 1116:
#line 8437 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 14209 "parser.c" /* yacc.c:1646  */
    break;

  case 1117:
#line 8444 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (FOR_PHRASE);
	(yyval) = cb_build_tallying_data ((yyvsp[-1]));
  }
#line 14218 "parser.c" /* yacc.c:1646  */
    break;

  case 1118:
#line 8449 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (CHARACTERS_PHRASE);
	(yyval) = cb_build_tallying_characters ((yyvsp[0]));
  }
#line 14227 "parser.c" /* yacc.c:1646  */
    break;

  case 1119:
#line 8454 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_all ();
  }
#line 14236 "parser.c" /* yacc.c:1646  */
    break;

  case 1120:
#line 8459 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_leading ();
  }
#line 14245 "parser.c" /* yacc.c:1646  */
    break;

  case 1121:
#line 8464 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_trailing ();
  }
#line 14254 "parser.c" /* yacc.c:1646  */
    break;

  case 1122:
#line 8469 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (VALUE_REGION_PHRASE);
	(yyval) = cb_build_tallying_value ((yyvsp[-1]), (yyvsp[0]));
  }
#line 14263 "parser.c" /* yacc.c:1646  */
    break;

  case 1123:
#line 8476 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14269 "parser.c" /* yacc.c:1646  */
    break;

  case 1124:
#line 8477 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 14275 "parser.c" /* yacc.c:1646  */
    break;

  case 1125:
#line 8482 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_replacing_characters ((yyvsp[-1]), (yyvsp[0]));
	inspect_keyword = 0;
  }
#line 14284 "parser.c" /* yacc.c:1646  */
    break;

  case 1126:
#line 8487 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14292 "parser.c" /* yacc.c:1646  */
    break;

  case 1128:
#line 8494 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 1; }
#line 14298 "parser.c" /* yacc.c:1646  */
    break;

  case 1129:
#line 8495 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 2; }
#line 14304 "parser.c" /* yacc.c:1646  */
    break;

  case 1130:
#line 8496 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 3; }
#line 14310 "parser.c" /* yacc.c:1646  */
    break;

  case 1131:
#line 8497 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 4; }
#line 14316 "parser.c" /* yacc.c:1646  */
    break;

  case 1132:
#line 8502 "parser.y" /* yacc.c:1646  */
    {
	switch (inspect_keyword) {
		case 1:
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 2:
			(yyval) = cb_build_replacing_leading ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 3:
			(yyval) = cb_build_replacing_first ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 4:
			(yyval) = cb_build_replacing_trailing ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		default:
			cb_error_x (CB_TREE (current_statement),
				    _("INSPECT missing ALL/FIRST/LEADING/TRAILING"));
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
	}
  }
#line 14342 "parser.c" /* yacc.c:1646  */
    break;

  case 1133:
#line 8529 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_inspect_region_start ();
  }
#line 14350 "parser.c" /* yacc.c:1646  */
    break;

  case 1134:
#line 8533 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 14358 "parser.c" /* yacc.c:1646  */
    break;

  case 1135:
#line 8537 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 14366 "parser.c" /* yacc.c:1646  */
    break;

  case 1136:
#line 8541 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 14374 "parser.c" /* yacc.c:1646  */
    break;

  case 1137:
#line 8545 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 14382 "parser.c" /* yacc.c:1646  */
    break;

  case 1138:
#line 8552 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_before", (yyvsp[0]));
  }
#line 14390 "parser.c" /* yacc.c:1646  */
    break;

  case 1139:
#line 8559 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_after", (yyvsp[0]));
  }
#line 14398 "parser.c" /* yacc.c:1646  */
    break;

  case 1140:
#line 8568 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MERGE", 0);
	current_statement->flag_merge = 1;
  }
#line 14407 "parser.c" /* yacc.c:1646  */
    break;

  case 1142:
#line 8580 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MOVE", 0);
  }
#line 14415 "parser.c" /* yacc.c:1646  */
    break;

  case 1144:
#line 8588 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14423 "parser.c" /* yacc.c:1646  */
    break;

  case 1145:
#line 8592 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move_corresponding ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14431 "parser.c" /* yacc.c:1646  */
    break;

  case 1146:
#line 8602 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MULTIPLY", TERM_MULTIPLY);
  }
#line 14439 "parser.c" /* yacc.c:1646  */
    break;

  case 1148:
#line 8611 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '*', (yyvsp[-3]));
  }
#line 14447 "parser.c" /* yacc.c:1646  */
    break;

  case 1149:
#line 8615 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '*', (yyvsp[-3])));
  }
#line 14455 "parser.c" /* yacc.c:1646  */
    break;

  case 1150:
#line 8622 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MULTIPLY);
  }
#line 14463 "parser.c" /* yacc.c:1646  */
    break;

  case 1151:
#line 8626 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MULTIPLY);
  }
#line 14471 "parser.c" /* yacc.c:1646  */
    break;

  case 1152:
#line 8636 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("OPEN", 0);
  }
#line 14479 "parser.c" /* yacc.c:1646  */
    break;

  case 1154:
#line 8644 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree x;

	if ((yyvsp[-2]) && (yyvsp[0])) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if ((yyvsp[0])) {
		x = (yyvsp[0]);
	} else {
		x = (yyvsp[-2]);
	}
	for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), (yyvsp[-3]), x);
		}
	}
  }
#line 14504 "parser.c" /* yacc.c:1646  */
    break;

  case 1155:
#line 8665 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree x;

	if ((yyvsp[-2]) && (yyvsp[0])) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if ((yyvsp[0])) {
		x = (yyvsp[0]);
	} else {
		x = (yyvsp[-2]);
	}
	for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), (yyvsp[-3]), x);
		}
	}
  }
#line 14529 "parser.c" /* yacc.c:1646  */
    break;

  case 1156:
#line 8688 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_INPUT); }
#line 14535 "parser.c" /* yacc.c:1646  */
    break;

  case 1157:
#line 8689 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_OUTPUT); }
#line 14541 "parser.c" /* yacc.c:1646  */
    break;

  case 1158:
#line 8690 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_I_O); }
#line 14547 "parser.c" /* yacc.c:1646  */
    break;

  case 1159:
#line 8691 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_EXTEND); }
#line 14553 "parser.c" /* yacc.c:1646  */
    break;

  case 1160:
#line 8695 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14559 "parser.c" /* yacc.c:1646  */
    break;

  case 1161:
#line 8696 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14565 "parser.c" /* yacc.c:1646  */
    break;

  case 1162:
#line 8700 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14571 "parser.c" /* yacc.c:1646  */
    break;

  case 1163:
#line 8701 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14577 "parser.c" /* yacc.c:1646  */
    break;

  case 1164:
#line 8702 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 14583 "parser.c" /* yacc.c:1646  */
    break;

  case 1165:
#line 8704 "parser.y" /* yacc.c:1646  */
    {
	(void)cb_verify (CB_OBSOLETE, "REVERSED");
	(yyval) = NULL;
  }
#line 14592 "parser.c" /* yacc.c:1646  */
    break;

  case 1166:
#line 8715 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PERFORM", TERM_PERFORM);
	/* Turn off field debug - PERFORM is special */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 14603 "parser.c" /* yacc.c:1646  */
    break;

  case 1168:
#line 8726 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[0]), (yyvsp[-1]));
	start_debug = save_debug;
  }
#line 14612 "parser.c" /* yacc.c:1646  */
    break;

  case 1169:
#line 8731 "parser.y" /* yacc.c:1646  */
    {
	CB_ADD_TO_CHAIN ((yyvsp[0]), perform_stack);
	/* Restore field debug before inline statements */
	start_debug = save_debug;
  }
#line 14622 "parser.c" /* yacc.c:1646  */
    break;

  case 1170:
#line 8737 "parser.y" /* yacc.c:1646  */
    {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 14631 "parser.c" /* yacc.c:1646  */
    break;

  case 1171:
#line 8742 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[-1]), NULL);
	start_debug = save_debug;
  }
#line 14640 "parser.c" /* yacc.c:1646  */
    break;

  case 1172:
#line 8750 "parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-4) - (0)]), PERFORM);
	}
  }
#line 14652 "parser.c" /* yacc.c:1646  */
    break;

  case 1173:
#line 8758 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), PERFORM);
  }
#line 14660 "parser.c" /* yacc.c:1646  */
    break;

  case 1174:
#line 8765 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), PERFORM);
  }
#line 14668 "parser.c" /* yacc.c:1646  */
    break;

  case 1175:
#line 8769 "parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-2) - (1)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-2) - (1)]), PERFORM);
	}
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
  }
#line 14682 "parser.c" /* yacc.c:1646  */
    break;

  case 1176:
#line 8782 "parser.y" /* yacc.c:1646  */
    {
	/* Return from $1 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[0]));
  }
#line 14693 "parser.c" /* yacc.c:1646  */
    break;

  case 1177:
#line 8789 "parser.y" /* yacc.c:1646  */
    {
	/* Return from $3 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[-2]))->flag_decl_ok = 1;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14705 "parser.c" /* yacc.c:1646  */
    break;

  case 1178:
#line 8800 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_once (NULL);
  }
#line 14713 "parser.c" /* yacc.c:1646  */
    break;

  case 1179:
#line 8804 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_times ((yyvsp[-1]));
	current_program->loop_counter++;
  }
#line 14722 "parser.c" /* yacc.c:1646  */
    break;

  case 1180:
#line 8809 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_forever (NULL);
  }
#line 14730 "parser.c" /* yacc.c:1646  */
    break;

  case 1181:
#line 8813 "parser.y" /* yacc.c:1646  */
    {
	cb_tree varying;

	if (!(yyvsp[0])) {
		(yyval) = cb_build_perform_forever (NULL);
	} else {
		varying = CB_LIST_INIT (cb_build_perform_varying (NULL, NULL, NULL, (yyvsp[0])));
		(yyval) = cb_build_perform_until ((yyvsp[-2]), varying);
	}
  }
#line 14745 "parser.c" /* yacc.c:1646  */
    break;

  case 1182:
#line 8824 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_until ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14753 "parser.c" /* yacc.c:1646  */
    break;

  case 1183:
#line 8830 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 14759 "parser.c" /* yacc.c:1646  */
    break;

  case 1184:
#line 8831 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14765 "parser.c" /* yacc.c:1646  */
    break;

  case 1185:
#line 8835 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14771 "parser.c" /* yacc.c:1646  */
    break;

  case 1186:
#line 8836 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14777 "parser.c" /* yacc.c:1646  */
    break;

  case 1187:
#line 8839 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 14783 "parser.c" /* yacc.c:1646  */
    break;

  case 1188:
#line 8841 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 14789 "parser.c" /* yacc.c:1646  */
    break;

  case 1189:
#line 8846 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_varying ((yyvsp[-6]), (yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 14797 "parser.c" /* yacc.c:1646  */
    break;

  case 1190:
#line 8856 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READ", TERM_READ);
  }
#line 14805 "parser.c" /* yacc.c:1646  */
    break;

  case 1192:
#line 8865 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-6]))) {
		struct cb_file	*cf;

		cf = CB_FILE(cb_ref ((yyvsp[-6])));
		if ((yyvsp[-2]) && (cf->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error_x (CB_TREE (current_statement),
				    _("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if ((yyvsp[-1]) &&
		      (cf->organization != COB_ORG_RELATIVE &&
		       cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("KEY clause invalid with this file type"));
		} else if (current_statement->handler_type == INVALID_KEY_HANDLER &&
			   (cf->organization != COB_ORG_RELATIVE &&
			    cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("INVALID KEY clause invalid with this file type"));
		} else {
			cb_emit_read ((yyvsp[-6]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[-2]));
		}
	}
  }
#line 14833 "parser.c" /* yacc.c:1646  */
    break;

  case 1193:
#line 8891 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14839 "parser.c" /* yacc.c:1646  */
    break;

  case 1194:
#line 8892 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14845 "parser.c" /* yacc.c:1646  */
    break;

  case 1195:
#line 8897 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14853 "parser.c" /* yacc.c:1646  */
    break;

  case 1196:
#line 8901 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int3;
  }
#line 14861 "parser.c" /* yacc.c:1646  */
    break;

  case 1197:
#line 8905 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 14869 "parser.c" /* yacc.c:1646  */
    break;

  case 1198:
#line 8909 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 14877 "parser.c" /* yacc.c:1646  */
    break;

  case 1199:
#line 8913 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 14885 "parser.c" /* yacc.c:1646  */
    break;

  case 1200:
#line 8917 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int3;
  }
#line 14893 "parser.c" /* yacc.c:1646  */
    break;

  case 1201:
#line 8921 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int4;
  }
#line 14901 "parser.c" /* yacc.c:1646  */
    break;

  case 1202:
#line 8927 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14907 "parser.c" /* yacc.c:1646  */
    break;

  case 1203:
#line 8928 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14913 "parser.c" /* yacc.c:1646  */
    break;

  case 1206:
#line 8938 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), READ);
  }
#line 14921 "parser.c" /* yacc.c:1646  */
    break;

  case 1207:
#line 8942 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), READ);
  }
#line 14929 "parser.c" /* yacc.c:1646  */
    break;

  case 1208:
#line 8952 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READY TRACE", 0);
	cb_emit_ready_trace ();
  }
#line 14938 "parser.c" /* yacc.c:1646  */
    break;

  case 1209:
#line 8962 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RELEASE", 0);
  }
#line 14946 "parser.c" /* yacc.c:1646  */
    break;

  case 1211:
#line 8970 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_release ((yyvsp[-1]), (yyvsp[0]));
  }
#line 14954 "parser.c" /* yacc.c:1646  */
    break;

  case 1212:
#line 8980 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RESET TRACE", 0);
	cb_emit_reset_trace ();
  }
#line 14963 "parser.c" /* yacc.c:1646  */
    break;

  case 1213:
#line 8990 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RETURN", TERM_RETURN);
  }
#line 14971 "parser.c" /* yacc.c:1646  */
    break;

  case 1215:
#line 8999 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_return ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 14979 "parser.c" /* yacc.c:1646  */
    break;

  case 1216:
#line 9006 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RETURN);
  }
#line 14987 "parser.c" /* yacc.c:1646  */
    break;

  case 1217:
#line 9010 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RETURN);
  }
#line 14995 "parser.c" /* yacc.c:1646  */
    break;

  case 1218:
#line 9020 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("REWRITE", TERM_REWRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 15006 "parser.c" /* yacc.c:1646  */
    break;

  case 1220:
#line 9032 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_rewrite ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
	start_debug = save_debug;
  }
#line 15015 "parser.c" /* yacc.c:1646  */
    break;

  case 1221:
#line 9040 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15023 "parser.c" /* yacc.c:1646  */
    break;

  case 1222:
#line 9044 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 15031 "parser.c" /* yacc.c:1646  */
    break;

  case 1223:
#line 9048 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 15039 "parser.c" /* yacc.c:1646  */
    break;

  case 1224:
#line 9055 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), REWRITE);
  }
#line 15047 "parser.c" /* yacc.c:1646  */
    break;

  case 1225:
#line 9059 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), REWRITE);
  }
#line 15055 "parser.c" /* yacc.c:1646  */
    break;

  case 1226:
#line 9069 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
#line 15064 "parser.c" /* yacc.c:1646  */
    break;

  case 1227:
#line 9080 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEARCH", TERM_SEARCH);
  }
#line 15072 "parser.c" /* yacc.c:1646  */
    break;

  case 1229:
#line 9089 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_search ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 15080 "parser.c" /* yacc.c:1646  */
    break;

  case 1230:
#line 9094 "parser.y" /* yacc.c:1646  */
    {
	current_statement->name = (const char *)"SEARCH ALL";
	cb_emit_search_all ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 15089 "parser.c" /* yacc.c:1646  */
    break;

  case 1231:
#line 9101 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15095 "parser.c" /* yacc.c:1646  */
    break;

  case 1232:
#line 9102 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15101 "parser.c" /* yacc.c:1646  */
    break;

  case 1233:
#line 9107 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15109 "parser.c" /* yacc.c:1646  */
    break;

  case 1234:
#line 9112 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15117 "parser.c" /* yacc.c:1646  */
    break;

  case 1235:
#line 9119 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 15125 "parser.c" /* yacc.c:1646  */
    break;

  case 1236:
#line 9123 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), (yyvsp[-1]));
  }
#line 15133 "parser.c" /* yacc.c:1646  */
    break;

  case 1237:
#line 9131 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_if_check_break ((yyvsp[-1]), (yyvsp[0]));
  }
#line 15141 "parser.c" /* yacc.c:1646  */
    break;

  case 1238:
#line 9138 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SEARCH);
  }
#line 15149 "parser.c" /* yacc.c:1646  */
    break;

  case 1239:
#line 9142 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SEARCH);
  }
#line 15157 "parser.c" /* yacc.c:1646  */
    break;

  case 1240:
#line 9152 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SET", 0);
	setattr_val_on = 0;
	setattr_val_off = 0;
	cobc_cs_check = CB_CS_SET;
  }
#line 15168 "parser.c" /* yacc.c:1646  */
    break;

  case 1241:
#line 9159 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 15176 "parser.c" /* yacc.c:1646  */
    break;

  case 1249:
#line 9175 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 15182 "parser.c" /* yacc.c:1646  */
    break;

  case 1250:
#line 9176 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 15188 "parser.c" /* yacc.c:1646  */
    break;

  case 1251:
#line 9180 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 15194 "parser.c" /* yacc.c:1646  */
    break;

  case 1252:
#line 9181 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 15200 "parser.c" /* yacc.c:1646  */
    break;

  case 1253:
#line 9188 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_setenv ((yyvsp[-2]), (yyvsp[0]));
  }
#line 15208 "parser.c" /* yacc.c:1646  */
    break;

  case 1254:
#line 9197 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_attribute ((yyvsp[-2]), setattr_val_on, setattr_val_off);
  }
#line 15216 "parser.c" /* yacc.c:1646  */
    break;

  case 1257:
#line 9209 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BELL);
  }
#line 15224 "parser.c" /* yacc.c:1646  */
    break;

  case 1258:
#line 9213 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BLINK);
  }
#line 15232 "parser.c" /* yacc.c:1646  */
    break;

  case 1259:
#line 9217 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_HIGHLIGHT);
	check_not_highlight_and_lowlight (setattr_val_on | setattr_val_off,
					  COB_SCREEN_HIGHLIGHT);
  }
#line 15242 "parser.c" /* yacc.c:1646  */
    break;

  case 1260:
#line 9223 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LOWLIGHT);
	check_not_highlight_and_lowlight (setattr_val_on | setattr_val_off,
					  COB_SCREEN_LOWLIGHT);
  }
#line 15252 "parser.c" /* yacc.c:1646  */
    break;

  case 1261:
#line 9229 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_REVERSE);
  }
#line 15260 "parser.c" /* yacc.c:1646  */
    break;

  case 1262:
#line 9233 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_UNDERLINE);
  }
#line 15268 "parser.c" /* yacc.c:1646  */
    break;

  case 1263:
#line 9237 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LEFTLINE);
  }
#line 15276 "parser.c" /* yacc.c:1646  */
    break;

  case 1264:
#line 9241 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_OVERLINE);
  }
#line 15284 "parser.c" /* yacc.c:1646  */
    break;

  case 1265:
#line 9250 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-3]), cb_build_ppointer ((yyvsp[0])));
  }
#line 15292 "parser.c" /* yacc.c:1646  */
    break;

  case 1266:
#line 9254 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-2]), (yyvsp[0]));
  }
#line 15300 "parser.c" /* yacc.c:1646  */
    break;

  case 1267:
#line 9263 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_up_down ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 15308 "parser.c" /* yacc.c:1646  */
    break;

  case 1270:
#line 9277 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_on_off ((yyvsp[-2]), (yyvsp[0]));
  }
#line 15316 "parser.c" /* yacc.c:1646  */
    break;

  case 1273:
#line 9291 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_true ((yyvsp[-2]));
  }
#line 15324 "parser.c" /* yacc.c:1646  */
    break;

  case 1274:
#line 9295 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_false ((yyvsp[-2]));
  }
#line 15332 "parser.c" /* yacc.c:1646  */
    break;

  case 1275:
#line 9304 "parser.y" /* yacc.c:1646  */
    {
	  cb_emit_set_last_exception_to_off ();
  }
#line 15340 "parser.c" /* yacc.c:1646  */
    break;

  case 1276:
#line 9313 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SORT", 0);
  }
#line 15348 "parser.c" /* yacc.c:1646  */
    break;

  case 1278:
#line 9321 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[-3]));
	if (CB_VALID_TREE (x)) {
		if (CB_INVALID_TREE ((yyvsp[-2]))) {
			if (CB_FILE_P (x)) {
				cb_error (_("file sort requires KEY phrase"));
			} else {
				/* FIXME: use key definition from OCCURS */
				cb_error (_("table sort without keys not implemented yet"));
			}
			(yyval) = NULL;
		} else {
			cb_emit_sort_init ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
			(yyval)= (yyvsp[-3]);
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 15374 "parser.c" /* yacc.c:1646  */
    break;

  case 1279:
#line 9343 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) && CB_VALID_TREE ((yyvsp[-6]))) {
		cb_emit_sort_finish ((yyvsp[-6]));
	}
  }
#line 15384 "parser.c" /* yacc.c:1646  */
    break;

  case 1280:
#line 9352 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15392 "parser.c" /* yacc.c:1646  */
    break;

  case 1281:
#line 9357 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree lparm;

	if ((yyvsp[0]) == NULL) {
		l = CB_LIST_INIT (NULL);
	} else {
		l = (yyvsp[0]);
	}
	lparm = l;
	for (; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-2]);
	}
	(yyval) = cb_list_append ((yyvsp[-4]), lparm);
  }
#line 15412 "parser.c" /* yacc.c:1646  */
    break;

  case 1282:
#line 9375 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15418 "parser.c" /* yacc.c:1646  */
    break;

  case 1283:
#line 9376 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 15424 "parser.c" /* yacc.c:1646  */
    break;

  case 1285:
#line 9381 "parser.y" /* yacc.c:1646  */
    {
	/* The OC sort is a stable sort. ie. Dups are per default in order */
	/* Therefore nothing to do here */
  }
#line 15433 "parser.c" /* yacc.c:1646  */
    break;

  case 1286:
#line 9388 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 15439 "parser.c" /* yacc.c:1646  */
    break;

  case 1287:
#line 9389 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_ref ((yyvsp[0])); }
#line 15445 "parser.c" /* yacc.c:1646  */
    break;

  case 1288:
#line 9394 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) && CB_FILE_P (cb_ref ((yyvsp[0])))) {
		cb_error (_("file sort requires USING or INPUT PROCEDURE"));
	}
  }
#line 15455 "parser.c" /* yacc.c:1646  */
    break;

  case 1289:
#line 9400 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-2])))) {
			cb_error (_("USING invalid with table SORT"));
		} else {
			cb_emit_sort_using ((yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 15469 "parser.c" /* yacc.c:1646  */
    break;

  case 1290:
#line 9410 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-4])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-4])))) {
			cb_error (_("INPUT PROCEDURE invalid with table SORT"));
		} else if (current_statement->flag_merge) {
			cb_error (_("INPUT PROCEDURE invalid with MERGE"));
		} else {
			cb_emit_sort_input ((yyvsp[0]));
		}
	}
  }
#line 15485 "parser.c" /* yacc.c:1646  */
    break;

  case 1291:
#line 9425 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (0)]) && CB_FILE_P (cb_ref ((yyvsp[(-1) - (0)])))) {
		cb_error (_("file sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
#line 15495 "parser.c" /* yacc.c:1646  */
    break;

  case 1292:
#line 9431 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (2)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (2)])))) {
			cb_error (_("GIVING invalid with table SORT"));
		} else {
			cb_emit_sort_giving ((yyvsp[(-1) - (2)]), (yyvsp[0]));
		}
	}
  }
#line 15509 "parser.c" /* yacc.c:1646  */
    break;

  case 1293:
#line 9441 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (4)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (4)])))) {
			cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
		} else {
			cb_emit_sort_output ((yyvsp[0]));
		}
	}
  }
#line 15523 "parser.c" /* yacc.c:1646  */
    break;

  case 1294:
#line 9457 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("START", TERM_START);
	start_tree = cb_int (COB_EQ);
  }
#line 15532 "parser.c" /* yacc.c:1646  */
    break;

  case 1296:
#line 9467 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) && !(yyvsp[-2])) {
		cb_error_x (CB_TREE (current_statement),
			    _("SIZE/LENGTH invalid here"));
	} else {
		cb_emit_start ((yyvsp[-3]), start_tree, (yyvsp[-2]), (yyvsp[-1]));
	}
  }
#line 15545 "parser.c" /* yacc.c:1646  */
    break;

  case 1297:
#line 9479 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15553 "parser.c" /* yacc.c:1646  */
    break;

  case 1298:
#line 9483 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15561 "parser.c" /* yacc.c:1646  */
    break;

  case 1299:
#line 9490 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15569 "parser.c" /* yacc.c:1646  */
    break;

  case 1300:
#line 9494 "parser.y" /* yacc.c:1646  */
    {
	start_tree = (yyvsp[-1]);
	(yyval) = (yyvsp[0]);
  }
#line 15578 "parser.c" /* yacc.c:1646  */
    break;

  case 1301:
#line 9499 "parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_FI);
	(yyval) = NULL;
  }
#line 15587 "parser.c" /* yacc.c:1646  */
    break;

  case 1302:
#line 9504 "parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_LA);
	(yyval) = NULL;
  }
#line 15596 "parser.c" /* yacc.c:1646  */
    break;

  case 1303:
#line 9511 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_EQ); }
#line 15602 "parser.c" /* yacc.c:1646  */
    break;

  case 1304:
#line 9512 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LE : COB_GT); }
#line 15608 "parser.c" /* yacc.c:1646  */
    break;

  case 1305:
#line 9513 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GE : COB_LT); }
#line 15614 "parser.c" /* yacc.c:1646  */
    break;

  case 1306:
#line 9514 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LT : COB_GE); }
#line 15620 "parser.c" /* yacc.c:1646  */
    break;

  case 1307:
#line 9515 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GT : COB_LE); }
#line 15626 "parser.c" /* yacc.c:1646  */
    break;

  case 1308:
#line 9516 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_NE); }
#line 15632 "parser.c" /* yacc.c:1646  */
    break;

  case 1309:
#line 9521 "parser.y" /* yacc.c:1646  */
    {
	cb_error_x (CB_TREE (current_statement),
		    _("NOT EQUAL condition not allowed on START statement"));
  }
#line 15641 "parser.c" /* yacc.c:1646  */
    break;

  case 1312:
#line 9534 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), START);
  }
#line 15649 "parser.c" /* yacc.c:1646  */
    break;

  case 1313:
#line 9538 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), START);
  }
#line 15657 "parser.c" /* yacc.c:1646  */
    break;

  case 1314:
#line 9548 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP RUN", 0);
  }
#line 15665 "parser.c" /* yacc.c:1646  */
    break;

  case 1315:
#line 9552 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_stop_run ((yyvsp[0]));
	check_unreached = 1;
	cobc_cs_check = 0;
  }
#line 15675 "parser.c" /* yacc.c:1646  */
    break;

  case 1316:
#line 9558 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP", 0);
	cb_verify (cb_stop_literal_statement, "STOP literal");
	cb_emit_display (CB_LIST_INIT ((yyvsp[0])), cb_int0, cb_int1, NULL,
			 NULL, 1, DEVICE_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
	cobc_cs_check = 0;
  }
#line 15688 "parser.c" /* yacc.c:1646  */
    break;

  case 1317:
#line 9570 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = current_program->cb_return_code;
  }
#line 15696 "parser.c" /* yacc.c:1646  */
    break;

  case 1318:
#line 9574 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15704 "parser.c" /* yacc.c:1646  */
    break;

  case 1319:
#line 9578 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15712 "parser.c" /* yacc.c:1646  */
    break;

  case 1320:
#line 9582 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int1;
	}
  }
#line 15724 "parser.c" /* yacc.c:1646  */
    break;

  case 1321:
#line 9590 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int0;
	}
  }
#line 15736 "parser.c" /* yacc.c:1646  */
    break;

  case 1322:
#line 9601 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15744 "parser.c" /* yacc.c:1646  */
    break;

  case 1323:
#line 9605 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15752 "parser.c" /* yacc.c:1646  */
    break;

  case 1324:
#line 9611 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15758 "parser.c" /* yacc.c:1646  */
    break;

  case 1325:
#line 9612 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 15764 "parser.c" /* yacc.c:1646  */
    break;

  case 1326:
#line 9613 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 15770 "parser.c" /* yacc.c:1646  */
    break;

  case 1327:
#line 9614 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 15776 "parser.c" /* yacc.c:1646  */
    break;

  case 1328:
#line 9621 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STRING", TERM_STRING);
	save_tree = NULL;
  }
#line 15785 "parser.c" /* yacc.c:1646  */
    break;

  case 1330:
#line 9631 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_string (save_tree, (yyvsp[-2]), (yyvsp[-1]));
  }
#line 15793 "parser.c" /* yacc.c:1646  */
    break;

  case 1333:
#line 9643 "parser.y" /* yacc.c:1646  */
    {
    if (!save_tree) {
		save_tree = CB_LIST_INIT ((yyvsp[-1]));
	} else {
		save_tree = cb_list_add (save_tree, (yyvsp[-1]));
	}
	if ((yyvsp[0])) {
		save_tree = cb_list_add (save_tree, (yyvsp[0]));
	}
  }
#line 15808 "parser.c" /* yacc.c:1646  */
    break;

  case 1334:
#line 9657 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15814 "parser.c" /* yacc.c:1646  */
    break;

  case 1335:
#line 9659 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15820 "parser.c" /* yacc.c:1646  */
    break;

  case 1336:
#line 9663 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR (cb_int0, NULL); }
#line 15826 "parser.c" /* yacc.c:1646  */
    break;

  case 1337:
#line 9664 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[0]), NULL); }
#line 15832 "parser.c" /* yacc.c:1646  */
    break;

  case 1338:
#line 9668 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15838 "parser.c" /* yacc.c:1646  */
    break;

  case 1339:
#line 9669 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15844 "parser.c" /* yacc.c:1646  */
    break;

  case 1340:
#line 9674 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), STRING);
  }
#line 15852 "parser.c" /* yacc.c:1646  */
    break;

  case 1341:
#line 9678 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), STRING);
  }
#line 15860 "parser.c" /* yacc.c:1646  */
    break;

  case 1342:
#line 9688 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUBTRACT", TERM_SUBTRACT);
  }
#line 15868 "parser.c" /* yacc.c:1646  */
    break;

  case 1344:
#line 9697 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '-', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 15876 "parser.c" /* yacc.c:1646  */
    break;

  case 1345:
#line 9701 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list (CB_BUILD_CHAIN ((yyvsp[-3]), (yyvsp[-5])), '-'));
  }
#line 15884 "parser.c" /* yacc.c:1646  */
    break;

  case 1346:
#line 9705 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_sub, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 15892 "parser.c" /* yacc.c:1646  */
    break;

  case 1347:
#line 9712 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SUBTRACT);
  }
#line 15900 "parser.c" /* yacc.c:1646  */
    break;

  case 1348:
#line 9716 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SUBTRACT);
  }
#line 15908 "parser.c" /* yacc.c:1646  */
    break;

  case 1349:
#line 9726 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUPPRESS", 0);
	if (!in_declaratives) {
		cb_error_x (CB_TREE (current_statement),
			    _("SUPPRESS statement must be within DECLARATIVES"));
	}
	CB_PENDING("SUPPRESS");
  }
#line 15921 "parser.c" /* yacc.c:1646  */
    break;

  case 1352:
#line 9744 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TERMINATE", 0);
	CB_PENDING("TERMINATE");
  }
#line 15930 "parser.c" /* yacc.c:1646  */
    break;

  case 1354:
#line 9753 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 15940 "parser.c" /* yacc.c:1646  */
    break;

  case 1355:
#line 9759 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 15950 "parser.c" /* yacc.c:1646  */
    break;

  case 1356:
#line 9770 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TRANSFORM", 0);
  }
#line 15958 "parser.c" /* yacc.c:1646  */
    break;

  case 1358:
#line 9778 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_build_converting ((yyvsp[-2]), (yyvsp[0]), cb_build_inspect_region_start ());
	cb_emit_inspect ((yyvsp[-4]), x, TRANSFORM_STATEMENT);
  }
#line 15969 "parser.c" /* yacc.c:1646  */
    break;

  case 1359:
#line 9791 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNLOCK", 0);
  }
#line 15977 "parser.c" /* yacc.c:1646  */
    break;

  case 1361:
#line 9799 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-1]))) {
		if (CB_FILE (cb_ref ((yyvsp[-1])))->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("UNLOCK invalid for SORT files"));
		} else {
			cb_emit_unlock ((yyvsp[-1]));
		}
	}
  }
#line 15992 "parser.c" /* yacc.c:1646  */
    break;

  case 1362:
#line 9815 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNSTRING", TERM_UNSTRING);
  }
#line 16000 "parser.c" /* yacc.c:1646  */
    break;

  case 1364:
#line 9825 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_unstring ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 16008 "parser.c" /* yacc.c:1646  */
    break;

  case 1365:
#line 9831 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16014 "parser.c" /* yacc.c:1646  */
    break;

  case 1366:
#line 9833 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16020 "parser.c" /* yacc.c:1646  */
    break;

  case 1367:
#line 9837 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 16026 "parser.c" /* yacc.c:1646  */
    break;

  case 1368:
#line 9839 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 16032 "parser.c" /* yacc.c:1646  */
    break;

  case 1369:
#line 9844 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_delimited ((yyvsp[-1]), (yyvsp[0]));
  }
#line 16040 "parser.c" /* yacc.c:1646  */
    break;

  case 1370:
#line 9850 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 16046 "parser.c" /* yacc.c:1646  */
    break;

  case 1371:
#line 9852 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 16052 "parser.c" /* yacc.c:1646  */
    break;

  case 1372:
#line 9857 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_into ((yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 16060 "parser.c" /* yacc.c:1646  */
    break;

  case 1373:
#line 9863 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16066 "parser.c" /* yacc.c:1646  */
    break;

  case 1374:
#line 9864 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16072 "parser.c" /* yacc.c:1646  */
    break;

  case 1375:
#line 9868 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16078 "parser.c" /* yacc.c:1646  */
    break;

  case 1376:
#line 9869 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16084 "parser.c" /* yacc.c:1646  */
    break;

  case 1377:
#line 9873 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16090 "parser.c" /* yacc.c:1646  */
    break;

  case 1378:
#line 9874 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16096 "parser.c" /* yacc.c:1646  */
    break;

  case 1379:
#line 9879 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), UNSTRING);
  }
#line 16104 "parser.c" /* yacc.c:1646  */
    break;

  case 1380:
#line 9883 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), UNSTRING);
  }
#line 16112 "parser.c" /* yacc.c:1646  */
    break;

  case 1381:
#line 9893 "parser.y" /* yacc.c:1646  */
    {
	skip_statements = 0;
	in_debugging = 0;
  }
#line 16121 "parser.c" /* yacc.c:1646  */
    break;

  case 1388:
#line 9911 "parser.y" /* yacc.c:1646  */
    {
	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (!current_section) {
		cb_error (_("SECTION header missing before USE statement"));
	} else {
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 1;
		current_section->flag_skip_label = 0;
		CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
		if (use_global_ind) {
			current_section->flag_global = 1;
			current_program->global_list =
				cb_list_add (current_program->global_list,
					     CB_TREE (current_section));
		}
		emit_statement (cb_build_comment ("USE AFTER ERROR"));
	}
  }
#line 16147 "parser.c" /* yacc.c:1646  */
    break;

  case 1389:
#line 9936 "parser.y" /* yacc.c:1646  */
    {
	use_global_ind = 0;
  }
#line 16155 "parser.c" /* yacc.c:1646  */
    break;

  case 1390:
#line 9940 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		use_global_ind = 1;
		current_program->flag_global_use = 1;
	}
  }
#line 16168 "parser.c" /* yacc.c:1646  */
    break;

  case 1391:
#line 9952 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			set_up_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
#line 16182 "parser.c" /* yacc.c:1646  */
    break;

  case 1392:
#line 9962 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
#line 16191 "parser.c" /* yacc.c:1646  */
    break;

  case 1393:
#line 9967 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
#line 16200 "parser.c" /* yacc.c:1646  */
    break;

  case 1394:
#line 9972 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
#line 16209 "parser.c" /* yacc.c:1646  */
    break;

  case 1395:
#line 9977 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
#line 16218 "parser.c" /* yacc.c:1646  */
    break;

  case 1396:
#line 9985 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		plabel;
	char		name[64];

	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (current_program->nested_level) {
		cb_error (_("USE DEBUGGING not supported in contained program"));
	} else {
		in_debugging = 1;
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 0;
		current_section->flag_is_debug_sect = 1;
		if (!needs_debug_item) {
			needs_debug_item = 1;
			cb_build_debug_item ();
		}
		if (!current_program->flag_debugging) {
			skip_statements = 1;
			current_section->flag_skip_label = 1;
		} else {
			current_program->flag_gen_debug = 1;
			sprintf (name, "EXIT SECTION %d", cb_id);
			plabel = cb_build_reference (name);
			plabel = cb_build_label (plabel, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
			current_section->exit_label = plabel;
			emit_statement (cb_build_comment ("USE FOR DEBUGGING"));
		}
	}
  }
#line 16257 "parser.c" /* yacc.c:1646  */
    break;

  case 1399:
#line 10028 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;
	cb_tree		x;
	cb_tree		z;

	if (current_program->flag_debugging) {
		CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
		CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
		CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 0;
		z = CB_LIST_INIT ((yyvsp[0]));
		current_program->debug_list =
			cb_list_append (current_program->debug_list, z);
		/* Check backward refs to file/data names */
		/* Label refs will be checked later (forward/backward ref) */
		if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
			l = CB_VALUE(CB_WORD_ITEMS ((yyvsp[0])));
			switch (CB_TREE_TAG (l)) {
			case CB_TAG_FILE:
				CB_FILE (l)->debug_section = current_section;
				CB_FILE (l)->flag_fl_debug = 1;
				break;
			case CB_TAG_FIELD:
				{
					x = cb_ref((yyvsp[0]));
					if(CB_INVALID_TREE(x)) {
						break;
					}
					needs_field_debug = 1;
					CB_FIELD(x)->debug_section = current_section;
					CB_FIELD(x)->flag_field_debug = 1;
					CB_PURPOSE(z) = x;
					break;
				}
			default:
				break;
			}
		}
	}
  }
#line 16301 "parser.c" /* yacc.c:1646  */
    break;

  case 1400:
#line 10068 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_debugging) {
		if (current_program->all_procedure) {
			cb_error (_("duplicate USE DEBUGGING ON ALL PROCEDURES"));
		} else {
			current_program->all_procedure = current_section;
		}
	}
  }
#line 16315 "parser.c" /* yacc.c:1646  */
    break;

  case 1401:
#line 10078 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	if (current_program->flag_debugging) {
		/* Reference must be a data item */
		x = cb_ref ((yyvsp[0]));
		if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
			cb_error (_("invalid target for DEBUGGING ALL"));
		} else {
			needs_field_debug = 1;
			CB_FIELD (x)->debug_section = current_section;
			CB_FIELD (x)->flag_field_debug = 1;
			CB_FIELD (x)->flag_all_debug = 1;
			CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
			CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
			CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 1;
			CB_CHAIN_PAIR (current_program->debug_list, x, (yyvsp[0]));
		}
	}
  }
#line 16340 "parser.c" /* yacc.c:1646  */
    break;

  case 1406:
#line 10108 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "USE AT");
	}
  }
#line 16350 "parser.c" /* yacc.c:1646  */
    break;

  case 1407:
#line 10117 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM START"));
	/* emit_entry ("_START", 0, NULL); */
	CB_PENDING ("USE AT PROGRAM START");
  }
#line 16360 "parser.c" /* yacc.c:1646  */
    break;

  case 1408:
#line 10123 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM END"));
	/* emit_entry ("_END", 0, NULL); */
	CB_PENDING ("USE AT PROGRAM END");
  }
#line 16370 "parser.c" /* yacc.c:1646  */
    break;

  case 1409:
#line 10133 "parser.y" /* yacc.c:1646  */
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE BEFORE REPORTING"));
	CB_PENDING ("USE BEFORE REPORTING");
  }
#line 16380 "parser.c" /* yacc.c:1646  */
    break;

  case 1410:
#line 10142 "parser.y" /* yacc.c:1646  */
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING ("USE AFTER EXCEPTION CONDITION");
  }
#line 16390 "parser.c" /* yacc.c:1646  */
    break;

  case 1413:
#line 10158 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("WRITE", TERM_WRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 16401 "parser.c" /* yacc.c:1646  */
    break;

  case 1415:
#line 10170 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-4]))) {
		cb_emit_write ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
	}
	start_debug = save_debug;
  }
#line 16412 "parser.c" /* yacc.c:1646  */
    break;

  case 1416:
#line 10179 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16418 "parser.c" /* yacc.c:1646  */
    break;

  case 1417:
#line 10180 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16424 "parser.c" /* yacc.c:1646  */
    break;

  case 1418:
#line 10185 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 16432 "parser.c" /* yacc.c:1646  */
    break;

  case 1419:
#line 10189 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_lines ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 16440 "parser.c" /* yacc.c:1646  */
    break;

  case 1420:
#line 10193 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 16448 "parser.c" /* yacc.c:1646  */
    break;

  case 1421:
#line 10197 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_page ((yyvsp[-2]));
  }
#line 16456 "parser.c" /* yacc.c:1646  */
    break;

  case 1422:
#line 10203 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 16462 "parser.c" /* yacc.c:1646  */
    break;

  case 1423:
#line 10204 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_AFTER; }
#line 16468 "parser.c" /* yacc.c:1646  */
    break;

  case 1427:
#line 10215 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), WRITE);
  }
#line 16476 "parser.c" /* yacc.c:1646  */
    break;

  case 1428:
#line 10219 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), WRITE);
  }
#line 16484 "parser.c" /* yacc.c:1646  */
    break;

  case 1431:
#line 10233 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 16495 "parser.c" /* yacc.c:1646  */
    break;

  case 1432:
#line 10243 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16503 "parser.c" /* yacc.c:1646  */
    break;

  case 1433:
#line 10247 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16511 "parser.c" /* yacc.c:1646  */
    break;

  case 1434:
#line 10254 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16520 "parser.c" /* yacc.c:1646  */
    break;

  case 1439:
#line 10272 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16529 "parser.c" /* yacc.c:1646  */
    break;

  case 1444:
#line 10288 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 16540 "parser.c" /* yacc.c:1646  */
    break;

  case 1445:
#line 10298 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16548 "parser.c" /* yacc.c:1646  */
    break;

  case 1446:
#line 10302 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16556 "parser.c" /* yacc.c:1646  */
    break;

  case 1447:
#line 10309 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16565 "parser.c" /* yacc.c:1646  */
    break;

  case 1450:
#line 10322 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16574 "parser.c" /* yacc.c:1646  */
    break;

  case 1453:
#line 10334 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT SIZE ERROR before SIZE ERROR"));
	}
  }
#line 16585 "parser.c" /* yacc.c:1646  */
    break;

  case 1454:
#line 10344 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16593 "parser.c" /* yacc.c:1646  */
    break;

  case 1455:
#line 10348 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16601 "parser.c" /* yacc.c:1646  */
    break;

  case 1456:
#line 10355 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16610 "parser.c" /* yacc.c:1646  */
    break;

  case 1459:
#line 10368 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16619 "parser.c" /* yacc.c:1646  */
    break;

  case 1462:
#line 10380 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT OVERFLOW before OVERFLOW"));
	}
  }
#line 16630 "parser.c" /* yacc.c:1646  */
    break;

  case 1463:
#line 10390 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16638 "parser.c" /* yacc.c:1646  */
    break;

  case 1464:
#line 10394 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16646 "parser.c" /* yacc.c:1646  */
    break;

  case 1465:
#line 10401 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16655 "parser.c" /* yacc.c:1646  */
    break;

  case 1468:
#line 10414 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16664 "parser.c" /* yacc.c:1646  */
    break;

  case 1470:
#line 10426 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
  }
#line 16672 "parser.c" /* yacc.c:1646  */
    break;

  case 1472:
#line 10435 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
	}
  }
#line 16682 "parser.c" /* yacc.c:1646  */
    break;

  case 1473:
#line 10444 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16690 "parser.c" /* yacc.c:1646  */
    break;

  case 1474:
#line 10448 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16698 "parser.c" /* yacc.c:1646  */
    break;

  case 1475:
#line 10455 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16707 "parser.c" /* yacc.c:1646  */
    break;

  case 1478:
#line 10468 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16716 "parser.c" /* yacc.c:1646  */
    break;

  case 1480:
#line 10479 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT AT END-OF-PAGE before AT END-OF-PAGE"));
	}
  }
#line 16727 "parser.c" /* yacc.c:1646  */
    break;

  case 1481:
#line 10489 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16735 "parser.c" /* yacc.c:1646  */
    break;

  case 1482:
#line 10493 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16743 "parser.c" /* yacc.c:1646  */
    break;

  case 1483:
#line 10500 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16752 "parser.c" /* yacc.c:1646  */
    break;

  case 1486:
#line 10513 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16761 "parser.c" /* yacc.c:1646  */
    break;

  case 1490:
#line 10529 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT INVALID KEY before INVALID KEY"));
	}
  }
#line 16772 "parser.c" /* yacc.c:1646  */
    break;

  case 1491:
#line 10539 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16780 "parser.c" /* yacc.c:1646  */
    break;

  case 1492:
#line 10543 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 16788 "parser.c" /* yacc.c:1646  */
    break;

  case 1493:
#line 10550 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 16797 "parser.c" /* yacc.c:1646  */
    break;

  case 1496:
#line 10563 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 16806 "parser.c" /* yacc.c:1646  */
    break;

  case 1497:
#line 10573 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_one;
  }
#line 16814 "parser.c" /* yacc.c:1646  */
    break;

  case 1498:
#line 10577 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 16822 "parser.c" /* yacc.c:1646  */
    break;

  case 1499:
#line 10587 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_cond ((yyvsp[0]));
  }
#line 16830 "parser.c" /* yacc.c:1646  */
    break;

  case 1500:
#line 10594 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_expr ((yyvsp[0]));
  }
#line 16838 "parser.c" /* yacc.c:1646  */
    break;

  case 1501:
#line 10600 "parser.y" /* yacc.c:1646  */
    {
	current_expr = NULL;
	cb_exp_line = cb_source_line;
  }
#line 16847 "parser.c" /* yacc.c:1646  */
    break;

  case 1502:
#line 10605 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_expr);
  }
#line 16855 "parser.c" /* yacc.c:1646  */
    break;

  case 1506:
#line 10617 "parser.y" /* yacc.c:1646  */
    { push_expr ('x', (yyvsp[0])); }
#line 16861 "parser.c" /* yacc.c:1646  */
    break;

  case 1507:
#line 10618 "parser.y" /* yacc.c:1646  */
    { push_expr ('C', (yyvsp[0])); }
#line 16867 "parser.c" /* yacc.c:1646  */
    break;

  case 1508:
#line 10620 "parser.y" /* yacc.c:1646  */
    { push_expr ('(', NULL); }
#line 16873 "parser.c" /* yacc.c:1646  */
    break;

  case 1509:
#line 10621 "parser.y" /* yacc.c:1646  */
    { push_expr (')', NULL); }
#line 16879 "parser.c" /* yacc.c:1646  */
    break;

  case 1510:
#line 10623 "parser.y" /* yacc.c:1646  */
    { push_expr ('+', NULL); }
#line 16885 "parser.c" /* yacc.c:1646  */
    break;

  case 1511:
#line 10624 "parser.y" /* yacc.c:1646  */
    { push_expr ('-', NULL); }
#line 16891 "parser.c" /* yacc.c:1646  */
    break;

  case 1512:
#line 10625 "parser.y" /* yacc.c:1646  */
    { push_expr ('*', NULL); }
#line 16897 "parser.c" /* yacc.c:1646  */
    break;

  case 1513:
#line 10626 "parser.y" /* yacc.c:1646  */
    { push_expr ('/', NULL); }
#line 16903 "parser.c" /* yacc.c:1646  */
    break;

  case 1514:
#line 10627 "parser.y" /* yacc.c:1646  */
    { push_expr ('^', NULL); }
#line 16909 "parser.c" /* yacc.c:1646  */
    break;

  case 1515:
#line 10629 "parser.y" /* yacc.c:1646  */
    { push_expr ('=', NULL); }
#line 16915 "parser.c" /* yacc.c:1646  */
    break;

  case 1516:
#line 10630 "parser.y" /* yacc.c:1646  */
    { push_expr ('>', NULL); }
#line 16921 "parser.c" /* yacc.c:1646  */
    break;

  case 1517:
#line 10631 "parser.y" /* yacc.c:1646  */
    { push_expr ('<', NULL); }
#line 16927 "parser.c" /* yacc.c:1646  */
    break;

  case 1518:
#line 10632 "parser.y" /* yacc.c:1646  */
    { push_expr (']', NULL); }
#line 16933 "parser.c" /* yacc.c:1646  */
    break;

  case 1519:
#line 10633 "parser.y" /* yacc.c:1646  */
    { push_expr ('[', NULL); }
#line 16939 "parser.c" /* yacc.c:1646  */
    break;

  case 1520:
#line 10634 "parser.y" /* yacc.c:1646  */
    { push_expr ('~', NULL); }
#line 16945 "parser.c" /* yacc.c:1646  */
    break;

  case 1521:
#line 10636 "parser.y" /* yacc.c:1646  */
    { push_expr ('!', NULL); }
#line 16951 "parser.c" /* yacc.c:1646  */
    break;

  case 1522:
#line 10637 "parser.y" /* yacc.c:1646  */
    { push_expr ('&', NULL); }
#line 16957 "parser.c" /* yacc.c:1646  */
    break;

  case 1523:
#line 10638 "parser.y" /* yacc.c:1646  */
    { push_expr ('|', NULL); }
#line 16963 "parser.c" /* yacc.c:1646  */
    break;

  case 1524:
#line 10640 "parser.y" /* yacc.c:1646  */
    { push_expr ('O', NULL); }
#line 16969 "parser.c" /* yacc.c:1646  */
    break;

  case 1525:
#line 10641 "parser.y" /* yacc.c:1646  */
    { push_expr ('9', NULL); }
#line 16975 "parser.c" /* yacc.c:1646  */
    break;

  case 1526:
#line 10642 "parser.y" /* yacc.c:1646  */
    { push_expr ('A', NULL); }
#line 16981 "parser.c" /* yacc.c:1646  */
    break;

  case 1527:
#line 10643 "parser.y" /* yacc.c:1646  */
    { push_expr ('L', NULL); }
#line 16987 "parser.c" /* yacc.c:1646  */
    break;

  case 1528:
#line 10644 "parser.y" /* yacc.c:1646  */
    { push_expr ('U', NULL); }
#line 16993 "parser.c" /* yacc.c:1646  */
    break;

  case 1529:
#line 10647 "parser.y" /* yacc.c:1646  */
    { push_expr ('P', NULL); }
#line 16999 "parser.c" /* yacc.c:1646  */
    break;

  case 1530:
#line 10648 "parser.y" /* yacc.c:1646  */
    { push_expr ('N', NULL); }
#line 17005 "parser.c" /* yacc.c:1646  */
    break;

  case 1539:
#line 10678 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 17013 "parser.c" /* yacc.c:1646  */
    break;

  case 1540:
#line 10682 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17021 "parser.c" /* yacc.c:1646  */
    break;

  case 1544:
#line 10693 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '+', (yyvsp[0])); }
#line 17027 "parser.c" /* yacc.c:1646  */
    break;

  case 1545:
#line 10694 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '-', (yyvsp[0])); }
#line 17033 "parser.c" /* yacc.c:1646  */
    break;

  case 1546:
#line 10695 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17039 "parser.c" /* yacc.c:1646  */
    break;

  case 1547:
#line 10699 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '*', (yyvsp[0])); }
#line 17045 "parser.c" /* yacc.c:1646  */
    break;

  case 1548:
#line 10700 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '/', (yyvsp[0])); }
#line 17051 "parser.c" /* yacc.c:1646  */
    break;

  case 1549:
#line 10701 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17057 "parser.c" /* yacc.c:1646  */
    break;

  case 1550:
#line 10706 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_binary_op ((yyvsp[-2]), '^', (yyvsp[0]));
  }
#line 17065 "parser.c" /* yacc.c:1646  */
    break;

  case 1551:
#line 10709 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17071 "parser.c" /* yacc.c:1646  */
    break;

  case 1552:
#line 10713 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17077 "parser.c" /* yacc.c:1646  */
    break;

  case 1553:
#line 10714 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op (cb_zero, '-', (yyvsp[0])); }
#line 17083 "parser.c" /* yacc.c:1646  */
    break;

  case 1554:
#line 10715 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17089 "parser.c" /* yacc.c:1646  */
    break;

  case 1555:
#line 10718 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 17095 "parser.c" /* yacc.c:1646  */
    break;

  case 1556:
#line 10719 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17101 "parser.c" /* yacc.c:1646  */
    break;

  case 1557:
#line 10730 "parser.y" /* yacc.c:1646  */
    {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("invalid LINAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = linage_file->linage_ctr;
	}
  }
#line 17117 "parser.c" /* yacc.c:1646  */
    break;

  case 1558:
#line 10742 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_FILE (cb_ref ((yyvsp[0])))->linage_ctr;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 17130 "parser.c" /* yacc.c:1646  */
    break;

  case 1559:
#line 10751 "parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1) {
		cb_error (_("LINE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (report_count == 0) {
		cb_error (_("invalid LINE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->line_counter;
	}
  }
#line 17146 "parser.c" /* yacc.c:1646  */
    break;

  case 1560:
#line 10763 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_REPORT (cb_ref ((yyvsp[0])))->line_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 17159 "parser.c" /* yacc.c:1646  */
    break;

  case 1561:
#line 10772 "parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1) {
		cb_error (_("PAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (report_count == 0) {
		cb_error (_("invalid PAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->page_counter;
	}
  }
#line 17175 "parser.c" /* yacc.c:1646  */
    break;

  case 1562:
#line 10784 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_REPORT (cb_ref ((yyvsp[0])))->page_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 17188 "parser.c" /* yacc.c:1646  */
    break;

  case 1563:
#line 10798 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17194 "parser.c" /* yacc.c:1646  */
    break;

  case 1564:
#line 10800 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 17200 "parser.c" /* yacc.c:1646  */
    break;

  case 1565:
#line 10805 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 17208 "parser.c" /* yacc.c:1646  */
    break;

  case 1566:
#line 10813 "parser.y" /* yacc.c:1646  */
    { cb_build_identifier ((yyvsp[0]), 0); }
#line 17214 "parser.c" /* yacc.c:1646  */
    break;

  case 1567:
#line 10820 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x;

	x = cb_ref ((yyvsp[0]));
	if (!CB_FIELD_P (x)) {
		(yyval) = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x ((yyvsp[0]), _("'%s' not indexed"), cb_name ((yyvsp[0])));
		listprint_suppress ();
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		listprint_restore ();
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 17235 "parser.c" /* yacc.c:1646  */
    break;

  case 1568:
#line 10842 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 17243 "parser.c" /* yacc.c:1646  */
    break;

  case 1569:
#line 10846 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	if (CB_VALID_TREE ((yyvsp[0]))) {
		for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME ((yyvsp[0])), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ((yyvsp[0]), _("multiple reference to '%s' "),
					    CB_NAME ((yyvsp[0])));
				break;
			}
		}
		if (!l) {
			(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
		}
	}
  }
#line 17265 "parser.c" /* yacc.c:1646  */
    break;

  case 1570:
#line 10867 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 17278 "parser.c" /* yacc.c:1646  */
    break;

  case 1571:
#line 10908 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 17291 "parser.c" /* yacc.c:1646  */
    break;

  case 1572:
#line 10921 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 17297 "parser.c" /* yacc.c:1646  */
    break;

  case 1573:
#line 10923 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 17303 "parser.c" /* yacc.c:1646  */
    break;

  case 1574:
#line 10927 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17309 "parser.c" /* yacc.c:1646  */
    break;

  case 1575:
#line 10933 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17315 "parser.c" /* yacc.c:1646  */
    break;

  case 1576:
#line 10935 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 17321 "parser.c" /* yacc.c:1646  */
    break;

  case 1577:
#line 10940 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_REFERENCE ((yyval))->offset = CB_TREE (current_section);
	CB_REFERENCE ((yyval))->flag_in_decl = !!in_declaratives;
	CB_REFERENCE ((yyval))->section = current_section;
	CB_REFERENCE ((yyval))->paragraph = current_paragraph;
	CB_ADD_TO_CHAIN ((yyval), current_program->label_list);
  }
#line 17334 "parser.c" /* yacc.c:1646  */
    break;

  case 1580:
#line 10954 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 17342 "parser.c" /* yacc.c:1646  */
    break;

  case 1581:
#line 10961 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_reference ((char *)(CB_LITERAL ((yyvsp[0]))->data));
	(yyval)->source_file = (yyvsp[0])->source_file;
	(yyval)->source_line = (yyvsp[0])->source_line;
  }
#line 17352 "parser.c" /* yacc.c:1646  */
    break;

  case 1582:
#line 10971 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 17358 "parser.c" /* yacc.c:1646  */
    break;

  case 1583:
#line 10972 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 17364 "parser.c" /* yacc.c:1646  */
    break;

  case 1584:
#line 10977 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 17373 "parser.c" /* yacc.c:1646  */
    break;

  case 1585:
#line 10985 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 17382 "parser.c" /* yacc.c:1646  */
    break;

  case 1586:
#line 10993 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 17390 "parser.c" /* yacc.c:1646  */
    break;

  case 1587:
#line 10997 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17398 "parser.c" /* yacc.c:1646  */
    break;

  case 1588:
#line 11004 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_REFERENCE((yyval))->flag_optional = 1;
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 17408 "parser.c" /* yacc.c:1646  */
    break;

  case 1591:
#line 11020 "parser.y" /* yacc.c:1646  */
    {
	if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 17421 "parser.c" /* yacc.c:1646  */
    break;

  case 1592:
#line 11029 "parser.y" /* yacc.c:1646  */
    {
	  yyclearin;
	  yyerrok;
	  (yyval) = cb_error_node;
  }
#line 17431 "parser.c" /* yacc.c:1646  */
    break;

  case 1593:
#line 11040 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE ((yyvsp[0]))->flag_duped || CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = NULL;
	} else {
		CB_WORD_COUNT ((yyvsp[0]))++;
		(yyval) = (yyvsp[0]);
	}
  }
#line 17445 "parser.c" /* yacc.c:1646  */
    break;

  case 1594:
#line 11057 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 17453 "parser.c" /* yacc.c:1646  */
    break;

  case 1595:
#line 11061 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17461 "parser.c" /* yacc.c:1646  */
    break;

  case 1598:
#line 11070 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 17469 "parser.c" /* yacc.c:1646  */
    break;

  case 1599:
#line 11076 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17475 "parser.c" /* yacc.c:1646  */
    break;

  case 1600:
#line 11077 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17481 "parser.c" /* yacc.c:1646  */
    break;

  case 1601:
#line 11082 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 17489 "parser.c" /* yacc.c:1646  */
    break;

  case 1602:
#line 11086 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17497 "parser.c" /* yacc.c:1646  */
    break;

  case 1610:
#line 11106 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 17505 "parser.c" /* yacc.c:1646  */
    break;

  case 1611:
#line 11110 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 17513 "parser.c" /* yacc.c:1646  */
    break;

  case 1612:
#line 11114 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 17521 "parser.c" /* yacc.c:1646  */
    break;

  case 1613:
#line 11118 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_ppointer ((yyvsp[0]));
  }
#line 17529 "parser.c" /* yacc.c:1646  */
    break;

  case 1614:
#line 11122 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 17537 "parser.c" /* yacc.c:1646  */
    break;

  case 1615:
#line 11126 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	cb_tree		switch_id;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME (x)->category != CB_SWITCH_NAME) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic identifier"));
			(yyval) = cb_error_node;
		} else {
			switch_id = cb_int (CB_SYSTEM_NAME (x)->token);
			(yyval) = CB_BUILD_FUNCALL_1 ("cob_switch_value", switch_id);
		}
	} else {
		(yyval) = cb_error_node;
	}
  }
#line 17559 "parser.c" /* yacc.c:1646  */
    break;

  case 1616:
#line 11147 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 17567 "parser.c" /* yacc.c:1646  */
    break;

  case 1617:
#line 11151 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17575 "parser.c" /* yacc.c:1646  */
    break;

  case 1625:
#line 11168 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 17583 "parser.c" /* yacc.c:1646  */
    break;

  case 1626:
#line 11172 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 17591 "parser.c" /* yacc.c:1646  */
    break;

  case 1627:
#line 11176 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 17599 "parser.c" /* yacc.c:1646  */
    break;

  case 1632:
#line 11193 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 17607 "parser.c" /* yacc.c:1646  */
    break;

  case 1633:
#line 11200 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 17615 "parser.c" /* yacc.c:1646  */
    break;

  case 1639:
#line 11218 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 17623 "parser.c" /* yacc.c:1646  */
    break;

  case 1641:
#line 11226 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 17631 "parser.c" /* yacc.c:1646  */
    break;

  case 1644:
#line 11235 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 17639 "parser.c" /* yacc.c:1646  */
    break;

  case 1646:
#line 11240 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_zero;
  }
#line 17647 "parser.c" /* yacc.c:1646  */
    break;

  case 1647:
#line 11247 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 17655 "parser.c" /* yacc.c:1646  */
    break;

  case 1649:
#line 11255 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 17663 "parser.c" /* yacc.c:1646  */
    break;

  case 1651:
#line 11263 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 17671 "parser.c" /* yacc.c:1646  */
    break;

  case 1654:
#line 11273 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 0); }
#line 17677 "parser.c" /* yacc.c:1646  */
    break;

  case 1655:
#line 11277 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 1); }
#line 17683 "parser.c" /* yacc.c:1646  */
    break;

  case 1656:
#line 11281 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17689 "parser.c" /* yacc.c:1646  */
    break;

  case 1657:
#line 11282 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 17695 "parser.c" /* yacc.c:1646  */
    break;

  case 1658:
#line 11287 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 17703 "parser.c" /* yacc.c:1646  */
    break;

  case 1659:
#line 11294 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != cb_error_node
	    && cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("'%s' is not numeric"), cb_name ((yyvsp[0])));
	}
  }
#line 17714 "parser.c" /* yacc.c:1646  */
    break;

  case 1660:
#line 11304 "parser.y" /* yacc.c:1646  */
    {
	int     reference_to_existing_object;
	  
	if (CB_REFERENCE_P ((yyvsp[0])) && (CB_FIELD_P (cb_ref ((yyvsp[0])))
				    || CB_FILE_P (cb_ref ((yyvsp[0]))))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
	        reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[0])) && cb_ref ((yyvsp[0])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[0])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field or file"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 17734 "parser.c" /* yacc.c:1646  */
    break;

  case 1661:
#line 11323 "parser.y" /* yacc.c:1646  */
    {
	int     reference_to_existing_object;
	  
	if (CB_REFERENCE_P ((yyvsp[0])) && CB_FIELD_P (cb_ref ((yyvsp[0])))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
	        reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[0])) && cb_ref ((yyvsp[0])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[0])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 17753 "parser.c" /* yacc.c:1646  */
    break;

  case 1662:
#line 11341 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 17764 "parser.c" /* yacc.c:1646  */
    break;

  case 1663:
#line 11348 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 17775 "parser.c" /* yacc.c:1646  */
    break;

  case 1664:
#line 11355 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 17786 "parser.c" /* yacc.c:1646  */
    break;

  case 1665:
#line 11362 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 17797 "parser.c" /* yacc.c:1646  */
    break;

  case 1666:
#line 11372 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_identifier ((yyvsp[0]), 0);
  }
#line 17805 "parser.c" /* yacc.c:1646  */
    break;

  case 1667:
#line 11379 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (CB_REFERENCE_P ((yyvsp[-2]))) {
		CB_REFERENCE ((yyvsp[-2]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 17819 "parser.c" /* yacc.c:1646  */
    break;

  case 1668:
#line 11389 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 17833 "parser.c" /* yacc.c:1646  */
    break;

  case 1669:
#line 11399 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 17847 "parser.c" /* yacc.c:1646  */
    break;

  case 1670:
#line 11409 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		CB_REFERENCE ((yyvsp[0]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 17861 "parser.c" /* yacc.c:1646  */
    break;

  case 1671:
#line 11422 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17869 "parser.c" /* yacc.c:1646  */
    break;

  case 1672:
#line 11426 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 17878 "parser.c" /* yacc.c:1646  */
    break;

  case 1673:
#line 11434 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-3]))->subs = cb_list_reverse ((yyvsp[-1]));
  }
#line 17887 "parser.c" /* yacc.c:1646  */
    break;

  case 1674:
#line 11442 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-4]))->offset = (yyvsp[-2]);
  }
#line 17895 "parser.c" /* yacc.c:1646  */
    break;

  case 1675:
#line 11446 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-5]))->offset = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-5]))->length = (yyvsp[-1]);
  }
#line 17904 "parser.c" /* yacc.c:1646  */
    break;

  case 1676:
#line 11456 "parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || CB_LITERAL ((yyvsp[0]))->sign < 0
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("non-negative integer value expected"));
		(yyval) = cb_build_numeric_literal(-1, "1", 0);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 17919 "parser.c" /* yacc.c:1646  */
    break;

  case 1677:
#line 11470 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else if (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1 || n > 256) {
			cb_error (_("invalid symbolic integer"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 17943 "parser.c" /* yacc.c:1646  */
    break;

  case 1678:
#line 11493 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || CB_LITERAL ((yyvsp[0]))->sign
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("unsigned positive integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1) {
			cb_error (_("unsigned positive integer value expected"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 17966 "parser.c" /* yacc.c:1646  */
    break;

  case 1679:
#line 11515 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) == CB_CATEGORY_NUMERIC) {
		if (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale) {
			cb_error (_("integer value expected"));
		} else {
			n = cb_get_int ((yyvsp[0]));
			if (n < 1 || n > 256) {
				cb_error (_("invalid CLASS value"));
			}
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 17986 "parser.c" /* yacc.c:1646  */
    break;

  case 1680:
#line 11530 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 17992 "parser.c" /* yacc.c:1646  */
    break;

  case 1681:
#line 11531 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 17998 "parser.c" /* yacc.c:1646  */
    break;

  case 1682:
#line 11532 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 18004 "parser.c" /* yacc.c:1646  */
    break;

  case 1683:
#line 11533 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 18010 "parser.c" /* yacc.c:1646  */
    break;

  case 1684:
#line 11534 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 18016 "parser.c" /* yacc.c:1646  */
    break;

  case 1685:
#line 11535 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 18022 "parser.c" /* yacc.c:1646  */
    break;

  case 1686:
#line 11540 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18030 "parser.c" /* yacc.c:1646  */
    break;

  case 1687:
#line 11544 "parser.y" /* yacc.c:1646  */
    {
	struct cb_literal	*l;

	if (CB_LITERAL_P ((yyvsp[0]))) {
		/* We must not alter the original definition */
		l = cobc_parse_malloc (sizeof(struct cb_literal));
		*l = *(CB_LITERAL((yyvsp[0])));
		l->all = 1;
		(yyval) = CB_TREE (l);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 18048 "parser.c" /* yacc.c:1646  */
    break;

  case 1688:
#line 11561 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18056 "parser.c" /* yacc.c:1646  */
    break;

  case 1689:
#line 11565 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_concat_literals ((yyvsp[-2]), (yyvsp[0]));
  }
#line 18064 "parser.c" /* yacc.c:1646  */
    break;

  case 1690:
#line 11571 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18070 "parser.c" /* yacc.c:1646  */
    break;

  case 1691:
#line 11572 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 18076 "parser.c" /* yacc.c:1646  */
    break;

  case 1692:
#line 11573 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 18082 "parser.c" /* yacc.c:1646  */
    break;

  case 1693:
#line 11574 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 18088 "parser.c" /* yacc.c:1646  */
    break;

  case 1694:
#line 11575 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 18094 "parser.c" /* yacc.c:1646  */
    break;

  case 1695:
#line 11576 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 18100 "parser.c" /* yacc.c:1646  */
    break;

  case 1696:
#line 11577 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 18106 "parser.c" /* yacc.c:1646  */
    break;

  case 1697:
#line 11584 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), NULL, (yyvsp[0]), 0);
  }
#line 18114 "parser.c" /* yacc.c:1646  */
    break;

  case 1698:
#line 11588 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), CB_LIST_INIT ((yyvsp[-2])), (yyvsp[0]), 0);
  }
#line 18122 "parser.c" /* yacc.c:1646  */
    break;

  case 1699:
#line 11592 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18130 "parser.c" /* yacc.c:1646  */
    break;

  case 1700:
#line 11596 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18138 "parser.c" /* yacc.c:1646  */
    break;

  case 1701:
#line 11600 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 18146 "parser.c" /* yacc.c:1646  */
    break;

  case 1702:
#line 11604 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18154 "parser.c" /* yacc.c:1646  */
    break;

  case 1703:
#line 11608 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18162 "parser.c" /* yacc.c:1646  */
    break;

  case 1704:
#line 11612 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18170 "parser.c" /* yacc.c:1646  */
    break;

  case 1705:
#line 11616 "parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18178 "parser.c" /* yacc.c:1646  */
    break;

  case 1706:
#line 11620 "parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 18186 "parser.c" /* yacc.c:1646  */
    break;

  case 1707:
#line 11624 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 0);
  }
#line 18194 "parser.c" /* yacc.c:1646  */
    break;

  case 1708:
#line 11628 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 1);
  }
#line 18202 "parser.c" /* yacc.c:1646  */
    break;

  case 1718:
#line 11653 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18210 "parser.c" /* yacc.c:1646  */
    break;

  case 1719:
#line 11657 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), NULL);
  }
#line 18218 "parser.c" /* yacc.c:1646  */
    break;

  case 1720:
#line 11661 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 18226 "parser.c" /* yacc.c:1646  */
    break;

  case 1721:
#line 11668 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18234 "parser.c" /* yacc.c:1646  */
    break;

  case 1722:
#line 11672 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 18242 "parser.c" /* yacc.c:1646  */
    break;

  case 1723:
#line 11676 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18250 "parser.c" /* yacc.c:1646  */
    break;

  case 1724:
#line 11683 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_int0);
  }
#line 18261 "parser.c" /* yacc.c:1646  */
    break;

  case 1725:
#line 11690 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int1);
  }
#line 18272 "parser.c" /* yacc.c:1646  */
    break;

  case 1726:
#line 11697 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int2);
  }
#line 18283 "parser.c" /* yacc.c:1646  */
    break;

  case 1727:
#line 11707 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 18294 "parser.c" /* yacc.c:1646  */
    break;

  case 1728:
#line 11714 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, (yyvsp[0]));
  }
#line 18305 "parser.c" /* yacc.c:1646  */
    break;

  case 1729:
#line 11724 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 18316 "parser.c" /* yacc.c:1646  */
    break;

  case 1730:
#line 11731 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_ref ((yyvsp[0])));
  }
#line 18327 "parser.c" /* yacc.c:1646  */
    break;

  case 1731:
#line 11741 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 18335 "parser.c" /* yacc.c:1646  */
    break;

  case 1732:
#line 11745 "parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 4) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 18349 "parser.c" /* yacc.c:1646  */
    break;

  case 1733:
#line 11758 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 18357 "parser.c" /* yacc.c:1646  */
    break;

  case 1734:
#line 11762 "parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 3) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 18371 "parser.c" /* yacc.c:1646  */
    break;

  case 1735:
#line 11776 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 1;
  }
#line 18379 "parser.c" /* yacc.c:1646  */
    break;

  case 1736:
#line 11784 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18385 "parser.c" /* yacc.c:1646  */
    break;

  case 1737:
#line 11785 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18391 "parser.c" /* yacc.c:1646  */
    break;

  case 1738:
#line 11789 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18397 "parser.c" /* yacc.c:1646  */
    break;

  case 1739:
#line 11790 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18403 "parser.c" /* yacc.c:1646  */
    break;

  case 1740:
#line 11794 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18409 "parser.c" /* yacc.c:1646  */
    break;

  case 1741:
#line 11795 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18415 "parser.c" /* yacc.c:1646  */
    break;

  case 1742:
#line 11800 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18423 "parser.c" /* yacc.c:1646  */
    break;

  case 1743:
#line 11804 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18431 "parser.c" /* yacc.c:1646  */
    break;

  case 1744:
#line 11811 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18439 "parser.c" /* yacc.c:1646  */
    break;

  case 1745:
#line 11815 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18447 "parser.c" /* yacc.c:1646  */
    break;

  case 1746:
#line 11822 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18453 "parser.c" /* yacc.c:1646  */
    break;

  case 1747:
#line 11823 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18459 "parser.c" /* yacc.c:1646  */
    break;

  case 1748:
#line 11824 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 18465 "parser.c" /* yacc.c:1646  */
    break;

  case 1749:
#line 11828 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18471 "parser.c" /* yacc.c:1646  */
    break;

  case 1750:
#line 11829 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 18477 "parser.c" /* yacc.c:1646  */
    break;

  case 1751:
#line 11833 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (cb_flag_optional_file); }
#line 18483 "parser.c" /* yacc.c:1646  */
    break;

  case 1752:
#line 11834 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18489 "parser.c" /* yacc.c:1646  */
    break;

  case 1753:
#line 11835 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18495 "parser.c" /* yacc.c:1646  */
    break;

  case 1754:
#line 11840 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 18503 "parser.c" /* yacc.c:1646  */
    break;

  case 1755:
#line 11844 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int (COB_STORE_ROUND);
	}
	cobc_cs_check = 0;
  }
#line 18516 "parser.c" /* yacc.c:1646  */
    break;

  case 1756:
#line 11856 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
#line 18525 "parser.c" /* yacc.c:1646  */
    break;

  case 1757:
#line 11861 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	cobc_cs_check = 0;
  }
#line 18534 "parser.c" /* yacc.c:1646  */
    break;

  case 1758:
#line 11869 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_AWAY_FROM_ZERO);
  }
#line 18542 "parser.c" /* yacc.c:1646  */
    break;

  case 1759:
#line 11873 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 18550 "parser.c" /* yacc.c:1646  */
    break;

  case 1760:
#line 11877 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 18558 "parser.c" /* yacc.c:1646  */
    break;

  case 1761:
#line 11881 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_TOWARD_ZERO);
  }
#line 18566 "parser.c" /* yacc.c:1646  */
    break;

  case 1762:
#line 11885 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 18574 "parser.c" /* yacc.c:1646  */
    break;

  case 1763:
#line 11889 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_GREATER);
  }
#line 18582 "parser.c" /* yacc.c:1646  */
    break;

  case 1764:
#line 11893 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_LESSER);
  }
#line 18590 "parser.c" /* yacc.c:1646  */
    break;

  case 1765:
#line 11897 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 18598 "parser.c" /* yacc.c:1646  */
    break;

  case 1766:
#line 11903 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18604 "parser.c" /* yacc.c:1646  */
    break;

  case 1767:
#line 11904 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18610 "parser.c" /* yacc.c:1646  */
    break;


#line 18614 "parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 12076 "parser.y" /* yacc.c:1906  */

