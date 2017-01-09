/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 268 of yacc.c  */
#line 27 "parser.y"

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
int				suppress_data_exceptions = 0;
unsigned int			cobc_repeat_last_token = 0;
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
static struct cb_cd		*current_cd;
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
static unsigned int		same_area;
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
static cob_flags_t		set_attr_val_on;
static cob_flags_t		set_attr_val_off;
static cob_flags_t		check_duplicate;
static cob_flags_t		check_on_off_duplicate;
static cob_flags_t		check_pic_duplicate;
static cob_flags_t		check_comp_duplicate;
static cob_flags_t		check_line_col_duplicate;
static unsigned int		skip_statements;
static unsigned int		start_debug;
static unsigned int		save_debug;
static unsigned int		needs_field_debug;
static unsigned int		needs_debug_item;
static unsigned int		env_div_seen;
static cob_flags_t		header_check;
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
#define	COBC_HD_COMMUNICATION_SECTION	(1U << 11)
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
void print_bits (cob_flags_t num)
{
	unsigned int 	size = sizeof (cob_flags_t);
	unsigned int	max_pow = 1 << (size * 8 - 1);
	int 		i = 0;

	for(; i < size * 8; ++i){
		/* Print last bit and shift left. */
		fprintf (stderr, "%u ", num & max_pow ? 1 : 0);
		num = num << 1;
 	}
	fprintf (stderr, "\n");
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
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", name);
		COBC_ABORT ();
	}
	if (cb_warn_terminator) {
		cb_warning_x (CB_TREE (current_statement),
			_("%s statement not terminated by END-%s"),
			name, name);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_error (cb_tree stmt, const unsigned int termid, const char *name)
{
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_error", name);
		COBC_ABORT ();
	}
	cb_error_x (CB_TREE (current_statement),
		_("%s statement not terminated by END-%s"),
		name, name);

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_clear (cb_tree stmt, const unsigned int termid)
{
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", current_statement->name);
		COBC_ABORT ();
	}
	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
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
setup_use_file (struct cb_file *fileptr)
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
emit_duplicate_clause_message (const char *clause)
{
	if (cb_relaxed_syntax_checks) {
		cb_warning (_("duplicate %s clause"), clause);
	} else {
		cb_error (_("duplicate %s clause"), clause);
	}
}

static void
check_repeated (const char *clause, const cob_flags_t bitval, cob_flags_t *already_seen)
{
	if (*already_seen & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		*already_seen |= bitval;
	}
}

static void
setup_occurs (void)
{
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->indexes == COB_MAX_SUBSCRIPTS) {
		cb_error (_ ("maximum OCCURS depth exceeded (%d)"),
			COB_MAX_SUBSCRIPTS);
	} else {
		current_field->indexes++;
	}

	if (current_field->flag_unbounded) {
		if (current_field->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (CB_TREE(current_field), _("'%s' is not in LINKAGE SECTION"),
				cb_name (CB_TREE(current_field)));
		}
	}

	if (current_field->flag_item_based) {
		cb_error (_ ("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_ ("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	}
	current_field->flag_occurs = 1;
}

static void
setup_occurs_min_max (cb_tree occurs_min, cb_tree occurs_max)
{
	if (occurs_max) {
		current_field->occurs_min = cb_get_int (occurs_min);
		if (occurs_max != cb_int0) {
			current_field->occurs_max = cb_get_int (occurs_max);
			if (!current_field->depending) {
				if (cb_relaxed_syntax_checks) {
					cb_warning (_ ("TO phrase without DEPENDING phrase"));
					cb_warning (_ ("maximum number of occurences assumed to be exact number"));
					current_field->occurs_min = 1; /* Checkme: why using 1 ? */
				} else {
					cb_error (_ ("TO phrase without DEPENDING phrase"));
				}
			}
			if (current_field->occurs_max <= current_field->occurs_min) {
				cb_error (_ ("OCCURS TO must be greater than OCCURS FROM"));
			}
		} else {
			current_field->occurs_max = 0;
		}
	} else {
		current_field->occurs_min = 1; /* Checkme: why using 1 ? */
		current_field->occurs_max = cb_get_int (occurs_min);
		if (current_field->depending) {
			cb_verify (cb_odo_without_to, _ ("ODO without TO phrase"));
		}
	}
}

static void
check_relaxed_syntax (const cob_flags_t lev)
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
	case COBC_HD_COMMUNICATION_SECTION:
		s = "COMMUNICATION SECTION";
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
check_headers_present (const cob_flags_t lev1, const cob_flags_t lev2,
		       const cob_flags_t lev3, const cob_flags_t lev4)
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
	set_attr_val_on = 0;
	set_attr_val_off = 0;
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
	suppress_data_exceptions = 0;
	same_area = 1;
	memset ((void *)eval_check, 0, sizeof(eval_check));
	memset ((void *)term_array, 0, sizeof(term_array));
	linage_file = NULL;
	current_file = NULL;
	current_cd = NULL;
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
		} else {
			prev = l;
		}
		if (prev->chain != NULL) {
			l = CB_LIST (prev->chain);
		} else {
			l = NULL;
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
				if (prev->chain != NULL) {
					l = CB_LIST (prev->chain);
				} else {
					l = NULL;
				}
				break;
			} else {
				prev = l;
				if (l->chain != NULL) {
					l = CB_LIST (l->chain);
				} else {
					l = NULL;
				}
			}
		}
	}
}

static int
setup_program (cb_tree id, cb_tree as_literal, const unsigned char type)
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

/* verify and set picture sign for currency */
static void
set_currency_picture_symbol (const cb_tree x)
{
	unsigned char	*s		= CB_LITERAL (x)->data;

	if (CB_LITERAL (x)->size != 1) {
		cb_error_x (x, _("PICTURE SYMBOL for CURRENCY must be one character long"));
		return;
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
		cb_error_x (x, _("invalid character '%c' in PICTURE SYMBOL for CURRENCY"), s[0]);
		return;
	default:
		break;
	}
	current_program->currency_symbol = s[0];
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
setup_prototype (cb_tree prototype_name, cb_tree ext_name,
		  const int type, const int is_current_element)
{
	cb_tree	prototype;
	int	name_redefinition_allowed;

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
check_not_both (const cob_flags_t flag1, const cob_flags_t flag2,
		const char *flag1_name, const char *flag2_name,
		const cob_flags_t flags, const cob_flags_t flag_to_set)
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
check_not_highlight_and_lowlight (const cob_flags_t flags,
				  const cob_flags_t flag_to_set)
{
	check_not_both (COB_SCREEN_HIGHLIGHT, COB_SCREEN_LOWLIGHT,
			"HIGHLIGHT", "LOWLIGHT", flags, flag_to_set);
}

static void
set_screen_attr (const char *clause, const cob_flags_t bitval)
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
set_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			const char *confl_clause, const cob_flags_t confl_bit,
			const int local_check_duplicate, cob_flags_t *flags)
{
	if (local_check_duplicate && (*flags & bitval)) {
		emit_duplicate_clause_message (clause);
	} else if (*flags & confl_bit) {
		emit_conflicting_clause_message (clause, confl_clause);
	} else {
	*flags |= bitval;
	}
}

static COB_INLINE COB_A_INLINE void
set_screen_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			       const char *confl_clause,
			       const cob_flags_t confl_bit)
{
	set_attr_with_conflict (clause, bitval, confl_clause, confl_bit, 1,
				&current_field->screen_flag);
}

static COB_INLINE COB_A_INLINE int
has_dispattr (const cob_flags_t attrib)
{
	return current_statement->attr_ptr
		&& current_statement->attr_ptr->dispattrs & attrib;
}

static void
attach_attrib_to_cur_stmt (void)
{
	if (!current_statement->attr_ptr) {
		current_statement->attr_ptr =
			cobc_parse_malloc (sizeof(struct cb_attr_struct));
	}
}

static COB_INLINE COB_A_INLINE void
set_dispattr (const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	current_statement->attr_ptr->dispattrs |= attrib;
}

static COB_INLINE COB_A_INLINE void
set_dispattr_with_conflict (const char *attrib_name, const cob_flags_t attrib,
			    const char *confl_name,
			    const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_attr_with_conflict (attrib_name, attrib, confl_name, confl_attrib, 0,
				&current_statement->attr_ptr->dispattrs);
}

static void
bit_set_attr (const cb_tree on_off, const cob_flags_t attr_val)
{
	if (on_off == cb_int1) {
		set_attr_val_on |= attr_val;
	} else {
		set_attr_val_off |= attr_val;
	}
}

static void
set_field_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
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
set_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
	     cb_tree timeout, cb_tree prompt, cb_tree size_is,
	     const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	current_statement->attr_ptr->dispattrs |= attrib;
}

static void
set_attribs_with_conflict  (cb_tree fgc, cb_tree bgc, cb_tree scroll,
			    cb_tree timeout, cb_tree prompt, cb_tree size_is,
			    const char *clause_name, const cob_flags_t attrib,
			    const char *confl_name, const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	set_dispattr_with_conflict (clause_name, attrib, confl_name,
				    confl_attrib);
}

static cob_flags_t
zero_conflicting_flag (const cob_flags_t screen_flag, cob_flags_t parent_flag,
				const cob_flags_t flag1, const cob_flags_t flag2)
{
	if (screen_flag & flag1) {
		parent_flag &= ~flag2;
	} else if (screen_flag & flag2) {
		parent_flag &= ~flag1;
	}

	return parent_flag;
}

static cob_flags_t
zero_conflicting_flags (const cob_flags_t screen_flag, cob_flags_t parent_flag)
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
check_and_set_usage (const enum cb_usage usage)
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
deduce_display_type (cb_tree x_list, cb_tree local_upon_value, cb_tree local_line_column,
		     struct cb_attr_struct * const attr_ptr)
{
	int	using_default_device_which_is_crt =
		local_upon_value == NULL && get_default_display_device () == cb_null;

	if (contains_only_screen_fields ((struct cb_list *) x_list)) {
		if (!contains_one_screen_field ((struct cb_list *) x_list)
		    || attr_ptr) {
			cb_verify_x (x_list, cb_accept_display_extensions,
				     _("non-standard DISPLAY"));
		}

		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screens may only be displayed on CRT"));
		}

		return SCREEN_DISPLAY;
	} else if (contains_fields_and_screens ((struct cb_list *) x_list)) {
		cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		return MIXED_DISPLAY;
	} else if (local_line_column || attr_ptr) {
		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screen clauses may only be used for DISPLAY on CRT"));
		}

		cb_verify_x (x_list, cb_accept_display_extensions,
			     _("non-standard DISPLAY"));

		return FIELD_ON_SCREEN_DISPLAY;
	} else if (local_upon_value == cb_null || using_default_device_which_is_crt) {
		/* This is the only format permitted by the standard */
		return FIELD_ON_SCREEN_DISPLAY;
	} else if (display_type == FIELD_ON_SCREEN_DISPLAY && local_upon_value == NULL) {
		/* This is for when fields without clauses follow fields with screen clauses */
		return FIELD_ON_SCREEN_DISPLAY;
	} else {
		return DEVICE_DISPLAY;
	}
}

static void
set_display_type (cb_tree x_list, cb_tree local_upon_value,
		  cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
	display_type = deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);
}

static void
error_if_different_display_type (cb_tree x_list, cb_tree local_upon_value,
				 cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
        const enum cb_display_type	type =
		deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);

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
		CB_REFERENCE_P (x) && CB_FIELD (cb_ref (x))
		&& CB_FIELD (cb_ref (x))->usage != CB_USAGE_DISPLAY;

	if (is_numeric_literal) {
		cb_error_x (x, _("%s is not an alphanumeric literal"), CB_LITERAL (x)->data);
	} else if (is_field_with_usage_not_display) {
		cb_error_x (x, _("'%s' is not USAGE DISPLAY"), cb_name (x));
	}
}



/* Line 268 of yacc.c  */
#line 1662 "parser.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
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
     AREAS = 279,
     ARGUMENT_NUMBER = 280,
     ARGUMENT_VALUE = 281,
     AS = 282,
     ASCENDING = 283,
     ASCII = 284,
     ASSIGN = 285,
     AT = 286,
     ATTRIBUTE = 287,
     AUTO = 288,
     AUTOMATIC = 289,
     AWAY_FROM_ZERO = 290,
     BACKGROUND_COLOR = 291,
     BASED = 292,
     BEFORE = 293,
     BELL = 294,
     BINARY = 295,
     BINARY_C_LONG = 296,
     BINARY_CHAR = 297,
     BINARY_DOUBLE = 298,
     BINARY_LONG = 299,
     BINARY_SHORT = 300,
     BLANK = 301,
     BLINK = 302,
     BLOCK = 303,
     BOTTOM = 304,
     BY = 305,
     BYTE_LENGTH = 306,
     CALL = 307,
     CANCEL = 308,
     CAPACITY = 309,
     CARD_PUNCH = 310,
     CARD_READER = 311,
     CASSETTE = 312,
     CD = 313,
     CF = 314,
     CH = 315,
     CHAINING = 316,
     CHARACTER = 317,
     CHARACTERS = 318,
     CLASS = 319,
     CLASSIFICATION = 320,
     CLASS_NAME = 321,
     CLOSE = 322,
     CODE = 323,
     CODE_SET = 324,
     COLLATING = 325,
     COL = 326,
     COLS = 327,
     COLUMN = 328,
     COLUMNS = 329,
     COMMA = 330,
     COMMAND_LINE = 331,
     COMMA_DELIM = 332,
     COMMIT = 333,
     COMMON = 334,
     COMMUNICATION = 335,
     COMP = 336,
     COMPUTE = 337,
     COMP_1 = 338,
     COMP_2 = 339,
     COMP_3 = 340,
     COMP_4 = 341,
     COMP_5 = 342,
     COMP_6 = 343,
     COMP_X = 344,
     CONCATENATE_FUNC = 345,
     CONDITION = 346,
     CONFIGURATION = 347,
     CONSTANT = 348,
     CONTAINS = 349,
     CONTENT = 350,
     CONTINUE = 351,
     CONTROL = 352,
     CONTROLS = 353,
     CONVERSION = 354,
     CONVERTING = 355,
     COPY = 356,
     CORRESPONDING = 357,
     COUNT = 358,
     CRT = 359,
     CRT_UNDER = 360,
     CURRENCY = 361,
     CURRENT_DATE_FUNC = 362,
     CURSOR = 363,
     CYCLE = 364,
     DATA = 365,
     DATE = 366,
     DAY = 367,
     DAY_OF_WEEK = 368,
     DE = 369,
     DEBUGGING = 370,
     DECIMAL_POINT = 371,
     DECLARATIVES = 372,
     DEFAULT = 373,
     DELETE = 374,
     DELIMITED = 375,
     DELIMITER = 376,
     DEPENDING = 377,
     DESCENDING = 378,
     DESTINATION = 379,
     DETAIL = 380,
     DISABLE = 381,
     DISC = 382,
     DISK = 383,
     DISPLAY = 384,
     DISPLAY_OF_FUNC = 385,
     DIVIDE = 386,
     DIVISION = 387,
     DOWN = 388,
     DUPLICATES = 389,
     DYNAMIC = 390,
     EBCDIC = 391,
     EC = 392,
     ECHO = 393,
     EGI = 394,
     EIGHTY_EIGHT = 395,
     ENABLE = 396,
     ELSE = 397,
     EMI = 398,
     END = 399,
     END_ACCEPT = 400,
     END_ADD = 401,
     END_CALL = 402,
     END_COMPUTE = 403,
     END_DELETE = 404,
     END_DISPLAY = 405,
     END_DIVIDE = 406,
     END_EVALUATE = 407,
     END_FUNCTION = 408,
     END_IF = 409,
     END_MULTIPLY = 410,
     END_PERFORM = 411,
     END_PROGRAM = 412,
     END_READ = 413,
     END_RECEIVE = 414,
     END_RETURN = 415,
     END_REWRITE = 416,
     END_SEARCH = 417,
     END_START = 418,
     END_STRING = 419,
     END_SUBTRACT = 420,
     END_UNSTRING = 421,
     END_WRITE = 422,
     ENTRY = 423,
     ENVIRONMENT = 424,
     ENVIRONMENT_NAME = 425,
     ENVIRONMENT_VALUE = 426,
     EOL = 427,
     EOP = 428,
     EOS = 429,
     EQUAL = 430,
     ERASE = 431,
     ERROR = 432,
     ESCAPE = 433,
     ESI = 434,
     EVALUATE = 435,
     EVENT_STATUS = 436,
     EXCEPTION = 437,
     EXCEPTION_CONDITION = 438,
     EXCLUSIVE = 439,
     EXIT = 440,
     EXPONENTIATION = 441,
     EXTEND = 442,
     EXTERNAL = 443,
     F = 444,
     FD = 445,
     FILE_CONTROL = 446,
     FILE_ID = 447,
     FILLER = 448,
     FINAL = 449,
     FIRST = 450,
     FIXED = 451,
     FLOAT_BINARY_128 = 452,
     FLOAT_BINARY_32 = 453,
     FLOAT_BINARY_64 = 454,
     FLOAT_DECIMAL_16 = 455,
     FLOAT_DECIMAL_34 = 456,
     FLOAT_DECIMAL_7 = 457,
     FLOAT_EXTENDED = 458,
     FLOAT_LONG = 459,
     FLOAT_SHORT = 460,
     FOOTING = 461,
     FOR = 462,
     FOREGROUND_COLOR = 463,
     FOREVER = 464,
     FORMATTED_DATE_FUNC = 465,
     FORMATTED_DATETIME_FUNC = 466,
     FORMATTED_TIME_FUNC = 467,
     FREE = 468,
     FROM = 469,
     FROM_CRT = 470,
     FULL = 471,
     FUNCTION = 472,
     FUNCTION_ID = 473,
     FUNCTION_NAME = 474,
     GENERATE = 475,
     GIVING = 476,
     GLOBAL = 477,
     GO = 478,
     GOBACK = 479,
     GREATER = 480,
     GREATER_OR_EQUAL = 481,
     GRID = 482,
     GROUP = 483,
     HEADING = 484,
     HIGHLIGHT = 485,
     HIGH_VALUE = 486,
     ID = 487,
     IDENTIFICATION = 488,
     IF = 489,
     IGNORE = 490,
     IGNORING = 491,
     IN = 492,
     INDEX = 493,
     INDEXED = 494,
     INDICATE = 495,
     INITIALIZE = 496,
     INITIALIZED = 497,
     INITIATE = 498,
     INPUT = 499,
     INPUT_OUTPUT = 500,
     INSPECT = 501,
     INTO = 502,
     INTRINSIC = 503,
     INVALID = 504,
     INVALID_KEY = 505,
     IS = 506,
     I_O = 507,
     I_O_CONTROL = 508,
     JUSTIFIED = 509,
     KEPT = 510,
     KEY = 511,
     KEYBOARD = 512,
     LABEL = 513,
     LAST = 514,
     LEADING = 515,
     LEFT = 516,
     LEFTLINE = 517,
     LENGTH = 518,
     LENGTH_FUNC = 519,
     LENGTH_OF = 520,
     LESS = 521,
     LESS_OR_EQUAL = 522,
     LIMIT = 523,
     LIMITS = 524,
     LINAGE = 525,
     LINAGE_COUNTER = 526,
     LINE = 527,
     LINE_COUNTER = 528,
     LINES = 529,
     LINKAGE = 530,
     LITERAL = 531,
     LOCALE = 532,
     LOCALE_DATE_FUNC = 533,
     LOCALE_TIME_FUNC = 534,
     LOCALE_TIME_FROM_FUNC = 535,
     LOCAL_STORAGE = 536,
     LOCK = 537,
     LOWER = 538,
     LOWER_CASE_FUNC = 539,
     LOWLIGHT = 540,
     LOW_VALUE = 541,
     MANUAL = 542,
     MAGNETIC_TAPE = 543,
     MEMORY = 544,
     MERGE = 545,
     MESSAGE = 546,
     MINUS = 547,
     MNEMONIC_NAME = 548,
     MODE = 549,
     MOVE = 550,
     MULTIPLE = 551,
     MULTIPLY = 552,
     NAME = 553,
     NATIONAL = 554,
     NATIONAL_EDITED = 555,
     NATIONAL_OF_FUNC = 556,
     NATIVE = 557,
     NEAREST_AWAY_FROM_ZERO = 558,
     NEAREST_EVEN = 559,
     NEAREST_TOWARD_ZERO = 560,
     NEGATIVE = 561,
     NEXT = 562,
     NEXT_PAGE = 563,
     NO = 564,
     NO_DATA = 565,
     NO_ECHO = 566,
     NORMAL = 567,
     NOT = 568,
     NOTHING = 569,
     NOT_END = 570,
     NOT_EOP = 571,
     NOT_ESCAPE = 572,
     NOT_EQUAL = 573,
     NOT_EXCEPTION = 574,
     NOT_INVALID_KEY = 575,
     NOT_OVERFLOW = 576,
     NOT_SIZE_ERROR = 577,
     NO_ADVANCING = 578,
     NUMBER = 579,
     NUMBERS = 580,
     NUMERIC = 581,
     NUMERIC_EDITED = 582,
     NUMVALC_FUNC = 583,
     OBJECT_COMPUTER = 584,
     OCCURS = 585,
     OF = 586,
     OFF = 587,
     OMITTED = 588,
     ON = 589,
     ONLY = 590,
     OPEN = 591,
     OPTIONAL = 592,
     OR = 593,
     ORDER = 594,
     ORGANIZATION = 595,
     OTHER = 596,
     OUTPUT = 597,
     OVERLINE = 598,
     PACKED_DECIMAL = 599,
     PADDING = 600,
     PAGE = 601,
     PAGE_COUNTER = 602,
     PARAGRAPH = 603,
     PERFORM = 604,
     PH = 605,
     PF = 606,
     PICTURE = 607,
     PICTURE_SYMBOL = 608,
     PLUS = 609,
     POINTER = 610,
     POSITION = 611,
     POSITIVE = 612,
     PRESENT = 613,
     PREVIOUS = 614,
     PRINT = 615,
     PRINTER = 616,
     PRINTER_1 = 617,
     PRINTING = 618,
     PROCEDURE = 619,
     PROCEDURES = 620,
     PROCEED = 621,
     PROGRAM = 622,
     PROGRAM_ID = 623,
     PROGRAM_NAME = 624,
     PROGRAM_POINTER = 625,
     PROHIBITED = 626,
     PROMPT = 627,
     PROTECTED = 628,
     QUEUE = 629,
     QUOTE = 630,
     RANDOM = 631,
     RD = 632,
     READ = 633,
     READY_TRACE = 634,
     RECEIVE = 635,
     RECORD = 636,
     RECORDING = 637,
     RECORDS = 638,
     RECURSIVE = 639,
     REDEFINES = 640,
     REEL = 641,
     REFERENCE = 642,
     REFERENCES = 643,
     RELATIVE = 644,
     RELEASE = 645,
     REMAINDER = 646,
     REMOVAL = 647,
     RENAMES = 648,
     REPLACE = 649,
     REPLACING = 650,
     REPORT = 651,
     REPORTING = 652,
     REPORTS = 653,
     REPOSITORY = 654,
     REQUIRED = 655,
     RESERVE = 656,
     RESET = 657,
     RESET_TRACE = 658,
     RETRY = 659,
     RETURN = 660,
     RETURNING = 661,
     REVERSE = 662,
     REVERSE_FUNC = 663,
     REVERSE_VIDEO = 664,
     REVERSED = 665,
     REWIND = 666,
     REWRITE = 667,
     RF = 668,
     RH = 669,
     RIGHT = 670,
     ROLLBACK = 671,
     ROUNDED = 672,
     RUN = 673,
     S = 674,
     SAME = 675,
     SCREEN = 676,
     SCREEN_CONTROL = 677,
     SCROLL = 678,
     SD = 679,
     SEARCH = 680,
     SECONDS = 681,
     SECTION = 682,
     SECURE = 683,
     SEGMENT = 684,
     SEGMENT_LIMIT = 685,
     SELECT = 686,
     SEMI_COLON = 687,
     SENTENCE = 688,
     SEPARATE = 689,
     SEQUENCE = 690,
     SEQUENTIAL = 691,
     SET = 692,
     SEVENTY_EIGHT = 693,
     SHARING = 694,
     SIGN = 695,
     SIGNED = 696,
     SIGNED_INT = 697,
     SIGNED_LONG = 698,
     SIGNED_SHORT = 699,
     SIXTY_SIX = 700,
     SIZE = 701,
     SIZE_ERROR = 702,
     SORT = 703,
     SORT_MERGE = 704,
     SOURCE = 705,
     SOURCE_COMPUTER = 706,
     SPACE = 707,
     SPECIAL_NAMES = 708,
     STANDARD = 709,
     STANDARD_1 = 710,
     STANDARD_2 = 711,
     START = 712,
     STATIC = 713,
     STATUS = 714,
     STDCALL = 715,
     STEP = 716,
     STOP = 717,
     STRING = 718,
     SUB_QUEUE_1 = 719,
     SUB_QUEUE_2 = 720,
     SUB_QUEUE_3 = 721,
     SUBSTITUTE_FUNC = 722,
     SUBSTITUTE_CASE_FUNC = 723,
     SUBTRACT = 724,
     SUM = 725,
     SUPPRESS = 726,
     SYMBOLIC = 727,
     SYNCHRONIZED = 728,
     SYSTEM_DEFAULT = 729,
     SYSTEM_OFFSET = 730,
     TAB = 731,
     TABLE = 732,
     TALLYING = 733,
     TAPE = 734,
     TERMINAL = 735,
     TERMINATE = 736,
     TEXT = 737,
     TEST = 738,
     THAN = 739,
     THEN = 740,
     THRU = 741,
     TIME = 742,
     TIME_OUT = 743,
     TIMES = 744,
     TO = 745,
     TOK_AMPER = 746,
     TOK_CLOSE_PAREN = 747,
     TOK_COLON = 748,
     TOK_DIV = 749,
     TOK_DOT = 750,
     TOK_EQUAL = 751,
     TOK_FALSE = 752,
     TOK_FILE = 753,
     TOK_GREATER = 754,
     TOK_INITIAL = 755,
     TOK_LESS = 756,
     TOK_MINUS = 757,
     TOK_MUL = 758,
     TOK_NULL = 759,
     TOK_OVERFLOW = 760,
     TOK_OPEN_PAREN = 761,
     TOK_PLUS = 762,
     TOK_TRUE = 763,
     TOP = 764,
     TOWARD_GREATER = 765,
     TOWARD_LESSER = 766,
     TRAILING = 767,
     TRANSFORM = 768,
     TRIM_FUNC = 769,
     TRUNCATION = 770,
     TYPE = 771,
     U = 772,
     UNBOUNDED = 773,
     UNDERLINE = 774,
     UNIT = 775,
     UNLOCK = 776,
     UNSIGNED = 777,
     UNSIGNED_INT = 778,
     UNSIGNED_LONG = 779,
     UNSIGNED_SHORT = 780,
     UNSTRING = 781,
     UNTIL = 782,
     UP = 783,
     UPDATE = 784,
     UPON = 785,
     UPON_ARGUMENT_NUMBER = 786,
     UPON_COMMAND_LINE = 787,
     UPON_ENVIRONMENT_NAME = 788,
     UPON_ENVIRONMENT_VALUE = 789,
     UPPER = 790,
     UPPER_CASE_FUNC = 791,
     USAGE = 792,
     USE = 793,
     USER = 794,
     USER_DEFAULT = 795,
     USER_FUNCTION_NAME = 796,
     USING = 797,
     V = 798,
     VALUE = 799,
     VARIABLE = 800,
     VARYING = 801,
     WAIT = 802,
     WHEN = 803,
     WHEN_COMPILED_FUNC = 804,
     WITH = 805,
     WORD = 806,
     WORDS = 807,
     WORKING_STORAGE = 808,
     WRITE = 809,
     YYYYDDD = 810,
     YYYYMMDD = 811,
     ZERO = 812,
     SHIFT_PREFER = 813,
     PURGE = 814,
     SEND = 815,
     OVERFLOW = 816
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 343 of yacc.c  */
#line 2266 "parser.c"

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
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
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
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   9768

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  562
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  941
/* YYNRULES -- Number of rules.  */
#define YYNRULES  2168
/* YYNRULES -- Number of states.  */
#define YYNSTATES  3138

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   816

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
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
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    11,    13,    16,    18,
      20,    21,    24,    29,    34,    35,    37,    39,    42,    46,
      50,    54,    55,    59,    63,    64,    65,    74,    75,    82,
      84,    86,    88,    90,    91,    94,    95,    99,   101,   103,
     105,   107,   110,   113,   115,   117,   121,   122,   126,   132,
     133,   137,   138,   140,   142,   145,   148,   149,   154,   155,
     159,   160,   164,   165,   170,   171,   174,   178,   181,   183,
     186,   188,   190,   192,   194,   200,   204,   208,   213,   215,
     217,   219,   221,   223,   226,   227,   228,   233,   234,   237,
     241,   243,   246,   250,   254,   258,   262,   264,   267,   268,
     271,   272,   274,   277,   281,   283,   286,   288,   290,   292,
     294,   296,   298,   300,   302,   304,   306,   308,   310,   311,
     315,   318,   322,   326,   328,   329,   331,   333,   337,   342,
     343,   349,   351,   353,   355,   357,   359,   361,   363,   366,
     368,   372,   373,   378,   380,   384,   386,   388,   390,   392,
     394,   396,   398,   400,   403,   404,   407,   411,   413,   416,
     420,   422,   425,   427,   430,   435,   437,   440,   442,   446,
     451,   457,   458,   462,   466,   472,   476,   481,   485,   489,
     495,   496,   500,   501,   504,   505,   508,   509,   512,   513,
     520,   521,   524,   526,   528,   530,   532,   534,   536,   538,
     540,   542,   544,   546,   548,   550,   556,   562,   568,   574,
     580,   586,   588,   590,   592,   594,   596,   598,   600,   602,
     604,   606,   608,   610,   612,   614,   615,   619,   620,   622,
     624,   626,   628,   629,   631,   633,   638,   640,   642,   644,
     652,   653,   658,   662,   666,   668,   673,   674,   676,   678,
     679,   685,   688,   691,   693,   694,   699,   705,   708,   712,
     714,   716,   720,   722,   725,   730,   735,   740,   742,   746,
     751,   756,   760,   762,   764,   768,   771,   774,   777,   778,
     781,   785,   787,   790,   792,   794,   800,   801,   803,   805,
     807,   808,   815,   817,   820,   823,   824,   827,   828,   839,
     840,   844,   845,   849,   850,   853,   856,   857,   863,   867,
     869,   871,   872,   875,   878,   881,   883,   885,   887,   889,
     891,   893,   895,   897,   899,   905,   906,   908,   910,   915,
     922,   932,   933,   937,   938,   941,   942,   945,   949,   955,
     961,   963,   965,   967,   969,   973,   979,   980,   983,   985,
     987,   989,   994,   997,  1000,  1005,  1007,  1009,  1011,  1013,
    1015,  1017,  1019,  1024,  1025,  1028,  1031,  1034,  1037,  1039,
    1042,  1043,  1044,  1050,  1051,  1054,  1057,  1058,  1064,  1065,
    1068,  1073,  1077,  1082,  1083,  1085,  1087,  1089,  1092,  1097,
    1102,  1107,  1112,  1117,  1122,  1127,  1132,  1137,  1142,  1147,
    1159,  1160,  1162,  1164,  1167,  1172,  1177,  1182,  1189,  1194,
    1198,  1203,  1204,  1206,  1208,  1210,  1213,  1218,  1223,  1228,
    1233,  1238,  1243,  1250,  1251,  1252,  1258,  1259,  1260,  1263,
    1266,  1270,  1272,  1274,  1276,  1277,  1282,  1286,  1289,  1290,
    1292,  1294,  1296,  1297,  1300,  1302,  1305,  1308,  1312,  1314,
    1316,  1318,  1320,  1322,  1324,  1326,  1328,  1330,  1332,  1334,
    1336,  1339,  1341,  1343,  1345,  1347,  1349,  1351,  1353,  1355,
    1357,  1363,  1364,  1367,  1368,  1373,  1379,  1380,  1386,  1389,
    1392,  1393,  1396,  1398,  1400,  1402,  1404,  1406,  1408,  1410,
    1412,  1414,  1416,  1418,  1420,  1422,  1425,  1429,  1430,  1433,
    1434,  1436,  1439,  1441,  1443,  1447,  1449,  1451,  1453,  1455,
    1457,  1459,  1461,  1463,  1465,  1467,  1469,  1471,  1473,  1475,
    1477,  1479,  1481,  1483,  1485,  1487,  1490,  1493,  1496,  1499,
    1502,  1505,  1508,  1511,  1514,  1517,  1519,  1521,  1523,  1525,
    1527,  1529,  1531,  1533,  1535,  1537,  1541,  1545,  1552,  1553,
    1556,  1564,  1574,  1583,  1584,  1587,  1588,  1591,  1592,  1595,
    1596,  1600,  1601,  1605,  1606,  1608,  1610,  1611,  1617,  1619,
    1621,  1622,  1626,  1628,  1631,  1633,  1636,  1639,  1643,  1645,
    1646,  1652,  1654,  1657,  1659,  1663,  1664,  1669,  1672,  1675,
    1676,  1677,  1683,  1684,  1685,  1691,  1692,  1693,  1699,  1700,
    1703,  1704,  1711,  1712,  1715,  1718,  1721,  1725,  1727,  1729,
    1732,  1735,  1737,  1740,  1745,  1747,  1752,  1755,  1756,  1759,
    1761,  1763,  1765,  1767,  1769,  1773,  1778,  1783,  1788,  1792,
    1793,  1796,  1797,  1803,  1804,  1807,  1809,  1811,  1813,  1815,
    1817,  1819,  1821,  1823,  1825,  1827,  1829,  1831,  1833,  1835,
    1837,  1839,  1843,  1845,  1847,  1850,  1852,  1855,  1857,  1859,
    1860,  1863,  1866,  1867,  1870,  1875,  1880,  1881,  1885,  1887,
    1889,  1893,  1900,  1903,  1907,  1910,  1913,  1917,  1920,  1922,
    1925,  1928,  1930,  1932,  1934,  1937,  1940,  1942,  1947,  1950,
    1954,  1955,  1956,  1962,  1963,  1965,  1968,  1972,  1974,  1975,
    1980,  1984,  1985,  1988,  1991,  1994,  1996,  1998,  2001,  2004,
    2006,  2008,  2010,  2012,  2014,  2016,  2018,  2020,  2022,  2024,
    2026,  2028,  2030,  2035,  2037,  2039,  2045,  2051,  2055,  2059,
    2061,  2063,  2065,  2067,  2069,  2071,  2073,  2075,  2078,  2081,
    2084,  2086,  2089,  2091,  2094,  2096,  2098,  2100,  2102,  2103,
    2105,  2107,  2108,  2110,  2112,  2116,  2119,  2120,  2121,  2122,
    2132,  2133,  2138,  2139,  2140,  2144,  2145,  2149,  2151,  2154,
    2159,  2160,  2163,  2166,  2167,  2171,  2175,  2180,  2185,  2189,
    2190,  2192,  2193,  2196,  2199,  2200,  2201,  2209,  2210,  2213,
    2215,  2217,  2220,  2222,  2224,  2225,  2232,  2233,  2236,  2239,
    2241,  2242,  2244,  2245,  2246,  2250,  2251,  2254,  2257,  2259,
    2261,  2263,  2265,  2267,  2269,  2271,  2273,  2275,  2277,  2279,
    2281,  2283,  2285,  2287,  2289,  2291,  2293,  2295,  2297,  2299,
    2301,  2303,  2305,  2307,  2309,  2311,  2313,  2315,  2317,  2319,
    2321,  2323,  2325,  2327,  2329,  2331,  2333,  2335,  2337,  2339,
    2341,  2343,  2345,  2347,  2349,  2351,  2353,  2355,  2357,  2359,
    2361,  2363,  2365,  2368,  2371,  2372,  2377,  2378,  2383,  2387,
    2391,  2396,  2400,  2405,  2409,  2413,  2418,  2423,  2427,  2432,
    2436,  2441,  2447,  2451,  2456,  2460,  2464,  2468,  2470,  2472,
    2473,  2475,  2477,  2480,  2482,  2484,  2486,  2489,  2493,  2495,
    2498,  2501,  2504,  2507,  2511,  2515,  2519,  2523,  2525,  2527,
    2529,  2531,  2533,  2535,  2537,  2539,  2541,  2543,  2545,  2547,
    2552,  2554,  2556,  2558,  2560,  2565,  2569,  2571,  2574,  2576,
    2578,  2582,  2586,  2590,  2594,  2598,  2601,  2603,  2605,  2607,
    2609,  2611,  2613,  2615,  2616,  2618,  2619,  2624,  2629,  2635,
    2642,  2643,  2646,  2647,  2649,  2650,  2654,  2658,  2663,  2664,
    2667,  2668,  2672,  2674,  2677,  2682,  2683,  2686,  2687,  2692,
    2693,  2700,  2701,  2703,  2705,  2707,  2709,  2711,  2712,  2713,
    2717,  2719,  2722,  2725,  2729,  2730,  2733,  2736,  2739,  2740,
    2744,  2747,  2750,  2755,  2757,  2759,  2761,  2763,  2764,  2767,
    2770,  2771,  2773,  2776,  2779,  2780,  2782,  2785,  2786,  2788,
    2789,  2793,  2795,  2798,  2800,  2802,  2803,  2807,  2810,  2814,
    2815,  2817,  2821,  2825,  2828,  2829,  2834,  2839,  2840,  2842,
    2844,  2846,  2847,  2852,  2857,  2860,  2862,  2865,  2866,  2868,
    2869,  2873,  2877,  2878,  2882,  2883,  2886,  2888,  2891,  2893,
    2894,  2899,  2903,  2907,  2911,  2915,  2918,  2921,  2923,  2925,
    2928,  2929,  2933,  2935,  2937,  2939,  2942,  2944,  2947,  2949,
    2951,  2954,  2957,  2960,  2963,  2966,  2968,  2970,  2972,  2975,
    2978,  2980,  2982,  2985,  2988,  2990,  2992,  2994,  2996,  3000,
    3002,  3006,  3010,  3014,  3018,  3019,  3021,  3022,  3027,  3032,
    3039,  3046,  3055,  3064,  3065,  3067,  3068,  3072,  3073,  3077,
    3080,  3081,  3086,  3089,  3091,  3095,  3097,  3099,  3101,  3104,
    3106,  3108,  3111,  3114,  3118,  3121,  3125,  3127,  3131,  3134,
    3136,  3138,  3140,  3141,  3144,  3145,  3147,  3148,  3152,  3153,
    3156,  3158,  3161,  3163,  3165,  3167,  3168,  3171,  3172,  3176,
    3178,  3179,  3183,  3185,  3186,  3190,  3194,  3195,  3199,  3202,
    3203,  3210,  3214,  3217,  3219,  3220,  3222,  3223,  3227,  3233,
    3234,  3237,  3238,  3242,  3246,  3247,  3250,  3252,  3255,  3260,
    3262,  3264,  3266,  3268,  3270,  3272,  3274,  3275,  3279,  3280,
    3284,  3286,  3289,  3290,  3294,  3297,  3299,  3301,  3303,  3306,
    3308,  3310,  3312,  3313,  3317,  3320,  3326,  3328,  3331,  3334,
    3337,  3339,  3341,  3343,  3346,  3348,  3351,  3356,  3359,  3360,
    3362,  3364,  3366,  3368,  3373,  3374,  3376,  3378,  3381,  3384,
    3388,  3392,  3393,  3397,  3398,  3402,  3406,  3411,  3412,  3417,
    3422,  3429,  3430,  3432,  3433,  3437,  3439,  3442,  3448,  3450,
    3452,  3454,  3456,  3457,  3461,  3462,  3466,  3469,  3471,  3472,
    3476,  3479,  3480,  3485,  3488,  3489,  3491,  3493,  3495,  3497,
    3501,  3502,  3505,  3507,  3511,  3515,  3516,  3520,  3522,  3524,
    3526,  3530,  3538,  3539,  3543,  3544,  3549,  3557,  3558,  3561,
    3562,  3564,  3567,  3569,  3572,  3576,  3580,  3582,  3583,  3585,
    3587,  3592,  3597,  3600,  3601,  3603,  3605,  3609,  3612,  3613,
    3617,  3619,  3621,  3622,  3624,  3626,  3627,  3632,  3638,  3640,
    3642,  3643,  3646,  3649,  3650,  3652,  3655,  3656,  3658,  3661,
    3662,  3664,  3665,  3669,  3672,  3674,  3675,  3680,  3685,  3686,
    3688,  3689,  3694,  3700,  3701,  3703,  3706,  3710,  3711,  3713,
    3715,  3716,  3721,  3726,  3733,  3734,  3737,  3738,  3741,  3743,
    3746,  3750,  3751,  3753,  3754,  3758,  3761,  3767,  3768,  3770,
    3773,  3776,  3779,  3782,  3785,  3786,  3789,  3790,  3794,  3796,
    3798,  3800,  3802,  3804,  3806,  3808,  3810,  3812,  3814,  3816,
    3821,  3825,  3827,  3830,  3833,  3836,  3839,  3842,  3845,  3848,
    3851,  3854,  3859,  3863,  3868,  3870,  3873,  3877,  3879,  3882,
    3886,  3890,  3895,  3896,  3900,  3901,  3909,  3910,  3916,  3917,
    3920,  3921,  3924,  3925,  3929,  3930,  3933,  3938,  3939,  3942,
    3947,  3948,  3953,  3958,  3959,  3963,  3964,  3969,  3971,  3973,
    3975,  3978,  3981,  3984,  3987,  3989,  3991,  3994,  3996,  3997,
    3999,  4000,  4005,  4008,  4009,  4012,  4014,  4019,  4024,  4025,
    4027,  4029,  4031,  4033,  4035,  4036,  4041,  4047,  4049,  4052,
    4055,  4056,  4060,  4062,  4064,  4065,  4070,  4071,  4073,  4074,
    4079,  4084,  4091,  4098,  4099,  4101,  4104,  4105,  4107,  4108,
    4112,  4114,  4117,  4118,  4122,  4128,  4129,  4133,  4136,  4137,
    4142,  4149,  4150,  4154,  4156,  4160,  4163,  4166,  4169,  4173,
    4174,  4178,  4179,  4183,  4184,  4188,  4189,  4191,  4192,  4196,
    4198,  4200,  4202,  4204,  4206,  4214,  4215,  4217,  4219,  4221,
    4223,  4225,  4227,  4232,  4234,  4237,  4239,  4242,  4246,  4247,
    4249,  4252,  4254,  4258,  4260,  4262,  4267,  4269,  4271,  4273,
    4274,  4279,  4286,  4287,  4290,  4291,  4296,  4300,  4304,  4306,
    4308,  4309,  4311,  4313,  4314,  4316,  4317,  4320,  4323,  4324,
    4326,  4329,  4331,  4333,  4334,  4336,  4339,  4341,  4343,  4344,
    4347,  4350,  4351,  4353,  4356,  4357,  4359,  4362,  4363,  4366,
    4369,  4370,  4372,  4375,  4376,  4378,  4381,  4382,  4385,  4388,
    4389,  4391,  4394,  4395,  4397,  4400,  4403,  4406,  4409,  4412,
    4413,  4415,  4418,  4419,  4421,  4424,  4427,  4430,  4431,  4433,
    4436,  4437,  4439,  4442,  4443,  4445,  4448,  4451,  4452,  4454,
    4457,  4458,  4460,  4463,  4464,  4467,  4469,  4471,  4472,  4475,
    4477,  4480,  4483,  4485,  4487,  4489,  4491,  4493,  4495,  4497,
    4499,  4501,  4503,  4505,  4507,  4509,  4511,  4513,  4515,  4517,
    4519,  4521,  4523,  4525,  4527,  4529,  4531,  4533,  4535,  4538,
    4540,  4542,  4544,  4546,  4548,  4550,  4552,  4556,  4557,  4559,
    4561,  4565,  4569,  4571,  4575,  4579,  4581,  4585,  4587,  4590,
    4593,  4595,  4599,  4601,  4603,  4607,  4609,  4613,  4615,  4619,
    4621,  4624,  4627,  4629,  4631,  4634,  4636,  4638,  4641,  4643,
    4645,  4647,  4649,  4652,  4654,  4655,  4658,  4660,  4662,  4664,
    4668,  4670,  4672,  4675,  4677,  4679,  4681,  4684,  4686,  4688,
    4690,  4692,  4694,  4696,  4698,  4701,  4703,  4705,  4709,  4710,
    4712,  4714,  4717,  4719,  4721,  4723,  4725,  4727,  4729,  4731,
    4734,  4737,  4740,  4745,  4749,  4751,  4753,  4756,  4758,  4760,
    4762,  4764,  4766,  4768,  4770,  4773,  4776,  4779,  4781,  4783,
    4785,  4787,  4789,  4791,  4793,  4795,  4797,  4799,  4801,  4803,
    4805,  4807,  4809,  4811,  4813,  4815,  4817,  4819,  4821,  4823,
    4825,  4827,  4829,  4831,  4833,  4835,  4837,  4839,  4841,  4843,
    4846,  4848,  4850,  4852,  4854,  4858,  4861,  4864,  4866,  4868,
    4872,  4875,  4878,  4880,  4882,  4886,  4890,  4895,  4901,  4903,
    4905,  4907,  4909,  4911,  4913,  4915,  4917,  4919,  4921,  4923,
    4926,  4928,  4932,  4934,  4936,  4938,  4940,  4942,  4944,  4946,
    4949,  4955,  4961,  4967,  4972,  4977,  4983,  4989,  4995,  5001,
    5007,  5010,  5013,  5015,  5017,  5019,  5021,  5023,  5025,  5027,
    5029,  5031,  5032,  5037,  5043,  5044,  5048,  5051,  5053,  5057,
    5061,  5062,  5065,  5067,  5071,  5073,  5077,  5079,  5083,  5085,
    5089,  5090,  5091,  5093,  5094,  5096,  5097,  5099,  5100,  5103,
    5104,  5107,  5108,  5110,  5112,  5113,  5115,  5116,  5118,  5121,
    5122,  5125,  5126,  5130,  5132,  5134,  5136,  5138,  5140,  5142,
    5144,  5146,  5147,  5150,  5152,  5154,  5156,  5158,  5160,  5162,
    5164,  5166,  5168,  5170,  5172,  5174,  5176,  5178,  5180,  5182,
    5184,  5186,  5188,  5190,  5192,  5194,  5196,  5198,  5200,  5202,
    5204,  5206,  5208,  5210,  5212,  5214,  5216,  5218,  5220,  5222,
    5224,  5226,  5228,  5230,  5232,  5234,  5236,  5238,  5240,  5242,
    5244,  5246,  5248,  5250,  5252,  5254,  5256,  5258,  5260,  5262,
    5264,  5266,  5268,  5270,  5272,  5274,  5276,  5278,  5280,  5282,
    5284,  5286,  5288,  5290,  5292,  5294,  5296,  5297,  5299,  5300,
    5302,  5303,  5305,  5306,  5308,  5309,  5311,  5313,  5314,  5316,
    5317,  5319,  5320,  5322,  5323,  5325,  5326,  5328,  5329,  5331,
    5332,  5334,  5335,  5337,  5338,  5340,  5341,  5344,  5345,  5347,
    5348,  5350,  5351,  5353,  5354,  5356,  5357,  5359,  5360,  5362,
    5365,  5366,  5368,  5369,  5371,  5372,  5374,  5375,  5377,  5378,
    5380,  5382,  5383,  5385,  5386,  5388,  5390,  5391,  5393,  5394,
    5396,  5398,  5399,  5402,  5405,  5406,  5408,  5409,  5411,  5412,
    5414,  5415,  5417,  5418,  5420,  5422,  5423,  5425,  5426,  5428,
    5429,  5432,  5434,  5436,  5437,  5439,  5440,  5442,  5443,  5445,
    5446,  5448,  5449,  5451,  5453,  5454,  5456,  5457,  5459,  5460,
    5462,  5463,  5465,  5468,  5469,  5471,  5472,  5474,  5475,  5477,
    5478,  5480,  5481,  5483,  5484,  5486,  5487,  5489,  5490,  5492,
    5493,  5495,  5496,  5498,  5500,  5501,  5503,  5504,  5508,  5509,
    5511,  5514,  5516,  5518,  5520,  5522,  5524,  5526,  5528,  5530,
    5532,  5534,  5536,  5538,  5540,  5542,  5544,  5546,  5548,  5550,
    5552,  5555,  5558,  5560,  5562,  5564,  5566,  5568,  5570,  5573,
    5575,  5579,  5582,  5584,  5586,  5588,  5591,  5593,  5596,  5598,
    5601,  5603,  5606,  5608,  5611,  5613,  5616,  5618,  5621
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     563,     0,    -1,    -1,   564,   565,    -1,   568,    -1,   566,
      -1,   567,    -1,   566,   567,    -1,   570,    -1,   571,    -1,
      -1,   569,   576,    -1,   577,   578,   576,   572,    -1,   577,
     581,   576,   575,    -1,    -1,   573,    -1,   574,    -1,   573,
     574,    -1,   157,   584,   495,    -1,   153,   584,   495,    -1,
     590,   705,   887,    -1,    -1,   233,   132,   495,    -1,   232,
     132,   495,    -1,    -1,    -1,   368,   579,   495,   583,   585,
     580,   586,   495,    -1,    -1,   218,   582,   495,   583,   585,
     495,    -1,   369,    -1,   276,    -1,   369,    -1,   276,    -1,
      -1,    27,   276,    -1,    -1,  1442,   587,  1459,    -1,    79,
      -1,   588,    -1,   589,    -1,   188,    -1,   589,    79,    -1,
      79,   589,    -1,   500,    -1,   384,    -1,   591,   592,   655,
      -1,    -1,   169,   132,   495,    -1,   593,   594,   616,   617,
     610,    -1,    -1,    92,   427,   495,    -1,    -1,   595,    -1,
     599,    -1,   595,   599,    -1,   599,   595,    -1,    -1,   451,
     495,   596,   597,    -1,    -1,   609,   598,   495,    -1,    -1,
    1478,   115,   294,    -1,    -1,   329,   495,   600,   601,    -1,
      -1,   609,   495,    -1,   609,   602,   495,    -1,   602,   495,
      -1,   603,    -1,   602,   603,    -1,   604,    -1,   605,    -1,
     606,    -1,   607,    -1,   289,   446,  1442,  1382,  1488,    -1,
    1494,  1442,  1340,    -1,   430,  1442,  1382,    -1,  1428,    65,
    1442,   608,    -1,  1340,    -1,   277,    -1,   540,    -1,   474,
      -1,   551,    -1,   609,   551,    -1,    -1,    -1,   399,   495,
     611,   612,    -1,    -1,   613,   495,    -1,   613,     1,   495,
      -1,   614,    -1,   613,   614,    -1,   217,     9,   248,    -1,
     217,   551,   585,    -1,   217,   615,   248,    -1,   367,   551,
     585,    -1,   219,    -1,   615,   219,    -1,    -1,   453,   495,
      -1,    -1,   618,    -1,   619,   495,    -1,   618,   619,   495,
      -1,   620,    -1,   619,   620,    -1,   621,    -1,   627,    -1,
     636,    -1,   646,    -1,   643,    -1,   647,    -1,   649,    -1,
     650,    -1,   651,    -1,   652,    -1,   653,    -1,   654,    -1,
      -1,   551,   622,   623,    -1,  1442,   104,    -1,  1382,  1442,
    1344,    -1,  1442,  1344,   624,    -1,   625,    -1,    -1,   625,
      -1,   626,    -1,  1168,  1456,  1344,    -1,   626,  1168,  1456,
    1344,    -1,    -1,    11,  1344,   628,  1442,   629,    -1,   302,
      -1,   455,    -1,   456,    -1,   136,    -1,    29,    -1,   630,
      -1,   631,    -1,   630,   631,    -1,   634,    -1,   634,   486,
     634,    -1,    -1,   634,    17,   632,   633,    -1,   634,    -1,
     633,    17,   634,    -1,   276,    -1,   452,    -1,   557,    -1,
     375,    -1,   231,    -1,   286,    -1,   452,    -1,   557,    -1,
     638,   637,    -1,    -1,   237,   551,    -1,   472,  1429,   639,
      -1,   640,    -1,   639,   640,    -1,   641,  1443,   642,    -1,
    1345,    -1,   641,  1345,    -1,  1383,    -1,   642,  1383,    -1,
      64,  1344,  1442,   644,    -1,   645,    -1,   644,   645,    -1,
    1385,    -1,  1385,   486,  1385,    -1,   277,  1344,  1442,   276,
      -1,   106,  1463,  1442,   276,   648,    -1,    -1,  1478,   353,
     276,    -1,   116,  1442,    75,    -1,   326,   440,  1442,   512,
     434,    -1,   108,  1442,  1339,    -1,   104,   459,  1442,  1339,
      -1,   422,  1442,  1339,    -1,   181,  1442,  1339,    -1,   656,
     657,   659,   658,   695,    -1,    -1,   245,   427,   495,    -1,
      -1,   191,   495,    -1,    -1,   253,   495,    -1,    -1,   659,
     660,    -1,    -1,   431,  1410,  1344,   661,   662,   495,    -1,
      -1,   662,   663,    -1,   664,    -1,   672,    -1,   674,    -1,
     676,    -1,   678,    -1,   680,    -1,   684,    -1,   686,    -1,
     687,    -1,   688,    -1,   690,    -1,   691,    -1,   693,    -1,
      30,  1475,   669,   668,   670,    -1,    30,  1475,   669,   666,
     671,    -1,    30,  1475,   669,   667,   671,    -1,    30,  1475,
     669,   129,   671,    -1,    30,  1475,   669,   257,   671,    -1,
      30,  1475,   669,   665,   671,    -1,   361,    -1,   362,    -1,
     360,    -1,   127,    -1,   128,    -1,   479,    -1,   376,    -1,
      55,    -1,    56,    -1,    57,    -1,   244,    -1,   245,    -1,
     288,    -1,   342,    -1,    -1,   272,     7,  1433,    -1,    -1,
     188,    -1,   135,    -1,   276,    -1,  1379,    -1,    -1,   276,
      -1,  1379,    -1,     4,  1451,  1442,   673,    -1,   436,    -1,
     135,    -1,   376,    -1,    19,  1460,  1444,  1442,   689,  1404,
     675,    -1,    -1,   471,   548,     9,  1388,    -1,   471,   548,
     635,    -1,  1479,  1442,   677,    -1,   551,    -1,   679,   459,
    1442,  1339,    -1,    -1,   498,    -1,   448,    -1,    -1,   681,
     282,  1451,  1442,   682,    -1,   287,   683,    -1,    34,   683,
      -1,   184,    -1,    -1,   550,   282,   334,  1487,    -1,   550,
     282,   334,   296,  1487,    -1,   550,   416,    -1,   340,  1442,
     685,    -1,   685,    -1,   239,    -1,  1460,  1426,   436,    -1,
     389,    -1,   272,   436,    -1,   345,  1428,  1442,  1343,    -1,
     381,   121,  1442,   455,    -1,   381,  1444,  1442,   689,    -1,
    1339,    -1,  1339,   496,  1338,    -1,  1339,   450,  1442,  1338,
      -1,   389,  1444,  1442,  1339,    -1,   401,   692,  1422,    -1,
     309,    -1,  1382,    -1,   439,  1478,   694,    -1,     9,  1457,
      -1,   309,  1457,    -1,   378,   335,    -1,    -1,   696,   495,
      -1,   696,     1,   495,    -1,   697,    -1,   696,   697,    -1,
     698,    -1,   700,    -1,   420,   699,  1421,  1435,  1328,    -1,
      -1,   381,    -1,   448,    -1,   449,    -1,    -1,   296,   701,
    1433,  1470,  1430,   702,    -1,   703,    -1,   702,   703,    -1,
    1329,   704,    -1,    -1,   356,  1382,    -1,    -1,   707,   708,
     709,   706,   760,   741,   822,   824,   826,   871,    -1,    -1,
     110,   132,   495,    -1,    -1,   498,   427,   495,    -1,    -1,
     709,   710,    -1,   711,   762,    -1,    -1,   713,  1329,   712,
     714,   495,    -1,   713,     1,   495,    -1,   190,    -1,   424,
      -1,    -1,   714,   715,    -1,  1442,   188,    -1,  1442,   222,
      -1,   716,    -1,   718,    -1,   722,    -1,   723,    -1,   726,
      -1,   727,    -1,   733,    -1,   736,    -1,   738,    -1,    48,
    1430,  1382,   721,   717,    -1,    -1,   383,    -1,    63,    -1,
     381,  1430,  1382,  1429,    -1,   381,  1430,  1382,   490,  1382,
    1429,    -1,   381,  1442,   546,  1437,  1466,   720,   721,  1429,
     719,    -1,    -1,   122,  1455,  1339,    -1,    -1,  1436,  1382,
      -1,    -1,   490,  1382,    -1,   258,  1489,  1485,    -1,   544,
     331,   724,  1442,   725,    -1,   544,   331,   192,  1442,   725,
      -1,   551,    -1,   232,    -1,   276,    -1,  1379,    -1,   110,
    1489,  1341,    -1,   270,  1442,  1343,  1449,   728,    -1,    -1,
     728,   729,    -1,   730,    -1,   731,    -1,   732,    -1,  1478,
     206,  1424,  1343,    -1,   509,  1343,    -1,    49,  1343,    -1,
     382,  1451,  1442,   734,    -1,   189,    -1,   543,    -1,   196,
      -1,   545,    -1,   735,    -1,   517,    -1,   419,    -1,    69,
    1442,   677,   737,    -1,    -1,   207,  1338,    -1,   739,   740,
      -1,   396,  1442,    -1,   398,  1420,    -1,  1344,    -1,   740,
    1344,    -1,    -1,    -1,    80,   427,   495,   742,   743,    -1,
      -1,   743,   744,    -1,   745,   762,    -1,    -1,    58,  1344,
     746,   747,   495,    -1,    -1,   747,   748,    -1,  1435,  1440,
     244,   749,    -1,  1435,   342,   753,    -1,  1435,  1440,   252,
     756,    -1,    -1,   750,    -1,   752,    -1,   751,    -1,   750,
     751,    -1,  1469,   374,  1442,  1375,    -1,  1469,   464,  1442,
    1375,    -1,  1469,   465,  1442,  1375,    -1,  1469,   466,  1442,
    1375,    -1,   291,   111,  1442,  1375,    -1,   291,   487,  1442,
    1375,    -1,  1469,   450,  1442,  1375,    -1,   482,   263,  1442,
    1375,    -1,   144,   256,  1442,  1375,    -1,   459,   256,  1442,
    1375,    -1,  1450,   103,  1442,  1375,    -1,  1375,  1375,  1375,
    1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,    -1,    -1,
     754,    -1,   755,    -1,   754,   755,    -1,   124,   103,  1442,
    1375,    -1,   482,   263,  1442,  1375,    -1,   459,   256,  1442,
    1375,    -1,   124,   477,   330,  1382,  1473,   809,    -1,   177,
     256,  1442,  1375,    -1,   124,  1442,  1375,    -1,   472,   124,
    1442,  1375,    -1,    -1,   757,    -1,   759,    -1,   758,    -1,
     757,   758,    -1,   291,   111,  1442,  1375,    -1,   291,   487,
    1442,  1375,    -1,  1469,   480,  1442,  1375,    -1,   482,   263,
    1442,  1375,    -1,   144,   256,  1442,  1375,    -1,   459,   256,
    1442,  1375,    -1,  1375,  1375,  1375,  1375,  1375,  1375,    -1,
      -1,    -1,   553,   427,   495,   761,   762,    -1,    -1,    -1,
     763,   764,    -1,   765,   495,    -1,   764,   765,   495,    -1,
     781,    -1,   777,    -1,   779,    -1,    -1,   767,   768,   766,
     784,    -1,   767,     1,   495,    -1,  1402,   551,    -1,    -1,
     193,    -1,   769,    -1,   551,    -1,    -1,  1442,   222,    -1,
    1386,    -1,   265,   772,    -1,   263,   772,    -1,    51,  1454,
     772,    -1,  1376,    -1,    42,    -1,    45,    -1,    44,    -1,
      43,    -1,    41,    -1,   776,    -1,   794,    -1,   795,    -1,
     773,    -1,   774,    -1,   775,    -1,     1,   495,    -1,   198,
      -1,   202,    -1,   199,    -1,   200,    -1,   197,    -1,   201,
      -1,   203,    -1,   355,    -1,   370,    -1,   445,   769,   393,
    1379,   778,    -1,    -1,   486,  1379,    -1,    -1,   140,   769,
     780,   816,    -1,   767,   769,    93,   770,   783,    -1,    -1,
     438,   769,   782,   789,   816,    -1,  1423,   771,    -1,   214,
     551,    -1,    -1,   784,   785,    -1,   786,    -1,   787,    -1,
     790,    -1,   791,    -1,   792,    -1,   796,    -1,   799,    -1,
     812,    -1,   813,    -1,   814,    -1,   815,    -1,   816,    -1,
     821,    -1,   385,  1376,    -1,  1442,   188,   788,    -1,    -1,
      27,   276,    -1,    -1,   790,    -1,  1442,   222,    -1,   352,
      -1,   793,    -1,   537,  1442,   793,    -1,    40,    -1,    81,
      -1,   794,    -1,   795,    -1,    85,    -1,    86,    -1,    87,
      -1,    88,    -1,    89,    -1,   129,    -1,   238,    -1,   344,
      -1,   355,    -1,   370,    -1,   444,    -1,   442,    -1,   443,
      -1,   525,    -1,   523,    -1,   524,    -1,    42,  1464,    -1,
      42,   522,    -1,    45,  1464,    -1,    45,   522,    -1,    44,
    1464,    -1,    44,   522,    -1,    43,  1464,    -1,    43,   522,
      -1,    41,  1464,    -1,    41,   522,    -1,   198,    -1,   199,
      -1,   197,    -1,   200,    -1,   201,    -1,   299,    -1,    83,
      -1,   205,    -1,    84,    -1,   204,    -1,  1465,   260,  1414,
      -1,  1465,   512,  1414,    -1,   330,  1382,   800,  1473,   803,
     798,    -1,    -1,   461,  1382,    -1,   330,  1382,   800,  1473,
     803,   806,   809,    -1,   330,   802,   518,  1473,   122,  1455,
    1339,   806,   809,    -1,   330,   135,   804,   801,   800,   805,
     806,   809,    -1,    -1,   490,  1382,    -1,    -1,   214,  1382,
      -1,    -1,  1382,   490,    -1,    -1,   122,  1455,  1339,    -1,
      -1,    54,  1437,   551,    -1,    -1,   242,    -1,   807,    -1,
      -1,   807,   808,  1444,  1442,  1338,    -1,    28,    -1,   123,
      -1,    -1,   239,  1427,   810,    -1,   811,    -1,   810,   811,
      -1,   551,    -1,   254,  1462,    -1,   473,  1445,    -1,    46,
    1476,   557,    -1,    37,    -1,    -1,   544,  1443,   818,   817,
     820,    -1,   819,    -1,   818,   819,    -1,   771,    -1,   771,
     486,   771,    -1,    -1,  1477,   497,  1442,   771,    -1,    21,
     263,    -1,    21,   326,    -1,    -1,    -1,   281,   427,   495,
     823,   762,    -1,    -1,    -1,   275,   427,   495,   825,   762,
      -1,    -1,    -1,   396,   427,   495,   827,   828,    -1,    -1,
     828,   829,    -1,    -1,   377,  1331,   830,   831,   495,   845,
      -1,    -1,   831,   832,    -1,     1,   495,    -1,  1442,   222,
      -1,    68,  1442,  1362,    -1,   833,    -1,   836,    -1,  1502,
     834,    -1,  1434,   835,    -1,  1375,    -1,   835,  1375,    -1,
     346,  1448,   837,   838,    -1,  1384,    -1,  1384,  1486,  1384,
    1481,    -1,  1384,  1486,    -1,    -1,   838,   839,    -1,   840,
      -1,   841,    -1,   842,    -1,   843,    -1,   844,    -1,   229,
    1442,  1384,    -1,   195,  1495,  1442,  1384,    -1,   259,  1496,
    1442,  1384,    -1,   259,  1495,  1442,  1384,    -1,   206,  1442,
    1384,    -1,    -1,   845,   846,    -1,    -1,   767,   768,   847,
     848,   495,    -1,    -1,   848,   849,    -1,   850,    -1,   854,
      -1,   860,    -1,   791,    -1,   870,    -1,   796,    -1,   812,
      -1,   862,    -1,   814,    -1,   868,    -1,   855,    -1,   816,
      -1,   858,    -1,   869,    -1,   797,    -1,   859,    -1,   516,
    1442,   851,    -1,  1500,    -1,  1498,    -1,  1496,   852,    -1,
    1495,    -1,  1497,   852,    -1,  1499,    -1,  1501,    -1,    -1,
    1375,   853,    -1,   194,   853,    -1,    -1,   338,   346,    -1,
     307,   228,  1442,   865,    -1,   470,  1454,  1353,   856,    -1,
      -1,   402,  1455,   857,    -1,  1375,    -1,   194,    -1,   358,
     548,  1304,    -1,   546,  1375,   214,  1355,    50,  1355,    -1,
     861,   864,    -1,   272,  1453,  1443,    -1,   274,  1420,    -1,
     863,   866,    -1,  1480,  1453,  1443,    -1,  1481,  1420,    -1,
     865,    -1,   864,   865,    -1,   354,  1382,    -1,  1384,    -1,
     308,    -1,   867,    -1,   866,   867,    -1,   354,  1382,    -1,
    1384,    -1,   450,  1442,  1355,  1411,    -1,   228,  1439,    -1,
     537,  1442,   129,    -1,    -1,    -1,   421,   427,   495,   872,
     873,    -1,    -1,   874,    -1,   875,   495,    -1,   874,   875,
     495,    -1,   781,    -1,    -1,   767,   768,   876,   877,    -1,
     767,     1,   495,    -1,    -1,   877,   878,    -1,    46,   272,
      -1,    46,   421,    -1,    39,    -1,    47,    -1,   176,   879,
      -1,   176,   880,    -1,   230,    -1,   285,    -1,   931,    -1,
     519,    -1,   343,    -1,   227,    -1,   262,    -1,    33,    -1,
     476,    -1,   428,    -1,   930,    -1,   400,    -1,   216,    -1,
     372,    62,  1442,  1362,    -1,   372,    -1,   500,    -1,   272,
    1452,  1442,   883,  1365,    -1,  1480,  1452,  1442,   884,  1365,
      -1,   208,  1442,  1365,    -1,    36,  1442,  1365,    -1,   792,
      -1,   814,    -1,   886,    -1,   812,    -1,   796,    -1,   816,
      -1,   791,    -1,   885,    -1,   542,  1375,    -1,   214,  1368,
      -1,   490,  1375,    -1,   172,    -1,  1432,   272,    -1,   174,
      -1,  1432,   421,    -1,   354,    -1,   507,    -1,   292,    -1,
     502,    -1,    -1,   881,    -1,   882,    -1,    -1,   881,    -1,
     882,    -1,   330,  1382,  1473,    -1,  1442,   222,    -1,    -1,
      -1,    -1,   364,   132,   891,   899,   495,   888,   900,   889,
     902,    -1,    -1,   890,   913,   495,   902,    -1,    -1,    -1,
     542,   892,   894,    -1,    -1,    61,   893,   894,    -1,   895,
      -1,   894,   895,    -1,   896,   897,   898,   551,    -1,    -1,
    1427,   387,    -1,  1427,   544,    -1,    -1,   446,  1442,    33,
      -1,   446,  1442,   118,    -1,   522,   446,  1442,    33,    -1,
     522,   446,  1442,  1382,    -1,   446,  1442,  1382,    -1,    -1,
     337,    -1,    -1,   406,   333,    -1,   406,   551,    -1,    -1,
      -1,   117,   495,   901,   902,   144,   117,   495,    -1,    -1,
     902,   903,    -1,   904,    -1,   907,    -1,   913,   495,    -1,
     908,    -1,   495,    -1,    -1,   551,   427,   909,   495,   905,
     906,    -1,    -1,  1241,   495,    -1,   551,   495,    -1,   551,
      -1,    -1,  1382,    -1,    -1,    -1,   911,   912,   913,    -1,
      -1,   914,   915,    -1,   913,   915,    -1,   916,    -1,   934,
      -1,   939,    -1,   943,    -1,   948,    -1,   968,    -1,   972,
      -1,   980,    -1,   976,    -1,   981,    -1,   982,    -1,   987,
      -1,   992,    -1,  1006,    -1,  1010,    -1,  1012,    -1,  1015,
      -1,  1029,    -1,  1033,    -1,  1036,    -1,  1039,    -1,  1043,
      -1,  1044,    -1,  1048,    -1,  1058,    -1,  1061,    -1,  1079,
      -1,  1081,    -1,  1084,    -1,  1088,    -1,  1095,    -1,  1107,
      -1,  1109,    -1,  1124,    -1,  1125,    -1,  1135,    -1,  1138,
      -1,  1139,    -1,  1143,    -1,  1149,    -1,  1150,    -1,  1158,
      -1,  1165,    -1,  1181,    -1,  1191,    -1,  1200,    -1,  1205,
      -1,  1214,    -1,  1218,    -1,  1220,    -1,  1223,    -1,  1226,
      -1,  1229,    -1,  1256,    -1,   307,   433,    -1,     1,  1415,
      -1,    -1,     3,   917,   918,   933,    -1,    -1,   920,   919,
     921,  1264,    -1,  1375,   214,   924,    -1,  1375,   214,  1481,
      -1,  1375,   214,   111,   556,    -1,  1375,   214,   111,    -1,
    1375,   214,   112,   555,    -1,  1375,   214,   112,    -1,  1375,
     214,   113,    -1,  1375,   214,   178,   256,    -1,  1375,   214,
     182,   459,    -1,  1375,   214,   487,    -1,  1375,   214,   539,
     298,    -1,  1375,   214,    76,    -1,  1375,   214,   171,  1264,
      -1,  1375,   214,   169,  1358,  1264,    -1,  1375,   214,    25,
      -1,  1375,   214,    26,  1264,    -1,  1375,   214,  1333,    -1,
    1375,   214,   551,    -1,  1330,  1450,   103,    -1,  1375,    -1,
     333,    -1,    -1,   922,    -1,   923,    -1,   922,   923,    -1,
     925,    -1,   215,    -1,   928,    -1,  1478,   929,    -1,  1425,
     487,  1366,    -1,   274,    -1,   272,   324,    -1,  1424,   926,
      -1,  1424,   927,    -1,    31,  1365,    -1,   272,  1452,  1365,
      -1,  1480,  1452,  1365,    -1,   356,  1452,  1365,    -1,   294,
    1442,    48,    -1,    33,    -1,   476,    -1,    39,    -1,    47,
      -1,    99,    -1,   216,    -1,   230,    -1,   262,    -1,   283,
      -1,   285,    -1,   930,    -1,   343,    -1,   372,    62,  1442,
    1362,    -1,   372,    -1,   400,    -1,   931,    -1,   428,    -1,
     373,   446,  1442,  1365,    -1,   446,  1442,  1365,    -1,   519,
      -1,   309,   932,    -1,   932,    -1,   535,    -1,   208,  1442,
    1365,    -1,    36,  1442,  1365,    -1,   423,   528,  1303,    -1,
     423,   133,  1303,    -1,   488,  1419,  1366,    -1,   309,   138,
      -1,   311,    -1,   332,    -1,   409,    -1,   410,    -1,   407,
      -1,   529,    -1,   118,    -1,    -1,   145,    -1,    -1,     5,
     935,   936,   938,    -1,  1349,   490,  1323,  1276,    -1,  1349,
     937,   221,  1323,  1276,    -1,   102,  1375,   490,  1375,  1411,
    1276,    -1,    -1,   490,  1350,    -1,    -1,   146,    -1,    -1,
      10,   940,   941,    -1,  1375,  1405,   942,    -1,  1317,    63,
    1406,   942,    -1,    -1,   406,  1347,    -1,    -1,    18,   944,
     945,    -1,   946,    -1,   945,   946,    -1,  1335,   490,   947,
    1335,    -1,    -1,   366,   490,    -1,    -1,    52,   949,   950,
     967,    -1,    -1,   952,   953,   951,   954,   959,   962,    -1,
      -1,   458,    -1,   460,    -1,   293,    -1,  1363,    -1,   369,
      -1,    -1,    -1,   542,   955,   956,    -1,   957,    -1,   956,
     957,    -1,   958,   333,    -1,   958,   897,  1351,    -1,    -1,
    1427,   387,    -1,  1427,    95,    -1,  1427,   544,    -1,    -1,
     960,  1441,  1375,    -1,   960,   961,    -1,   960,   314,    -1,
     960,     6,  1454,  1375,    -1,   406,    -1,   221,    -1,   504,
      -1,   333,    -1,    -1,   964,   965,    -1,   966,   963,    -1,
      -1,   964,    -1,   182,   910,    -1,   505,   910,    -1,    -1,
     966,    -1,   319,   910,    -1,    -1,   147,    -1,    -1,    53,
     969,   970,    -1,   971,    -1,   970,   971,    -1,  1362,    -1,
     369,    -1,    -1,    67,   973,   974,    -1,  1329,   975,    -1,
     974,  1329,   975,    -1,    -1,  1490,    -1,  1490,  1435,   392,
      -1,  1478,   309,   411,    -1,  1478,   282,    -1,    -1,    82,
     977,   978,   979,    -1,  1323,  1482,  1317,  1276,    -1,    -1,
     148,    -1,    78,    -1,    96,    -1,    -1,   119,   983,   984,
     986,    -1,  1329,  1460,  1116,  1297,    -1,   498,   985,    -1,
    1329,    -1,   985,  1329,    -1,    -1,   149,    -1,    -1,   126,
     988,   989,    -1,   991,  1330,   990,    -1,    -1,  1478,   256,
    1362,    -1,    -1,   244,  1471,    -1,   342,    -1,   252,   480,
      -1,   480,    -1,    -1,   129,   993,   994,  1005,    -1,  1362,
     533,  1271,    -1,  1362,   534,  1271,    -1,  1362,   531,  1271,
      -1,  1362,   532,  1271,    -1,   995,  1271,    -1,   996,  1348,
      -1,  1349,    -1,   997,    -1,   996,   997,    -1,    -1,   999,
     998,  1000,    -1,  1349,    -1,   333,    -1,  1001,    -1,  1000,
    1001,    -1,  1002,    -1,  1478,   323,    -1,   928,    -1,   925,
      -1,  1478,  1004,    -1,   530,  1333,    -1,   530,   551,    -1,
     530,   361,    -1,   530,  1003,    -1,   104,    -1,   105,    -1,
      39,    -1,    46,   272,    -1,    46,   421,    -1,    47,    -1,
      99,    -1,   176,   879,    -1,   176,   880,    -1,   230,    -1,
     285,    -1,   343,    -1,   931,    -1,   446,  1442,  1365,    -1,
     519,    -1,   208,  1442,  1365,    -1,    36,  1442,  1365,    -1,
     423,   528,  1303,    -1,   423,   133,  1303,    -1,    -1,   150,
      -1,    -1,   131,  1007,  1008,  1009,    -1,  1350,   247,  1323,
    1276,    -1,  1350,   247,  1350,   221,  1323,  1276,    -1,  1350,
      50,  1350,   221,  1323,  1276,    -1,  1350,   247,  1350,   221,
    1324,   391,  1324,  1276,    -1,  1350,    50,  1350,   221,  1324,
     391,  1324,  1276,    -1,    -1,   151,    -1,    -1,   141,  1011,
     989,    -1,    -1,   168,  1013,  1014,    -1,   276,   954,    -1,
      -1,   180,  1016,  1017,  1028,    -1,  1018,  1020,    -1,  1019,
      -1,  1018,    17,  1019,    -1,  1305,    -1,   508,    -1,   497,
      -1,  1021,  1023,    -1,  1021,    -1,  1022,    -1,  1021,  1022,
      -1,  1024,   910,    -1,   548,   341,   910,    -1,   548,  1025,
      -1,  1024,   548,  1025,    -1,  1026,    -1,  1025,    17,  1026,
      -1,  1306,  1027,    -1,    21,    -1,   508,    -1,   497,    -1,
      -1,   486,  1305,    -1,    -1,   152,    -1,    -1,   185,  1030,
    1031,    -1,    -1,   367,  1032,    -1,   217,    -1,   349,   109,
      -1,   349,    -1,   427,    -1,   348,    -1,    -1,   960,  1350,
      -1,    -1,   213,  1034,  1035,    -1,  1346,    -1,    -1,   220,
    1037,  1038,    -1,  1379,    -1,    -1,   223,  1040,  1041,    -1,
    1474,  1334,  1042,    -1,    -1,   122,  1455,  1375,    -1,   224,
    1032,    -1,    -1,   234,  1045,  1304,  1472,  1046,  1047,    -1,
     910,   142,   910,    -1,   142,   910,    -1,   910,    -1,    -1,
     154,    -1,    -1,   241,  1049,  1050,    -1,  1346,  1051,  1052,
    1053,  1057,    -1,    -1,  1478,   193,    -1,    -1,     9,  1474,
     544,    -1,  1056,  1474,   544,    -1,    -1,   395,  1054,    -1,
    1055,    -1,  1054,  1055,    -1,  1056,  1431,    50,  1350,    -1,
      12,    -1,    15,    -1,   326,    -1,    16,    -1,   327,    -1,
     299,    -1,   300,    -1,    -1,  1472,  1474,   118,    -1,    -1,
     243,  1059,  1060,    -1,  1331,    -1,  1060,  1331,    -1,    -1,
     246,  1062,  1063,    -1,  1064,  1065,    -1,  1375,    -1,  1386,
      -1,  1389,    -1,  1066,  1068,    -1,  1066,    -1,  1068,    -1,
    1069,    -1,    -1,   478,  1067,  1070,    -1,   395,  1072,    -1,
     100,  1358,   490,  1359,  1076,    -1,  1071,    -1,  1070,  1071,
      -1,  1373,   207,    -1,    63,  1076,    -1,     9,    -1,   260,
      -1,   512,    -1,  1358,  1076,    -1,  1073,    -1,  1072,  1073,
      -1,    63,    50,  1358,  1076,    -1,  1074,  1075,    -1,    -1,
       9,    -1,   260,    -1,   195,    -1,   512,    -1,  1358,    50,
    1359,  1076,    -1,    -1,  1077,    -1,  1078,    -1,  1077,  1078,
      -1,  1078,  1077,    -1,    38,  1440,  1350,    -1,     8,  1440,
    1350,    -1,    -1,   290,  1080,  1183,    -1,    -1,   295,  1082,
    1083,    -1,  1350,   490,  1346,    -1,   102,  1350,   490,  1346,
      -1,    -1,   297,  1085,  1086,  1087,    -1,  1350,    50,  1323,
    1276,    -1,  1350,    50,  1350,   221,  1323,  1276,    -1,    -1,
     155,    -1,    -1,   336,  1089,  1090,    -1,  1091,    -1,  1090,
    1091,    -1,  1092,  1093,  1116,  1328,  1094,    -1,   244,    -1,
     342,    -1,   252,    -1,   187,    -1,    -1,   439,  1478,   694,
      -1,    -1,  1478,   309,   411,    -1,  1478,   282,    -1,   410,
      -1,    -1,   349,  1096,  1097,    -1,  1101,  1102,    -1,    -1,
    1102,  1098,   910,  1099,    -1,  1102,  1100,    -1,    -1,   156,
      -1,   156,    -1,   495,    -1,  1335,    -1,  1335,   486,  1335,
      -1,    -1,  1364,   489,    -1,   209,    -1,  1103,   527,  1104,
      -1,  1103,   546,  1105,    -1,    -1,  1478,   483,  1261,    -1,
     185,    -1,  1304,    -1,  1106,    -1,  1105,     8,  1106,    -1,
    1375,   214,  1350,    50,  1350,   527,  1304,    -1,    -1,   559,
    1108,  1330,    -1,    -1,   378,  1110,  1111,  1123,    -1,  1329,
    1408,  1460,  1112,  1113,  1121,  1122,    -1,    -1,   247,  1375,
      -1,    -1,  1114,    -1,  1115,  1119,    -1,  1120,    -1,   236,
     282,    -1,  1478,   235,   282,    -1,     7,  1455,   282,    -1,
    1117,    -1,    -1,  1117,    -1,  1118,    -1,   404,  1435,  1317,
     489,    -1,   404,  1435,  1317,   426,    -1,   404,   209,    -1,
      -1,  1120,    -1,  1147,    -1,  1478,   255,   282,    -1,  1478,
     547,    -1,    -1,   256,  1442,  1375,    -1,  1297,    -1,  1287,
      -1,    -1,   158,    -1,   379,    -1,    -1,   380,  1126,  1127,
    1134,    -1,  1330,  1128,   247,  1375,  1129,    -1,   291,    -1,
     429,    -1,    -1,  1131,  1132,    -1,  1133,  1130,    -1,    -1,
    1131,    -1,   310,   910,    -1,    -1,  1133,    -1,   110,   910,
      -1,    -1,   159,    -1,    -1,   390,  1136,  1137,    -1,  1325,
    1259,    -1,   403,    -1,    -1,   405,  1140,  1141,  1142,    -1,
    1329,  1460,  1112,  1286,    -1,    -1,   160,    -1,    -1,   412,
    1144,  1145,  1148,    -1,  1326,  1259,  1116,  1146,  1297,    -1,
      -1,  1147,    -1,  1478,   282,    -1,  1478,   309,   282,    -1,
      -1,   161,    -1,   416,    -1,    -1,   425,  1151,  1152,  1157,
      -1,  1327,  1153,  1154,  1155,    -1,     9,  1327,  1154,   548,
    1305,   910,    -1,    -1,   546,  1375,    -1,    -1,   144,   910,
      -1,  1156,    -1,  1156,  1155,    -1,   548,  1304,   910,    -1,
      -1,   162,    -1,    -1,   560,  1159,  1160,    -1,  1330,  1162,
      -1,  1330,  1161,  1163,  1260,  1164,    -1,    -1,  1162,    -1,
     214,  1375,    -1,  1478,  1375,    -1,  1478,   179,    -1,  1478,
     143,    -1,  1478,   139,    -1,    -1,   395,  1446,    -1,    -1,
     437,  1166,  1167,    -1,  1170,    -1,  1171,    -1,  1174,    -1,
    1175,    -1,  1176,    -1,  1178,    -1,  1180,    -1,   334,    -1,
     332,    -1,   528,    -1,   133,    -1,   169,  1358,   490,  1358,
      -1,  1369,    32,  1172,    -1,  1173,    -1,  1172,  1173,    -1,
      39,  1168,    -1,    47,  1168,    -1,   230,  1168,    -1,   285,
    1168,    -1,   409,  1168,    -1,   519,  1168,    -1,   262,  1168,
      -1,   343,  1168,    -1,  1346,   490,   168,  1357,    -1,  1346,
     490,  1350,    -1,  1346,  1169,    50,  1350,    -1,  1177,    -1,
    1176,  1177,    -1,  1332,   490,  1168,    -1,  1179,    -1,  1178,
    1179,    -1,  1346,   490,   508,    -1,  1346,   490,   497,    -1,
     259,   182,   490,   332,    -1,    -1,   448,  1182,  1183,    -1,
      -1,  1370,  1185,  1187,  1188,  1184,  1189,  1190,    -1,    -1,
    1185,  1455,   808,  1444,  1186,    -1,    -1,  1186,  1379,    -1,
      -1,  1493,  1438,    -1,    -1,  1479,  1442,  1339,    -1,    -1,
     542,  1328,    -1,   244,   364,  1442,  1101,    -1,    -1,   221,
    1328,    -1,   342,   364,  1442,  1101,    -1,    -1,   457,  1192,
    1193,  1199,    -1,  1329,  1195,  1194,  1297,    -1,    -1,  1478,
    1492,  1317,    -1,    -1,   256,  1442,  1196,  1375,    -1,   195,
      -1,   259,    -1,  1310,    -1,  1409,  1311,    -1,  1409,  1312,
      -1,  1409,  1313,    -1,  1409,  1314,    -1,  1197,    -1,  1198,
      -1,   313,  1310,    -1,   318,    -1,    -1,   163,    -1,    -1,
     462,   418,  1201,  1202,    -1,   462,  1204,    -1,    -1,   960,
    1350,    -1,  1350,    -1,  1478,   177,  1468,  1203,    -1,  1478,
     312,  1468,  1203,    -1,    -1,  1350,    -1,   276,    -1,   452,
      -1,   557,    -1,   375,    -1,    -1,   463,  1206,  1207,  1213,
      -1,  1208,   247,  1375,  1212,  1281,    -1,  1209,    -1,  1208,
    1209,    -1,  1350,  1210,    -1,    -1,   120,  1427,  1211,    -1,
     446,    -1,  1350,    -1,    -1,  1478,   355,  1442,  1375,    -1,
      -1,   164,    -1,    -1,   469,  1215,  1216,  1217,    -1,  1349,
     214,  1323,  1276,    -1,  1349,   214,  1350,   221,  1323,  1276,
      -1,   102,  1375,   214,  1375,  1411,  1276,    -1,    -1,   165,
      -1,   471,  1219,    -1,    -1,   363,    -1,    -1,   481,  1221,
    1222,    -1,  1331,    -1,  1222,  1331,    -1,    -1,   513,  1224,
    1225,    -1,  1372,   214,  1358,   490,  1359,    -1,    -1,   521,
    1227,  1228,    -1,  1329,  1461,    -1,    -1,   526,  1230,  1231,
    1240,    -1,  1375,  1232,  1235,  1212,  1239,  1281,    -1,    -1,
     120,  1427,  1233,    -1,  1234,    -1,  1233,   338,  1234,    -1,
    1403,  1358,    -1,   247,  1236,    -1,  1235,  1236,    -1,  1375,
    1237,  1238,    -1,    -1,   121,  1437,  1375,    -1,    -1,   103,
    1437,  1375,    -1,    -1,   478,  1437,  1375,    -1,    -1,   166,
      -1,    -1,   538,  1242,  1243,    -1,  1244,    -1,  1247,    -1,
    1251,    -1,  1253,    -1,  1254,    -1,  1245,  1419,  1467,  1483,
    1458,  1455,  1246,    -1,    -1,   222,    -1,  1328,    -1,   244,
      -1,   342,    -1,   252,    -1,   187,    -1,  1435,   115,  1455,
    1248,    -1,  1249,    -1,  1248,  1249,    -1,  1336,    -1,     9,
     365,    -1,     9,  1250,  1379,    -1,    -1,   388,    -1,   388,
     331,    -1,   331,    -1,  1424,   367,  1252,    -1,   457,    -1,
     144,    -1,  1245,    38,   397,  1375,    -1,  1255,    -1,   183,
      -1,   137,    -1,    -1,   554,  1257,  1258,  1263,    -1,  1326,
    1259,  1260,  1116,  1146,  1262,    -1,    -1,   214,  1368,    -1,
      -1,  1261,  1418,  1365,  1447,    -1,  1261,  1418,  1333,    -1,
    1261,  1418,   346,    -1,    38,    -1,     8,    -1,    -1,  1298,
      -1,  1292,    -1,    -1,   167,    -1,    -1,  1266,  1268,    -1,
    1269,  1265,    -1,    -1,  1266,    -1,  1267,   910,    -1,   178,
      -1,   182,    -1,    -1,  1269,    -1,  1270,   910,    -1,   317,
      -1,   319,    -1,    -1,  1273,  1274,    -1,  1275,  1272,    -1,
      -1,  1273,    -1,   182,   910,    -1,    -1,  1275,    -1,   319,
     910,    -1,    -1,  1278,  1279,    -1,  1280,  1277,    -1,    -1,
    1278,    -1,   447,   910,    -1,    -1,  1280,    -1,   322,   910,
      -1,    -1,  1283,  1284,    -1,  1285,  1282,    -1,    -1,  1283,
      -1,   505,   910,    -1,    -1,  1285,    -1,   321,   910,    -1,
    1289,  1290,    -1,  1291,  1289,    -1,  1289,  1290,    -1,  1291,
    1288,    -1,    -1,  1289,    -1,   144,   910,    -1,    -1,  1291,
      -1,   315,   910,    -1,  1294,  1295,    -1,  1296,  1293,    -1,
      -1,  1294,    -1,   173,   910,    -1,    -1,  1296,    -1,   316,
     910,    -1,    -1,  1298,    -1,  1300,  1301,    -1,  1302,  1299,
      -1,    -1,  1300,    -1,   250,   910,    -1,    -1,  1302,    -1,
     320,   910,    -1,    -1,  1367,  1491,    -1,  1305,    -1,  1306,
      -1,    -1,  1307,  1308,    -1,  1309,    -1,  1308,   251,    -1,
    1308,  1309,    -1,  1350,    -1,    66,    -1,   506,    -1,   492,
      -1,   507,    -1,   502,    -1,   503,    -1,   494,    -1,   186,
      -1,  1310,    -1,  1311,    -1,  1312,    -1,  1313,    -1,  1314,
      -1,   318,    -1,   313,    -1,    20,    -1,   338,    -1,   333,
      -1,   326,    -1,    12,    -1,    13,    -1,    14,    -1,   357,
      -1,   306,    -1,   496,    -1,   175,  1474,    -1,   499,    -1,
     225,    -1,   501,    -1,   266,    -1,   226,    -1,   267,    -1,
    1317,    -1,  1315,  1316,  1317,    -1,    -1,    77,    -1,   432,
      -1,  1317,   507,  1318,    -1,  1317,   502,  1318,    -1,  1318,
      -1,  1318,   503,  1319,    -1,  1318,   494,  1319,    -1,  1319,
      -1,  1320,   186,  1319,    -1,  1320,    -1,   507,  1321,    -1,
     502,  1321,    -1,  1321,    -1,   506,  1317,   492,    -1,  1355,
      -1,   271,    -1,   271,  1484,   551,    -1,   273,    -1,   273,
    1484,   551,    -1,   347,    -1,   347,  1484,   551,    -1,  1324,
      -1,  1323,  1324,    -1,  1347,  1411,    -1,  1379,    -1,  1325,
      -1,   498,   551,    -1,  1379,    -1,  1329,    -1,  1328,  1329,
      -1,   551,    -1,   551,    -1,   551,    -1,  1333,    -1,  1332,
    1333,    -1,   293,    -1,    -1,  1334,  1335,    -1,  1336,    -1,
    1379,    -1,  1337,    -1,  1337,  1484,  1337,    -1,   276,    -1,
    1339,    -1,  1338,  1339,    -1,  1379,    -1,   551,    -1,  1342,
      -1,  1341,  1342,    -1,   551,    -1,  1339,    -1,   276,    -1,
     551,    -1,     1,    -1,   551,    -1,  1347,    -1,  1346,  1347,
      -1,  1377,    -1,  1387,    -1,     6,  1454,  1376,    -1,    -1,
    1349,    -1,  1350,    -1,  1349,  1350,    -1,  1375,    -1,  1352,
      -1,  1374,    -1,  1352,    -1,  1386,    -1,  1389,    -1,  1322,
      -1,   265,  1376,    -1,   265,  1387,    -1,   265,  1389,    -1,
       6,  1454,  1356,  1357,    -1,     6,  1454,  1376,    -1,   293,
      -1,  1355,    -1,  1353,  1355,    -1,  1375,    -1,  1387,    -1,
    1389,    -1,  1375,    -1,  1387,    -1,  1389,    -1,  1322,    -1,
     265,  1376,    -1,   265,  1387,    -1,   265,  1389,    -1,   367,
      -1,   168,    -1,  1376,    -1,   276,    -1,  1360,    -1,  1361,
      -1,  1375,    -1,  1387,    -1,  1389,    -1,  1375,    -1,  1386,
      -1,  1375,    -1,   276,    -1,  1375,    -1,   276,    -1,  1389,
      -1,  1375,    -1,   771,    -1,  1389,    -1,  1369,    -1,  1382,
      -1,   557,    -1,  1369,    -1,  1384,    -1,  1369,    -1,  1382,
      -1,  1375,    -1,  1386,    -1,  1389,    -1,  1371,    -1,  1371,
      -1,  1379,    -1,  1379,  1380,    -1,  1375,    -1,  1375,    -1,
    1376,    -1,  1376,    -1,  1379,  1380,  1381,    -1,  1379,  1380,
      -1,  1379,  1381,    -1,  1379,    -1,  1378,    -1,  1379,  1380,
    1381,    -1,  1379,  1380,    -1,  1379,  1381,    -1,  1379,    -1,
     551,    -1,   551,  1484,  1379,    -1,   506,  1315,   492,    -1,
     506,  1317,   493,   492,    -1,   506,  1317,   493,  1317,   492,
      -1,   276,    -1,   276,    -1,   276,    -1,   276,    -1,   452,
      -1,   557,    -1,   375,    -1,   231,    -1,   286,    -1,   504,
      -1,  1387,    -1,     9,  1388,    -1,  1388,    -1,  1387,   491,
    1388,    -1,   276,    -1,   452,    -1,   557,    -1,   375,    -1,
     231,    -1,   286,    -1,   504,    -1,  1390,  1393,    -1,  1391,
     506,  1354,   492,  1393,    -1,  1392,   506,  1315,   492,  1393,
      -1,   514,   506,  1395,   492,  1393,    -1,   264,   506,  1396,
     492,    -1,   328,   506,  1398,   492,    -1,   278,   506,  1399,
     492,  1393,    -1,   279,   506,  1399,   492,  1393,    -1,   280,
     506,  1399,   492,  1393,    -1,   211,   506,  1400,   492,  1393,
      -1,   212,   506,  1401,   492,  1393,    -1,   219,  1394,    -1,
     541,  1394,    -1,   107,    -1,   549,    -1,   536,    -1,   284,
      -1,   408,    -1,    90,    -1,   210,    -1,   467,    -1,   468,
      -1,    -1,   506,  1317,   493,   492,    -1,   506,  1317,   493,
    1317,   492,    -1,    -1,   506,  1315,   492,    -1,   506,   492,
      -1,  1354,    -1,  1354,  1316,   260,    -1,  1354,  1316,   512,
      -1,    -1,  1397,  1354,    -1,  1354,    -1,  1354,  1316,  1354,
      -1,  1317,    -1,  1317,  1316,  1339,    -1,  1315,    -1,  1315,
    1316,   475,    -1,  1315,    -1,  1315,  1316,   475,    -1,    -1,
      -1,     9,    -1,    -1,  1493,    -1,    -1,   242,    -1,    -1,
     242,  1407,    -1,    -1,   490,  1361,    -1,    -1,   307,    -1,
     359,    -1,    -1,   313,    -1,    -1,   337,    -1,   313,   337,
      -1,    -1,   417,  1412,    -1,    -1,   294,  1442,  1413,    -1,
      35,    -1,   303,    -1,   304,    -1,   305,    -1,   371,    -1,
     510,    -1,   511,    -1,   515,    -1,    -1,   434,  1428,    -1,
     495,    -1,  1416,    -1,  1417,    -1,     3,    -1,     5,    -1,
      10,    -1,    18,    -1,    52,    -1,    53,    -1,    67,    -1,
      78,    -1,    82,    -1,    96,    -1,   119,    -1,   129,    -1,
     131,    -1,   142,    -1,   168,    -1,   180,    -1,   185,    -1,
     213,    -1,   220,    -1,   223,    -1,   224,    -1,   234,    -1,
     241,    -1,   243,    -1,   246,    -1,   290,    -1,   295,    -1,
     297,    -1,   307,    -1,   336,    -1,   349,    -1,   378,    -1,
     390,    -1,   405,    -1,   412,    -1,   416,    -1,   425,    -1,
     437,    -1,   448,    -1,   457,    -1,   462,    -1,   463,    -1,
     469,    -1,   471,    -1,   481,    -1,   513,    -1,   521,    -1,
     526,    -1,   554,    -1,   145,    -1,   146,    -1,   147,    -1,
     148,    -1,   149,    -1,   150,    -1,   151,    -1,   152,    -1,
     154,    -1,   155,    -1,   156,    -1,   158,    -1,   159,    -1,
     160,    -1,   161,    -1,   162,    -1,   163,    -1,   164,    -1,
     165,    -1,   166,    -1,   167,    -1,    -1,     7,    -1,    -1,
       8,    -1,    -1,    22,    -1,    -1,    23,    -1,    -1,    23,
      -1,    24,    -1,    -1,    27,    -1,    -1,    31,    -1,    -1,
      38,    -1,    -1,    40,    -1,    -1,    50,    -1,    -1,    62,
      -1,    -1,    63,    -1,    -1,    94,    -1,    -1,   110,    -1,
      -1,   144,  1454,    -1,    -1,   498,    -1,    -1,   194,    -1,
      -1,   207,    -1,    -1,   214,    -1,    -1,   237,    -1,    -1,
     339,    -1,   237,   339,    -1,    -1,   240,    -1,    -1,   500,
      -1,    -1,   247,    -1,    -1,   251,    -1,    -1,   251,    -1,
      22,    -1,    -1,   256,    -1,    -1,   261,    -1,   415,    -1,
      -1,   272,    -1,    -1,   272,    -1,   274,    -1,    -1,   268,
    1442,    -1,   269,  1420,    -1,    -1,   274,    -1,    -1,   291,
      -1,    -1,   294,    -1,    -1,   324,    -1,    -1,   324,    -1,
     325,    -1,    -1,   331,    -1,    -1,   334,    -1,    -1,   459,
     251,    -1,   459,    -1,   251,    -1,    -1,   341,    -1,    -1,
     364,    -1,    -1,   367,    -1,    -1,   381,    -1,    -1,   381,
      -1,   383,    -1,    -1,   415,    -1,    -1,   440,    -1,    -1,
     441,    -1,    -1,   440,    -1,   440,   251,    -1,    -1,   446,
      -1,    -1,   454,    -1,    -1,   459,    -1,    -1,   472,    -1,
      -1,   479,    -1,    -1,   480,    -1,    -1,   485,    -1,    -1,
     489,    -1,    -1,   490,    -1,    -1,   490,    -1,   542,    -1,
      -1,   548,    -1,    -1,   548,   437,   490,    -1,    -1,   550,
      -1,    70,   435,    -1,   435,    -1,    73,    -1,    71,    -1,
      74,    -1,    72,    -1,   496,    -1,   175,    -1,   182,    -1,
     177,    -1,   237,    -1,   331,    -1,   454,    -1,   333,    -1,
     272,    -1,   274,    -1,   381,    -1,   383,    -1,    63,    -1,
     552,    -1,   381,  1442,    -1,   383,  1420,    -1,   386,    -1,
     520,    -1,   272,    -1,   274,    -1,   446,    -1,   263,    -1,
     550,   134,    -1,   134,    -1,   367,    70,   435,    -1,    70,
     435,    -1,   435,    -1,   125,    -1,   114,    -1,    97,   229,
      -1,    60,    -1,    97,   206,    -1,    59,    -1,   346,   229,
      -1,   350,    -1,   346,   206,    -1,   351,    -1,   396,   229,
      -1,   414,    -1,   396,   206,    -1,   413,    -1,    97,  1442,
      -1,    98,  1420,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  2288,  2288,  2288,  2321,  2322,  2326,  2327,  2331,  2332,
    2336,  2336,  2359,  2370,  2376,  2377,  2381,  2382,  2386,  2394,
    2403,  2411,  2412,  2413,  2418,  2422,  2417,  2438,  2437,  2453,
    2464,  2471,  2472,  2479,  2480,  2483,  2484,  2488,  2497,  2506,
    2507,  2514,  2515,  2519,  2523,  2533,  2538,  2539,  2548,  2555,
    2556,  2566,  2567,  2568,  2569,  2570,  2583,  2582,  2592,  2593,
    2596,  2597,  2611,  2610,  2620,  2621,  2622,  2623,  2627,  2628,
    2632,  2633,  2634,  2635,  2639,  2647,  2654,  2661,  2672,  2676,
    2680,  2684,  2691,  2692,  2697,  2699,  2698,  2709,  2710,  2711,
    2718,  2719,  2723,  2727,  2733,  2734,  2744,  2749,  2759,  2760,
    2772,  2773,  2777,  2778,  2782,  2783,  2787,  2788,  2789,  2790,
    2791,  2792,  2793,  2794,  2795,  2796,  2797,  2798,  2806,  2805,
    2834,  2845,  2858,  2866,  2869,  2870,  2874,  2881,  2896,  2917,
    2916,  2940,  2946,  2952,  2958,  2964,  2970,  2980,  2984,  2991,
    2995,  3000,  2999,  3010,  3014,  3021,  3022,  3023,  3024,  3025,
    3026,  3030,  3031,  3038,  3053,  3056,  3063,  3071,  3075,  3086,
    3106,  3114,  3125,  3126,  3132,  3153,  3154,  3158,  3162,  3183,
    3206,  3281,  3284,  3293,  3312,  3328,  3346,  3364,  3381,  3398,
    3408,  3409,  3416,  3417,  3425,  3426,  3436,  3437,  3442,  3441,
    3471,  3472,  3476,  3477,  3478,  3479,  3480,  3481,  3482,  3483,
    3484,  3485,  3486,  3487,  3488,  3495,  3501,  3511,  3522,  3535,
    3548,  3581,  3582,  3583,  3588,  3589,  3590,  3591,  3595,  3596,
    3597,  3598,  3599,  3600,  3601,  3604,  3605,  3611,  3612,  3616,
    3620,  3621,  3626,  3629,  3630,  3637,  3645,  3646,  3647,  3654,
    3678,  3680,  3685,  3695,  3703,  3718,  3725,  3727,  3728,  3734,
    3734,  3741,  3746,  3751,  3758,  3759,  3760,  3764,  3775,  3776,
    3780,  3785,  3790,  3795,  3806,  3817,  3827,  3835,  3836,  3837,
    3843,  3854,  3861,  3862,  3868,  3876,  3877,  3878,  3884,  3885,
    3886,  3893,  3894,  3898,  3899,  3905,  3933,  3934,  3935,  3936,
    3943,  3942,  3958,  3959,  3963,  3966,  3967,  3977,  3974,  3991,
    3992,  4000,  4001,  4009,  4010,  4014,  4035,  4034,  4051,  4058,
    4062,  4068,  4069,  4073,  4083,  4098,  4099,  4100,  4101,  4102,
    4103,  4104,  4105,  4106,  4113,  4120,  4120,  4120,  4126,  4146,
    4180,  4211,  4212,  4219,  4220,  4224,  4225,  4232,  4243,  4248,
    4259,  4260,  4264,  4265,  4271,  4282,  4300,  4301,  4305,  4306,
    4307,  4311,  4318,  4325,  4334,  4343,  4344,  4345,  4346,  4347,
    4356,  4357,  4363,  4400,  4401,  4414,  4429,  4430,  4434,  4444,
    4457,  4459,  4458,  4473,  4474,  4478,  4495,  4494,  4515,  4516,
    4520,  4521,  4522,  4525,  4527,  4528,  4532,  4533,  4537,  4538,
    4539,  4540,  4541,  4542,  4543,  4544,  4545,  4546,  4547,  4551,
    4555,  4557,  4561,  4562,  4566,  4567,  4568,  4569,  4570,  4571,
    4572,  4575,  4577,  4578,  4582,  4583,  4587,  4588,  4589,  4590,
    4591,  4592,  4596,  4601,  4603,  4602,  4618,  4621,  4621,  4638,
    4639,  4643,  4644,  4645,  4647,  4646,  4661,  4674,  4682,  4687,
    4693,  4697,  4707,  4710,  4722,  4723,  4724,  4725,  4729,  4733,
    4737,  4741,  4745,  4749,  4753,  4757,  4761,  4765,  4769,  4773,
    4777,  4788,  4789,  4793,  4794,  4798,  4799,  4800,  4804,  4805,
    4809,  4834,  4837,  4845,  4844,  4857,  4881,  4880,  4894,  4898,
    4907,  4911,  4920,  4921,  4922,  4923,  4924,  4925,  4926,  4927,
    4928,  4929,  4930,  4931,  4932,  4939,  4963,  4991,  4994,  5002,
    5003,  5007,  5032,  5043,  5044,  5048,  5052,  5056,  5060,  5064,
    5068,  5072,  5076,  5080,  5084,  5088,  5092,  5096,  5101,  5106,
    5110,  5114,  5122,  5126,  5130,  5138,  5142,  5146,  5150,  5154,
    5158,  5162,  5166,  5170,  5178,  5186,  5190,  5194,  5198,  5202,
    5206,  5214,  5215,  5219,  5220,  5226,  5233,  5246,  5255,  5256,
    5265,  5272,  5284,  5302,  5303,  5307,  5308,  5312,  5313,  5316,
    5317,  5322,  5323,  5330,  5331,  5338,  5362,  5363,  5380,  5381,
    5384,  5385,  5392,  5393,  5398,  5409,  5420,  5431,  5442,  5471,
    5470,  5479,  5480,  5484,  5485,  5488,  5489,  5501,  5510,  5524,
    5526,  5525,  5545,  5547,  5546,  5562,  5564,  5563,  5572,  5573,
    5580,  5579,  5592,  5593,  5594,  5601,  5606,  5610,  5611,  5617,
    5624,  5628,  5629,  5635,  5672,  5676,  5681,  5687,  5688,  5693,
    5694,  5695,  5696,  5697,  5701,  5708,  5715,  5722,  5729,  5735,
    5736,  5741,  5740,  5747,  5748,  5752,  5753,  5754,  5755,  5756,
    5757,  5758,  5759,  5760,  5761,  5762,  5763,  5764,  5765,  5766,
    5767,  5771,  5778,  5779,  5780,  5781,  5782,  5783,  5784,  5787,
    5788,  5789,  5792,  5793,  5797,  5804,  5810,  5811,  5815,  5816,
    5820,  5827,  5831,  5838,  5839,  5843,  5850,  5851,  5855,  5856,
    5860,  5861,  5862,  5866,  5867,  5871,  5872,  5876,  5883,  5890,
    5898,  5900,  5899,  5920,  5921,  5925,  5926,  5930,  5932,  5931,
    5991,  6009,  6010,  6014,  6019,  6024,  6028,  6032,  6037,  6042,
    6047,  6052,  6056,  6060,  6065,  6070,  6075,  6080,  6085,  6090,
    6099,  6103,  6107,  6112,  6116,  6120,  6125,  6130,  6135,  6140,
    6141,  6142,  6143,  6144,  6145,  6146,  6147,  6148,  6157,  6162,
    6173,  6174,  6178,  6179,  6183,  6184,  6188,  6189,  6194,  6197,
    6201,  6209,  6212,  6216,  6224,  6235,  6243,  6245,  6255,  6244,
    6282,  6282,  6317,  6321,  6320,  6334,  6333,  6353,  6354,  6359,
    6381,  6383,  6387,  6398,  6400,  6408,  6416,  6424,  6453,  6486,
    6489,  6502,  6507,  6517,  6548,  6550,  6549,  6586,  6587,  6591,
    6592,  6593,  6610,  6611,  6622,  6621,  6671,  6672,  6676,  6725,
    6745,  6748,  6767,  6772,  6766,  6785,  6785,  6817,  6824,  6825,
    6826,  6827,  6828,  6829,  6830,  6831,  6832,  6833,  6834,  6835,
    6836,  6837,  6838,  6839,  6840,  6841,  6842,  6843,  6844,  6845,
    6846,  6847,  6848,  6849,  6850,  6851,  6852,  6853,  6854,  6855,
    6856,  6857,  6858,  6859,  6860,  6861,  6862,  6863,  6864,  6865,
    6866,  6867,  6868,  6869,  6870,  6871,  6872,  6873,  6874,  6875,
    6876,  6877,  6878,  6892,  6904,  6903,  6914,  6913,  6947,  6951,
    6955,  6960,  6965,  6970,  6975,  6979,  6983,  6987,  6991,  6996,
    7000,  7004,  7008,  7012,  7016,  7020,  7024,  7031,  7032,  7038,
    7040,  7044,  7045,  7049,  7050,  7054,  7058,  7059,  7068,  7069,
    7073,  7089,  7105,  7118,  7122,  7123,  7127,  7134,  7140,  7146,
    7151,  7156,  7161,  7166,  7172,  7177,  7183,  7189,  7200,  7205,
    7210,  7215,  7220,  7225,  7231,  7236,  7241,  7246,  7252,  7258,
    7264,  7269,  7274,  7281,  7288,  7297,  7298,  7299,  7303,  7304,
    7305,  7309,  7310,  7314,  7318,  7336,  7335,  7344,  7348,  7352,
    7358,  7359,  7366,  7370,  7381,  7380,  7389,  7393,  7405,  7406,
    7414,  7413,  7422,  7423,  7427,  7433,  7433,  7440,  7439,  7452,
    7451,  7485,  7489,  7494,  7499,  7519,  7525,  7533,  7537,  7536,
    7553,  7554,  7559,  7567,  7591,  7593,  7597,  7606,  7619,  7622,
    7626,  7630,  7635,  7658,  7659,  7663,  7664,  7668,  7672,  7676,
    7687,  7691,  7698,  7702,  7710,  7714,  7721,  7728,  7732,  7743,
    7742,  7754,  7758,  7765,  7766,  7776,  7775,  7783,  7788,  7796,
    7797,  7798,  7799,  7800,  7808,  7807,  7816,  7823,  7827,  7837,
    7848,  7866,  7865,  7874,  7878,  7882,  7887,  7895,  7899,  7910,
    7909,  7918,  7921,  7923,  7929,  7931,  7932,  7933,  7934,  7942,
    7941,  7953,  7957,  7961,  7965,  7969,  7973,  7981,  7990,  7991,
    7996,  7995,  8040,  8044,  8052,  8053,  8057,  8061,  8066,  8070,
    8071,  8075,  8079,  8083,  8087,  8094,  8095,  8099,  8104,  8110,
    8116,  8121,  8126,  8132,  8138,  8144,  8150,  8155,  8160,  8165,
    8170,  8175,  8180,  8187,  8197,  8201,  8212,  8211,  8220,  8224,
    8228,  8232,  8236,  8243,  8247,  8258,  8257,  8269,  8268,  8277,
    8296,  8295,  8319,  8327,  8328,  8333,  8344,  8355,  8369,  8373,
    8380,  8381,  8386,  8395,  8404,  8409,  8418,  8419,  8424,  8486,
    8487,  8488,  8492,  8493,  8497,  8501,  8512,  8511,  8523,  8524,
    8545,  8559,  8581,  8603,  8623,  8646,  8647,  8655,  8654,  8663,
    8674,  8673,  8683,  8690,  8689,  8702,  8711,  8715,  8726,  8742,
    8741,  8750,  8754,  8758,  8765,  8769,  8780,  8779,  8787,  8795,
    8796,  8800,  8801,  8802,  8807,  8810,  8817,  8821,  8829,  8836,
    8837,  8838,  8839,  8840,  8841,  8842,  8854,  8857,  8867,  8866,
    8875,  8881,  8893,  8892,  8901,  8905,  8906,  8907,  8911,  8912,
    8913,  8914,  8921,  8920,  8941,  8951,  8960,  8964,  8971,  8976,
    8981,  8986,  8991,  8996,  9004,  9005,  9009,  9014,  9020,  9022,
    9023,  9024,  9025,  9029,  9057,  9060,  9064,  9068,  9072,  9079,
    9086,  9096,  9095,  9108,  9107,  9115,  9119,  9130,  9129,  9138,
    9142,  9149,  9153,  9164,  9163,  9171,  9172,  9176,  9201,  9202,
    9203,  9204,  9208,  9209,  9213,  9214,  9215,  9216,  9228,  9227,
    9239,  9246,  9245,  9257,  9266,  9274,  9281,  9285,  9298,  9305,
    9317,  9320,  9325,  9329,  9340,  9347,  9348,  9352,  9353,  9356,
    9357,  9362,  9372,  9371,  9384,  9383,  9392,  9421,  9422,  9426,
    9430,  9434,  9438,  9445,  9446,  9450,  9454,  9457,  9459,  9463,
    9472,  9473,  9474,  9477,  9479,  9483,  9487,  9491,  9499,  9500,
    9504,  9505,  9509,  9513,  9523,  9534,  9533,  9542,  9547,  9548,
    9552,  9553,  9554,  9558,  9559,  9563,  9567,  9568,  9572,  9576,
    9580,  9590,  9589,  9597,  9607,  9618,  9617,  9626,  9633,  9637,
    9648,  9647,  9659,  9668,  9671,  9675,  9679,  9686,  9690,  9700,
    9712,  9711,  9720,  9724,  9733,  9734,  9739,  9742,  9750,  9754,
    9761,  9769,  9773,  9784,  9783,  9791,  9794,  9799,  9801,  9805,
    9811,  9812,  9813,  9814,  9817,  9819,  9826,  9825,  9839,  9840,
    9841,  9842,  9843,  9844,  9845,  9849,  9850,  9854,  9855,  9861,
    9870,  9877,  9878,  9882,  9886,  9890,  9896,  9902,  9906,  9910,
    9914,  9923,  9927,  9936,  9945,  9946,  9950,  9959,  9960,  9964,
    9968,  9977,  9987,  9986,  9995,  9994, 10026, 10029, 10049, 10050,
   10053, 10054, 10062, 10063, 10068, 10073, 10083, 10099, 10104, 10114,
   10131, 10130, 10140, 10153, 10156, 10164, 10167, 10172, 10177, 10185,
   10186, 10187, 10188, 10189, 10190, 10194, 10202, 10203, 10207, 10211,
   10222, 10221, 10231, 10244, 10247, 10251, 10255, 10263, 10275, 10278,
   10285, 10286, 10287, 10288, 10295, 10294, 10304, 10311, 10312, 10316,
   10331, 10332, 10337, 10338, 10342, 10343, 10347, 10351, 10362, 10361,
   10370, 10374, 10378, 10385, 10389, 10399, 10410, 10411, 10418, 10417,
   10426, 10432, 10444, 10443, 10451, 10465, 10464, 10472, 10489, 10488,
   10497, 10505, 10506, 10511, 10512, 10517, 10524, 10525, 10530, 10537,
   10538, 10542, 10543, 10547, 10548, 10552, 10556, 10567, 10566, 10575,
   10576, 10577, 10578, 10579, 10583, 10610, 10613, 10625, 10635, 10640,
   10645, 10650, 10658, 10698, 10699, 10703, 10746, 10756, 10779, 10780,
   10781, 10782, 10786, 10795, 10801, 10811, 10820, 10829, 10830, 10837,
   10836, 10848, 10858, 10859, 10864, 10867, 10871, 10875, 10882, 10883,
   10887, 10888, 10889, 10893, 10897, 10909, 10910, 10911, 10921, 10925,
   10932, 10940, 10941, 10945, 10946, 10950, 10958, 10959, 10964, 10965,
   10966, 10976, 10980, 10987, 10995, 10996, 11000, 11010, 11011, 11012,
   11022, 11026, 11033, 11041, 11042, 11046, 11056, 11057, 11058, 11068,
   11072, 11079, 11087, 11088, 11092, 11103, 11104, 11111, 11113, 11122,
   11126, 11133, 11141, 11142, 11146, 11156, 11157, 11167, 11171, 11178,
   11186, 11187, 11191, 11201, 11202, 11206, 11207, 11217, 11221, 11228,
   11236, 11237, 11241, 11251, 11255, 11265, 11272, 11279, 11279, 11290,
   11291, 11292, 11296, 11297, 11299, 11300, 11302, 11303, 11304, 11305,
   11306, 11308, 11309, 11310, 11311, 11312, 11313, 11315, 11316, 11317,
   11319, 11320, 11321, 11322, 11323, 11326, 11327, 11331, 11332, 11336,
   11337, 11341, 11342, 11346, 11350, 11356, 11360, 11366, 11367, 11368,
   11372, 11373, 11374, 11378, 11379, 11380, 11384, 11388, 11392, 11393,
   11394, 11397, 11398, 11408, 11420, 11429, 11441, 11450, 11462, 11477,
   11478, 11483, 11492, 11498, 11508, 11522, 11544, 11548, 11569, 11581,
   11622, 11636, 11637, 11642, 11648, 11649, 11654, 11666, 11667, 11668,
   11675, 11686, 11687, 11691, 11699, 11707, 11711, 11718, 11727, 11728,
   11734, 11743, 11754, 11771, 11775, 11782, 11783, 11784, 11791, 11792,
   11796, 11800, 11807, 11808, 11812, 11813, 11817, 11818, 11819, 11820,
   11824, 11828, 11832, 11836, 11840, 11861, 11865, 11872, 11873, 11874,
   11878, 11879, 11880, 11881, 11882, 11886, 11890, 11897, 11898, 11902,
   11903, 11907, 11914, 11921, 11922, 11923, 11927, 11928, 11932, 11936,
   11940, 11944, 11945, 11949, 11953, 11954, 11958, 11962, 11963, 11970,
   11974, 11978, 11982, 11986, 11990, 11991, 11997, 12001, 12005, 12006,
   12010, 12017, 12027, 12046, 12064, 12071, 12078, 12085, 12095, 12102,
   12112, 12122, 12132, 12145, 12149, 12157, 12165, 12169, 12179, 12193,
   12216, 12238, 12254, 12255, 12256, 12257, 12258, 12259, 12263, 12267,
   12284, 12288, 12295, 12296, 12297, 12298, 12299, 12300, 12301, 12307,
   12311, 12315, 12319, 12323, 12327, 12331, 12335, 12339, 12343, 12347,
   12351, 12355, 12362, 12363, 12367, 12368, 12369, 12373, 12374, 12375,
   12376, 12380, 12384, 12388, 12395, 12399, 12403, 12410, 12417, 12424,
   12434, 12434, 12445, 12452, 12462, 12469, 12479, 12483, 12496, 12500,
   12515, 12523, 12524, 12528, 12529, 12533, 12534, 12539, 12542, 12550,
   12553, 12560, 12562, 12563, 12567, 12568, 12572, 12573, 12574, 12579,
   12582, 12595, 12599, 12607, 12611, 12615, 12619, 12623, 12627, 12631,
   12635, 12642, 12643, 12649, 12653, 12657, 12664, 12665, 12666, 12667,
   12668, 12669, 12670, 12671, 12672, 12673, 12674, 12675, 12676, 12677,
   12678, 12679, 12680, 12681, 12682, 12683, 12684, 12685, 12686, 12687,
   12688, 12689, 12690, 12691, 12692, 12693, 12694, 12695, 12696, 12697,
   12698, 12699, 12700, 12701, 12702, 12703, 12704, 12705, 12706, 12707,
   12708, 12709, 12710, 12711, 12712, 12716, 12717, 12718, 12719, 12720,
   12721, 12722, 12723, 12724, 12725, 12726, 12727, 12728, 12729, 12730,
   12731, 12732, 12733, 12734, 12735, 12736, 12743, 12743, 12744, 12744,
   12745, 12745, 12746, 12746, 12747, 12747, 12747, 12748, 12748, 12749,
   12749, 12750, 12750, 12751, 12751, 12752, 12752, 12753, 12753, 12754,
   12754, 12755, 12755, 12756, 12756, 12757, 12757, 12758, 12758, 12759,
   12759, 12760, 12760, 12761, 12761, 12762, 12762, 12763, 12763, 12763,
   12764, 12764, 12765, 12765, 12766, 12766, 12767, 12767, 12768, 12768,
   12768, 12769, 12769, 12770, 12770, 12770, 12771, 12771, 12772, 12772,
   12772, 12773, 12773, 12773, 12774, 12774, 12775, 12775, 12776, 12776,
   12777, 12777, 12778, 12778, 12778, 12779, 12779, 12780, 12780, 12781,
   12781, 12781, 12781, 12782, 12782, 12783, 12783, 12784, 12784, 12785,
   12785, 12786, 12786, 12786, 12787, 12787, 12788, 12788, 12789, 12789,
   12790, 12790, 12790, 12791, 12791, 12792, 12792, 12793, 12793, 12794,
   12794, 12795, 12795, 12796, 12796, 12797, 12797, 12798, 12798, 12799,
   12799, 12800, 12800, 12800, 12801, 12801, 12802, 12802, 12803, 12803,
   12807, 12807, 12808, 12808, 12809, 12809, 12810, 12810, 12811, 12811,
   12812, 12812, 12813, 12813, 12814, 12814, 12815, 12815, 12816, 12816,
   12817, 12817, 12818, 12818, 12819, 12819, 12820, 12820, 12821, 12821,
   12824, 12825, 12826, 12830, 12830, 12831, 12831, 12832, 12832, 12833,
   12833, 12834, 12834, 12835, 12835, 12836, 12836, 12837, 12837
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "ACCEPT", "ACCESS", "ADD",
  "ADDRESS", "ADVANCING", "AFTER", "ALL", "ALLOCATE", "ALPHABET",
  "ALPHABETIC", "\"ALPHABETIC-LOWER\"", "\"ALPHABETIC-UPPER\"",
  "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"", "ALSO", "ALTER", "ALTERNATE",
  "AND", "ANY", "ARE", "AREA", "AREAS", "\"ARGUMENT-NUMBER\"",
  "\"ARGUMENT-VALUE\"", "AS", "ASCENDING", "ASCII", "ASSIGN", "AT",
  "ATTRIBUTE", "AUTO", "AUTOMATIC", "\"AWAY-FROM-ZERO\"",
  "\"BACKGROUND-COLOR\"", "BASED", "BEFORE", "BELL", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SHORT\"", "BLANK", "BLINK", "BLOCK",
  "BOTTOM", "BY", "\"BYTE-LENGTH\"", "CALL", "CANCEL", "CAPACITY",
  "\"CARD-PUNCH\"", "\"CARD-READER\"", "CASSETTE", "CD", "CF", "CH",
  "CHAINING", "CHARACTER", "CHARACTERS", "CLASS", "CLASSIFICATION",
  "\"class-name\"", "CLOSE", "CODE", "\"CODE-SET\"", "COLLATING", "COL",
  "COLS", "COLUMN", "COLUMNS", "COMMA", "\"COMMAND-LINE\"",
  "\"comma delimiter\"", "COMMIT", "COMMON", "COMMUNICATION", "COMP",
  "COMPUTE", "\"COMP-1\"", "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"",
  "\"COMP-5\"", "\"COMP-6\"", "\"COMP-X\"", "\"FUNCTION CONCATENATE\"",
  "CONDITION", "CONFIGURATION", "CONSTANT", "CONTAINS", "CONTENT",
  "CONTINUE", "CONTROL", "CONTROLS", "CONVERSION", "CONVERTING", "COPY",
  "CORRESPONDING", "COUNT", "CRT", "\"CRT-UNDER\"", "CURRENCY",
  "\"FUNCTION CURRENT-DATE\"", "CURSOR", "CYCLE", "DATA", "DATE", "DAY",
  "\"DAY-OF-WEEK\"", "DE", "DEBUGGING", "\"DECIMAL-POINT\"",
  "DECLARATIVES", "DEFAULT", "DELETE", "DELIMITED", "DELIMITER",
  "DEPENDING", "DESCENDING", "DESTINATION", "DETAIL", "DISABLE", "DISC",
  "DISK", "DISPLAY", "\"FUNCTION DISPLAY-OF\"", "DIVIDE", "DIVISION",
  "DOWN", "DUPLICATES", "DYNAMIC", "EBCDIC", "EC", "ECHO", "EGI", "\"88\"",
  "ENABLE", "ELSE", "EMI", "END", "\"END-ACCEPT\"", "\"END-ADD\"",
  "\"END-CALL\"", "\"END-COMPUTE\"", "\"END-DELETE\"", "\"END-DISPLAY\"",
  "\"END-DIVIDE\"", "\"END-EVALUATE\"", "\"END FUNCTION\"", "\"END-IF\"",
  "\"END-MULTIPLY\"", "\"END-PERFORM\"", "\"END PROGRAM\"", "\"END-READ\"",
  "\"END-RECEIVE\"", "\"END-RETURN\"", "\"END-REWRITE\"", "\"END-SEARCH\"",
  "\"END-START\"", "\"END-STRING\"", "\"END-SUBTRACT\"",
  "\"END-UNSTRING\"", "\"END-WRITE\"", "ENTRY", "ENVIRONMENT",
  "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL", "EOP", "EOS",
  "EQUAL", "ERASE", "ERROR", "ESCAPE", "ESI", "EVALUATE",
  "\"EVENT STATUS\"", "EXCEPTION", "\"EXCEPTION CONDITION\"", "EXCLUSIVE",
  "EXIT", "\"Exponentiation operator\"", "EXTEND", "EXTERNAL", "F", "FD",
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
  "LAST", "LEADING", "LEFT", "LEFTLINE", "LENGTH",
  "\"FUNCTION LENGTH/BYTE-LENGTH\"", "\"LENGTH OF\"", "LESS",
  "\"LESS OR EQUAL\"", "LIMIT", "LIMITS", "LINAGE", "\"LINAGE-COUNTER\"",
  "LINE", "\"LINE-COUNTER\"", "LINES", "LINKAGE", "\"Literal\"", "LOCALE",
  "\"FUNCTION LOCALE-DATE\"", "\"FUNCTION LOCALE-TIME\"",
  "\"FUNCTION LOCALE-TIME-FROM-SECONDS\"", "\"LOCAL-STORAGE\"", "LOCK",
  "LOWER", "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-VALUE\"",
  "MANUAL", "\"MAGNETIC-TAPE\"", "MEMORY", "MERGE", "MESSAGE", "MINUS",
  "\"Mnemonic name\"", "MODE", "MOVE", "MULTIPLE", "MULTIPLY", "NAME",
  "NATIONAL", "\"NATIONAL-EDITED\"", "\"FUNCTION NATIONAL-OF\"", "NATIVE",
  "\"NEAREST-AWAY-FROM-ZERO\"", "\"NEAREST-EVEN\"",
  "\"NEAREST-TOWARD-ZERO\"", "NEGATIVE", "NEXT", "\"NEXT PAGE\"", "NO",
  "\"NO DATA\"", "\"NO-ECHO\"", "NORMAL", "NOT", "NOTHING", "\"NOT END\"",
  "\"NOT EOP\"", "\"NOT ESCAPE\"", "\"NOT EQUAL\"", "\"NOT EXCEPTION\"",
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
  "PROHIBITED", "PROMPT", "\"PROTECTED\"", "QUEUE", "QUOTE", "RANDOM",
  "RD", "READ", "\"READY TRACE\"", "RECEIVE", "RECORD", "RECORDING",
  "RECORDS", "RECURSIVE", "REDEFINES", "REEL", "REFERENCE", "REFERENCES",
  "RELATIVE", "RELEASE", "REMAINDER", "REMOVAL", "RENAMES", "REPLACE",
  "REPLACING", "REPORT", "REPORTING", "REPORTS", "REPOSITORY", "REQUIRED",
  "RESERVE", "RESET", "\"RESET TRACE\"", "RETRY", "RETURN", "RETURNING",
  "REVERSE", "\"FUNCTION REVERSE\"", "\"REVERSE-VIDEO\"", "REVERSED",
  "REWIND", "REWRITE", "RF", "RH", "RIGHT", "ROLLBACK", "ROUNDED", "RUN",
  "S", "SAME", "SCREEN", "\"SCREEN-CONTROL\"", "SCROLL", "SD", "SEARCH",
  "SECONDS", "SECTION", "SECURE", "SEGMENT", "\"SEGMENT-LIMIT\"", "SELECT",
  "\"semi-colon\"", "SENTENCE", "SEPARATE", "SEQUENCE", "SEQUENTIAL",
  "SET", "\"78\"", "SHARING", "SIGN", "SIGNED", "\"SIGNED-INT\"",
  "\"SIGNED-LONG\"", "\"SIGNED-SHORT\"", "\"66\"", "SIZE",
  "\"SIZE ERROR\"", "SORT", "\"SORT-MERGE\"", "SOURCE",
  "\"SOURCE-COMPUTER\"", "SPACE", "\"SPECIAL-NAMES\"", "STANDARD",
  "\"STANDARD-1\"", "\"STANDARD-2\"", "START", "STATIC", "STATUS",
  "STDCALL", "STEP", "STOP", "STRING", "SUB_QUEUE_1", "SUB_QUEUE_2",
  "SUB_QUEUE_3", "\"FUNCTION SUBSTITUTE\"", "\"FUNCTION SUBSTITUTE-CASE\"",
  "SUBTRACT", "SUM", "SUPPRESS", "SYMBOLIC", "SYNCHRONIZED",
  "\"SYSTEM-DEFAULT\"", "\"SYSTEM-OFFSET\"", "TAB", "TABLE", "TALLYING",
  "TAPE", "TERMINAL", "TERMINATE", "TEXT", "TEST", "THAN", "THEN", "THRU",
  "TIME", "\"TIME-OUT\"", "TIMES", "TO", "\"&\"", "\")\"", "\":\"",
  "\"/\"", "\".\"", "\"=\"", "\"FALSE\"", "\"FILE\"", "\">\"",
  "\"INITIAL\"", "\"<\"", "\"-\"", "\"*\"", "\"NULL\"", "\"OVERFLOW\"",
  "\"(\"", "\"+\"", "\"TRUE\"", "TOP", "\"TOWARD-GREATER\"",
  "\"TOWARD-LESSER\"", "TRAILING", "TRANSFORM", "\"FUNCTION TRIM\"",
  "TRUNCATION", "TYPE", "U", "UNBOUNDED", "UNDERLINE", "UNIT", "UNLOCK",
  "UNSIGNED", "\"UNSIGNED-INT\"", "\"UNSIGNED-LONG\"",
  "\"UNSIGNED-SHORT\"", "UNSTRING", "UNTIL", "UP", "UPDATE", "UPON",
  "\"UPON ARGUMENT-NUMBER\"", "\"UPON COMMAND-LINE\"",
  "\"UPON ENVIRONMENT-NAME\"", "\"UPON ENVIRONMENT-VALUE\"", "UPPER",
  "\"FUNCTION UPPER-CASE\"", "USAGE", "USE", "USER", "\"USER-DEFAULT\"",
  "\"User function name\"", "USING", "V", "VALUE", "VARIABLE", "VARYING",
  "WAIT", "WHEN", "\"FUNCTION WHEN-COMPILED\"", "WITH", "\"Identifier\"",
  "WORDS", "\"WORKING-STORAGE\"", "WRITE", "YYYYDDD", "YYYYMMDD", "ZERO",
  "SHIFT_PREFER", "PURGE", "SEND", "OVERFLOW", "$accept", "start", "$@1",
  "nested_list", "source_element_list", "source_element", "simple_prog",
  "$@2", "program_definition", "function_definition", "_end_program_list",
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
  "printer_name", "general_device_name", "line_seq_device_name",
  "_line_adv_file", "_ext_clause", "assignment_name", "_assignment_name",
  "access_mode_clause", "access_mode", "alternative_record_key_clause",
  "_suppress_clause", "collating_sequence_clause", "alphabet_name",
  "file_status_clause", "_file_or_sort", "lock_mode_clause", "$@13",
  "lock_mode", "_lock_with", "organization_clause", "organization",
  "padding_character_clause", "record_delimiter_clause",
  "record_key_clause", "key_or_split_keys", "relative_key_clause",
  "reserve_clause", "no_or_integer", "sharing_clause", "sharing_option",
  "_i_o_control", "i_o_control_list", "i_o_control_clause", "same_clause",
  "_same_option", "multiple_file_tape_clause", "$@14",
  "multiple_file_list", "multiple_file", "_multiple_file_position",
  "_data_division", "$@15", "_data_division_header",
  "_file_section_header", "_file_description_sequence", "file_description",
  "file_description_entry", "$@16", "file_type",
  "_file_description_clause_sequence", "file_description_clause",
  "block_contains_clause", "_records_or_characters", "record_clause",
  "_record_depending", "_from_integer", "_to_integer",
  "label_records_clause", "value_of_clause", "file_id", "valueof_name",
  "data_records_clause", "linage_clause", "_linage_sequence",
  "linage_lines", "linage_footing", "linage_top", "linage_bottom",
  "recording_mode_clause", "recording_mode", "u_or_s", "code_set_clause",
  "_for_sub_records_clause", "report_clause", "report_keyword",
  "rep_name_list", "_communication_section", "$@17",
  "_communication_description_sequence", "communication_description",
  "communication_description_entry", "$@18",
  "_communication_description_clause_sequence",
  "communication_description_clause", "_input_cd_clauses",
  "named_input_cd_clauses", "named_input_cd_clause",
  "unnamed_input_cd_clauses", "_output_cd_clauses", "output_cd_clauses",
  "output_cd_clause", "_i_o_cd_clauses", "named_i_o_cd_clauses",
  "named_i_o_cd_clause", "unnamed_i_o_cd_clauses",
  "_working_storage_section", "$@19", "_record_description_list", "$@20",
  "record_description_list_2", "data_description", "$@21", "level_number",
  "_entry_name", "user_entry_name", "const_global", "lit_or_length",
  "con_identifier", "fp32_usage", "fp64_usage", "fp128_usage",
  "pointer_len", "renames_entry", "_renames_thru", "condition_name_entry",
  "$@22", "constant_entry", "$@23", "constant_source",
  "_data_description_clause_sequence", "data_description_clause",
  "redefines_clause", "external_clause", "_as_extname", "_global_clause",
  "global_clause", "picture_clause", "usage_clause", "usage",
  "float_usage", "double_usage", "sign_clause", "report_occurs_clause",
  "_occurs_step", "occurs_clause", "_occurs_to_integer",
  "_occurs_from_integer", "_occurs_integer_to", "_occurs_depending",
  "_capacity_in", "_occurs_initialized", "_occurs_keys",
  "_occurs_key_list", "ascending_or_descending", "_occurs_indexed",
  "occurs_index_list", "occurs_index", "justified_clause",
  "synchronized_clause", "blank_clause", "based_clause", "value_clause",
  "$@24", "value_item_list", "value_item", "_false_is",
  "any_length_clause", "_local_storage_section", "$@25",
  "_linkage_section", "$@26", "_report_section", "$@27",
  "_report_description_sequence", "report_description", "$@28",
  "_report_description_options", "report_description_option",
  "control_clause", "control_field_list", "identifier_list",
  "page_limit_clause", "page_line_column", "_page_heading_list",
  "page_detail", "heading_clause", "first_detail", "last_heading",
  "last_detail", "footing_clause", "_report_group_description_list",
  "report_group_description_entry", "$@29", "_report_group_options",
  "report_group_option", "type_clause", "type_option", "_control_final",
  "_or_page", "next_group_clause", "sum_clause_list", "_reset_clause",
  "data_or_final", "present_when_condition", "varying_clause",
  "line_clause", "line_keyword_clause", "column_clause",
  "col_keyword_clause", "report_line_integer_list", "line_or_plus",
  "report_col_integer_list", "col_or_plus", "source_clause",
  "group_indicate_clause", "report_usage_clause", "_screen_section",
  "$@30", "_screen_description_list", "screen_description_list",
  "screen_description", "$@31", "_screen_options", "screen_option", "eol",
  "eos", "plus_plus", "minus_minus", "_screen_line_plus_minus",
  "_screen_col_plus_minus", "screen_occurs_clause", "global_screen_opt",
  "_procedure_division", "$@32", "$@33", "$@34",
  "_procedure_using_chaining", "$@35", "$@36", "procedure_param_list",
  "procedure_param", "_procedure_type", "_size_optional",
  "_procedure_optional", "_procedure_returning", "_procedure_declaratives",
  "$@37", "_procedure_list", "procedure", "section_header", "$@38",
  "_use_statement", "paragraph_header", "invalid_statement", "_segment",
  "statement_list", "@39", "@40", "statements", "$@41", "statement",
  "accept_statement", "$@42", "accept_body", "$@43", "accp_identifier",
  "_accept_clauses", "accept_clauses", "accept_clause", "lines_or_number",
  "at_line_column", "line_number", "column_number", "mode_is_block",
  "accp_attr", "no_echo", "reverse_video", "update_default", "end_accept",
  "add_statement", "$@44", "add_body", "_add_to", "end_add",
  "allocate_statement", "$@45", "allocate_body", "allocate_returning",
  "alter_statement", "$@46", "alter_body", "alter_entry", "_proceed_to",
  "call_statement", "$@47", "call_body", "$@48", "mnemonic_conv",
  "id_or_lit_or_func_or_program_name", "call_using", "$@49",
  "call_param_list", "call_param", "call_type", "call_returning",
  "return_give", "null_or_omitted", "call_exception_phrases",
  "_call_on_exception", "call_on_exception", "_call_not_on_exception",
  "call_not_on_exception", "end_call", "cancel_statement", "$@50",
  "cancel_body", "id_or_lit_or_program_name", "close_statement", "$@51",
  "close_body", "close_option", "compute_statement", "$@52",
  "compute_body", "end_compute", "commit_statement", "continue_statement",
  "delete_statement", "$@53", "delete_body", "delete_file_list",
  "end_delete", "disable_statement", "$@54", "enable_disable_handling",
  "_enable_disable_key", "communication_mode", "display_statement", "$@55",
  "display_body", "screen_or_device_display", "display_list",
  "display_atom", "$@56", "disp_list", "display_clauses", "display_clause",
  "display_upon", "crt_under", "disp_attr", "end_display",
  "divide_statement", "$@57", "divide_body", "end_divide",
  "enable_statement", "$@58", "entry_statement", "$@59", "entry_body",
  "evaluate_statement", "$@60", "evaluate_body", "evaluate_subject_list",
  "evaluate_subject", "evaluate_condition_list", "evaluate_case_list",
  "evaluate_case", "evaluate_other", "evaluate_when_list",
  "evaluate_object_list", "evaluate_object", "_evaluate_thru_expr",
  "end_evaluate", "exit_statement", "$@61", "exit_body",
  "exit_program_returning", "free_statement", "$@62", "free_body",
  "generate_statement", "$@63", "generate_body", "goto_statement", "$@64",
  "go_body", "goto_depending", "goback_statement", "if_statement", "$@65",
  "if_else_statements", "end_if", "initialize_statement", "$@66",
  "initialize_body", "initialize_filler", "initialize_value",
  "initialize_replacing", "initialize_replacing_list",
  "initialize_replacing_item", "initialize_category", "initialize_default",
  "initiate_statement", "$@67", "initiate_body", "inspect_statement",
  "$@68", "inspect_body", "send_identifier", "inspect_list",
  "inspect_tallying", "$@69", "inspect_replacing", "inspect_converting",
  "tallying_list", "tallying_item", "replacing_list", "replacing_item",
  "rep_keyword", "replacing_region", "inspect_region", "inspect_before",
  "inspect_after", "merge_statement", "$@70", "move_statement", "$@71",
  "move_body", "multiply_statement", "$@72", "multiply_body",
  "end_multiply", "open_statement", "$@73", "open_body", "open_file_entry",
  "open_mode", "open_sharing", "open_option", "perform_statement", "$@74",
  "perform_body", "$@75", "end_perform", "term_or_dot",
  "perform_procedure", "perform_option", "perform_test", "cond_or_exit",
  "perform_varying_list", "perform_varying", "purge_statement", "$@76",
  "read_statement", "$@77", "read_body", "read_into", "lock_phrases",
  "ignoring_lock", "advancing_lock_or_retry", "_retry_phrase",
  "retry_phrase", "retry_options", "_extended_with_lock",
  "extended_with_lock", "read_key", "read_handler", "end_read",
  "ready_statement", "receive_statement", "$@78", "receive_body",
  "message_or_segment", "_data_sentence_phrases", "_no_data_sentence",
  "no_data_sentence", "_with_data_sentence", "with_data_sentence",
  "end_receive", "release_statement", "$@79", "release_body",
  "reset_statement", "return_statement", "$@80", "return_body",
  "end_return", "rewrite_statement", "$@81", "rewrite_body", "_with_lock",
  "with_lock", "end_rewrite", "rollback_statement", "search_statement",
  "$@82", "search_body", "search_varying", "search_at_end", "search_whens",
  "search_when", "end_search", "send_statement", "$@83", "send_body",
  "_from_identifier", "from_identifier", "with_indicator",
  "_replacing_line", "set_statement", "$@84", "set_body", "on_or_off",
  "up_or_down", "set_environment", "set_attr", "set_attr_clause",
  "set_attr_one", "set_to", "set_up_down", "set_to_on_off_sequence",
  "set_to_on_off", "set_to_true_false_sequence", "set_to_true_false",
  "set_last_exception_to_off", "sort_statement", "$@85", "sort_body",
  "@86", "sort_key_list", "_key_list", "_sort_duplicates",
  "sort_collating", "sort_input", "sort_output", "start_statement", "$@87",
  "start_body", "sizelen_clause", "start_key", "start_op", "disallowed_op",
  "not_equal_op", "end_start", "stop_statement", "$@88", "stop_returning",
  "_status_x", "stop_literal", "string_statement", "$@89", "string_body",
  "string_item_list", "string_item", "_string_delimited",
  "string_delimiter", "_with_pointer", "end_string", "subtract_statement",
  "$@90", "subtract_body", "end_subtract", "suppress_statement",
  "_printing", "terminate_statement", "$@91", "terminate_body",
  "transform_statement", "$@92", "transform_body", "unlock_statement",
  "$@93", "unlock_body", "unstring_statement", "$@94", "unstring_body",
  "_unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "_unstring_into_delimiter", "_unstring_into_count", "_unstring_tallying",
  "end_unstring", "use_statement", "$@95", "use_phrase",
  "use_file_exception", "use_global", "use_file_exception_target",
  "use_debugging", "debugging_list", "debugging_target", "_all_refs",
  "use_start_end", "program_start_end", "use_reporting", "use_exception",
  "use_ex_keyw", "write_statement", "$@96", "write_body", "from_option",
  "write_option", "before_or_after", "write_handler", "end_write",
  "_accept_exception_phrases", "_accp_on_exception", "accp_on_exception",
  "escape_or_exception", "_accp_not_on_exception", "accp_not_on_exception",
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
  "condition", "expr", "partial_expr", "$@97", "expr_tokens", "expr_token",
  "eq", "gt", "lt", "ge", "le", "exp_list", "_e_sep", "exp", "exp_term",
  "exp_factor", "exp_unary", "exp_atom", "line_linage_page_counter",
  "arithmetic_x_list", "arithmetic_x", "record_name",
  "file_or_record_name", "table_name", "file_name_list", "file_name",
  "cd_name", "report_name", "mnemonic_name_list", "mnemonic_name",
  "procedure_name_list", "procedure_name", "label", "integer_label",
  "reference_list", "reference", "single_reference",
  "optional_reference_list", "optional_reference", "reference_or_literal",
  "undefined_word", "unique_word", "target_x_list", "target_x", "_x_list",
  "x_list", "x", "call_x", "x_common", "report_x_list", "expr_x",
  "arith_x", "prog_or_entry", "alnum_or_id", "simple_display_value",
  "simple_display_all_value", "simple_value", "simple_all_value",
  "id_or_lit", "id_or_lit_or_func", "id_or_lit_or_length_or_func",
  "num_id_or_lit", "positive_id_or_lit", "pos_num_id_or_lit",
  "from_parameter", "sub_identifier", "sort_identifier",
  "sub_identifier_1", "display_identifier", "numeric_identifier",
  "identifier_or_file_name", "identifier", "identifier_1",
  "target_identifier", "target_identifier_1", "qualified_word", "subref",
  "refmod", "integer", "symbolic_integer", "report_integer", "class_value",
  "literal", "basic_literal", "basic_value", "function", "func_no_parm",
  "func_one_parm", "func_multi_parm", "func_refmod", "func_args",
  "trim_args", "length_arg", "$@98", "numvalc_args", "locale_dt_args",
  "formatted_datetime_args", "formatted_time_args", "not_const_word",
  "flag_all", "flag_duplicates", "flag_initialized", "flag_initialized_to",
  "to_init_val", "_flag_next", "_flag_not", "flag_optional",
  "flag_rounded", "round_mode", "round_choice", "flag_separate",
  "error_stmt_recover", "verb", "scope_terminator", "_advancing", "_after",
  "_are", "_area", "_areas", "_as", "_at", "_before", "_binary", "_by",
  "_character", "_characters", "_contains", "_data", "_end_of", "_file",
  "_final", "_for", "_from", "_in", "_in_order", "_indicate", "_initial",
  "_into", "_is", "_is_are", "_key", "_left_or_right", "_line",
  "_line_or_lines", "_limits", "_lines", "_message", "_mode", "_number",
  "_numbers", "_of", "_on", "_onoff_status", "_other", "_procedure",
  "_program", "_record", "_records", "_right", "_sign", "_signed",
  "_sign_is", "_size", "_standard", "_status", "_symbolic", "_tape",
  "_terminal", "_then", "_times", "_to", "_to_using", "_when",
  "_when_set_to", "_with", "coll_sequence", "column_or_col",
  "columns_or_cols", "comp_equal", "exception_or_error", "in_of",
  "label_option", "line_or_lines", "lock_records", "object_char_or_word",
  "records", "reel_or_unit", "scroll_line_or_lines", "size_or_length",
  "with_dups", "prog_coll_sequence", "detail_keyword", "ch_keyword",
  "cf_keyword", "ph_keyword", "pf_keyword", "rh_keyword", "rf_keyword",
  "control_keyword", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
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
     775,   776,   777,   778,   779,   780,   781,   782,   783,   784,
     785,   786,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   814,
     815,   816
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   562,   564,   563,   565,   565,   566,   566,   567,   567,
     569,   568,   570,   571,   572,   572,   573,   573,   574,   575,
     576,   577,   577,   577,   579,   580,   578,   582,   581,   583,
     583,   584,   584,   585,   585,   586,   586,   587,   587,   587,
     587,   588,   588,   589,   589,   590,   591,   591,   592,   593,
     593,   594,   594,   594,   594,   594,   596,   595,   597,   597,
     598,   598,   600,   599,   601,   601,   601,   601,   602,   602,
     603,   603,   603,   603,   604,   605,   606,   607,   608,   608,
     608,   608,   609,   609,   610,   611,   610,   612,   612,   612,
     613,   613,   614,   614,   614,   614,   615,   615,   616,   616,
     617,   617,   618,   618,   619,   619,   620,   620,   620,   620,
     620,   620,   620,   620,   620,   620,   620,   620,   622,   621,
     623,   623,   623,   623,   624,   624,   625,   626,   626,   628,
     627,   629,   629,   629,   629,   629,   629,   630,   630,   631,
     631,   632,   631,   633,   633,   634,   634,   634,   634,   634,
     634,   635,   635,   636,   637,   637,   638,   639,   639,   640,
     641,   641,   642,   642,   643,   644,   644,   645,   645,   646,
     647,   648,   648,   649,   650,   651,   652,   653,   654,   655,
     656,   656,   657,   657,   658,   658,   659,   659,   661,   660,
     662,   662,   663,   663,   663,   663,   663,   663,   663,   663,
     663,   663,   663,   663,   663,   664,   664,   664,   664,   664,
     664,   665,   665,   665,   666,   666,   666,   666,   667,   667,
     667,   667,   667,   667,   667,   668,   668,   669,   669,   669,
     670,   670,   671,   671,   671,   672,   673,   673,   673,   674,
     675,   675,   675,   676,   677,   678,   679,   679,   679,   681,
     680,   682,   682,   682,   683,   683,   683,   683,   684,   684,
     685,   685,   685,   685,   686,   687,   688,   689,   689,   689,
     690,   691,   692,   692,   693,   694,   694,   694,   695,   695,
     695,   696,   696,   697,   697,   698,   699,   699,   699,   699,
     701,   700,   702,   702,   703,   704,   704,   706,   705,   707,
     707,   708,   708,   709,   709,   710,   712,   711,   711,   713,
     713,   714,   714,   715,   715,   715,   715,   715,   715,   715,
     715,   715,   715,   715,   716,   717,   717,   717,   718,   718,
     718,   719,   719,   720,   720,   721,   721,   722,   723,   723,
     724,   724,   725,   725,   726,   727,   728,   728,   729,   729,
     729,   730,   731,   732,   733,   734,   734,   734,   734,   734,
     735,   735,   736,   737,   737,   738,   739,   739,   740,   740,
     741,   742,   741,   743,   743,   744,   746,   745,   747,   747,
     748,   748,   748,   749,   749,   749,   750,   750,   751,   751,
     751,   751,   751,   751,   751,   751,   751,   751,   751,   752,
     753,   753,   754,   754,   755,   755,   755,   755,   755,   755,
     755,   756,   756,   756,   757,   757,   758,   758,   758,   758,
     758,   758,   759,   760,   761,   760,   762,   763,   762,   764,
     764,   765,   765,   765,   766,   765,   765,   767,   768,   768,
     768,   769,   770,   770,   771,   771,   771,   771,   772,   772,
     772,   772,   772,   772,   772,   772,   772,   772,   772,   772,
     772,   773,   773,   774,   774,   775,   775,   775,   776,   776,
     777,   778,   778,   780,   779,   781,   782,   781,   783,   783,
     784,   784,   785,   785,   785,   785,   785,   785,   785,   785,
     785,   785,   785,   785,   785,   786,   787,   788,   788,   789,
     789,   790,   791,   792,   792,   793,   793,   793,   793,   793,
     793,   793,   793,   793,   793,   793,   793,   793,   793,   793,
     793,   793,   793,   793,   793,   793,   793,   793,   793,   793,
     793,   793,   793,   793,   793,   793,   793,   793,   793,   793,
     793,   794,   794,   795,   795,   796,   796,   797,   798,   798,
     799,   799,   799,   800,   800,   801,   801,   802,   802,   803,
     803,   804,   804,   805,   805,   806,   807,   807,   808,   808,
     809,   809,   810,   810,   811,   812,   813,   814,   815,   817,
     816,   818,   818,   819,   819,   820,   820,   821,   821,   822,
     823,   822,   824,   825,   824,   826,   827,   826,   828,   828,
     830,   829,   831,   831,   831,   832,   832,   832,   832,   833,
     834,   835,   835,   836,   837,   837,   837,   838,   838,   839,
     839,   839,   839,   839,   840,   841,   842,   843,   844,   845,
     845,   847,   846,   848,   848,   849,   849,   849,   849,   849,
     849,   849,   849,   849,   849,   849,   849,   849,   849,   849,
     849,   850,   851,   851,   851,   851,   851,   851,   851,   852,
     852,   852,   853,   853,   854,   855,   856,   856,   857,   857,
     858,   859,   860,   861,   861,   862,   863,   863,   864,   864,
     865,   865,   865,   866,   866,   867,   867,   868,   869,   870,
     871,   872,   871,   873,   873,   874,   874,   875,   876,   875,
     875,   877,   877,   878,   878,   878,   878,   878,   878,   878,
     878,   878,   878,   878,   878,   878,   878,   878,   878,   878,
     878,   878,   878,   878,   878,   878,   878,   878,   878,   878,
     878,   878,   878,   878,   878,   878,   878,   878,   878,   878,
     879,   879,   880,   880,   881,   881,   882,   882,   883,   883,
     883,   884,   884,   884,   885,   886,   887,   888,   889,   887,
     890,   887,   891,   892,   891,   893,   891,   894,   894,   895,
     896,   896,   896,   897,   897,   897,   897,   897,   897,   898,
     898,   899,   899,   899,   900,   901,   900,   902,   902,   903,
     903,   903,   903,   903,   905,   904,   906,   906,   907,   908,
     909,   909,   911,   912,   910,   914,   913,   913,   915,   915,
     915,   915,   915,   915,   915,   915,   915,   915,   915,   915,
     915,   915,   915,   915,   915,   915,   915,   915,   915,   915,
     915,   915,   915,   915,   915,   915,   915,   915,   915,   915,
     915,   915,   915,   915,   915,   915,   915,   915,   915,   915,
     915,   915,   915,   915,   915,   915,   915,   915,   915,   915,
     915,   915,   915,   915,   917,   916,   919,   918,   918,   918,
     918,   918,   918,   918,   918,   918,   918,   918,   918,   918,
     918,   918,   918,   918,   918,   918,   918,   920,   920,   921,
     921,   922,   922,   923,   923,   923,   923,   923,   924,   924,
     925,   925,   925,   926,   927,   927,   928,   929,   929,   929,
     929,   929,   929,   929,   929,   929,   929,   929,   929,   929,
     929,   929,   929,   929,   929,   929,   929,   929,   929,   929,
     929,   929,   929,   929,   929,   930,   930,   930,   931,   931,
     931,   932,   932,   933,   933,   935,   934,   936,   936,   936,
     937,   937,   938,   938,   940,   939,   941,   941,   942,   942,
     944,   943,   945,   945,   946,   947,   947,   949,   948,   951,
     950,   952,   952,   952,   952,   953,   953,   954,   955,   954,
     956,   956,   957,   957,   958,   958,   958,   958,   959,   959,
     959,   959,   959,   960,   960,   961,   961,   962,   962,   962,
     963,   963,   964,   964,   965,   965,   966,   967,   967,   969,
     968,   970,   970,   971,   971,   973,   972,   974,   974,   975,
     975,   975,   975,   975,   977,   976,   978,   979,   979,   980,
     981,   983,   982,   984,   984,   985,   985,   986,   986,   988,
     987,   989,   990,   990,   991,   991,   991,   991,   991,   993,
     992,   994,   994,   994,   994,   994,   995,   995,   996,   996,
     998,   997,   999,   999,  1000,  1000,  1001,  1001,  1001,  1001,
    1001,  1002,  1002,  1002,  1002,  1003,  1003,  1004,  1004,  1004,
    1004,  1004,  1004,  1004,  1004,  1004,  1004,  1004,  1004,  1004,
    1004,  1004,  1004,  1004,  1005,  1005,  1007,  1006,  1008,  1008,
    1008,  1008,  1008,  1009,  1009,  1011,  1010,  1013,  1012,  1014,
    1016,  1015,  1017,  1018,  1018,  1019,  1019,  1019,  1020,  1020,
    1021,  1021,  1022,  1023,  1024,  1024,  1025,  1025,  1026,  1026,
    1026,  1026,  1027,  1027,  1028,  1028,  1030,  1029,  1031,  1031,
    1031,  1031,  1031,  1031,  1031,  1032,  1032,  1034,  1033,  1035,
    1037,  1036,  1038,  1040,  1039,  1041,  1042,  1042,  1043,  1045,
    1044,  1046,  1046,  1046,  1047,  1047,  1049,  1048,  1050,  1051,
    1051,  1052,  1052,  1052,  1053,  1053,  1054,  1054,  1055,  1056,
    1056,  1056,  1056,  1056,  1056,  1056,  1057,  1057,  1059,  1058,
    1060,  1060,  1062,  1061,  1063,  1064,  1064,  1064,  1065,  1065,
    1065,  1065,  1067,  1066,  1068,  1069,  1070,  1070,  1071,  1071,
    1071,  1071,  1071,  1071,  1072,  1072,  1073,  1073,  1074,  1074,
    1074,  1074,  1074,  1075,  1076,  1076,  1076,  1076,  1076,  1077,
    1078,  1080,  1079,  1082,  1081,  1083,  1083,  1085,  1084,  1086,
    1086,  1087,  1087,  1089,  1088,  1090,  1090,  1091,  1092,  1092,
    1092,  1092,  1093,  1093,  1094,  1094,  1094,  1094,  1096,  1095,
    1097,  1098,  1097,  1097,  1099,  1099,  1100,  1100,  1101,  1101,
    1102,  1102,  1102,  1102,  1102,  1103,  1103,  1104,  1104,  1105,
    1105,  1106,  1108,  1107,  1110,  1109,  1111,  1112,  1112,  1113,
    1113,  1113,  1113,  1114,  1114,  1115,  1115,  1116,  1116,  1117,
    1118,  1118,  1118,  1119,  1119,  1120,  1120,  1120,  1121,  1121,
    1122,  1122,  1123,  1123,  1124,  1126,  1125,  1127,  1128,  1128,
    1129,  1129,  1129,  1130,  1130,  1131,  1132,  1132,  1133,  1134,
    1134,  1136,  1135,  1137,  1138,  1140,  1139,  1141,  1142,  1142,
    1144,  1143,  1145,  1146,  1146,  1147,  1147,  1148,  1148,  1149,
    1151,  1150,  1152,  1152,  1153,  1153,  1154,  1154,  1155,  1155,
    1156,  1157,  1157,  1159,  1158,  1160,  1160,  1161,  1161,  1162,
    1163,  1163,  1163,  1163,  1164,  1164,  1166,  1165,  1167,  1167,
    1167,  1167,  1167,  1167,  1167,  1168,  1168,  1169,  1169,  1170,
    1171,  1172,  1172,  1173,  1173,  1173,  1173,  1173,  1173,  1173,
    1173,  1174,  1174,  1175,  1176,  1176,  1177,  1178,  1178,  1179,
    1179,  1180,  1182,  1181,  1184,  1183,  1185,  1185,  1186,  1186,
    1187,  1187,  1188,  1188,  1189,  1189,  1189,  1190,  1190,  1190,
    1192,  1191,  1193,  1194,  1194,  1195,  1195,  1195,  1195,  1196,
    1196,  1196,  1196,  1196,  1196,  1197,  1198,  1198,  1199,  1199,
    1201,  1200,  1200,  1202,  1202,  1202,  1202,  1202,  1203,  1203,
    1204,  1204,  1204,  1204,  1206,  1205,  1207,  1208,  1208,  1209,
    1210,  1210,  1211,  1211,  1212,  1212,  1213,  1213,  1215,  1214,
    1216,  1216,  1216,  1217,  1217,  1218,  1219,  1219,  1221,  1220,
    1222,  1222,  1224,  1223,  1225,  1227,  1226,  1228,  1230,  1229,
    1231,  1232,  1232,  1233,  1233,  1234,  1235,  1235,  1236,  1237,
    1237,  1238,  1238,  1239,  1239,  1240,  1240,  1242,  1241,  1243,
    1243,  1243,  1243,  1243,  1244,  1245,  1245,  1246,  1246,  1246,
    1246,  1246,  1247,  1248,  1248,  1249,  1249,  1249,  1250,  1250,
    1250,  1250,  1251,  1252,  1252,  1253,  1254,  1255,  1255,  1257,
    1256,  1258,  1259,  1259,  1260,  1260,  1260,  1260,  1261,  1261,
    1262,  1262,  1262,  1263,  1263,  1264,  1264,  1264,  1265,  1265,
    1266,  1267,  1267,  1268,  1268,  1269,  1270,  1270,  1271,  1271,
    1271,  1272,  1272,  1273,  1274,  1274,  1275,  1276,  1276,  1276,
    1277,  1277,  1278,  1279,  1279,  1280,  1281,  1281,  1281,  1282,
    1282,  1283,  1284,  1284,  1285,  1286,  1286,  1287,  1287,  1288,
    1288,  1289,  1290,  1290,  1291,  1292,  1292,  1293,  1293,  1294,
    1295,  1295,  1296,  1297,  1297,  1298,  1298,  1299,  1299,  1300,
    1301,  1301,  1302,  1303,  1303,  1304,  1305,  1307,  1306,  1308,
    1308,  1308,  1309,  1309,  1309,  1309,  1309,  1309,  1309,  1309,
    1309,  1309,  1309,  1309,  1309,  1309,  1309,  1309,  1309,  1309,
    1309,  1309,  1309,  1309,  1309,  1309,  1309,  1310,  1310,  1311,
    1311,  1312,  1312,  1313,  1314,  1315,  1315,  1316,  1316,  1316,
    1317,  1317,  1317,  1318,  1318,  1318,  1319,  1319,  1320,  1320,
    1320,  1321,  1321,  1322,  1322,  1322,  1322,  1322,  1322,  1323,
    1323,  1324,  1325,  1326,  1326,  1327,  1328,  1328,  1329,  1330,
    1331,  1332,  1332,  1333,  1334,  1334,  1335,  1336,  1336,  1336,
    1337,  1338,  1338,  1339,  1340,  1341,  1341,  1342,  1343,  1343,
    1344,  1344,  1345,  1346,  1346,  1347,  1347,  1347,  1348,  1348,
    1349,  1349,  1350,  1350,  1351,  1351,  1352,  1352,  1352,  1352,
    1352,  1352,  1352,  1352,  1352,  1353,  1353,  1354,  1354,  1354,
    1355,  1355,  1355,  1355,  1355,  1355,  1355,  1356,  1356,  1357,
    1357,  1358,  1359,  1360,  1360,  1360,  1361,  1361,  1362,  1362,
    1363,  1363,  1363,  1364,  1364,  1364,  1365,  1365,  1365,  1366,
    1366,  1367,  1367,  1368,  1368,  1368,  1369,  1370,  1371,  1371,
    1372,  1373,  1374,  1375,  1376,  1376,  1376,  1376,  1377,  1378,
    1378,  1378,  1378,  1379,  1379,  1380,  1381,  1381,  1382,  1383,
    1384,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1386,  1386,
    1387,  1387,  1388,  1388,  1388,  1388,  1388,  1388,  1388,  1389,
    1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,
    1389,  1389,  1390,  1390,  1391,  1391,  1391,  1392,  1392,  1392,
    1392,  1393,  1393,  1393,  1394,  1394,  1394,  1395,  1395,  1395,
    1397,  1396,  1398,  1398,  1399,  1399,  1400,  1400,  1401,  1401,
    1402,  1403,  1403,  1404,  1404,  1405,  1405,  1406,  1406,  1407,
    1407,  1408,  1408,  1408,  1409,  1409,  1410,  1410,  1410,  1411,
    1411,  1412,  1412,  1413,  1413,  1413,  1413,  1413,  1413,  1413,
    1413,  1414,  1414,  1415,  1415,  1415,  1416,  1416,  1416,  1416,
    1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,
    1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,
    1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,
    1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,  1416,
    1416,  1416,  1416,  1416,  1416,  1417,  1417,  1417,  1417,  1417,
    1417,  1417,  1417,  1417,  1417,  1417,  1417,  1417,  1417,  1417,
    1417,  1417,  1417,  1417,  1417,  1417,  1418,  1418,  1419,  1419,
    1420,  1420,  1421,  1421,  1422,  1422,  1422,  1423,  1423,  1424,
    1424,  1425,  1425,  1426,  1426,  1427,  1427,  1428,  1428,  1429,
    1429,  1430,  1430,  1431,  1431,  1432,  1432,  1433,  1433,  1434,
    1434,  1435,  1435,  1436,  1436,  1437,  1437,  1438,  1438,  1438,
    1439,  1439,  1440,  1440,  1441,  1441,  1442,  1442,  1443,  1443,
    1443,  1444,  1444,  1445,  1445,  1445,  1446,  1446,  1447,  1447,
    1447,  1448,  1448,  1448,  1449,  1449,  1450,  1450,  1451,  1451,
    1452,  1452,  1453,  1453,  1453,  1454,  1454,  1455,  1455,  1456,
    1456,  1456,  1456,  1457,  1457,  1458,  1458,  1459,  1459,  1460,
    1460,  1461,  1461,  1461,  1462,  1462,  1463,  1463,  1464,  1464,
    1465,  1465,  1465,  1466,  1466,  1467,  1467,  1468,  1468,  1469,
    1469,  1470,  1470,  1471,  1471,  1472,  1472,  1473,  1473,  1474,
    1474,  1475,  1475,  1475,  1476,  1476,  1477,  1477,  1478,  1478,
    1479,  1479,  1480,  1480,  1481,  1481,  1482,  1482,  1483,  1483,
    1484,  1484,  1485,  1485,  1486,  1486,  1487,  1487,  1488,  1488,
    1489,  1489,  1490,  1490,  1491,  1491,  1492,  1492,  1493,  1493,
    1494,  1494,  1494,  1495,  1495,  1496,  1496,  1497,  1497,  1498,
    1498,  1499,  1499,  1500,  1500,  1501,  1501,  1502,  1502
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
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
       5,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     3,     0,     1,     1,
       1,     1,     0,     1,     1,     4,     1,     1,     1,     7,
       0,     4,     3,     3,     1,     4,     0,     1,     1,     0,
       5,     2,     2,     1,     0,     4,     5,     2,     3,     1,
       1,     3,     1,     2,     4,     4,     4,     1,     3,     4,
       4,     3,     1,     1,     3,     2,     2,     2,     0,     2,
       3,     1,     2,     1,     1,     5,     0,     1,     1,     1,
       0,     6,     1,     2,     2,     0,     2,     0,    10,     0,
       3,     0,     3,     0,     2,     2,     0,     5,     3,     1,
       1,     0,     2,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     5,     0,     1,     1,     4,     6,
       9,     0,     3,     0,     2,     0,     2,     3,     5,     5,
       1,     1,     1,     1,     3,     5,     0,     2,     1,     1,
       1,     4,     2,     2,     4,     1,     1,     1,     1,     1,
       1,     1,     4,     0,     2,     2,     2,     2,     1,     2,
       0,     0,     5,     0,     2,     2,     0,     5,     0,     2,
       4,     3,     4,     0,     1,     1,     1,     2,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,    11,
       0,     1,     1,     2,     4,     4,     4,     6,     4,     3,
       4,     0,     1,     1,     1,     2,     4,     4,     4,     4,
       4,     4,     6,     0,     0,     5,     0,     0,     2,     2,
       3,     1,     1,     1,     0,     4,     3,     2,     0,     1,
       1,     1,     0,     2,     1,     2,     2,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       5,     0,     2,     0,     4,     5,     0,     5,     2,     2,
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     3,     0,     2,     0,
       1,     2,     1,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     3,     6,     0,     2,
       7,     9,     8,     0,     2,     0,     2,     0,     2,     0,
       3,     0,     3,     0,     1,     1,     0,     5,     1,     1,
       0,     3,     1,     2,     1,     2,     2,     3,     1,     0,
       5,     1,     2,     1,     3,     0,     4,     2,     2,     0,
       0,     5,     0,     0,     5,     0,     0,     5,     0,     2,
       0,     6,     0,     2,     2,     2,     3,     1,     1,     2,
       2,     1,     2,     4,     1,     4,     2,     0,     2,     1,
       1,     1,     1,     1,     3,     4,     4,     4,     3,     0,
       2,     0,     5,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     2,     1,     2,     1,     1,     0,
       2,     2,     0,     2,     4,     4,     0,     3,     1,     1,
       3,     6,     2,     3,     2,     2,     3,     2,     1,     2,
       2,     1,     1,     1,     2,     2,     1,     4,     2,     3,
       0,     0,     5,     0,     1,     2,     3,     1,     0,     4,
       3,     0,     2,     2,     2,     1,     1,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     1,     5,     5,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     2,
       1,     2,     1,     2,     1,     1,     1,     1,     0,     1,
       1,     0,     1,     1,     3,     2,     0,     0,     0,     9,
       0,     4,     0,     0,     3,     0,     3,     1,     2,     4,
       0,     2,     2,     0,     3,     3,     4,     4,     3,     0,
       1,     0,     2,     2,     0,     0,     7,     0,     2,     1,
       1,     2,     1,     1,     0,     6,     0,     2,     2,     1,
       0,     1,     0,     0,     3,     0,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     0,     4,     0,     4,     3,     3,
       4,     3,     4,     3,     3,     4,     4,     3,     4,     3,
       4,     5,     3,     4,     3,     3,     3,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     2,     3,     1,     2,
       2,     2,     2,     3,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       1,     1,     1,     1,     4,     3,     1,     2,     1,     1,
       3,     3,     3,     3,     3,     2,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     0,     4,     4,     5,     6,
       0,     2,     0,     1,     0,     3,     3,     4,     0,     2,
       0,     3,     1,     2,     4,     0,     2,     0,     4,     0,
       6,     0,     1,     1,     1,     1,     1,     0,     0,     3,
       1,     2,     2,     3,     0,     2,     2,     2,     0,     3,
       2,     2,     4,     1,     1,     1,     1,     0,     2,     2,
       0,     1,     2,     2,     0,     1,     2,     0,     1,     0,
       3,     1,     2,     1,     1,     0,     3,     2,     3,     0,
       1,     3,     3,     2,     0,     4,     4,     0,     1,     1,
       1,     0,     4,     4,     2,     1,     2,     0,     1,     0,
       3,     3,     0,     3,     0,     2,     1,     2,     1,     0,
       4,     3,     3,     3,     3,     2,     2,     1,     1,     2,
       0,     3,     1,     1,     1,     2,     1,     2,     1,     1,
       2,     2,     2,     2,     2,     1,     1,     1,     2,     2,
       1,     1,     2,     2,     1,     1,     1,     1,     3,     1,
       3,     3,     3,     3,     0,     1,     0,     4,     4,     6,
       6,     8,     8,     0,     1,     0,     3,     0,     3,     2,
       0,     4,     2,     1,     3,     1,     1,     1,     2,     1,
       1,     2,     2,     3,     2,     3,     1,     3,     2,     1,
       1,     1,     0,     2,     0,     1,     0,     3,     0,     2,
       1,     2,     1,     1,     1,     0,     2,     0,     3,     1,
       0,     3,     1,     0,     3,     3,     0,     3,     2,     0,
       6,     3,     2,     1,     0,     1,     0,     3,     5,     0,
       2,     0,     3,     3,     0,     2,     1,     2,     4,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     0,     3,
       1,     2,     0,     3,     2,     1,     1,     1,     2,     1,
       1,     1,     0,     3,     2,     5,     1,     2,     2,     2,
       1,     1,     1,     2,     1,     2,     4,     2,     0,     1,
       1,     1,     1,     4,     0,     1,     1,     2,     2,     3,
       3,     0,     3,     0,     3,     3,     4,     0,     4,     4,
       6,     0,     1,     0,     3,     1,     2,     5,     1,     1,
       1,     1,     0,     3,     0,     3,     2,     1,     0,     3,
       2,     0,     4,     2,     0,     1,     1,     1,     1,     3,
       0,     2,     1,     3,     3,     0,     3,     1,     1,     1,
       3,     7,     0,     3,     0,     4,     7,     0,     2,     0,
       1,     2,     1,     2,     3,     3,     1,     0,     1,     1,
       4,     4,     2,     0,     1,     1,     3,     2,     0,     3,
       1,     1,     0,     1,     1,     0,     4,     5,     1,     1,
       0,     2,     2,     0,     1,     2,     0,     1,     2,     0,
       1,     0,     3,     2,     1,     0,     4,     4,     0,     1,
       0,     4,     5,     0,     1,     2,     3,     0,     1,     1,
       0,     4,     4,     6,     0,     2,     0,     2,     1,     2,
       3,     0,     1,     0,     3,     2,     5,     0,     1,     2,
       2,     2,     2,     2,     0,     2,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       3,     1,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     4,     3,     4,     1,     2,     3,     1,     2,     3,
       3,     4,     0,     3,     0,     7,     0,     5,     0,     2,
       0,     2,     0,     3,     0,     2,     4,     0,     2,     4,
       0,     4,     4,     0,     3,     0,     4,     1,     1,     1,
       2,     2,     2,     2,     1,     1,     2,     1,     0,     1,
       0,     4,     2,     0,     2,     1,     4,     4,     0,     1,
       1,     1,     1,     1,     0,     4,     5,     1,     2,     2,
       0,     3,     1,     1,     0,     4,     0,     1,     0,     4,
       4,     6,     6,     0,     1,     2,     0,     1,     0,     3,
       1,     2,     0,     3,     5,     0,     3,     2,     0,     4,
       6,     0,     3,     1,     3,     2,     2,     2,     3,     0,
       3,     0,     3,     0,     3,     0,     1,     0,     3,     1,
       1,     1,     1,     1,     7,     0,     1,     1,     1,     1,
       1,     1,     4,     1,     2,     1,     2,     3,     0,     1,
       2,     1,     3,     1,     1,     4,     1,     1,     1,     0,
       4,     6,     0,     2,     0,     4,     3,     3,     1,     1,
       0,     1,     1,     0,     1,     0,     2,     2,     0,     1,
       2,     1,     1,     0,     1,     2,     1,     1,     0,     2,
       2,     0,     1,     2,     0,     1,     2,     0,     2,     2,
       0,     1,     2,     0,     1,     2,     0,     2,     2,     0,
       1,     2,     0,     1,     2,     2,     2,     2,     2,     0,
       1,     2,     0,     1,     2,     2,     2,     0,     1,     2,
       0,     1,     2,     0,     1,     2,     2,     0,     1,     2,
       0,     1,     2,     0,     2,     1,     1,     0,     2,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     3,     0,     1,     1,
       3,     3,     1,     3,     3,     1,     3,     1,     2,     2,
       1,     3,     1,     1,     3,     1,     3,     1,     3,     1,
       2,     2,     1,     1,     2,     1,     1,     2,     1,     1,
       1,     1,     2,     1,     0,     2,     1,     1,     1,     3,
       1,     1,     2,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     3,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     2,     4,     3,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     1,     3,     2,     2,     1,     1,     3,
       2,     2,     1,     1,     3,     3,     4,     5,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     2,
       5,     5,     5,     4,     4,     5,     5,     5,     5,     5,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     5,     0,     3,     2,     1,     3,     3,
       0,     2,     1,     3,     1,     3,     1,     3,     1,     3,
       0,     0,     1,     0,     1,     0,     1,     0,     2,     0,
       2,     0,     1,     1,     0,     1,     0,     1,     2,     0,
       2,     0,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     2,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     2,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       1,     0,     1,     0,     1,     1,     0,     1,     0,     1,
       1,     0,     2,     2,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     1,     0,     1,     0,     1,     0,
       2,     1,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     2,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     1,     0,     1,     0,     3,     0,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     2,     1,
       3,     2,     1,     1,     1,     2,     1,     2,     1,     2,
       1,     2,     1,     2,     1,     2,     1,     2,     2
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,    10,     1,     0,     0,     3,    21,     6,     4,
      46,     8,     9,     0,     0,     0,     7,     0,    11,   299,
      49,    27,    24,    46,    46,    23,    22,     0,     0,   760,
     301,     0,   180,    51,     0,     0,    14,     0,    47,     0,
       0,    20,   805,     0,   303,     0,     0,    45,   182,     0,
       0,    98,    52,    53,     0,     0,     0,    12,    15,    16,
       0,    13,   300,   762,     0,     0,     0,   297,    50,     0,
       0,   186,    62,    56,     0,   100,    54,    55,    30,    29,
      33,    33,    32,    31,     0,    17,     0,   765,   763,   781,
       0,   864,   945,   954,   960,   967,  1009,  1015,  1029,  1024,
    1030,  1031,  1039,  1049,  1096,  1105,  1107,  1110,  1136,  1147,
    1150,  1153,  1145,  1159,  1166,  1188,  1192,  1231,  1233,  1237,
       0,  1243,  1258,  1284,  1314,  1315,  1331,  1334,  1335,  1340,
    1349,  1350,  1376,  1412,  1430,     0,  1464,  1478,  1486,  1488,
     787,  1492,  1495,  1498,  1549,  1282,  1363,   807,   808,   809,
     810,   811,   812,   813,   814,   816,   815,   817,   818,   819,
     820,   821,   822,   823,   824,   825,   826,   827,   828,   829,
     830,   831,   832,   833,   834,   835,   836,   837,   838,   839,
     840,   841,   842,   843,   844,   845,   846,   847,   848,   849,
     850,   851,   852,   853,   854,   855,   856,   857,   858,   859,
     860,   861,   806,   302,   309,   310,   423,   304,   426,     0,
     181,   183,   184,    64,    58,    99,     0,     0,     0,  2086,
    2036,  2036,  2036,     0,     0,  2036,  2009,   118,    84,   101,
       0,   104,   106,   107,   108,   154,   110,   109,   111,   112,
     113,   114,   115,   116,   117,     0,     0,    25,    18,    19,
     770,   770,     0,     0,  1916,  1917,  1918,  1919,  1920,  1921,
    1922,  1923,  1924,  1925,  1926,  1927,  1928,  1929,  1965,  1966,
    1967,  1968,  1969,  1970,  1971,  1972,  1973,  1974,  1975,  1976,
    1977,  1978,  1979,  1980,  1981,  1982,  1983,  1984,  1985,  1930,
    1931,  1932,  1933,  1934,  1935,  1936,  1937,  1938,  1939,  1940,
    1941,  1942,  1943,  1944,  1945,  1946,  1947,  1948,  1949,  1950,
    1951,  1952,  1953,  1954,  1955,  1956,  1957,  1958,  1959,  1960,
    1913,  1961,  1962,  1963,  1964,   863,  1914,  1915,     0,     0,
       0,     0,   971,     0,     0,     0,     0,  1044,     0,     0,
    1044,     0,  1637,  1138,     0,     0,  2109,   994,   993,     0,
    1158,  1637,     0,     0,     0,     0,     0,     0,   862,     0,
    1270,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1460,  1463,  1450,  1461,  1462,  1452,     0,     0,  1487,  1485,
       0,   805,     0,     0,     0,     0,     0,     0,     0,   370,
     305,  1880,     0,  1708,   306,     0,  1896,   278,   187,  2008,
       0,     0,     0,  2036,  2152,    82,    63,  2007,    68,    70,
      71,    72,    73,  2007,     0,  2036,    57,    60,  1731,  1730,
     129,  2036,  2036,  2087,  2036,  2037,     0,     0,     0,  2036,
    2036,     0,  2010,     0,  2036,     0,    48,     0,   102,   105,
       0,   153,    34,    28,  2036,  2006,   770,   767,   773,     0,
     770,   782,   783,   757,   888,  1813,   943,   866,  2056,   887,
    1803,  1807,  2065,     0,  1857,     0,  1852,  1858,     0,     0,
    1864,  1836,     0,     0,  1693,  1695,  1832,     0,     0,     0,
    1855,  1837,  1754,     0,  1697,  1835,  1856,  1833,  1859,  1860,
    1838,     0,  1854,  1864,  1853,  1813,  1834,   952,  1748,   950,
    1740,  1743,  1742,  1746,  1828,  1830,  1747,  1861,     0,     0,
       0,     0,     0,     0,   955,     0,  1682,  1685,  1687,  1690,
    1763,  1692,  1885,  1761,  1762,  1720,   961,   962,     0,  1716,
    1718,  1717,   974,   972,   973,  1007,     0,  1779,  1014,  1010,
    1011,  1013,  1778,  1016,  1019,  2065,  1027,     0,  1699,  1899,
    1735,  1808,  1812,  1736,     0,  1037,  2079,  2103,     0,  1046,
    1048,  1040,     0,  1832,  1063,  1094,  1578,  1738,  1058,  1060,
    1057,     0,  1742,  1103,     0,  1106,   977,  1108,  1117,  1116,
    1134,     0,  1113,  1115,  1636,     0,  1140,  1144,  1142,  1145,
    1143,  1137,  1148,  1149,  1733,  1151,  1152,  2110,  1154,  1714,
    1146,  2105,  1635,  1167,  1169,  1710,  1189,  1190,  1193,     0,
    1195,  1196,  1197,  1232,  1416,  1797,  1798,     0,  1234,     0,
    1241,     0,  1251,  1248,  1250,  1249,  1244,  1245,  1252,  2065,
    1272,     0,     0,  1720,  2119,  1784,  1259,  1270,  1261,     0,
    1268,     0,  1783,  1717,   444,  1785,     0,  1312,  1891,  1709,
    1329,     0,  1332,  1552,  1702,  1338,  2079,     0,  1347,  1703,
    1552,     0,  1361,  1354,  1705,     0,     0,  1713,  1377,  1378,
    1379,  1380,  1381,  1382,  1404,  1383,  1407,  1384,     0,  1711,
       0,     0,  1796,  1812,  1413,  1448,  1435,  1453,  1476,     0,
    1467,  1470,     0,  1483,     0,  1489,  1490,   793,   799,   788,
     789,   790,   792,     0,  1493,     0,  1800,  1496,  2081,  1515,
    1501,  1563,  1552,  1283,  1364,  1367,     0,     0,   589,     0,
       0,     0,   428,     0,     0,   432,   433,   431,     0,   308,
     311,   185,     0,  1897,     0,   290,   286,   179,     0,   281,
     283,   284,  2151,  2036,     0,     0,    67,    69,    65,    83,
    2007,  2036,     0,     0,     0,  2036,     0,     0,     0,   175,
    1723,   173,   178,     0,     0,   177,  1732,   156,   157,  2038,
     160,  1818,  1386,  1385,   119,   123,   126,  2069,  2036,     0,
      85,   103,   155,     0,     0,   768,  2036,     0,   779,   771,
     772,   784,  2130,  2131,     0,   944,   865,   889,  2057,     0,
       0,     0,  1805,  1806,  2066,     0,  1829,     0,     0,     0,
       0,  1850,  1870,  1749,  1750,  1751,     0,     0,     0,     0,
       0,     0,     0,     0,  1851,   953,   946,     0,     0,  1741,
       0,     0,  1839,     0,     0,  1764,  1765,  1766,  1689,  1760,
       0,  1688,  1887,     0,     0,     0,     0,     0,  1886,   958,
     963,   965,     0,  1008,   968,  1781,   976,   969,   975,  1780,
    1782,  1012,  1019,  2142,  2143,  1017,     0,  1020,     0,  1028,
    1025,  2127,  2126,  1700,     0,  1901,  1701,  1810,  1811,  1034,
    1035,  1038,  1032,  2080,  1297,  2104,  1045,  1047,  1042,  1095,
    1050,   802,   802,  1055,  1584,  1581,  1059,  1056,  1739,  2118,
    1578,  1578,  1578,  1578,  1104,  1097,     0,     0,   978,  1109,
    1135,  1111,  1637,  1637,  1112,  1119,  1120,   802,  1662,  1663,
    1664,  1658,  1643,  2109,  1650,  1670,  1673,  1672,  1674,  1666,
    1657,  1656,  1661,  1660,  1659,  1665,  1645,  1649,  1667,  1669,
    1671,  1647,  1648,  1644,  1646,  1638,  1639,  1651,  1652,  1653,
    1654,  1655,  1642,  1141,  1139,  1734,  1156,  2106,   802,  1171,
       0,  1191,     0,  1218,  1202,  1194,  1199,  1200,  1201,  1420,
       0,  1799,     0,     0,  1242,  1238,     0,  1246,  2118,  1297,
       0,     0,   453,   449,   452,   451,   450,   541,   543,   465,
     461,   463,   464,   466,   462,   467,   544,   542,   468,   469,
     446,   457,   458,   459,   454,   455,   456,   448,   445,  1260,
    1266,  1267,   802,  1263,  1637,     0,     0,  1271,     0,  1313,
    1285,  1892,  1893,  2079,  1330,  1316,  1318,  1319,     0,     0,
    1333,  1339,  1336,  1287,  1704,  1348,  1341,  1297,  1356,  1362,
    1351,     0,  1356,     0,  1771,  1773,  1774,  1775,     0,  1405,
    1408,     0,     0,  1712,  1388,     0,  1387,     0,     0,  1810,
    1449,  1431,  1437,  2036,  1438,  1433,     0,  1451,  1455,     0,
    1477,  1465,     0,  1468,  2005,  1469,     0,  1484,  1479,     0,
    1491,   800,   798,   791,     0,  2082,  2083,  1497,  1516,  1499,
    2005,     0,  1564,  1550,  1554,     0,  2118,  1365,   424,     0,
       0,   592,   441,   473,   476,     0,     0,   429,     0,   439,
     434,   440,   437,  2036,  1898,   188,  2017,   287,   288,   289,
    1992,     0,   279,   282,     0,  2150,    76,    66,     0,  1724,
      75,    59,     0,     0,  1825,  1821,  1826,  1824,  1822,  1827,
    1823,   164,   165,   167,   176,   171,   169,     0,   158,  2040,
    2039,   161,     0,  2069,  2072,  2071,     0,     0,   120,   124,
      87,    26,    37,    40,    44,    43,  2077,    38,    39,     0,
    2036,   780,     0,     0,   758,  1814,  2000,  2002,   894,  2036,
    1565,   890,   891,   893,   895,     0,     0,     0,   886,   882,
    1565,  2125,  2124,   879,   871,   873,   874,     0,  1565,     0,
       0,     0,   898,   877,     0,   885,   868,   884,   869,  1677,
    1675,     0,  1804,  1768,  1767,     0,  1753,     0,  1677,  1675,
       0,  1677,     0,  1866,  1677,     0,     0,  1694,  1696,  1677,
       0,     0,     0,  1677,  1757,  1758,  1759,     0,  1698,  1677,
       0,  2065,  1587,   951,  1812,  1736,     0,  1831,     0,     0,
    1677,  1691,  1889,   958,  1681,  1680,  1684,  1683,  1686,     0,
     956,     0,     0,  1719,   977,  1018,  1023,     0,  2022,     0,
    1737,  1587,  2036,  1900,  1809,  1036,  2021,  1623,  1298,  1299,
    1041,     0,  1583,   803,  1586,  1579,  1585,  1580,  1582,     0,
    1069,  1068,  1061,  1064,  1066,     0,  1053,  1054,  1051,  1052,
       0,  1587,     0,   984,  1114,  1129,  1131,  1130,  1124,  1126,
    1132,  1637,  1121,  1118,  1637,  1122,  1668,  1640,  1641,  2067,
    1155,  1715,   802,  1163,  1164,  2109,  1179,  1180,  1182,  1184,
    1185,  1181,  1183,  1174,  2109,  1170,     0,  1219,     0,  1221,
    1220,  1222,  1204,  1214,     0,     0,  1198,  2149,  2068,     0,
    1422,     0,  2027,     0,  1235,  1587,     0,     0,     0,   447,
     460,  1264,  1277,  1273,  1278,  1274,  1279,     0,  1269,  1559,
    1558,  1276,  1287,     0,  1553,  1793,  1794,  1795,     0,     0,
    1343,   802,     0,  1355,     0,     0,     0,     0,  1406,     0,
    1410,  1409,  1402,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1390,  1391,  1894,  1623,     0,  1454,  2097,  2097,
    1474,     0,     0,  1587,     0,     0,   801,     0,  1881,     0,
    1474,  1297,  1986,  1369,  1554,     0,   426,   371,     0,     0,
     595,     0,   499,     0,   430,   436,   480,   442,  2011,  2036,
       0,     0,  2036,  2011,  2058,  2036,  1990,   307,     0,   312,
     315,   316,   317,   318,   319,   320,   321,   322,   323,     0,
       0,   190,  2018,  2101,  1993,  2021,   280,     0,    79,    81,
      80,    77,    78,    61,   135,   134,   149,   145,   150,   131,
     148,   146,   132,   133,   147,   130,   136,   137,   139,   166,
       0,   170,     0,   174,  1819,   159,   162,     0,  2070,   127,
     121,   122,   125,     0,     0,    86,     0,    90,    42,  2078,
      36,    41,   774,   775,   778,     0,   769,   785,   787,  1788,
     902,  1786,  1787,     0,  1571,  1572,  1576,  1577,   867,  1573,
     802,  1568,   802,   892,  2123,  2122,  2060,  2060,   900,   901,
    2060,     0,   907,  2036,   909,   910,   911,   942,  2036,   912,
     913,   914,   915,   916,     0,   936,   937,   918,   920,     0,
     921,   940,   938,   939,     0,   923,  2036,   908,  1988,   926,
     941,   929,   896,   917,   922,   928,   883,   870,   872,  1565,
     880,   875,   876,   899,   878,  1678,  1679,  1815,     0,     0,
       0,  1770,  1752,  1769,  1899,     0,  1861,     0,  1861,  1865,
    1843,  1871,     0,  1861,  1861,  1861,     0,  1844,     0,  1861,
       0,   802,   802,   947,  1593,  1590,  1810,  1811,  1587,     0,
    1861,  1861,     0,  1888,   957,   959,   966,   964,   988,  1022,
    1021,  1026,     0,  1302,     0,   802,   802,  1033,  1624,  1630,
    1627,     0,   805,  1075,  1076,  1073,  1072,  1074,  1071,  1065,
    2036,  1077,     0,  1080,  1081,  2015,  2036,  1084,  1085,  1067,
    1086,     0,  2036,  1089,  1087,  1070,     0,  1098,     0,   979,
     980,   773,     0,  1637,  1637,  1128,   802,  1125,     0,  1162,
     802,  1165,  1160,     0,     0,  1186,     0,     0,     0,  1215,
    1217,     0,  1210,  1224,  1211,  1212,  1203,  1206,  1224,     0,
    1773,  2148,     0,  2121,  1414,  2036,   568,   569,  2041,     0,
    2028,  1421,  1236,  1239,     0,  2073,  2073,     0,  1253,  1254,
    1706,  1265,  1262,     0,     0,  1289,  1320,  1288,   802,   802,
    1337,  1612,     0,  1623,  1344,     0,  1357,  1637,  1637,  1352,
    1358,  1389,  1411,  1401,  1403,  1393,  1394,  1395,  1399,  1396,
    1400,  1397,  1398,  1392,  1895,  1447,     0,  1444,  1445,  1439,
       0,  1432,  2147,  2146,     0,  2098,  1458,  1458,  1596,     0,
    1472,  1471,  1473,  1899,  1480,     0,   794,     0,  1882,  1502,
    1503,     0,  1506,  1509,  1513,  1507,  1343,  1987,     0,  1374,
    1373,  1372,  1371,  1370,   425,   373,   590,     0,     0,   690,
    2038,   474,     0,   500,     0,   471,  2036,  1997,     0,  2012,
       0,     0,  2036,  1990,     0,     0,     0,     0,     0,  2059,
    2036,   366,  1991,   367,     0,     0,   368,   313,   314,  2079,
    2102,  2011,     0,  2138,  2139,    74,   138,   141,     0,   168,
       0,   163,   128,     0,    96,    33,     0,    33,     0,    88,
      91,   776,   777,   787,   805,   906,  1566,  1574,  1570,  1567,
    1569,  1575,  2061,     0,     0,     0,  1820,   897,  1789,  1790,
       0,     0,   935,   927,  2036,  2036,  1633,  1633,     0,  1989,
       0,   881,  1676,  1816,     0,  1587,  1877,  1848,  1879,  1849,
    1875,  1845,  1846,  1847,  1873,  1868,  1869,  1842,  1737,  1595,
    1592,  1588,  1594,  1589,  1591,  1809,   948,  1862,     0,  1840,
    1841,  1890,  1776,  1777,   997,  2034,  1903,  1904,  1905,  1906,
    1907,  1908,  1909,  1910,  1902,     0,  1629,  1632,  1625,  1631,
    1626,  1628,  1043,     0,     0,  1078,  1079,  2065,   740,   742,
    1082,  1083,     0,     0,  1633,  1633,     0,  1587,  1699,  1587,
    1699,   981,   982,     0,   986,   985,   987,  1127,  1133,  1123,
    1157,  1161,  1172,  1175,  1176,  2013,  1168,  2109,  1173,  1224,
    1772,  1224,     0,  2032,  2032,  1209,  1225,  1226,  1207,  1213,
    1208,  2120,  1424,     0,  2042,  1418,  2029,  1587,  2074,   275,
     276,   277,  1257,  1247,  1707,     0,  1280,     0,  2067,     0,
    1308,  1290,  1303,  1296,  1292,  1305,     0,   802,   802,  1317,
    1326,  1323,  1611,  1614,  1605,  1613,  1606,  1342,  1345,     0,
     802,   802,  1359,  1446,  1436,  1440,  1441,  1442,  1443,  1434,
    1456,  1459,  1457,   802,   802,  1466,  1602,  1599,  2036,  1587,
    1587,   796,  1494,  1881,  1505,  2025,  1511,  2025,  1596,  1560,
    1557,  1556,  2048,  2046,  1366,   372,   426,   593,     0,     0,
     298,     0,   477,   501,     0,   470,     0,   578,   505,  2088,
    2088,  2088,  2088,  2088,  2114,   506,   509,   510,   511,   512,
     513,   514,   537,   535,   536,   538,   539,   515,  2084,   540,
     557,   516,   502,   517,   518,     0,  2091,   520,   521,   519,
    2043,   523,   524,   522,  2036,   481,   482,   483,   484,   485,
     486,   503,   507,   508,   487,   488,   489,   490,   491,   492,
     493,   494,     0,     0,  1998,     0,   475,     0,   443,   335,
     244,   363,  2140,  2141,  1727,   344,  1725,  2133,  2132,   337,
    1729,  1728,  2054,  2009,  2025,     0,  2036,   341,   340,  2036,
     369,  2058,  2079,  2111,   260,     0,  2036,  2007,  2041,   262,
       0,  2118,   248,   189,   247,   191,   192,   193,   194,   195,
     196,     0,   197,     0,   198,   259,   199,   200,   201,   202,
     203,   204,  2003,  2036,     0,   285,     0,   140,   172,    92,
      93,    97,    94,    95,    89,   805,   903,   905,   904,   931,
     930,     0,     0,   933,     0,  1791,  1792,   932,   925,   934,
    1817,   949,  1863,   802,   802,   802,   970,  1004,  1000,  2065,
    2035,   991,   996,   995,   990,     0,  1301,  1300,  1091,  2016,
     741,   743,  1090,  1093,  1092,  1088,  1100,     0,  1099,     0,
     983,  1745,  1744,  1802,  1177,  2014,     0,     0,  1205,  1216,
    1224,  2033,     0,     0,  1227,  1228,     0,     0,  1427,  1423,
    1417,  1240,  1256,     0,     0,     0,  1293,  2036,  1623,  1291,
    1304,     0,     0,     0,  1307,  1328,  1325,  1321,  1327,  1322,
    1324,  1346,  1353,  1360,  1604,  1601,  1597,  1603,  1598,  1600,
       0,  1482,  1481,  1517,   795,     0,  1504,  2026,     0,  2025,
    1508,     0,  1500,   802,   802,  1551,  1562,  1620,  1617,  1561,
    2049,  2050,  1555,  2047,  1375,     0,   374,   426,   591,   426,
     596,     0,   583,   579,   581,   472,   587,   588,  2089,   534,
     533,   526,   525,   532,   531,   530,   529,   528,   527,  2115,
       0,  2085,   575,   561,     0,   553,   495,  2092,  2044,  2045,
     576,     0,   497,  1911,  1911,   479,   478,     0,   325,     0,
     362,  1726,  2055,   346,     0,   328,  2093,   355,   357,   361,
     360,   356,   358,   354,   359,     0,     0,  2036,  2041,  2112,
    2113,   227,   263,  2079,  2036,  2036,  2036,  2036,   272,  1994,
     273,     0,  2036,  2058,  2004,     0,     0,   291,   292,   295,
     142,   143,     0,   919,   924,  2144,  2145,  1634,  1002,  1006,
    1003,   998,  1005,   999,  1001,     0,   989,  1587,  1587,     0,
    1187,  1223,  1230,  1229,  2036,  1425,     0,     0,  1415,  1419,
    1255,     0,  1295,     0,  1286,  1311,  1612,  1609,  1310,  1294,
    1306,  1475,  1525,   797,  1510,     0,  1514,  1619,  1622,  1615,
    1621,  1616,  1618,   376,   375,   594,   598,   691,     0,   585,
     582,   577,  2025,   555,  2107,   558,  2107,   504,     0,   496,
    2007,   545,   546,   336,   327,   326,   324,   364,  1721,   345,
    2009,  2094,   333,   342,   339,   343,   338,     0,  2036,   229,
     228,   225,   262,   258,     0,     0,     0,     0,  1995,  1996,
     271,   274,     0,  2036,   261,   243,   293,     0,   294,     0,
       0,   992,  1102,  1101,  1178,     0,  1428,  2036,  1637,  1309,
    1607,  1608,  1610,  2000,  1548,  1547,  1526,  1518,  1519,  1988,
    1520,  1521,  1522,  1523,  1546,     0,     0,  1512,   378,   597,
     693,   584,     0,   580,     0,     0,     0,   553,  2108,     0,
     554,   559,   498,  1912,  1722,     0,     0,   347,   348,   349,
     350,     0,   329,  2024,   335,     0,   237,   238,   236,   235,
       0,   218,   219,   220,   214,   215,   232,   221,   222,   232,
       0,   223,   224,   213,   211,   212,   217,   216,   232,   232,
     232,     0,   264,   265,   266,   267,   270,   245,     0,   296,
     144,   786,  1426,     0,  1281,     0,  2095,     0,  2067,  2021,
       0,   599,     0,   697,   692,   694,     0,     0,  2036,   562,
     556,     0,   563,  2067,  2067,   566,   353,   352,  1999,  2009,
     334,  1883,   233,   208,   234,   209,  2017,   210,   206,   207,
     230,   205,   231,  2036,     0,   254,   253,   254,   250,  1429,
       0,  2096,     0,  1544,  1543,  1542,     0,   377,   379,  2032,
     600,     0,   698,     0,   695,  2117,     0,   564,   566,     0,
       0,   570,   565,     0,   331,   240,  1884,   226,     0,   268,
       0,   252,   251,  1545,  2129,  2128,  2075,  1538,  1532,  1533,
    1535,   400,     0,     0,   700,   701,   696,   586,   570,   566,
     560,  2005,   550,  2041,   351,  2067,   330,     0,   239,   269,
       0,   257,  2076,  2067,  1541,  1536,  1539,     0,  1534,  2036,
       0,     0,     0,     0,   381,   401,   402,   383,   411,     0,
    2036,  2090,   552,   570,     0,  2036,     0,     0,     0,     0,
    1540,  1537,  2036,     0,     0,  2036,  2036,  2036,  2036,   403,
       0,  2057,     0,  2100,     0,   380,   384,   386,   385,     0,
       0,     0,     0,     0,     0,     0,   382,   412,   414,   413,
       0,     0,   604,  2036,  2036,  1990,  2051,   629,   603,   607,
     608,     0,  2019,   716,  2036,   705,  2114,   706,  2015,  2036,
       0,   721,   714,   709,   715,  2060,   710,     0,     0,   713,
     723,   720,   718,   717,     0,   724,   712,     0,   735,   729,
     733,   732,   730,   734,   702,   736,   731,   719,   711,     0,
    2060,   551,   574,   571,   572,     0,   332,     0,   151,   152,
     242,     0,  2136,  2137,   255,  1531,  1528,  1530,  1529,  1524,
    1527,     0,     0,   409,     0,     0,     0,     0,  2036,  2036,
    2036,  2036,  2036,   387,     0,  2036,  2036,  2036,  2036,  2036,
    2036,  2036,  2036,  2036,  2036,  2036,   415,     0,  2036,     0,
    2167,  2168,  2036,  1990,     0,   601,   605,  2020,   609,     0,
       0,   703,   704,   707,   708,     0,   738,  2036,  2107,  2036,
     739,   737,   755,  2036,   573,   567,   241,   256,   404,  2107,
     408,   406,   410,   405,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   606,  2052,  2053,   617,   614,   438,   630,
     610,   611,   728,   727,   748,   754,     0,   751,   570,   396,
     392,   393,   397,   395,     0,   398,   388,   394,   389,   390,
     391,   420,   416,   417,   421,   419,     0,   418,   613,  2134,
    2135,   616,   631,   440,   612,   746,   744,   747,   745,   749,
     750,     0,   722,   752,   753,     0,   407,     0,     0,     0,
    2036,  2036,     0,   618,   619,   620,   621,   622,   623,     0,
     633,   725,   726,     0,   422,  2154,  2153,  2036,     0,     0,
    2156,     0,  2036,  2036,   615,  2090,     0,     0,   628,   624,
    2155,     0,     0,  2030,  2062,  1990,     0,     0,     0,  2036,
    2065,   632,  2036,  2036,     0,   638,   640,   649,   641,   643,
     646,   634,   635,   636,   645,   647,   650,   637,     0,   642,
       0,   644,   648,   639,  2062,  1990,     0,   625,   627,   626,
    2031,   688,  2063,  2064,  2038,   674,  2036,   553,  1637,     0,
       0,     0,     0,     0,   682,     0,   672,   678,   681,     0,
     675,   683,   686,  2038,   677,     0,   673,     0,  2107,   670,
    1899,   666,  1755,  2158,     0,     0,  2160,  2162,     0,  2166,
    2164,   651,   655,   659,   659,   653,   657,   652,   658,   689,
       0,   680,   679,   685,   684,   676,     0,   664,   559,   687,
    2067,   665,  1756,  2157,  2161,  2159,  2165,  2163,   662,   654,
     662,   656,     0,   399,   548,     0,     0,   661,   660,     0,
       0,   547,   669,   667,   668,   663,   671,   549
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     6,     7,     8,     9,    10,    11,    12,
      57,    58,    59,    61,    18,    13,    23,    35,   444,    24,
      34,    80,    84,   246,   783,  1166,  1167,  1168,    19,    20,
      32,    33,    51,    52,   214,   416,   753,    53,   213,   406,
     407,   408,   409,   410,   411,   412,  1471,   413,   436,  1160,
    1505,  1506,  1507,  1846,    75,   228,   229,   230,   231,   232,
     434,   774,  1501,   775,   776,   233,   755,  1485,  1486,  1487,
    2186,  2400,  1488,  2830,   234,   441,   235,   767,   768,   769,
    1495,   236,  1141,  1142,   237,   238,  1491,   239,   240,   241,
     242,   243,   244,    47,    48,    71,   397,   212,   398,  1461,
    1829,  2165,  2166,  2588,  2589,  2590,  2591,  2491,  2641,  2633,
    2167,  2569,  2168,  2708,  2169,  2131,  2170,  2171,  2172,  2173,
    2648,  2681,  2174,  2175,  2176,  2177,  2178,  2594,  2179,  2180,
    2389,  2181,  1718,   737,   738,   739,   740,  1120,   741,  1116,
    2397,  2398,  2508,    29,   206,    30,    44,    67,   207,   208,
     730,   209,  1113,  1449,  1450,  2476,  1451,  2706,  2564,  2358,
    1452,  1453,  2149,  2484,  1454,  1455,  2479,  2557,  2558,  2559,
    2560,  1456,  2373,  2374,  1457,  2360,  1458,  1459,  1825,   718,
    1795,  2055,  2316,  2317,  2538,  2609,  2658,  2755,  2756,  2757,
    2758,  2724,  2725,  2726,  2766,  2767,  2768,  2769,   389,  1426,
     390,   391,   722,   723,  1436,   724,  1110,  1111,  1807,   635,
    1000,  1001,  1002,  1003,  1004,   725,  2065,   726,  1431,   727,
    1432,  2126,  1806,  2105,  2106,  2107,  2469,  1802,  1803,  2109,
    2110,  2111,  1005,  1006,  2114,  3027,  3131,  2115,  2466,  2547,
    2344,  2625,  2463,  2668,  2671,  2672,  1708,  2702,  2823,  2824,
    2116,  2117,  2118,  2119,  1801,  2459,  2323,  2324,  2543,  2121,
    1101,  2056,  1430,  2319,  1799,  2456,  2539,  2611,  2693,  2730,
    2778,  2779,  2878,  2930,  2780,  2926,  2958,  2983,  2984,  2985,
    2986,  2987,  2988,  2875,  2929,  2990,  3005,  3031,  3032,  3091,
    3119,  3127,  3033,  3034,  3111,  3133,  3035,  3036,  3037,  3038,
    3039,  3040,  3066,  3067,  3070,  3071,  3041,  3042,  3043,  2060,
    2540,  2614,  2615,  2616,  2695,  2731,  2814,  1940,  1941,  2969,
    2970,  2971,  2975,  2815,  2816,    41,   791,  1518,    42,    89,
     251,   250,   446,   447,   448,   788,  1172,   253,  1174,  1853,
     381,   699,   700,  2041,  2294,   701,   702,  1415,  1282,  1283,
    1642,   703,    65,   147,   148,   328,   456,   797,   457,  1180,
    1181,  1182,  1206,  1183,  1538,  1539,  1184,  1572,  1573,  1574,
    1575,   796,   149,   329,   497,   828,   826,   150,   330,   514,
    1260,   151,   331,   526,   527,  1262,   152,   332,   535,  1264,
     536,   857,   909,  1303,  1669,  1670,  1671,  1914,   349,  2224,
    2216,  2413,  2217,  2411,  2218,   854,   153,   333,   539,   540,
     154,   334,   543,   865,   155,   335,   546,   870,   156,   157,
     158,   336,   555,   879,   882,   159,   337,   561,  1280,   562,
     160,   338,   565,   566,   567,   568,   899,   569,  1292,  1293,
    1294,  1647,  1665,   890,   161,   339,   573,   905,   162,   340,
     163,   341,   577,   164,   342,   580,   581,   582,   914,   915,
     916,  1313,   917,  1308,  1309,  1675,   911,   165,   343,   591,
     350,   166,   344,   592,   167,   345,   595,   168,   346,   598,
    1320,   169,   170,   351,  1324,  1682,   171,   352,   603,   959,
    1333,  1685,  1963,  1964,  1965,  1966,   172,   353,   606,   173,
     354,   608,   609,   965,   966,  1345,   967,   968,  1696,  1697,
    1342,  1343,  1344,  1690,  1975,  1976,  1977,   174,   355,   175,
     356,   618,   176,   357,   620,   975,   177,   359,   626,   627,
     628,   979,  1993,   178,   360,   636,  1012,  1722,  1013,   637,
     638,   639,  1363,  1365,  1366,   179,   386,   180,   361,   647,
    1379,  2000,  2001,  2002,  1277,  1278,  1279,  2269,  2004,  2268,
    2434,  1020,   181,   182,   362,   650,  1028,  2009,  2279,  2010,
    2277,  2011,  1025,   183,   363,   652,   184,   185,   364,   655,
    1032,   186,   365,   658,  1733,  1734,  1036,   187,   188,   366,
     662,  1042,  1382,  1739,  1740,  1040,   189,   387,   714,  1096,
    1097,  1424,  2054,   190,   367,   668,   777,  1057,   669,   670,
    1402,  1403,   671,   672,   673,   674,   675,   676,   677,   191,
     368,   613,  1982,   969,  2260,  1350,  1704,  2258,  2428,   192,
     369,   685,  1405,  1065,  1756,  1757,  1758,  1061,   193,   687,
    1067,  2030,   375,   194,   376,   688,   689,   690,  1075,  1771,
    1768,  1071,   195,   377,   693,  1078,   196,   379,   197,   380,
     695,   198,   382,   704,   199,   383,   707,   200,   384,   709,
    1091,  1779,  1780,  1420,  1782,  2046,  2300,  2048,  1089,  2295,
    2442,  2527,  2528,  2529,  2839,  2530,  2688,  2689,  2717,  2531,
    2655,  2532,  2533,  2534,   201,   385,   711,  1030,  1421,  1422,
    2305,  1093,  1528,  1859,  1529,  1530,  1856,  1531,  1532,   893,
    1287,   894,  1285,   895,  1613,  1903,  1614,  1901,  1615,  2035,
    2288,  2036,  2286,  2037,  1730,  2435,  2521,  1731,  2014,  2015,
    2306,  2451,  2307,  2449,  2308,  1637,  1638,  1930,  1639,  1928,
    1640,  2203,   601,   602,   584,   585,   945,   946,   947,   948,
     949,   950,   951,  1209,  1588,  1219,   516,   517,   518,   519,
     498,   547,   873,   659,   660,   663,  1719,  1720,   458,   607,
     678,   679,   956,   640,   529,   530,  2477,  2141,  1130,  2135,
    2136,  2142,   420,   770,   593,   549,   897,   499,   500,  2240,
     501,  3081,  1233,   521,  1215,  1592,  1698,  1969,  1044,  1970,
     541,   858,   641,  1520,  1867,  2204,  1374,  1521,   614,   682,
     705,  1699,  2242,   502,   460,   550,   551,   461,   802,   803,
    1522,  1496,  3068,  1143,   503,   504,   505,   506,   507,   508,
     509,   832,   811,  1240,  1225,  1226,  1237,  1230,  1220,  1222,
     728,  1781,  2675,   849,  1253,  1623,  1023,  1760,   734,   876,
    1273,  1924,  2471,   325,   326,   327,  1788,  1880,  1823,  1465,
    2500,  2127,  1185,  1186,  2395,   449,   414,   433,  1810,  2246,
    1942,  1463,  2879,  1269,  2565,  2298,  1711,  3051,  2252,  2225,
     426,  1152,  1985,  2350,  2314,  2312,  2874,  2363,  2760,  1820,
    1863,  3054,   805,  1351,  1156,  1989,  2713,  1510,  2182,  1087,
    2342,   424,  2330,  2123,  2482,  2652,  1766,  2761,  1831,   886,
     958,  2549,   599,  2381,  2340,  2544,   646,  1705,  1540,  1208,
     874,  2686,   794,  2139,  2961,  2834,  1835,  1814,   867,  2407,
    1764,  1352,   415,  2997,  3003,  3094,  3095,  3096,  3097,  3098,
    2782
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -2462
static const yytype_int16 yypact[] =
{
   -2462,   274,   965, -2462,   653,   975, -2462,   169, -2462, -2462,
    1005, -2462, -2462,    40,  -218,   755, -2462,  1131, -2462,  1054,
    1226, -2462, -2462,  1005,  1005, -2462, -2462,   908,  1224,   861,
     972,  1031,  1262,   -44,  1047,  1095,  1326,  1479, -2462,  1147,
    1548, -2462, -2462,  1274, -2462,  1243,  1314, -2462,  1564,  1271,
    1276,  1327,  1447,  1335,   788,   788,   906, -2462,  1326, -2462,
     906, -2462, -2462,    39,  3697,  4420,  1310,   695, -2462,  1318,
    1333, -2462, -2462, -2462,  1338,   968, -2462, -2462, -2462, -2462,
    1814,  1814, -2462, -2462,  1350, -2462,  1358, -2462, -2462,  1455,
    4803, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,   707, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    1426, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,   757, -2462, -2462,  1499, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462,  1311, -2462,   281,    61,
   -2462, -2462,   764,   821,  1315, -2462,    67,    67,  1406,  1427,
    1617,  1617,  1617,    67,  1430,  1617,  1808, -2462,  1473,   968,
    1312, -2462, -2462, -2462, -2462,  1636, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,  1598,  1380, -2462, -2462, -2462,
      31,    31,  -145,  1382, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,   572,  2961,
    8861,   -99,   -63,  -106,  1328,   269,  -241,   868,  5872,  6706,
     868,  1602,  1008,  1192,   269,  1329,  1391, -2462, -2462,  6706,
   -2462, -2462,   269,  1334,  2568,  1329,  5972,  6706, -2462,  1039,
    1702,  1328,  1336,  1329,  1328,   875,    71,   311,  1329,  1328,
   -2462, -2462, -2462, -2462, -2462, -2462,  6706,  6072, -2462, -2462,
    1334,   483,  1329,  1328,  1329,   875,  1336,  1336,  1463,  1811,
   -2462,   538,  1397, -2462, -2462,  1398,  1064,   -64, -2462, -2462,
    1460,  1450,  1829,  1617, -2462, -2462, -2462,   341, -2462, -2462,
   -2462, -2462, -2462,   871,  1833,  1617, -2462,   -24, -2462, -2462,
   -2462,  1617,  1617, -2462,  1617, -2462,  1329,  1825,  1329,  1617,
    1617,  1329, -2462,  1351,  1205,  1408, -2462,  1778, -2462, -2462,
    1353, -2462, -2462, -2462,   -78, -2462,    29, -2462,  -122,  -171,
      90, -2462, -2462, -2462, -2462,   874,  1756, -2462,  1614,  1693,
   -2462,  1402,  1578,  1238, -2462,  1329, -2462, -2462,  1404,  1411,
    1412, -2462,  1413,  6949,    11,    11, -2462,  1416,  1417,  1420,
   -2462, -2462, -2462,  1421,    11, -2462, -2462, -2462, -2462, -2462,
   -2462,  1423, -2462,  1412, -2462,    11, -2462,  1788, -2462,  6186,
   -2462, -2462, -2462, -2462,  1433, -2462, -2462,  1435,  1436,  1438,
    6949,  9031,  8861,  9031, -2462,   158,  1206, -2462,  1750, -2462,
   -2462, -2462,   246,  1433, -2462, -2462,   -99, -2462,  1449, -2462,
      11, -2462, -2462, -2462, -2462,  1790,  4229, -2462, -2462,  -106,
   -2462, -2462, -2462,  1328,  1145,  1578,  1797,    76, -2462,  1529,
   -2462, -2462,  1402,  1433,  1328,  1799,  1569,  1469,  1472, -2462,
   -2462, -2462,  1336,  1289, -2462,  1803,   898,  6225, -2462, -2462,
    5439,  1293,  1304,  1804,   145, -2462,  1414, -2462, -2462, -2462,
    1802,    75, -2462, -2462, -2462,  5269, -2462, -2462,  1848,   707,
   -2462, -2462, -2462,   269, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462,  1476, -2462, -2462,   280, -2462,  1334, -2462, -2462,    45,
   -2462, -2462, -2462, -2462, -2462, -2462,  1457,  6706, -2462,  1468,
    1809,  1918, -2462, -2462, -2462, -2462,  1039, -2462,  1530,  1578,
   -2462,  1369,  1369,   412, -2462, -2462, -2462,  7513,   -21,   915,
    1484,  1482, -2462,  1152, -2462, -2462,  1489,  1815,   847, -2462,
    1816,   733, -2462,  1760, -2462,  1817,  1569,  1425,  1818, -2462,
    1760,  1329,  1821,  1439, -2462,  6949,  1807, -2462, -2462, -2462,
   -2462, -2462, -2462,  1691, -2462,   269, -2462, -2462,   481, -2462,
     444,  1959, -2462,    57, -2462,  1830,  1296,  5721,  1832,  6385,
   -2462,  1877,  1329,  1835,  6536,  1334, -2462, -2462,  -166, -2462,
   -2462, -2462, -2462,  4147, -2462,  1784, -2462, -2462,  1181,  1836,
    1881,  1838,  1760, -2462, -2462,  1789,  1511,  1582,  1729,  1461,
    1461,  1461,   519,  1516,  7495, -2462, -2462, -2462,  1462, -2462,
   -2462, -2462,  1677, -2462,    67, -2462,   771, -2462,   124, -2462,
   -2462, -2462, -2462,  1617,  1580,  1740, -2462, -2462, -2462, -2462,
     995,  1617,  1466,  1523,  1905,  1617,  1277,  1329,  1748, -2462,
   -2462, -2462, -2462,  1749,  1515, -2462, -2462,  1351, -2462,    48,
   -2462, -2462, -2462, -2462, -2462, -2462,  1332,   -66,  1617,    59,
   -2462, -2462, -2462,  1537,   170, -2462,  1617,  1587,  1697, -2462,
   -2462,  1920, -2462, -2462,  1329, -2462, -2462,  7893, -2462,  1932,
    2168,  8861,  1533, -2462, -2462,   -65, -2462,  1551,  8861,  8861,
    8370, -2462, -2462, -2462,  1433, -2462,  1492,  1493,  8861,  8861,
    8861,  6949,  1494,  6949, -2462, -2462, -2462,  6891,  1837, -2462,
    1238,  8861, -2462,  6949,  8861, -2462,  1433, -2462, -2462, -2462,
    1128, -2462,  1812,  8861,  8861,  8861,  8861,  8861, -2462,  1644,
   -2462,  1690,  1781, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,  1145, -2462, -2462, -2462,  1157,    -1,  1329, -2462,
   -2462, -2462, -2462, -2462,  8861,  1766, -2462,  1533, -2462,  1328,
   -2462, -2462, -2462, -2462,  1657, -2462, -2462, -2462,   173, -2462,
   -2462, -2462, -2462, -2462,  1744,  1882, -2462, -2462,  5439,    85,
     898,   898,   898,   898, -2462, -2462,  6706,  6891, -2462, -2462,
   -2462, -2462,  1008,   131, -2462,  1517, -2462,  1528, -2462, -2462,
   -2462, -2462, -2462,  1391, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,  4911, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462,   -14, -2462,  1929,  1589,
    1885, -2462,  6949,   120, -2462, -2462,  1685, -2462, -2462,    81,
    8861, -2462,  1591,   269, -2462, -2462,  6891, -2462,  1532,  1657,
    1369,  1588, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462,  1899,  1329,   -99, -2462,  1409, -2462,
   -2462, -2462, -2462,  1569, -2462, -2462, -2462, -2462,  1844,  2568,
   -2462, -2462, -2462,  1846, -2462, -2462, -2462,  1657,  1941, -2462,
   -2462,  1329,  1941,  1606, -2462, -2462,  1433, -2462,  1607, -2462,
   -2462,   577,  1332, -2462, -2462,  5745, -2462,  2049,   778,    65,
   -2462, -2462, -2462,  1617, -2462,   589,  6706, -2462, -2462,  1012,
   -2462, -2462,  1329, -2462,  2050, -2462,  1887, -2462, -2462,  6891,
   -2462,  1740, -2462, -2462,  6949, -2462, -2462, -2462, -2462, -2462,
    2050,  1855, -2462, -2462,  1409,  1329,  1532,   155, -2462,  1608,
    1678,  1831, -2462, -2462, -2462,  1718,  1618, -2462,  1620, -2462,
   -2462,  2019, -2462,  1828, -2462, -2462,  1619, -2462, -2462, -2462,
    2095,  1625, -2462, -2462,  1740, -2462, -2462, -2462,   595, -2462,
   -2462, -2462,  1827,   891, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462,  1277, -2462,  1637, -2462,   443, -2462,  1692, -2462, -2462,
   -2462, -2462,  1851,   -66, -2462,  1873,    67,    67, -2462,  1332,
     661, -2462,  -124, -2462, -2462, -2462,  1764, -2462,  2053,   897,
    1617, -2462,  1583,  1641, -2462, -2462,   333, -2462, -2462,  1617,
    1050,  7893, -2462, -2462, -2462,   665,  1650,  3562, -2462, -2462,
    1050, -2462, -2462, -2462,  1584,  1586, -2462,  6949,  1050,  1883,
    1683,  1819, -2462, -2462,  1847, -2462, -2462, -2462, -2462,    37,
    1210,  8861, -2462, -2462, -2462,   461, -2462,  1329,    55,  1042,
    1652,   137,  1654, -2462,   154,  1655,  6949, -2462, -2462,   307,
    1658,  1659,  1660,   179, -2462,  1433, -2462,  1661, -2462,   206,
    1663,  1578,   552, -2462,   -53,   -55,   269, -2462,  1223,  1666,
     211, -2462,  1669,  1644,  1206,  1206, -2462, -2462, -2462,   269,
   -2462,  1682,   -99, -2462,  1414, -2462, -2462,  1738, -2462,  1775,
   -2462,  1018,  1617, -2462, -2462, -2462,  1501,  1035, -2462, -2462,
   -2462,  1912, -2462, -2462, -2462, -2462, -2462, -2462, -2462,     2,
   -2462, -2462,  3683, -2462, -2462,  2331, -2462, -2462, -2462, -2462,
    1957,   552,  1958,    97, -2462, -2462, -2462, -2462,  2163, -2462,
    1696,   205, -2462, -2462,   131, -2462, -2462, -2462, -2462,  1852,
   -2462, -2462, -2462,  2045,  2034,  1391, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,  1794,  1391, -2462,  1700, -2462,  2141, -2462,
   -2462, -2462,  3121, -2462,  6949,  1897, -2462, -2462, -2462,  2058,
     106,   200,     0,   269,   269,   552,  1974,   379,  1328, -2462,
   -2462,  2040, -2462, -2462, -2462,  2189, -2462,  1985, -2462, -2462,
   -2462, -2462,  1846,  1329, -2462, -2462, -2462, -2462,  1329,    74,
     683, -2462,  1653, -2462,  1664,  6949,  1870,  1151, -2462,   461,
   -2462, -2462, -2462,  6706,  1332,  1332,  1332,  1332,  1332,  1332,
    1332,  1332,   778, -2462,   641,  1035,   -36, -2462,  1746,  1746,
     518,  6592,  1329,   552,  1987,  1716, -2462,  1723,  2205,  1329,
     650,  1657,  2211, -2462,  1409,   133,   281, -2462,  1725,  1796,
    1823,  1684,    42,  1329, -2462, -2462, -2462,    42,  2127,  1617,
    1363,  1363,  1617,   -10,  1933,  1617,  2208, -2462,  1900, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,    67,
    1233, -2462, -2462,  1753, -2462,  2027, -2462,    36, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462,  1099, -2462,    78, -2462,
    1277, -2462,  1886, -2462, -2462,  1851, -2462,    67, -2462, -2462,
   -2462, -2462, -2462,    63,  1694, -2462,   209, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,   189, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,  2193, -2462, -2462, -2462, -2462, -2462,  1431,
   -2462,  1544, -2462, -2462, -2462, -2462,  1922,  1922, -2462, -2462,
    1922,   522, -2462,  1617, -2462, -2462, -2462, -2462,  1617, -2462,
   -2462, -2462, -2462, -2462,    10, -2462, -2462, -2462,  2185,  1820,
   -2462, -2462, -2462, -2462,   -23, -2462,  1617, -2462,  2247, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1050,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  8861,  8495,
    1210, -2462, -2462, -2462,  1529,  8629,  1435,  8742,  1435, -2462,
   -2462, -2462,  1329,  1435,  1435,  1435,  6949, -2462,   491,  1435,
     -65, -2462, -2462, -2462,  1936,  1822,   -40,  2043,   552,  8830,
    1435,  1435,   593, -2462, -2462, -2462, -2462, -2462,   707, -2462,
   -2462, -2462,   876, -2462,  8861, -2462, -2462, -2462, -2462,  1945,
    2018,   536, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    1617, -2462,   828, -2462, -2462,  1453,  1617, -2462, -2462, -2462,
   -2462,   -15,  1617, -2462, -2462, -2462,   269, -2462,   269,  5590,
   -2462,   680,    -9,   131, -2462, -2462, -2462,  2163,  1329, -2462,
   -2462, -2462, -2462,  1730,  1283,    18,  1731,   593,  6949, -2462,
   -2462,  2226, -2462,  1549, -2462, -2462,  1897, -2462,  1549,  2070,
    2071, -2462,  1849, -2462, -2462,  1617, -2462, -2462,  2029,  1944,
   -2462, -2462,   269, -2462,   269,  1946,  1946,  1951, -2462,   921,
   -2462, -2462, -2462,  1329,  6706,    95,    88, -2462, -2462, -2462,
   -2462,  1973,  2146,  1035, -2462,  1268, -2462, -2462, -2462, -2462,
    1664, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462,   -34, -2462,  1329, -2462, -2462, -2462,
    1085, -2462, -2462, -2462,  8861, -2462,  6706,  6706,   514,  1937,
   -2462, -2462, -2462,  1529, -2462,   269, -2462,   593, -2462,  1953,
   -2462,  6949, -2462,  2172,  1824, -2462,   683, -2462,   832,  1902,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1800,  1867,  1888,
     183, -2462,  1684, -2462,  2076,  1826,  7774,   892,  2081, -2462,
    1740,  1755,  1617,  2208,  1757,   543,   551,  1740,  1758, -2462,
    1617, -2462, -2462, -2462,    86,  1448, -2462, -2462, -2462,  2017,
   -2462,  2127,  1328, -2462, -2462, -2462, -2462, -2462,  1099, -2462,
    2031, -2462, -2462,  2062, -2462,  1814,  1275,  1814,  1839, -2462,
   -2462, -2462, -2462, -2462,   718, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,   333,   333,   333, -2462, -2462, -2462, -2462,
     333,   333, -2462, -2462,  1617,  1617,   591,   591,   333, -2462,
     522, -2462,  1042, -2462,  1177,   -93, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  2092, -2462,
   -2462, -2462, -2462, -2462, -2462,  2093, -2462, -2462,  1179, -2462,
   -2462, -2462, -2462, -2462,   108,   143, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,  -181, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,  3296,   333, -2462, -2462,  1578, -2462, -2462,
   -2462, -2462,   852,   333,   591,   591,   333,   552,  1924,   552,
    1926, -2462, -2462,  6706, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,  1283, -2462,  2209, -2462,  1391, -2462,  1549,
   -2462,  1549,   593,  1840,  1840, -2462,  2312,  2283, -2462, -2462,
   -2462, -2462,   -87,  1329, -2462, -2462, -2462,   552, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,  1279, -2462,  2272,  1852,  2042,
    2069, -2462,   622, -2462, -2462, -2462,   795, -2462, -2462, -2462,
    2217,  2020, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  2054,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1042,
   -2462, -2462, -2462, -2462, -2462, -2462,  2010,  1842,  1617,   -93,
     552,  1805, -2462,  2205, -2462,  2098,  2229,  2098,   514,   954,
   -2462, -2462,  1480,  2066, -2462,  2284,   281, -2462,  1850,  1914,
   -2462,   480, -2462, -2462,  1329, -2462,  1023, -2462, -2462,  -202,
    -174,   544,   831,   872,  1806, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1938, -2462,
     960, -2462, -2462, -2462, -2462,  1329,  2097, -2462, -2462, -2462,
     728, -2462, -2462, -2462,  1617, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,  1323,   527, -2462,  1793, -2462,   480, -2462,  1861,
   -2462,  2145, -2462, -2462, -2462,  1757, -2462, -2462, -2462, -2462,
   -2462, -2462,  2082,    35,  2098,   669,  1617, -2462, -2462,  1617,
   -2462,  1933,  1569,   846, -2462,  1919,  1617,  2296,   735,   177,
      -3,  1532, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462,  1901, -2462,  2077, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,  2321,  1617,  1328,  1328,  1099, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,   230, -2462, -2462, -2462, -2462,
   -2462,   536,   333, -2462,  1491, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,  2044,   102,  1578,
   -2462, -2462, -2462, -2462, -2462,  1329, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,   269, -2462,   269,
   -2462, -2462, -2462, -2462, -2462, -2462,  2319,  2253, -2462, -2462,
    1549, -2462,  6706,  6706, -2462, -2462,  2009,  1328,    13, -2462,
    1329, -2462, -2462,  1964,  6706,  2094, -2462,  1617,   987, -2462,
   -2462,   531,  2101,  2102, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    1329, -2462, -2462, -2462, -2462,  1884, -2462, -2462,  1329,  2098,
   -2462,  1329, -2462, -2462, -2462, -2462, -2462,  2064,  2212, -2462,
   -2462, -2462, -2462, -2462, -2462,    67, -2462,   281, -2462,   281,
   -2462,  1891,  1903,   480, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    1834, -2462, -2462,  2334,  1872,  1904, -2462, -2462, -2462, -2462,
   -2462,  8054,  2366,  1961,  1961, -2462, -2462,  1740,   302,  1329,
   -2462, -2462, -2462, -2462,  1740, -2462,  1950, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,   594,   594,  1617,  2029, -2462,
   -2462,  1194, -2462,  1234,  1617,  1617,  1617,  1617, -2462,  1709,
   -2462,   379,  1617,  1933, -2462,  1963,  1755,  1328, -2462,  2046,
    2383, -2462,  2286, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,  1329, -2462,   -93,   -93,  6706,
   -2462, -2462, -2462, -2462,  1617,  1328,  1328,  2041, -2462, -2462,
   -2462,  1880, -2462,  1329, -2462, -2462,  1973,  2146, -2462, -2462,
   -2462, -2462,   780, -2462, -2462,  1329, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,   480,   988,
   -2462, -2462,  2098,  2194,  1923,  1740,  1923, -2462,  2137, -2462,
    2296, -2462, -2462, -2462, -2462, -2462, -2462,  1329, -2462,    34,
    1808, -2462,    38, -2462, -2462, -2462, -2462,   606,  1617, -2462,
   -2462,  2714, -2462, -2462,   551,  1960,  1329,  1329, -2462, -2462,
   -2462, -2462,  1329,  1617, -2462, -2462, -2462,  1740, -2462,  1099,
    1921, -2462, -2462, -2462, -2462,   -99,  1328,  1617, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1568,
   -2462, -2462, -2462, -2462, -2462,  2047,  2304, -2462, -2462,  2051,
    -200, -2462,  1983, -2462,  1927,  1875,  1740,  1931, -2462,  2301,
   -2462,  2305, -2462, -2462, -2462,   551,   551, -2462, -2462, -2462,
   -2462,  2223, -2462, -2462,  1861,  1740, -2462, -2462, -2462, -2462,
    1329, -2462, -2462, -2462, -2462, -2462,   617, -2462, -2462,   617,
    2424, -2462, -2462, -2462, -2462, -2462, -2462, -2462,   617,   617,
     617,   633, -2462, -2462, -2462,   838, -2462, -2462,   152, -2462,
   -2462, -2462, -2462,   -99, -2462,  2035,  1980,   193,  1852,   -32,
    1334, -2462,  7261, -2462, -2462,  -200,  1940,  1947,  1617, -2462,
   -2462,  1740,  2197,  1852,  1852, -2462, -2462, -2462,  2405,  1808,
   -2462,   -33, -2462, -2462, -2462, -2462,  1619, -2462, -2462, -2462,
   -2462, -2462, -2462,  1617,  1329,  1893, -2462,  1893, -2462, -2462,
    1329, -2462,  1525, -2462, -2462, -2462,    69, -2462, -2462,   618,
   -2462,  1949, -2462,  1952, -2462, -2462,   480, -2462, -2462,  1329,
    1329,  2202,   200,   551,  2323,  1978, -2462, -2462,  1329,  1329,
     -35, -2462, -2462, -2462, -2462, -2462,  2086,  1261,    69, -2462,
   -2462,   382,  1452,   270, -2462, -2462, -2462, -2462,  2202, -2462,
   -2462,  2050, -2462,  2029, -2462,  1852, -2462,  1907, -2462,  1329,
    2117, -2462, -2462,  1852, -2462, -2462,  2122,  1329, -2462,    84,
    2203,  2204,  2338,  2195, -2462,   382, -2462,  1255,   566,  1968,
     168,  8287, -2462,  2202,  1915,  1617,  1329,    60,   865,   465,
   -2462, -2462,  1617,  2138,  1329,  1617,  1617,  1617,  1617, -2462,
    2213,    22,  2214, -2462,  2210, -2462,  2119, -2462, -2462,  1329,
    2364,  1130,  2215,    53,  2216,  2218, -2462,   603, -2462, -2462,
    1329,  1994, -2462,  1617,  1617,  2208,  1535, -2462, -2462, -2462,
   -2462,  2255,  2281, -2462,  1617, -2462,  -105, -2462,  1453,  1617,
    2568, -2462, -2462, -2462, -2462,  1922, -2462,  2341,  1740, -2462,
    2418, -2462, -2462, -2462,  1329, -2462, -2462,  1329, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  2260,
    1922, -2462, -2462,  1915, -2462,  1329, -2462,  1238, -2462, -2462,
   -2462,  1389, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    1328,  1329,  1740, -2462,  1329,  1329,  1329,  1329,  1617,  1617,
    1617,  1617,  1617, -2462,  1329,  1617,  1617,  1617,  1617,  1617,
    1617,  1617,  1617,  1617,  1617,  1617, -2462,  1329,  1617,   536,
   -2462, -2462,  1617,  2208,  2207,  1934, -2462, -2462, -2462,  1329,
     333, -2462, -2462, -2462, -2462,   333, -2462,  1617,  1923,  1617,
   -2462, -2462, -2462,  1617, -2462,  1329, -2462, -2462, -2462,  1923,
   -2462, -2462, -2462, -2462,  1329,  1329,  1329,  1329,  1329,  1329,
    1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,
    1329,  1329,  1329, -2462, -2462, -2462, -2462,  1513,   -54, -2462,
    1329, -2462, -2462, -2462,  -108, -2462,   536,  -108,  2202, -2462,
   -2462, -2462, -2462, -2462,  1329, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462,  1329, -2462,  1251, -2462,
   -2462,  2207, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462,   333, -2462, -2462, -2462,   333, -2462,  1329,  1329,  1536,
    1617,  1617,  1519, -2462, -2462, -2462, -2462, -2462, -2462,  1724,
   -2462, -2462, -2462,  1329, -2462, -2462, -2462,  1617,  2207,  2207,
   -2462,  2257,  1617,  1617, -2462,  2719,  1329,  2207, -2462, -2462,
   -2462,  2207,  2207,  2244,  1486,  2208,  2261,  1740,  1942,  1617,
    1578, -2462,  1617,  1617,  1329, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1084, -2462,
     985, -2462, -2462, -2462,  1486,  2208,  1329, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462,   183, -2462,  1617,  1931, -2462,  9211,
    9211,  1418,  2359,  2277, -2462,  1740,  1084, -2462, -2462,  1740,
     985, -2462, -2462,   183, -2462,  1329, -2462,  1084,  1923, -2462,
    1529,  9173, -2462, -2462,  1297,  1422, -2462, -2462,  1428, -2462,
   -2462, -2462, -2462,   -70,   -70, -2462, -2462, -2462, -2462, -2462,
    9211, -2462, -2462, -2462, -2462, -2462,  1329, -2462,  2305, -2462,
    1852, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  2154, -2462,
    2154, -2462,  2444, -2462,  2036,    23,  2149, -2462, -2462,  9211,
    1740, -2462, -2462, -2462, -2462, -2462, -2462, -2462
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -2462, -2462, -2462, -2462, -2462,  2489, -2462, -2462, -2462, -2462,
   -2462, -2462,  2441, -2462,  1792, -2462, -2462, -2462, -2462, -2462,
   -2462,  2445,  2442,   -57, -2462, -2462, -2462,  1341, -2462, -2462,
   -2462, -2462, -2462,  2451, -2462, -2462, -2462,  2453, -2462, -2462,
    2100,  -248, -2462, -2462, -2462, -2462, -2462,  2292, -2462, -2462,
   -2462, -2462,  1002, -2462, -2462, -2462, -2462,  2282,   570, -2462,
   -2462, -2462, -2462,  1357, -2462, -2462, -2462, -2462, -2462,  1033,
   -2462, -2462, -1774, -2462, -2462, -2462, -2462, -2462,  1751, -2462,
   -2462, -2462, -2462,  1376, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  -901,
   -2462, -2462, -2462, -2462, -2462,   126, -2462, -2462, -2462, -2462,
   -2462,  -127, -2462,   140, -2462, -2462, -2462,   -42, -2462, -2462,
   -2462, -2462,   138, -2462, -2462,  1795, -2462, -2462, -2462, -2462,
   -2462,   134, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,   -29,
   -2462, -2462, -2462,   156, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  -226,
   -2462, -2462, -2462,  -189, -2462, -2462,  -229, -2462, -2462, -2462,
   -1379, -2462, -2462,  1843, -2462, -1853, -2461,  -711, -2462, -1990,
    -490, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -1258,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,   734, -2007,
    -188,   190, -1758, -1756, -1985, -2462, -2462, -2462, -2459, -2462,
   -2462,  -566, -2462, -2462, -2398, -2462,  -128, -1651, -2462,  -278,
   -1909, -2462, -1872, -2462, -1764, -2462, -2462,   223, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    -547,  -572, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -1394, -2462,  -520, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,   -61, -2462, -2462, -2462,  -239,  -237,  -382,
    -381, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,  2306,  1285, -2462,   887, -2462, -2462, -2462, -2462,
   -1368, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  -521, -2462,
   -2462,   -28, -2462,  2494, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462,  1379, -2462,  -782, -2462, -2462,  -776, -2462,  -169, -1269,
    1010, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
    1313, -2462, -2462, -2462,  2048, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,  1303, -2462, -2462,   901, -2462, -2462,  -647, -2462,
   -2462, -2462,   353, -2462,   355, -2462, -2462, -2462, -2462,  2037,
   -2462, -2462, -2462,  1711, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,  2235, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462,  2012, -2462, -2462, -2462,  1290,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1668, -2462, -2462,
    1671, -2462, -2462,  1267,   916, -2462, -2462, -2462, -2462, -2462,
    1998, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462,   625,  1631, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462,  1626, -2462, -2462,   899,
   -2462,  1252, -2462, -2462, -1625,   616,   620, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,  1971,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2256,
    1962, -2462, -2462, -2462,   879, -2462, -2462, -2462, -2462, -2462,
    1231, -2462, -2462, -2462,  -883,   873, -2462, -2462,   604, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,   596,
   -2462,   598, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462,   819, -1564, -2462, -2462, -2462, -2462,
   -2462, -2462,  1570,   870, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462,  -551, -2462, -2462, -2462,
   -2462,  1209, -2462, -2462, -2462,  1948, -2462,  1943, -2462, -2462,
   -2462,  2245, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462,   848, -2462, -2462, -2462, -2462, -2462,  1928, -2462, -2462,
    1199, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462,   580, -2462,  1200, -2462, -2462, -2462, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,   -62, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462,   738,  1198,  1615,
   -2462, -2462, -1060, -2462,  1093, -2462, -2462,  1096, -2462,   947,
   -2462,  1737, -2462,  1752, -1158, -2462,  1030, -2462,  1043,   601,
   -2462,   619, -2462,   623, -2462, -2462, -2462, -1667,   217, -1333,
   -2462, -2462,   352, -2462,   354, -1350,   613, -2462,  1024, -2462,
    1026,  -469, -1009,  -306,  -850, -2462, -2462,  1722, -1151,   909,
     910,   911,   913,  -529,   579,  -269,   996,   914, -2462,  1288,
    -152,  -752,  -307,  2313,  2293,  2016, -1778,  -190,   853,  -373,
   -2462,  -635, -2462,  -282, -1282,  1841, -1634,  -408,  1553, -2462,
     547, -2287,  -201,  1910,  -300,  -293, -2462,  -142,  -245, -2462,
     731, -2462,  -767, -1438, -2462,  1299,  -630,  -834, -2462,  1063,
    -332, -2462, -2462, -1744,   809, -2462,  -100,  -342, -2462,   761,
   -2462, -2462, -2462,   -41,  -420, -2462, -2462,  1707,  -494,  -511,
    -417,  1196, -1510,  1202,  -333,    79,  -461,   434, -2462, -2462,
   -2462,   153,  2201, -2462, -2462, -2462, -2462,  1032, -2462, -2462,
   -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -2462, -1582,
   -2462, -2462,   342, -2462, -2462, -2462, -2462,   166, -1781, -2462,
   -2462, -2462, -1527, -2462, -2462, -1061, -1995, -2050, -1338, -2462,
   -2462,    62, -2462, -1246, -2462, -1894, -2462, -2462, -1917, -2462,
    -221, -1761, -2082, -2462, -2462, -2462, -2462, -2462,  2239, -1971,
   -1503,  -345,  -542, -1308,  1547,   986, -2462, -2462,  -541, -2462,
   -2462, -2462,  -215, -2462, -2462, -2462,  1292, -1505, -2462, -2462,
    1019, -2381,  -878, -2462, -2462, -2462,  -384,   877, -1865, -1999,
   -2462, -2462,  1133, -2462, -2462,  -126, -2462,  1269, -2462, -2462,
   -2462,    77, -2462, -1790,  -352, -2462, -2462, -2462, -2462, -2462,
   -2462
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -2119
static const yytype_int16 yytable[] =
{
     427,   428,   806,   868,   431,  1364,   571,   696,  1103,  1104,
    1105,  1678,  1885,  1411,    64,   884,   421,   778,   759,   394,
     762,   611,   429,   765,   247,   681,  1664,   644,   548,  1418,
    1634,  1869,  2133,   754,  1864,  1043,   583,  1865,  2062,  2061,
    1066,   878,  2120,  1053,  2052,  1316,  1732,  1794,  2112,   528,
    2113,   594,   604,   813,  2185,  1761,  1239,  2253,   877,   594,
     418,   515,   392,  1310,  2187,  2016,  1249,   680,   418,  2827,
    1149,  2322,  1843,  1979,   594,  1242,  2386,  2387,  2687,   445,
     661,   445,   545,  2555,  1809,  2551,  1954,   980,  2622, -1798,
     835, -2118,   912,  2365,   574,  1837,  1358, -1799,   432,  1833,
      87,  1347,  1998,  1213,   600,  1817,  1643,  1644,  1319, -2067,
    1876,   619,   621,  1631,  1585,  1033,  1176,  1290,  1944,  2196,
    2197,  2198,   971,  1291,  3118,  1121,  2199,  2200,  1547,  1337,
    1576,   691,  1585,  2849,  2208,  1010, -2105,  2356,  1580,  1109,
     445,   923,  1008,  1667,   544,   962,   556,   445,  1872,  2219,
    1854,  2662,  1305,  2301,  1380,  1301, -1999,  2256, -1999,   747,
     866,  2005,  2384,  1158,  2862,  1207, -1828,  2881, -1807,    -5,
     537,   648,   878,   425,   656,  1268,  1702,   525,   520,   686,
    2377, -1805,   745,  1338,  2965,  1154,  2645,  2742,   451,  1059,
    2228,  2039, -2005,   708,   752,   906,   570,  1713,  2007,  2232,
     756,   757,  2235,   758, -2067,  1149,  1268,  2592,   763,   764,
    1848,  1007,  1007,   779,  1585,  1347,   789,  3132,  1728,  1832,
     960,   842,  1851,   784,  1355,  1153,  1305,  1762,  1706,  1611,
     532,  1585,   735,   961,  2426,   694,  2773,  1709,   720,  2328,
   -2118,   425,  1672,   840,   528,  2226,  2966,  2710,   792,  1162,
    2366,   871,  2563,  1759,   829,  1774,  1585,   554,    21,  2602,
    1164,  1081,   525,   538, -2036,  2774,  2775,  2328,  2626,  2627,
    2698,  2729,  1790,   771,     3,   545,  1791,    25,  2146,  1218,
    1221,  1224,  1844,  1585,  2213,    49,   545,   459,  1585,   522,
    2213,  1212,   542,   425, -1368,   667,  2488,   572, -1368,  1150,
     955,  2733,  1214,  1069,   644,  1250,  2388,   471,  2227, -1760,
     393,   955,  1792,   610, -2023,  1339,  2882,   545,  2147,   642,
    2329,   843,  1080,  1707,   786,   829,   844,  1413,  1126,  1082,
   -2118,  1999,  1336,  2322, -1368,   425,  2646,  2653,  -602,  1710,
     952,   706,   793,   710,  2248,   525,  2249,  2649,  2331,  1144,
   -2118, -1880,   476,   862,  1612,  2427,   736, -1999,  1163,   520,
     520,   520,   481,  1645,   880,  2474,  1274,  -602,  -602,  1247,
    1869,  1284,   972,   790,  2402,  1051,  1165, -2118,  1955,  1179,
    1340,  2711,   594,  2017,  1585,  1216,  2704,   955,  1715,  1729,
    2220, -2021,   907,  1155,  2967,   533,  1315,   534,  2008,  2968,
     787,     4,     5,   399, -2118,  2445,   452,    50,    22,   523,
    1763,   400,  2401,  1187,   553,  1348, -2005,   -35, -2005,   425,
     735,  -427,  2503,   553,   807,   898,  1503,  2214, -2041, -2118,
    2562,   553, -1760,  1984,  1150,  -766,   830,  1323,  2005,  2647,
     963, -1999,  1068,  2339,   691,   495,   553,  1683,  1270,   829,
     545,   485,   495,   801,  1417,  2257,  1686,  2221,  2404,  1601,
    1906,  1310,   938,  2657,  1310,   771,  1211,  2962,  2541,  1586,
     839,   839,   839, -2118,  1011,  2553,  2222, -2005,   866,  2425,
     665,   495,  1372,  -761, -2005,  2195,   495,  1586,   848,   463,
    1359,  1361,  -602,  2184,  1618,   859,  -764,  1102,   542,  1276,
     471,  1388,   747,   957,  1281,  1877,  2719,  2935, -2105,  2850,
    1290,   471,  2828,  1945,  2776,  1295,  1291,  1349,  2938,  1881,
     548,  -602,  1124,   964,  -766,  2364,   634,   749,   487,  1587,
    1128,   629,  1210,  1115,  1133,  1956, -2036,   495,  1786,  1570,
    2863,  1703,   471,  2556,   736,   476,  1676, -1876,  1274,  1229,
    1229,  1229,   814,  1646,  1164,   481,   476,  1157,   545,  2720,
    1007,  2743,  1248,   801,  1838,  1169,   481,  1579,  2545,  1586,
     666,  1211,   872, -2005,   495, -2005,  1504,  1054,  1159,  2674,
     490,    88,  1243,   545,   634,  -764,  1586,   476,  1834,   836,
     523,   523,   523,  2112,  1357,  2113,   642,   481,  3078,   766,
     548,  2436,   463,  2023,   667,  1271,   583,  2215,  1672,   771,
     419,  1586,   393,  2215,  1845,  1289,  -602,  2829,   419,  1122,
     495,  2735,   495,   913,  1045,  2421,   553,   495,  1306, -1878,
     401,  1349,  1341,   496, -2005,   634,  -761,  2148,  1586,  1307,
    -761, -2005, -2118,  1586,   485,   634,  1599,  2223,  2516,   520,
    2654,  1076,  2835,   829,  1648,   485,   520,   520,   520,   719,
     843,  1300,  1302,  2777,  1416,   844,   520,   520,   520,   548,
    1165, -1872,   553,  1354,  1321,   471,  2697,  2318,   719,   520,
     594,  1406,   520,   553,   495,  2475,   485,  2612,  1716,  1275,
    2265,   520,   520,   520,   520,   520,  1376,  3108, -1867,  1610,
     952,  -761,  1306,  1621,  1849, -1368, -1368,  1467,   402,  2836,
    2762,   471,  1425,  1307,  1691,  -761,  -761,  2837,  -759,  -427,
     476,   487,   520,   634,  2808,   697,  -427,  2211, -2041,  2021,
     481,  1356,   487,  1617,  1368,  2600,  1534,  1591,  1535,  1586,
   -1760,  2566,  2692,   631,  1046,   632,  2810,  2762, -1760, -1760,
    1616,  1895,  1514, -1760,   553,  1741,   476,  1717,   955,   553,
     839,  1492,  2612,   487,   524,  -602,   481,   839,   839,   839,
    2522,   403,   548,   490,   667, -2080,   404,   839,   839,   839,
    1234,   698,  1234,   471,   490,    14,  2273,  2353,   612,  2236,
     839,  2238,  1234,   839,   645,  1593, -2118,  1187,  1866, -1874,
     439,  1679,   839,   839,   839,   839,   839,  2838,   471,   843,
    1392,  2523,   537,  2018,   844,   490,   923,  1394,   520,   485,
     495,  1407,  2811,  1310,   471,  1395,   496,  2140,   476,  2261,
     634,   495,  -427,   839,  1414,  2033,   746,   496,   481,  1894,
    2019,  2721,  1404,  1745,  1746,  1747,  1748,  1749,  1750,  1751,
    1752,  -761, -2118,   476,  2722,   485,  2385,  2763,  2367,  2812,
    1736,  -756,   495,   481,  2723,  2368,  2820,   771,   496,   476,
    2483,  -759,  1468, -2118,  1611,  -759,  2137, -2118,  1503,   481,
     523,  2291,  2292,   399,   495,   204, -2007,   523,   523,   523,
    1519,   400,  1460,  2632,  2763, -2021,   487,   523,   523,   523,
    1235, -1832,  1235, -1832, -2118,   454,  1245,   815,  1295,  2640,
     523,  1916,  1235,   523,  1947,  2535,  1949,  2524,  2438,  2124,
    1474,  1045,   523,   523,   523,   523,   523,   485,   347,   839,
    1512, -2118,   487,   399,  1055,  2437,  -759,  1536,  2454,   548,
    2455,   400,  1590,  2042,   837,   524,   524,   524,   490,  1515,
    -759,  -759,   485,   523,  1754,  1499,  1500,   720,  1523,  1755,
    2691,  2840,  1987,  2525,   721, -2118,  1625,  2813,   485,  1593,
     860,  1052,  1056,  2112,  1367,  2113,   720, -1709,   697,   216,
    1627,  1915,  2567,   721,   490,  2328,  1245,  1268,  1375,  2348,
    3004,  1984, -2118,   634,  2871,   495,  1735,  2138,  3025,  1612,
    1383,   496,  2526,  1896,   487, -2118,  3045,   439,  1396,  1858,
    2679,  1861,   495,  1952,  -756,  1513,   393,   395,  -756,  2034,
    3026,  1537,  2928,  2040,  1026,  2764,  1769,  1475,  1504,   487,
    2272,  1410,   217,   370,   698, -2118,  1769,   496,  2753,  2354,
    1397,  1046,  2568,  1045,  2709,   487, -2099,  2732,  2765,   523,
    2273,  1632,   553,  1712,  1423,  1245,   490,   399,  1971,   520,
     594,   955,  2764,  1398,    78,   400,  2333,  1387,   634,  1469,
   -1880,   645,   218,   495,   219,  2753,   220,  2018,  2274,  -756,
     891,   490,  2821, -2099,   221,  2765,  -759,   495,  2369,  2247,
    1899,  1900,  2925,  -756,  -756,  2343,  3028,   490,  1852,  1047,
    1935,  2673,   495,   495,  2019,  1905,  2125,    15,   771,   496,
     401,   792,   557,   348,  1926,  1927,   615,   495,  2251,   205,
     558,  1399,  1476,   455,  2230,   667,   786,  2303,   495,   615,
     553,  1728,   371,  3029,   496,  1470,  2932,   938,  2250,   634,
    3044,  2933,   495,  2349,   495,   495,  1129, -1999,  1744,   222,
     496,  2044,  1117,  2051,  1021,  1959,  1045,    79,  1245,  1961,
     401,  2831,  1027,  1046,    28, -1709,  1772,  1477,   495, -2118,
     839, -2080,   634,   771,    17,   372,  1594,  1478,  2050,  1917,
    1918,  1919,    82,   -21,   495,  1234,  2370,  1400,   402,  1408,
    1898,  2895,  3002,  1479,  1890,   396,  2536,     4,     5,  1868,
     634,   495,   787, -2118,  1635,   793,  1022,  2012,  2013,   373,
     559,  1804,  2371,   697,  2372,   651,  1808,   892,  1811,  1118,
    1119,  1816,  1818,  2771,  1821,    40,   622,  2991,  1524,  -756,
   -2118,  2992,  1525,   634,  3055,   524,   771,  1635,   402,   713,
     715,  3030,   524,   524,   524,   223,  2832,  1920,  2833,  1936,
      26,   403,   524,   524,   524,  1236,   404,  1236,  1826,  2512,
    2513,  1866,  2771,    27,  3074,   524,  1480,  1236,   524,   698,
    2304,  3092,  2328,  2231,  1636,    83,  1046,   524,   524,   524,
     524,   524,  2613,   623,   401,  1635,  2326,  2976,  2643,  1913,
     523,   624,  2887,  3076,   224,  1326,  1842,  1401,  1327,  1328,
    2656,   403,  1729,  1045,  1700,  1235,   404,  1636,   524,  1932,
     925,   926,  3105,  2328,   374,  2669,  2670,  2893,    31,  1882,
    1884,   553,  1870,   216,  1409,   553,  1882,  1871,  1882,  2489,
    1476,  1992,  1726,   -21,  2644,  1995,  2379,  1727,   553,  3069,
    1611,  2006,  2274,  1481,  1045,  1878,  1482,  1483,   560,  2327,
    1908,   927,   928,  2335,  1913,  1636,    39,  2613, -2056,  1948,
    1866,  1950,   402,  2659,  2927,  1925,   748,  1526,  1958,  1527,
     981,  1773,   405,   657,  2690,  1477,   217,   732,  1783,  1783,
     553,   625,  2490,   495,  1793,  1478,  1921,  1922,  2380,  1519,
     225,  1923,  3064,  2129,  2337,  2229,  1047,  2736,  1037,  2750,
    2143,   733,  1735,    38,   524,  2739,  2690,   548,  2207,   586,
     982,   983,   984,   985,   986,   888,   218,  1369,   219,   955,
     220,  1827,   749,  1046,  1046,   403,   495, -2118,   221,  1934,
     404,  2020,   553,   553,   553,  1943,   520,   520,  3065,  1266,
     226,  1946,  1014,   520,  1913,   520,  2979,  1370,  1484,   418,
    1094,  2989,   987,   988, -2118,  1828,   425,  2980,    45,  2206,
    2206,  1015,  2818,  1377,  1046,  1612,  1267,   520,   548,   471,
      43,   634,   393,  2154,  1480,  2233,  2234,  3083,  3000,  1997,
    2981,   771,   520,    56,  1983, -2116,  2275,  2276,  3008,  3009,
    1127,  1062,   553,   222,  2191,  2029,  -365,  3047,  3109,  2282,
    2283,  3048,  3049,  3113,  2856,   578,  2155,    46,  1134,  2604,
    2982,  2352,  2284,  2285,   476,  3084,   579,  -365,  1047,   227,
     843,  2031,  2031,  2192,   481,   844,  3010,  2206,  2206,  1994,
    3072,   863,  2995,  2243,  2205,  2205,  2542,   772,  1868,   773,
     587,   588,    54,  2996,   843,  2063,  2751,   839,   839,   844,
    2018,  1481,  1063,  1135,   839,  1064,   839,  1973,  -365,   589,
    3072,  2262,  1085,  1136,  1086,  1234,   989,   990,   991,   992,
     993,   994,   995,   996,   997,  2259,  1879,  2019,   839,  3000,
    2857,  1912,  1329,  1330,   939,  2122,   940,  1974,  2263,   223,
      55,  2132,  2714,   839,  2858,  2859,  2860,  1937,  1325,  2145,
     542,  1326,  2205,  2205,  1327,  1328,  2605,   816,   817,  1331,
    1332,  2378,   520,   485,  1933,   883,  3001,   822,  2271,   590,
    1251,  3080,  3082,  2492,  2150,  1938,  2715,  1939,  3114, -2099,
     843,  1047,    60,  2995,  3116,   844,  -365,  1960,   224,  1913,
    2734, -1807,    62,  3112,  2996,   524,  1912,  1045,  1390,  2716,
    2995,  3115,  1137,  2201,  2202,  1700,  1484,  3117,   801,  1391,
    1236,  2996,  3122,   852,   772,   864,   773,   523,   523,  2210,
    -365,  2212,  3102,  2345,   523,  2346,   523,  2415,  2635,   843,
      63,   843,  1367,  3107,   844,  1235,   844,  2637,  2638,  2639,
     487,  3136,  2408,  2409,  2410,   634,  2727,   553,   523,  -365,
     845,    66,  2684,  1589,  2728, -2099,  -365,  2685,  1268,   846,
    1633,   463,   843,   523,  2752,  2024,  1619,   844,  -365, -2099,
   -2099, -2099,  1524,   839,   998,   843,  1525,  2753,   644,  1138,
     844,   785,  2498,  2499,   225,   785,  1912,  2754,    68,   999,
    1045,    69,   490,  2390,  1812,   553,  1813,   553,  1526,  1887,
    1527,  1889,  2310,   629,  2311,    70,  1891,  1892,  1893,  1256,
    1257,  1258,  1897,  2405,  3085,  2406,    72,  1046,  3086,  3087,
    2832,    73,  2833,  1909,  1910,  1046,    49,  2391,  1047,  1047,
      74,  1139,  2447,  2448,   226,  2959,    50,  2960,  2190,   216,
    2193,   553,   464,   553,   644,   496,  1191,  1595,  1192,   838,
    1597,   841,  3125,  2872,  2873,   203,   495,   438,  1602,   466,
    3052,  3053,  1606,   210,  3088,    36,    37,  2290,  1608,  1047,
   -1779, -1779, -1779, -1779,   900,   901,   902,   903,   211,  -365,
    -365,  3089,  3090,   215,  1140, -1778, -1778, -1778, -1778,  1254,
    1255,   245,   217,   523,  -365,   248,  -365,  1296,  1297,  1298,
    1299,  1231,  1232,   249,   553,  2332,  2334,  2336,  2338,   358,
    1046,   252,   378,   227,   388,   422,   405,   423,   425,  2403,
     430,   432,   435,   440,   442,   443,  1438,   453,   576,   393,
     495,   597,   218,  2351,   219,   605,   220,   649,  1329,  1330,
     716,   717,   729,   731,   221,   742,   743,  1439,   751,   744,
     761,   795,   766,   780,   782,   798,  1692,   800,   801,   804,
     808,   630,   467,   468,   469,  1331,  1332,   809,   810,   812,
     495,   470,   818,   819,   830,  2375,   820,   821,  2376,   823,
    2417,  1912,  2418,   471,   825,  2383,   847,   853,  1440,   851,
    2473,   831,   833,  -365,   834,   869,   875,  2480,   881,   885,
     883,  2478,   887,   889,   910,   904,   908,   953,   973,   222,
    1693,   957,  2396,   970,   974,   631,   472,   632,   976,   978,
    1016,  1017,  1018,  1019,  1029,  1024,  1034,  1031,   633,  1035,
     477,   478,   479,  1039,   667,  1041,   480,   464,   481,  1048,
     644,  1058,  -365,  1060,  2399,  1994,  1070,  1074,  1084,   419,
    1077,  1090,  1088,  1095,   466,  1092,  1098,  2422,  2423,  1099,
    1100,  1107,  1102,  1112,  1114,  1125,   771,  1129,  1131,  2431,
    1132,  2151,   524,   524,  1145,  1146,   553,  1147,   553,   524,
     483,   524,  1161,  1170,  1171,  1188,  2152,  1173,   531,  1211,
    1236,  1217,   552,  1227,  1228,  1238,  2433,  2153,  2550,  3079,
    1259,   552,   596,   524,  1252,   223,  1261,   525,  1246,   552,
    1272,  1276,   616,   892,   891,  1311,   553,   643,   524,  2554,
     654,  1322,   654,   664,   683,   616,  1314,   485,  1335,   425,
     963,  1353,   634,  1360,  1362,  1381,  1441,  1702,  2595,  2596,
    2599,  1373,   654,  1378,  2597,  2561,  1385,  1386,  1442,  1393,
     445,  1412,  1419,  1427,   224,  1428,  1429,   467,   468,   469,
     486,  1433,  1437,  1434,  2453,  1435,   470,  1462,  1464,   553,
    1466,  1473,  1047,  1490,  1498,   644,  1493,  1494,   471,  2620,
    1047,  1509,  1511,   760,  1516,   760,  1517,  1541,   760,  1581,
    1577,  1578,  1582,  1583,  1596,  1584,  1598,  1600,  2630,  1629,
    1603,  1604,  1605,  1607,   487,  1609,  2487,  1694,  1620,  1622,
     542,   472,  2595,  2494,  2495,  2496,  2497,  1630,  1641,   488,
     489,  2502,  1626,   476,  2514,   477,   478,   479,  1666,  1668,
    1673,   480,  1674,   481,  2416, -2118,  1348,  1680,  1681,  1684,
    1687,  1688,  1701,  1189,  1190,  1714,  1721,  1723,   524,  1724,
     225,  1737,  1742,  2515,  2550,  1765,   490,  2399,  1775,  1443,
    1444,  1776,  1738,  1777,  1778,  1047,   491,  2963,  1787,  1798,
    1796,  1809, -2056,  1797,  1445,   483,  1446,  1819,  1800, -1275,
    1822,  1824,  1830,   531,  1268,  1994,  2478,  2660,   492,  1840,
    1191,  1855,  1192,   493,  1193,  1847,  1862,  1874, -1275,  2441,
     226,   494,   634,   495,   552,  1879,  2154,  2444,  1611,   496,
    2446,  2699,  2700,  2750, -1806,  1636,  1875,  2570,  1635,  1612,
    2478,  2554,   485,   781,  1962,  1968,  1972,  1980, -1801,  1194,
    1195,  1196,  2598,  1986,  1981,  1984,  1991,  1988,  1729,  2155,
    1728,  2043,  2038,  2045,  2058,  2057,  2603,  2053,  2063,  -249,
     552,  2554,  2047,  2128,  2144,   486,  2130,  2188,  2134,  2059,
    2189,   552,  2064, -1753, -1804,  2237,   553,  2239,   553,  2245,
    1973,  1974,  2264,  1447,  2266,  2267,  1994,  2007,  2826,   227,
    2008,  2033,  2299,   644,  2194,  2297,  2281,  1197,  2313,  1198,
    2251,  2321,  2315,  2293,  2355,  2320,  1199,  2034,  2347,   487,
    1200,  2357,  2359,  2341,  2339,  2382,  2362,  2156,   399,  2393,
    2392,  2394,  2157,  2214,   488,   489,  2896,  1650,   664,  2419,
    1651,  2420,  1448,  2424,  2511,  2430,  2432,  1652,  1653,  2443,
    2304,  2888,   552,  2439,  2440,  2303,  2457,   552,  2462,  2458,
    2464,  2461,  2519,  2468,  2465,  2470,  2481,  2666,  2158,  2504,
    2509,   490,  2507,  2510,  2537,  2517,  2159,  2518,  2546,  1695,
    2751,   491,  2548,  2552,  2607,  2593,  2601,  2478,  2160,  2608,
    2617,  2621,  2678,  2623,  2618,  2899,  2619,  2624,  2610,  2628,
    1654,  2636,  2650,   492,  2651,  2664,  2523,  2665,   493,  2667,
    1201,  2701,  1202,  2680,  2694,  2705,   494,  2696,   495,  2707,
    2712,  2738,  1703,  2740,   496,  2737,  2161,  1376,  2748,  2745,
    2746,   667,  2747,  2772,   760,  2162,  2822,  2855,  2842,  2848,
    2851,  2861,  2864,  2852,  2868,  2877,  -246,  2876,  3060,  1872,
    2889,  2865,  2892,  1866,  3050, -1880,  3010,  2554,  3099,  3056,
    3058,  3100,  3126, -2099,  3129,  3135,    16,  3130,  2744,    85,
      81,  1175,    86,  1508,    77,    76,   417,  1655,  1850,  2781,
    2819,   437,  2163,   750,  2825,  2164,  1502,  1489,  1148,  1836,
    2682,  2841,  2505,  2493,  2844,  2845,  2846,  2847,  2631,  2501,
    2853,  2506,  2486,  1123,  1244,  2629,  2749,  2923,  2866,  1656,
    2108,  2467,  3124,  2809,  2703,  2894,  2460,  3121,  3128,  2883,
    3104,  2884,  2869,  2870,  2663,  2973,  2974,   450,  1953,   202,
    1533,  1657,  2817,  2880,  1873,  1106,  1624,  1628,  2885, -2099,
    1951,  2414,  2412,  1265,   850,   575,   861,   463,  2752,   896,
    1304,  1677,  1649, -2099, -2099, -2099,  1312,   954,  2244,  1957,
    1334,  2753,  1346,  2255,  1689,  1978,  2254,   977,  2003,  1009,
    3057,  2754,  1996,  1725,  2972,  2049,  2270,  2280,  2278,  2683,
    2022,  1753,  1384,   684,  1244,  2032,  1658,  1073,  1050,  1784,
    1785,  1049,  1789,  2296,  1860,  1857,  2718,  2904,  2905,  2906,
    2907,  2908,  1288,  1371,  2910,  2911,  2912,  2913,  2914,  2915,
    2916,  2917,  2918,  2919,  2920,  1904,  1286,  2922,  3101,  2302,
    1994,  2924,  3103,  2520,  1659,  1203,  2289,  1902,   464,  2287,
    2452,  2450,  2309,   531,  1931,  1929,  2934,  1318,  2936,  2025,
    2026,  2027,  2937,  2028,  1660,   466,   653,  1038,   712,  1151,
     552,  1472,  2361,  1244,  2241,  1911,  2759,  2770,  1743,  2209,
    2886,  1841,  1839,  1263,   824,  2606,  2472,   799,  2677,  3073,
    1497,  1767,  1990,  2843,  1967,  2897,  2183,  1204,  2676,  3093,
    1815,     0,     0,  3137,     0,     0,     0,     0,  2854,  1205,
       0,     0,     0,   531,     0,     0,     0,     0,     0,  2867,
       0,     0,     0,     0,     0,     0,     0,     0,  1561,     0,
    1562,  1563,     0,     0,     0,     0,     0,     0,     0,  1375,
       0,     0,     0,     0,  1661,     0,     0,     0,   552,  2998,
    2999,     0,     0,  2890,     0,  2074,  2891,     0,     0,  2571,
    2572,  2573,     0,     0,     0,     0,  3007,  1662,   467,   468,
     469,  3011,  3012,     0,     0,     0,  1244,   470,     0,     0,
    1534,  1191,  1535,  1192,     0,     0,     0,     0,  3059,   471,
    2898,  3061,  3062,  2900,  2901,  2902,  2903,     0,     0,     0,
       0,     0,     0,  2909,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2921,     0,   542,     0,
       0,     0,   472,     0,     0,  3077,     0,     0,  2931,     0,
       0,  2574,  2575,  2576,   476,     0,   477,   478,   479,     0,
    1663,     0,   480,     0,   481,     0,     0,     0,     0,     0,
       0,     0,     0,  2939,  2940,  2941,  2942,  2943,  2944,  2945,
    2946,  2947,  2948,  2949,  2950,  2951,  2952,  2953,  2954,  2955,
    2956,  2957,     0,   616,     0,     0,     0,     0,     0,  2964,
       0,     0,     0,     0,     0,   542,   483,     0,     0,     0,
       0,     0,     0,  2977,     0,     0,     0,   520,   520,     0,
       0,     0,     0,     0,     0,  2978,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   520,
       0,     0,     0,     0,     0,     0,  2993,  2994,     0,     0,
       0,     0,     0,   485,     0,     0,     0,  3013,   520,   552,
       0,     0,  3006,   552,     0,     0,     0,     0,  2577,  2578,
       0,     0,     0,     0,     0,  3046,   552,   462,     0,   531,
     463,  2579,     0,  2088,     0,     0,   486,   520,     0,     0,
       0,     0,     0,  3063,     0,     0,  2580,     0,     0,     0,
       0,  3014,     0,  3015,     0,     0,     0,     0,     0,     0,
       0,     0,  2581,     0,     0,  3075,     0,     0,   552,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   839,   839,
     487,     0,     0,     0,     0,     0,  3016,     0,     0,     0,
       0,     0,     0,     0,  3106,   488,   489,     0,     0,     0,
     839,     0,     0,     0,     0,     0,     0,     0,     0,  3017,
       0,   464,  3120,  3120,     0,     0,  2582,     0,     0,   839,
     552,   552,   552,   465,     0,  3123,     0,     0,   466,     0,
       0,  2092,   490,     0,  2583,  2584,  2585,  3018,     0,     0,
       0,     0,   491,     0,  3134,     0,     0,     0,   839,     0,
    2586,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   492,     0,     0,     0,     0,   493,
       0,     0,     0,     0,     0,     0,     0,   494,     0,   495,
     552,     0,     0,     0,     0,   496,     0,     0,     0,     0,
    1337,     0,     0,     0,     0,     0,     0,     0,   523,   523,
    1805,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2096,
     523,     0,     0,     0,     0,     0,     0,     0,     0,  3019,
       0,   467,   468,   469,     0,     0,     0,     0,     0,   523,
     470,     0,     0,     0,  1338,     0,     0,     0,     0,  3020,
       0,     0,   471,  2587,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   523,     0,
       0, -1218,     0,     0,  3021,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1377,   472,   473,     0, -1218,     0,
       0,     0,   474,     0,   475,  3022,     0,   476,     0,   477,
     478,   479,     0,     0,     0,   480,     0,   481,   616,     0,
       0,     0,     0,     0,   482,     0,  3023,     0,     0,     0,
       0,     0,     0,  1800,     0,  3024,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   483,
       0,     0,     0,     0,     0,     0,     0,    90,     0,    91,
       0,    92,     0,     0,     0,     0,    93,     0,   484,   760,
       0,     0,     0,     0,    94,     0,  1339,     0,     0,     0,
       0,     0,     0,     0,     0,   552,     0,     0,     0,     0,
       0, -1218, -1218, -1218,     0,     0,   485,     0,     0,     0,
   -1218,     0,     0,     0,     0,     0,     0,     0,    95,    96,
       0,     0, -1218,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    97,     0,     0,     0,     0,     0,   486,
       0,     0,     0,   552,    98,   552,     0,     0,    99,     0,
       0,  1340,     0,     0,     0, -1218,     0,     0,     0,     0,
       0,     0,   100,     0,     0,     0,     0, -1218,     0, -1218,
   -1218, -1218,     0,     0,     0, -1218,  -804, -1218,     0,     0,
       0,     0,     0,   487,     0,   101,     0,     0,     0,   552,
       0,   552,   102,     0,     0,   103,     0,   104,   488,   489,
       0,     0,     0,     0,     0,     0,     0,   105,  -804,     0,
    -804,  -804,  -804,  -804,  -804,  -804,  -804,  -804,  -804, -1218,
    -804,  -804,  -804,     0,  -804,  -804,  -804,  -804,  -804,  -804,
    -804,  -804,  -804,  -804,   106,   490,     0,     0,     0,  -804,
       0,     0,     0,     0,  -804,   491,   107,     0,  -804,     0,
       0,   108,   552,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   524,   524,   616, -1218,   492,     0,     0,
       0,     0,   493,     0,     0,     0,     0,     0,     0,   109,
     494,     0,   495,     0,     0,   524,   110,     0,   496,   111,
     112,     0,     0,   760,     0,     0,     0,     0,     0, -1218,
     113,     0,     0,     0,   524,     0,     0,   114,     0,   115,
       0,     0,   116,     0,     0,     0,  -804,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   524,     0,     0,     0,     0,     0,     0,
     616,   616,   616, -1218,     0,     0,     0,   616,   616,     0,
       0,     0,     0,   616,   616,   616,   117,   616, -1218, -1218,
       0,   118,     0,   119,     0,  1542,     0,     0,  1543,     0,
       0,  1544,     0,   120,     0,     0,  -804,     0,     0,  1545,
       0,  -804,  -804,  -804,     0,  -804,  -804,  -804,  -804,     0,
       0,     0,     0,     0,     0, -1218,     0,     0,     0,     0,
       0,     0,   121,  1341,     0, -1218,     0,     0,     0,     0,
       0,   616,     0,     0,     0,   122,     0,     0,     0,     0,
     616,   616,   616,   616,   552,     0,   552, -1218,     0,     0,
       0,  1546, -1218,     0,     0,     0,     0,     0,     0,     0,
   -1218,     0, -1218,     0,   123,   124,   125,     0, -1218,     0,
    1547,     0,     0,     0,     0,     0,   126,     0,     0,     0,
     760,     0,     0,     0,   552,     0,     0,     0,    90,   127,
      91,   128,    92,     0,     0,     0,     0,    93,   129,     0,
       0,     0,   130,     0,  1176,    94,     0,     0,     0, -2118,
       0,   131, -2118,     0,     0,     0,     0,     0,     0, -2118,
   -2118,     0,     0,   132,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -804,   133,     0,     0,   552,     0,    95,
      96,     0,     0,   134, -1999,     0, -1999,     0,   135,   136,
       0,     0,     0,     0,    97,   137,     0,   138,     0,     0,
    1548,  2325,     0,     0,     0,    98,     0,   139,  1549,    99,
       0,     0, -2118,     0,     0,     0,     0,     0,     0,     0,
       0,  -804,  1550,   100,     0,     0,     0,     0,     0,     0,
       0,  -804,     0,     0,     0,     0,     0,     0,     0,   141,
       0,     0,     0,     0,     0,     0,   101,   142,     0,     0,
       0,     0,   143,   102,  1551,     0,   103,     0,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,  -804,  1552,     0,  1553,     0,     0,
     144,     0,     0,     0,     0,   145,   146,     0,     0, -2118,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,  1554,     0,  1555,     0,     0,     0,   107,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0, -2118,     0,     0,  1556,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1557,     0,     0,     0,   616,
     109,     0,     0, -2118,     0,     0,     0,   110,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,     0,  1558,  1559,     0,     0,   114,     0,
     115,     0,     0,   116,   552,     0,   552,     0,     0,     0,
       0,     0,     0,     0,     0, -1999,     0,     0,     0,     0,
       0,     0,  1560,     0,     0,     0,     0,  2429, -2118,  1561,
       0,  1562,  1563,     0,     0,     0,     0,  1179,     0,     0,
       0,     0,     0,     0,     0,  1564,     0,   117,     0,     0,
    1565,     0,   118,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0, -2118,     0,  1566,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2118,     0,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,  1567, -1999,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
    1568,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   760,     0,     0,     0,
       0,     0,     0,     0,     0,   123,   124,   125,     0,     0,
       0,  1569,  2485,  2485,     0,     0,     0,   126,     0,     0,
   -2118,  1570, -2118, -2118,     0,     0,     0,  1571,     0,     0,
     127,     0,   128,     0,     0,     0, -2118,     0,     0,   129,
       0,     0,     0,   130,     0,     0,     0,     0,     0,     0,
       0,     0,   131,     0,     0,     0,     0,     0,     0, -2118,
       0,     0,     0,     0,   132,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   133,     0,     0,    90,     0,
      91,     0,    92,     0,   134,     0,     0,    93,     0,   135,
     136,     0,     0,     0,     0,    94,   137,     0,   138,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   139,     0,
       0,     0,     0,     0,   760,     0,     0,     0,     0,     0,
       0,     0,   140,     0,     0,     0,     0,     0,     0,    95,
      96,   760, -2118,   760,   760,     0,     0,     0,     0,   760,
     141,     0,     0,  1289,    97,     0,     0,     0,   142,     0,
       0,     0,   531,   143,     0,    98,     0,     0,     0,    99,
       0,     0,     0,   634,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
       0,   144,     0,     0,     0,     0,   145,   146,     0,     0,
       0,     0,   760,   760,     0,     0,   101,     0,     0,     0,
       0,     0,     0,   102,     0,     0,   103,   760,   104,     0,
       0,     0,     0,  2634,     0,     0,  2634,     0,   105,     0,
       0,     0,     0,     0,     0,  2634,  2634,  2634,  2642,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     531,     0,     0,     0,     0,   106,     0,     0,     0,   464,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,   108,     0,     0,     0,   466,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   760,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,   531,     0,     0,     0,   110,     0,     0,
     111,   112,     0,     0,     0,     0,   760,   760,     0,     0,
     760,   113,     0,     0,     0,   760,   760,     0,   114,     0,
     115,     0,     0,   116,     0,   531,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   760,     0,     0,     0,
       0,    90,     0,    91,  2741,    92,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,     0,   117,    94,   467,
     468,   469,   118,   760,   119,     0,     0,     0,   470,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,     0,    97,     0,     0,
       0,     0,     0,   472,     0,     0,   122,     0,    98,     0,
       0,     0,    99,     0,     0,   855,     0,   477,   478,   479,
       0,     0,     0,   480,     0,     0,   100,     0,     0,     0,
       0,     0,     0,     0,     0,   123,   124,   125,     0,     0,
       0,     0,   760,     0,     0,     0,     0,   126,     0,   101,
       0,     0,     0,     0,     0,     0,   102,     0,     0,   103,
     127,   104,   128,     0,     0,     0,     0,   483,     0,   129,
       0,   105,     0,   130,     0,     0,     0,     0,     0,     0,
       0,     0,   131,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   132,     0,     0,   616,   106,     0,
       0,     0,   616,     0,     0,   133,     0,     0,   856,     0,
     107,     0,   760,     0,   134,   108,     0,     0,     0,   135,
     136,     0,     0,     0,     0,     0,   137,     0,   138,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   139,     0,
       0,     0,     0,   109,     0,     0,     0,   486,     0,     0,
     110,     0,  1083,   111,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,     0,     0,     0,     0,     0,
     141,   114,     0,   115,     0,     0,   116,     0,   142,     0,
       0,     0,     0,   143,     0,     0,     0,     0,   616,     0,
       0,     0,   616,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   488,   489,     0,     0,
       0,   144,     0,     0,     0,     0,   145,   146,     0,     0,
     117,     0,     0,     0,     0,   118,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   491,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,   492,     0,     0,     0,   122,
     493,     0,     0,     0,     0,     0,     0,     0,   494,     0,
     495,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   124,
     125,     0,     0,     0,     0,     0,   254,     0,   255,     0,
     126,     0,     0,   256,     0,     0,     0,     0,     0,     0,
       0,   257,     0,   127,     0,   128,     0,     0,     0,     0,
       0,     0,   129,     0,     0,     0,   130,     0,     0,     0,
       0,     0,     0,     0,     0,   131,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   258,   259,   132,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   133,     0,
     260,     0,     0,     0,     0,     0,     0,   134,     0,     0,
       0,   261,   135,   136,     0,   262,     0,     0,     0,   137,
       0,   138,     0,     0,     0,     0,     0,     0,     0,   263,
       0,   139,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   462,     0,     0,
     463,     0,   264,   918,   919,   920,     0,     0,     0,     0,
       0,   921,   265,   141,   266,     0,     0,     0,     0,     0,
       0,   142,     0,     0,     0,   267,   143,     0,   268,   269,
     270,   271,   272,   273,   274,   275,     0,   276,   277,   278,
       0,   279,   280,   281,   282,   283,   284,   285,   286,   287,
     288,   289,     0,     0,   144,     0,     0,   922,     0,   145,
     146,     0,     0,   290,     0,     0,     0,     0,   291,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   464,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   292,     0,   466,     0,
       0,     0,     0,   293,     0,     0,   294,   295,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   296,     0,     0,
       0,     0,     0,     0,   297,     0,   298,     0,     0,   299,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   923,     0,     0,     0,
       0,     0,     0,   300,     0,     0,     0,   924,   301,     0,
     302,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     303,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   467,   468,   469,     0,     0,     0,     0,     0,     0,
     470,     0,     0,     0,     0,     0,   925,   926,     0,   304,
       0,     0,   471,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   305,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1317,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   472,   473,   927,   928,     0,
       0,   306,   474,     0,   475,     0,     0,   476,     0,   477,
     478,   479,     0,   307,     0,   480,     0,   481,     0,     0,
       0,     0,     0,     0,   482,     0,     0,     0,   308,     0,
       0,     0,     0,     0,     0,   309,     0,   929,     0,   310,
       0,     0,     0,     0,   930,     0,     0,     0,   311,   931,
       0,     0,     0,     0,     0,     0,     0,   932,     0,   483,
     312,     0,     0,     0,   933,     0,     0,     0,     0,   934,
       0,   313,     0,     0,     0,     0,     0,     0,   484,     0,
     314,     0,     0,     0,     0,   315,   316,     0,   935,     0,
       0,     0,   317,     0,   318,   462,     0,     0,   463,     0,
       0,   918,   919,   920,   319,     0,   485,     0,     0,   921,
       0,     0,     0,     0,     0,     0,     0,     0,   320,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   321,     0,     0,   486,
       0,     0,     0,     0,   322,     0,     0,     0,     0,   323,
       0,     0,     0,     0,     0,   922,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   324,     0,   464,
       0,     0,     0,   487,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   466,     0,   488,   489,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   936,     0,   937,     0,   938,     0,     0,
     939,     0,   940,   941,   942,   490,     0,   943,   944,     0,
       0,     0,     0,     0,     0,   491,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   923,   462,     0,   492,   463,     0,
       0,     0,   493,     0,     0,   924,     0,     0,     0,     0,
     494,     0,   495,     0,     0,     0,     0,     0,   496,     0,
   -1062,     0,     0,     0,     0, -1062,     0,     0, -1062,   467,
     468,   469,     0,     0,     0, -1062, -1062,     0,   470,     0,
       0,     0,     0,     0,   925,   926,     0,     0,     0,     0,
     471,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1062,     0, -1062,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   464,
       0,     0,     0,   472,   473,   927,   928,     0, -1062,     0,
     474,     0,   475,     0,     0,   476,   466,   477,   478,   479,
       0,     0,     0,   480,     0,   481,     0,     0,     0,     0,
       0,     0,   482,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   929,     0,     0,     0,     0,
       0,     0,   930,     0,     0,     0,     0,   931,     0,     0,
       0,     0,     0,     0,     0,   932,  -984,   483,     0,  -984,
       0,     0,   933,     0,     0,     0,     0,   934,     0,     0,
       0,     0,     0,     0,     0, -1062,   484,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   935,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     445,     0,     0,     0,   485,     0,     0, -1062,     0,   467,
     468,   469,     0,     0,     0,     0,     0,     0,   470,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -1062,
     471,     0,     0,     0,     0,     0,     0,   486,     0,     0,
    -984,     0,     0,     0,     0, -2005,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -984,     0,     0,
       0,     0,     0,   472,   473,     0,     0,     0,     0,     0,
     474, -1062,   475,     0,     0,   476,     0,   477,   478,   479,
       0,   487,     0,   480, -1062,   481,     0,   462,     0,     0,
     463,     0,   482, -1062,     0,     0,   488,   489,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   462,     0,     0,   463,     0,     0,     0,     0,     0,
       0,   936, -1062,   937,     0,   938,     0,   483,   939,     0,
     940,   941,   942,   490,     0,   943,   944,     0,     0,     0,
       0,     0, -1062,   491,     0,     0,   484,     0,     0,     0,
       0,     0,     0,     0,     0, -1062,     0,     0,     0,     0,
    -984,  -984,  -984,     0,     0,   492,     0,     0,     0,  -984,
     493,   464,     0,     0,   485,     0,     0,     0,   494,     0,
     495,  -984,     0,     0,     0,     0,   496,     0,   466,     0,
       0,     0,     0,     0,     0,   464,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1062,   486, -1062, -1062,
       0,     0,   466,     0,  -984,  -984,     0,     0,     0,     0,
       0,  -984, -1062,  -984,     0,     0,  -984,     0,  -984,  -984,
    -984,     0,     0,     0,  -984,     0,  -984,     0,   462,     0,
       0,   463,     0,  -984,     0, -1062,     0,     0,     0,     0,
       0,   487,     0,     0,     0,     0,     0,     0, -2118,     0,
       0,     0,     0,     0,     0,     0,   488,   489,     0,     0,
       0,     0,     0,  1389,     0,     0,     0,     0,  -984,     0,
       0,     0,     0,  -984,     0,     0,     0,     0,     0,     0,
       0,   467,   468,   469,     0,     0,     0,  -984,     0,     0,
     470,     0,   347,   490,     0,     0,     0,     0,     0,     0,
       0,     0,   471,   491,     0,   467,   468,   469, -1062,     0,
       0,     0,   464,     0,   470,  -984,     0,     0,     0, -1062,
       0,     0,     0,     0,     0,   492,   471, -2005,   462,   466,
     493,   463,     0,     0,     0,   472,   473,     0,   494, -1062,
     495,     0,   474,     0,   475,     0,   496,   476,  -984,   477,
     478,   479,     0,     0,     0,   480,     0,   481,     0,   472,
     473,     0,     0,     0,   482,     0,   474,     0,   475,     0,
       0,   476,     0,   477,   478,   479,     0,     0,     0,   480,
       0,   481,     0, -2118,     0,     0,  -984,     0,   482,     0,
       0,     0,  -984,     0,     0,     0,     0,     0,     0,   483,
       0,     0,     0,     0,     0,     0,     0,  -984,  -984,     0,
       0,     0,   464,     0,     0,     0,     0,     0,   484,     0,
       0,     0,     0,   483,   617,     0,     0,     0,   462,   466,
       0,   463,   467,   468,   469,     0,     0,     0,     0,     0,
       0,   470,   484,     0,  -984,     0,   485,     0,     0,     0,
       0,     0,     0,   471,  -984,     0,     0,     0,     0,     0,
       0,     0,  -984,     0,     0,     0,     0,     0,     0,     0,
     485,     0,     0,     0,     0,     0,  -984,   348,     0,   486,
       0,  -984,     0,     0, -2005,     0,   472,   473,     0,  -984,
       0,  -984,     0,   474,     0,   475,     0,  -984,   563,     0,
     477,   478,   479,   486,     0,     0,   480,     0,   481,     0,
       0,     0,   464,     0,     0,   482,     0,     0,     0,     0,
       0,     0,     0,   487,   692,     0,     0,     0,     0,   466,
       0,     0,   467,   468,   469,     0,     0,     0,   488,   489,
       0,   470,   462,     0,     0,   463,     0,   487,     0,     0,
     483,     0,     0,   471,     0,   564,     0,     0,     0,     0,
       0,     0,   488,   489,     0,     0,     0,     0,     0,   484,
       0,     0,     0,     0,     0,   490,     0,     0,     0,     0,
       0,   462,     0,     0,   463,   491,   472,   473,     0,     0,
       0,     0,  1390,   474,     0,   475,     0,   485,   476,   490,
     477,   478,   479,  1391,     0,     0,   480,   492,   481,   491,
       0,     0,   493,     0,     0,   482,     0,     0,     0,     0,
     494,   634,   495,     0,     0,     0,   464,     0,   496,     0,
     486,   492,   467,   468,   469,     0,   493,     0,     0,     0,
       0,   470,     0,   466,   494,     0,   495,     0,     0,     0,
     483,     0,   496,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   464,     0,     0,     0,   484,
       0,     0,     0,     0,   487,     0,     0,     0,     0,     0,
       0,     0,   466,     0,     0,     0,   472,   473,     0,   488,
     489,     0,     0,   474,     0,   475,     0,   485,   476,     0,
     477,   478,   479,     0,     0,     0,   480,     0,   481,     0,
       0,     0,     0,     0,     0,   482,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   490,     0,     0,     0,
     486,     0,     0,     0,     0,     0,   491,     0,     0,     0,
       0,   462,     0,     0,   463,     0,   467,   468,   469,     0,
     483,     0,     0,     0,     0,   470,     0,     0,   492,     0,
       0,     0,     0,   493,     0,     0,     0,   471,     0,   484,
       0,   494,     0,   495,   487,     0,     0,     0,     0,   496,
       0,     0,     0,     0,     0,   467,   468,   469,     0,   488,
     489,     0,     0,     0,   470,     0,     0,   485,     0,     0,
     472,   473,     0,     0,     0,     0,   471,   474,     0,   475,
       0,     0,   476,     0,   477,   478,   479,     0,     0,     0,
     480,     0,   481,     0,     0,   464,   490,     0,     0,   482,
     486,     0,     0,     0,     0,     0,   491,     0,     0,   472,
     473,     0,   466,     0,     0,     0,   474,     0,   475,     0,
       0,   476,     0,   477,   478,   479,     0,     0,   492,   480,
       0,   481,     0,   493,   483,     0,     0,     0,   482,     0,
       0,   494,     0,   495,   487,     0,     0,     0,     0,   496,
       0,     0,     0,   484,     0,     0,     0,     0,     0,   488,
     489,     0,   462,     0,     0,   463,     0,     0,     0,     0,
       0,     0,     0,   483,     0,     0,     0,     0,   564,     0,
       0,   485,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   484,     0,     0,     0,   490,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   491,     0,     0,     0,
       0,     0,     0,     0,   486,   467,   468,   469,   462,     0,
     485,   463,     0,     0,   470,     0,     0,     0,   492,     0,
       0,     0,     0,   493,     0,     0,   471,     0,     0,     0,
       0,   494,     0,   495,     0,     0,   464,     0,     0,   496,
       0,     0,  1072,   486,     0,     0,     0,     0,   487,     0,
       0,     0,     0,   466,     0,     0,     0,     0,     0,   472,
     473,     0,     0,   488,   489,     0,   474,     0,   475,     0,
       0,   476,     0,   477,   478,   479,     0,     0,     0,   480,
       0,   481,     0,     0,     0,     0,   827,   487,   482,     0,
       0,     0,   464,     0,     0,     0,     0,     0,     0,     0,
     490,     0,   488,   489,     0,     0,     0,     0,     0,   466,
     491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   462,   483,     0,   463,     0,     0,     0,     0,
       0,     0,   492,     0,     0,     0,     0,   493,     0,   490,
       0,     0,   484,     0,     0,   494,     0,   495,     0,   491,
       0,     0,     0,   496,     0,     0,   467,   468,   469,     0,
    1079,     0,     0,     0,     0,   470,     0,     0,     0,     0,
     485,   492,     0,     0,     0,     0,   493,   471,     0,     0,
       0,     0,     0,     0,   494,     0,   495,     0,     0,     0,
       0,     0,   496,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   486,     0,     0,   464,     0,     0,     0,
     472,   473,   467,   468,   469,     0,     0,   474,     0,   475,
       0,   470,   476,   466,   477,   478,   479,     0,     0,     0,
     480,     0,   481,   471,     0,     0,     0,     0,     0,   482,
       0,     0,     0,     0,     0,     0,     0,   487,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   488,   489,     0,     0,   472,   473,     0,     0,
       0,     0,     0,   474,   483,   475,     0,     0,   476,     0,
     477,   478,   479,     0,     0,     0,   480,     0,   481,     0,
       0,     0,     0,   484,     0,   482,     0,     0,     0,   490,
       0,     0,     0,     0,     0,     0,     0,  1241,     0,   491,
     463,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   485,     0,     0,     0,     0,   467,   468,   469,     0,
     483,   492,     0,     0,     0,   470,   493,     0,     0,     0,
       0,     0,     0,     0,   494,     0,   495,   471,     0,   484,
       0,     0,   496,     0,   486,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   485,     0,     0,
     472,   473,     0,     0,     0,     0,     0,   474,     0,   475,
       0,   464,   476,     0,   477,   478,   479,     0,   487,     0,
     480,     0,   481,     0,     0,     0,     0,     0,   466,   482,
     486,     0,     0,   488,   489,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   483,     0,     0,     0,  1770,   464,
     490,     0,     0,     0,   487,     0,     0,     0,     0,     0,
     491,     0,     0,   484,     0,     0,   466,     0,     0,   488,
     489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   492,     0,     0,     0,     0,   493,     0,     0,
       0,   485,     0,     0,     0,   494,     0,   495,     0,     0,
       0,     0,     0,   496,     0,     0,   490,     0,     0,     0,
       0,   467,   468,   469,     0,     0,   491,     0,     0,     0,
     470,     0,     0,     0,   486,     0,     0,     0,     0,     0,
       0,     0,   471,     0,     0,     0,     0,     0,   492,     0,
       0,     0,     0,   493,     0,     0,     0,     0,     0,     0,
       0,   494,     0,   495,     0,     0,     0,     0,     0,   496,
       0,     0,     0,     0,     0,   472,   473,     0,   487,   467,
     468,   469,   474,     0,   475,     0,     0,   476,   470,   477,
     478,   479,     0,   488,   489,   480,     0,   481,     0,     0,
     471,     0,     0,     0,   482,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     490,     0,     0,   472,     0,     0,     0,     0,     0,   483,
     491,     0,     0,     0,     0,   476,     0,   477,   478,   479,
       0,     0,     0,   480,     0,   481,     0,     0,   484,     0,
       0,     0,   492,     0,     0,     0,     0,   493,     0,     0,
       0,     0,     0,     0,     0,   494,     0,   495,     0,     0,
       0,     0,  2661,   496,     0,     0,   485,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   483,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -438,     0,     0,  -438,     0,   486,
    -438,  -438,  -438,  -438,  -438,  -438,  -438,  -438,  -438,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   485,     0,     0,     0,     0,     0,
       0,     0,  -438,     0,  -438,     0,     0,     0,     0,     0,
       0,     0,  -438,   487,  -438,  -438,  -438,  -438,  -438,  -438,
    -438,     0,     0,     0,     0,     0,     0,   486,   488,   489,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -438,     0,     0,     0,     0,   490,     0,     0,     0,     0,
       0,   487,     0,     0,     0,   491,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   488,   489,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   492,     0,     0,
       0,     0,   493,     0,     0,     0,     0,  -438,     0,     0,
     494,     0,   495,     0,     0,     0,     0,     0,   496,     0,
       0,     0,     0,   490,  1109,     0,     0,     0,  -438,  -438,
    -438,  -438,  -438,   491,     0,  -438,  -438,     0,     0,  -438,
       0,     0,     0,     0,     0,  -438,     0,  -438,     0,     0,
       0,     0,     0,  -438,     0,   492,     0,     0,  -438,     0,
     493,  -438,     0,     0,     0,     0,  1108,     0,   494,  -438,
     495,     0,     0,     0,     0,     0,   496,     0,     0,     0,
       0,     0,  -438,     0,     0,  -438,  -438,     0,     0,     0,
       0,  -438,   463,  -438,     0,     0,     0,     0,     0,     0,
       0,     0,  -438,  -438,     0,  -438,  -438,  -438,  -438,  -438,
    -438,  -438,     0,     0,     0,     0,  -438,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -438,     0,     0,     0,   629,     0,     0,     0,     0,     0,
    -438,     0,  -438,     0,     0,     0,  -438,     0,  -438,  -438,
    -438,  -438,  -438,  -438,  -438,     0,     0,     0,     0,     0,
       0,  -438,     0,  -438,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   464,  -438,  -438,     0,     0,     0,     0,
       0,     0,     0,  -438,     0,     0,  -438,     0,     0,     0,
     466,     0,     0,     0,  -438,     0,     0,     0,     0,     0,
       0,  -438,     0,  -438,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -438,     0,     0,     0,     0,     0,     0,  -438,     0,
    -438,  -438,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -438,     0,     0,     0,     0,  1109,  -438,
       0,     0,  -438,  -438,  -438,  -438,  -438,     0,     0,  -438,
    -438,  -438,     0,  -438,  -438,  -438,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -438,     0,     0,
       0,     0,   630,   467,   468,   469,     0,     0,     0,     0,
       0,     0,   470,  -438,     0,     0,     0,  -438,     0,     0,
       0,     0,     0,     0,   471,     0,  -438,     0,     0,  -438,
       0,  -438,     0,     0,     0,  -438,  -438,     0,     0,     0,
       0,  -438,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -438,     0,     0,   631,   472,   632,     0,
    -438,     0,     0,     0,  -438,  -438,  -438,     0,     0,   476,
       0,   477,   478,   479,  -438,  2066,     0,   480,  -438,   481,
       0,     0,     0,  -438,     0,  -438,     0,     0,     0,     0,
       0,  2067,  1102,     0,  2068,  2069,  2070,  2071,  2072,  2073,
    2074,     0,     0,     0,     0,  -438,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -438,
       0,   483,     0,     0,     0,     0,     0,  -438,     0,     0,
    -438,     0,     0,     0,     0,  2075,     0,   987,   988,  2076,
    2077,  2078,  2079,  2080,     0,  -438,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -438,     0,     0,     0,     0,     0,     0,     0,   485,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2081,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   486,     0,     0,  1176,     0, -2118,     0,     0, -2118,
       0,  1177, -2118,     0,     0,  -438,     0,  -438,  -438,  -438,
   -2118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -1999,   487, -1999,     0,  -438,     0,
       0,  2082,  2083,  2084,  2085,  2086,     0,     0,   996,   997,
     488,   489,     0,     0,     0,     0,     0,     0,     0,     0,
    -438,     0, -2118,     0,     0,     0, -2118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -438,     0,     0,
       0, -2118,  2087,     0,     0,     0,     0,   490,  -438,  -438,
    -438,     0,     0,     0,     0,   425,     0,   491,  2088,     0,
       0,     0,  -438,     0, -2090,     0,     0,     0,     0,  -438,
   -1275,     0,     0,     0,     0,     0,  1102,     0,     0,   492,
       0,     0,     0,     0,   493,     0,     0,     0,     0, -1275,
       0,     0,   494,   634,   495,     0,     0,     0,     0,     0,
     496,     0,     0,  2089,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2068,  2069,  2070,  2071,  2072,  2073,
       0, -2118,     0,     0,  2090,     0,     0,     0,  1178, -2118,
       0,     0,     0,     0,     0,     0,     0,     0,  2091,     0,
       0,     0,     0, -2118,     0,     0,  2092,     0,     0,  2093,
       0,     0,     0,     0,     0,  2075,     0,   987,   988,  2076,
    2077,  2078,  2079,  2080,  2094,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2118,     0,     0,     0,  2095,
       0,     0,     0,     0,     0, -1999,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2118,     0, -2118,     0,
       0,     0,     0,  2081,     0,     0,     0,  1179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2118,     0, -2118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2096,     0,  2097,  2098,  2099,     0,
       0,     0,     0,     0,     0, -2118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2100,     0, -1999,
       0,  2082,  2083,  2084,  2085,  2086,     0,     0,   996,   997,
       0,     0,     0,     0,     0, -2118, -2118,     0,     0,  -435,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2090,     0,     0,     0,
       0,     0,  2087, -2118,     0,     0,     0,  2101,  2102,  2103,
   -2118,     0, -2118, -2118,     0,     0,     0,     0,     0,     0,
       0,  2104,     0,     0,     0,     0, -2118,     0,  1800,     0,
    2783, -2118,     0,  2784,     0,     0,  2785,  2068,  2069,  2070,
    2071,  2072,  2073,  2786,  2787,     0,     0,     0,     0, -2118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2089,     0,     0,     0,     0,  1534,     0,
    1535,     0,     0,     0,     0,     0,     0,     0,  2075, -2118,
     987,   988,  2076,  2077,  2078,  2079,  2080,     0,     0,     0,
   -2001, -2118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2091,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2093,
       0,     0, -2118,     0,     0,     0,  2081,     0,     0,     0,
       0,     0, -2118,     0,  2094,     0,     0,     0, -2118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   634,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     464,     0,     0,  2788,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   466,     0,     0,
       0,     0,     0,     0,  2082,  2083,  2084,  2085,  2086,     0,
       0,   996,   997,     0,     0,  2789,  2097,  2098,  2099,     0,
       0,  2790,     0,  2791,     0,     0,     0,     0,     0, -2036,
       0,     0,     0,     0,  2792,     0,     0,  2793,     0,     0,
       0,     0,     0,     0,     0,  2087,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   425,     0,
       0,  2088,     0,     0,     0,     0,     0,     0,     0,  2794,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2795,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2796,     0,     0,     0,     0,  2101,  2102,  2103,
     467,   468,   469,     0,     0,   464,  2089,     0,     0,   470,
       0,     0,     0,     0,     0,     0,  2797,     0,  1555,     0,
       0,   471,   466,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2798,     0,  1556,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2799,  2091,     0,     0,   472,   510,     0,     0,     0,  2092,
       0,   474,  2093,   475,     0,     0,   476,     0,   477,   478,
     479,     0,     0,     0,   480,     0,   481,  2094,     0,  2800,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2801,     0,     0,
       0,     0,     0,     0,  1561,     0,  1562,  1563,   483,     0,
       0,     0,     0,     0,     0,   467,   468,   469,     0,     0,
       0,     0,     0,     0,   470,  2802,     0,   484,     0,   464,
       0,     0,     0,     0,     0,     0,   471,  2096,     0,  2097,
    2098,  2099,     0,     0,     0,     0,   466,     0,     0,     0,
       0,     0,     0,     0,     0,   485,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   472,
     510,     0,     0,  2803,     0,     0,   474,     0,   475,     0,
       0,   476,     0,   477,   478,   479,     0,  2804,   486,   480,
       0,   481,  -699,     0,     0,     0,     0,  2805,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2806,     0,     0,     0,
    2101,  2102,  2103,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   487,   483,  2104,     0,     0,     0,     0,  2807,
       0,  1800,   464,     0,     0,     0,     0,   488,   489,   467,
     468,   469,   484,     0,     0,     0,     0,     0,   470,   466,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,  1223,     0,     0,     0,     0,     0,     0,     0,
     485,     0,   511,     0,   490,     0,   512,   513,     0,     0,
       0,     0,     0,     0,   491,     0,     0,     0,     0,     0,
       0,     0,     0,   472,   510,     0,     0,     0,     0,     0,
     474,     0,   475,   486,     0,   476,   492,   477,   478,   479,
       0,   493,     0,   480,     0,   481,     0,     0,     0,   494,
     464,   495,     0,     0,     0,     0,     0,   496,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   466,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   487,     0,     0,
       0,   464,   467,   468,   469,     0,     0,   483,     0,     0,
       0,   470,   488,   489,     0,     0,     0,     0,   466,     0,
       0,     0,     0,   471,     0,     0,   484,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1883,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   511,     0,   490,
       0,   512,   513,     0,   485,     0,   472,   510,     0,   491,
       0,     0,     0,   474,     0,   475,     0,     0,   476,     0,
     477,   478,   479,     0,     0,     0,   480,     0,   481,     0,
       0,   492,     0,     0,     0,     0,   493,   486,     0,     0,
     467,   468,   469,     0,   494,     0,   495,     0,     0,   470,
       0,     0,   496,     0,     0,     0,     0,     0,     0,     0,
       0,   471,     0,     0,     0,     0,     0,     0,     0,     0,
     483,   467,   468,   469,     0,     0,     0,     0,     0,     0,
     470,   487,     0,     0,     0,     0,     0,     0,     0,   484,
       0,     0,   471,     0,   472,   510,   488,   489,     0,     0,
       0,   474,     0,   475,  1886,     0,   476,     0,   477,   478,
     479,     0,     0,     0,   480,     0,   481,   485,     0,     0,
       0,   464,     0,     0,     0,   472,   510,     0,     0,     0,
       0,   511,   474,   490,   475,   512,   513,   476,   466,   477,
     478,   479,     0,   491,     0,   480,     0,   481,     0,     0,
     486,     0,     0,     0,     0,     0,     0,     0,   483,     0,
       0,     0,     0,     0,     0,   492,     0,     0,     0,     0,
     493,     0,     0,     0,     0,     0,     0,   484,   494,     0,
     495,     0,     0,     0,     0,     0,   496,     0,     0,   483,
       0,     0,     0,     0,   487,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   485,     0,     0,   484,   488,
     489,     0,     0,     0,     0,     0,     0,  1888,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   485,     0,   486,     0,
       0,   467,   468,   469,   511,     0,   490,     0,   512,   513,
     470,     0,     0,     0,     0,     0,   491,     0,     0,     0,
       0,     0,   471,   464,     0,     0,     0,     0,     0,   486,
       0,     0,     0,     0,     0,     0,     0,     0,   492,     0,
     466,     0,   487,   493,     0,     0,     0,     0,     0,     0,
       0,   494,     0,   495,     0,   472,   510,   488,   489,   496,
       0,   464,   474,     0,   475,     0,     0,   476,     0,   477,
     478,   479,     0,   487,     0,   480,     0,   481,   466,     0,
       0,     0,  1907,     0,     0,     0,     0,     0,   488,   489,
       0,     0,   511,     0,   490,     0,   512,   513,     0,     0,
       0,     0,     0,     0,   491,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   483,
       0,     0,     0,   511,     0,   490,   492,   512,   513,     0,
       0,   493,     0,     0,     0,   491,     0,     0,   484,   494,
       0,   495,     0,   467,   468,   469,     0,   496,     0,     0,
       0,     0,   470,     0,     0,     0,     0,   492,     0,     0,
       0,     0,   493,     0,   471,     0,   485,     0,     0,     0,
     494,     0,   495,     0,     0,     0,     0,     0,   496,     0,
       0,   467,   468,   469,     0,     0,     0,     0,     0,     0,
     470,     0,     0,     0,     0,     0,     0,   472,   510,   486,
       0,     0,   471,     0,   474,     0,   475,     0,     0,   476,
       0,   477,   478,   479,     0,     0,     0,   480,     0,   481,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   472,   510,     0,     0,     0,
       0,     0,   474,   487,   475,     0,     0,   476,     0,   477,
     478,   479,     0,     0,     0,   480,     0,   481,   488,   489,
       0,   483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     484,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   490,     0,   512,     0,   483,
       0,     0,     0,     0,     0,   491,     0,     0,   485,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   484,     0,
       0,     0,     0,     0,     0,     0,     0,   492,     0,     0,
       0,     0,   493,     0,     0,  3110,     0,     0,     0,     0,
     494,   486,   495,     0,     0,     0,   485,     0,   496,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   486,
       0,     0,     0,     0,     0,   487,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     488,   489,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   487,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   490,   488,   489,
       0,     0,     0,     0,     0,     0,     0,   491,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   492,
       0,     0,     0,     0,   493,   490,     0,     0,     0,     0,
       0,     0,   494,     0,   495,   491,     0,     0,     0,     0,
     496,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   492,     0,     0,
       0,     0,   493,     0,     0,     0,     0,     0,     0,     0,
     494,     0,   495,     0,     0,     0,     0,     0,   496
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-2462))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
     221,   222,   463,   545,   225,  1014,   338,   380,   719,   720,
     721,  1319,  1594,  1074,    42,   556,   217,   434,   426,   209,
     428,   354,   223,   431,    81,   367,  1295,   360,   335,  1090,
    1276,  1541,  1813,   417,  1537,   665,   342,  1540,  1802,  1800,
     687,   552,  1806,   678,  1788,   923,  1379,  1426,  1806,   331,
    1806,   344,   352,   473,  1832,  1405,   823,  1974,   552,   352,
       1,   330,     1,   913,  1838,  1732,   833,   367,     1,     9,
      22,  2061,     9,  1698,   367,   827,  2158,  2159,     9,    50,
       9,    50,     6,    49,    94,  2466,    95,   629,  2547,    32,
     510,   115,    17,  2143,   339,    17,   979,    32,    63,    63,
      61,   134,     7,   168,   349,  1443,   104,   105,   122,    28,
     133,   356,   357,  1271,    77,   656,    31,   899,   133,  1863,
    1864,  1865,   616,   899,   194,     1,  1870,  1871,   118,     9,
    1190,   376,    77,   111,  1878,   156,   118,  2127,  1198,   193,
      50,   175,   632,  1301,   334,   100,   336,    50,   138,     6,
    1518,  2612,    21,  2047,  1037,   907,    71,   244,    73,   407,
     544,  1725,  2157,   104,   111,   800,   221,   272,   221,     0,
     276,   361,   683,   251,   364,   207,    70,   276,   330,   369,
    2151,   221,   403,    63,   292,   251,    34,   103,   333,   683,
    1934,  1773,    95,   383,   415,    50,   338,  1355,   110,  1943,
     421,   422,  1946,   424,   123,    22,   207,  2494,   429,   430,
       1,   631,   632,   434,    77,   134,   387,   194,   144,  1465,
     604,    63,    33,   444,   976,   776,    21,   263,    28,   322,
     293,    77,   296,   606,   221,   377,    68,   237,   438,   441,
     206,   251,  1303,   512,   526,   426,   354,   282,   237,    79,
    2144,   175,   214,  1404,   499,  1413,    77,   498,   218,  2515,
     384,   427,   276,   369,   222,    97,    98,   441,  2555,  2556,
    2668,     1,   139,   276,     0,     6,   143,   495,   192,   808,
     809,   810,   219,    77,   182,   329,     6,   328,    77,   330,
     182,   802,   333,   251,   139,   293,  2378,   338,   143,   251,
     593,  2699,   367,   687,   637,   834,   309,   231,   489,    63,
     551,   604,   179,   354,   276,   195,   421,     6,   232,   360,
     522,   502,   695,   123,   446,   570,   507,  1079,   745,   495,
     235,   236,   962,  2323,   179,   251,   184,   144,    68,   339,
     585,   382,   331,   384,  1969,   276,  1971,  2603,   522,   757,
     255,   551,   276,   543,   447,   342,   420,   272,   188,   511,
     512,   513,   286,   361,   554,    63,   877,    97,    98,   830,
    1880,   892,   617,   544,   144,   675,   500,   282,   387,   294,
     260,   416,   675,  1733,    77,   805,  2673,   680,     9,   315,
     247,   392,   247,   459,   502,   458,   917,   460,   310,   507,
     522,   232,   233,    62,   309,  2299,   551,   451,   368,   330,
     446,    70,  2186,   797,   335,   334,   387,   495,   387,   251,
     296,   140,  2393,   344,   465,   567,   217,   319,   251,   256,
    2480,   352,   186,   256,   251,   406,   491,   958,  2002,   287,
     395,   356,   687,   548,   689,   551,   367,  1325,   868,   694,
       6,   375,   551,   506,  1084,   542,  1334,   314,  2202,  1226,
    1618,  1311,   496,   495,  1314,   276,   506,  2928,  2458,   432,
     511,   512,   513,   193,   495,  2470,   333,   387,   862,  2257,
     169,   551,  1023,     0,   387,  1853,   551,   432,   242,     9,
     980,  1012,   222,  1831,  1246,   536,   406,   551,   539,   404,
     231,  1052,   750,   485,   888,   528,   124,  2888,   490,   487,
    1292,   231,   452,   528,   346,   899,  1292,   550,  2899,  1579,
     827,   251,   743,   478,   495,   490,   550,   551,   452,   492,
     751,    51,   801,   734,   755,   544,   546,   551,  1421,   529,
     487,   435,   231,   509,   420,   276,   341,   492,  1059,   818,
     819,   820,   473,   551,   384,   286,   276,   778,     6,   177,
     980,   477,   831,   506,   486,   786,   286,  1197,  2462,   432,
     259,   506,   496,   544,   551,   544,   367,   133,   779,  2629,
     504,   542,   827,     6,   550,   495,   432,   276,   552,   510,
     511,   512,   513,  2351,   978,  2351,   637,   286,  3057,   551,
     907,  2268,     9,  1754,   293,   874,   912,   505,  1669,   276,
     551,   432,   551,   505,   551,   530,   346,   557,   551,   495,
     551,  2703,   551,   548,   665,  2250,   547,   551,   497,   492,
     289,   550,   512,   557,   544,   550,   153,   551,   432,   508,
     157,   544,   547,   432,   375,   550,   492,   504,  2426,   801,
     457,   692,   187,   898,  1289,   375,   808,   809,   810,   140,
     502,   906,   907,   495,  1081,   507,   818,   819,   820,   976,
     500,   492,   593,   973,   956,   231,  2666,  2056,   140,   831,
     973,  1065,   834,   604,   551,   383,   375,  2540,   309,   879,
    1998,   843,   844,   845,   846,   847,  1029,  3078,   492,  1241,
     945,   218,   497,   492,   495,   550,   551,  1124,   367,   244,
     144,   231,  1096,   508,  1344,   232,   233,   252,     0,   438,
     276,   452,   874,   550,  2731,   495,   445,  1885,   551,  1738,
     286,   976,   452,  1244,  1016,  2509,    71,   276,    73,   432,
     494,   135,  2659,   263,   665,   265,  2731,   144,   502,   503,
    1244,   260,  1169,   507,   675,  1385,   276,   378,  1051,   680,
     801,  1145,  2615,   452,   330,   495,   286,   808,   809,   810,
    2437,   430,  1079,   504,   293,    40,   435,   818,   819,   820,
     821,   551,   823,   231,   504,   132,   255,   260,   354,  1947,
     831,  1949,   833,   834,   360,  1215,   353,  1181,   276,   492,
     230,  1322,   843,   844,   845,   846,   847,   342,   231,   502,
    1055,    31,   276,   282,   507,   504,   175,    39,   970,   375,
     551,  1066,  2731,  1673,   231,    47,   557,   276,   276,  1987,
     550,   551,   551,   874,  1079,   321,   495,   557,   286,  1606,
     309,   459,  1063,  1394,  1395,  1396,  1397,  1398,  1399,  1400,
    1401,   368,   263,   276,   472,   375,   121,   291,   189,  2731,
    1381,     0,   551,   286,   482,   196,  2731,   276,   557,   276,
     276,   153,   277,   355,   322,   157,   333,   255,   217,   286,
     801,  2039,  2040,    62,   551,   190,    65,   808,   809,   810,
     557,    70,  1113,   276,   291,   115,   452,   818,   819,   820,
     821,   489,   823,   491,   282,   333,   827,   473,  1292,   276,
     831,    35,   833,   834,  1666,  2442,  1668,   137,  2268,    27,
      29,   962,   843,   844,   845,   846,   847,   375,   221,   970,
      33,   309,   452,    62,   490,  2268,   218,   272,  2317,  1246,
    2319,    70,  1211,  1777,   510,   511,   512,   513,   504,  1170,
     232,   233,   375,   874,   313,  1156,  1157,   438,  1179,   318,
     342,  2739,  1714,   183,   445,   282,  1259,  2731,   375,  1389,
     536,   490,   528,  2731,  1015,  2731,   438,   103,   495,    11,
    1262,  1628,   376,   445,   504,   441,   907,   207,  1029,   261,
    2989,   256,   309,   550,  2775,   551,  1380,   454,  3005,   447,
    1041,   557,   222,   512,   452,   355,  3005,   437,   230,  1530,
    2644,  1532,   551,   333,   153,   118,   551,   253,   157,   505,
    3005,   356,  2875,  1775,   291,   459,  1410,   136,   367,   452,
     235,  1072,    64,   276,   551,   446,  1420,   557,   472,   512,
     262,   962,   436,  1084,  2678,   452,   480,  2698,   482,   970,
     255,  1272,   973,  1353,  1095,   976,   504,    62,  1688,  1211,
    1353,  1354,   459,   285,   276,    70,   522,   490,   550,   474,
     551,   637,   104,   551,   106,   472,   108,   282,   547,   218,
     182,   504,  2733,   480,   116,   482,   368,   551,   419,  1967,
    1611,  1612,  2873,   232,   233,   135,  3005,   504,  1515,   665,
     272,  2628,   551,   551,   309,  1616,   214,   132,   276,   557,
     289,   237,   244,   406,  1635,  1636,   355,   551,   500,   424,
     252,   343,   231,   551,   272,   293,   446,   173,   551,   368,
    1051,   144,   375,  3005,   557,   540,  2880,   496,  1972,   550,
    3005,  2885,   551,   415,   551,   551,   551,   367,  1393,   181,
     557,  1781,   381,  1788,   307,  1676,  1197,   369,  1079,  1680,
     289,   296,   429,  1084,   110,   291,  1411,   276,   551,   547,
    1211,   436,   550,   276,   169,   418,  1217,   286,   346,   303,
     304,   305,   276,   218,   551,  1226,   517,   409,   367,   177,
    1610,  2825,  2982,   302,  1602,   431,  2442,   232,   233,  1541,
     550,   551,   522,   282,   250,   331,   359,  1728,  1729,   452,
     342,  1432,   543,   495,   545,   362,  1437,   319,  1439,   448,
     449,  1442,  1443,  2728,  1445,   364,   187,  2971,   178,   368,
     309,  2975,   182,   550,  3015,   801,   276,   250,   367,   386,
     387,  3005,   808,   809,   810,   277,   381,   371,   383,   421,
     495,   430,   818,   819,   820,   821,   435,   823,  1459,  2417,
    2418,   276,  2767,   132,  3045,   831,   375,   833,   834,   551,
     316,  3061,   441,   421,   320,   369,  1197,   843,   844,   845,
     846,   847,  2540,   244,   289,   250,   263,  2938,   450,  1622,
    1211,   252,  2795,  3054,   326,    12,  1497,   519,    15,    16,
    2608,   430,   315,  1344,  1345,  1226,   435,   320,   874,  1641,
     225,   226,  3073,   441,   557,  2623,  2624,  2820,    92,  1588,
    1589,  1242,  1543,    11,   312,  1246,  1595,  1548,  1597,   135,
     231,   410,  1373,   368,   496,  1719,   490,  1378,  1259,   354,
     322,  1725,   547,   452,  1385,  1566,   455,   456,   480,   326,
    1619,   266,   267,   522,  1687,   320,   132,  2615,   103,  1666,
     276,  1668,   367,  2609,  2874,  1634,   495,   317,  1674,   319,
       1,  1412,   551,   498,  2656,   276,    64,   313,  1419,  1420,
    1301,   342,   188,   551,  1425,   286,   510,   511,   542,   557,
     422,   515,   308,  1810,   522,  1937,   962,  2705,   660,   144,
    1817,   337,  1786,   495,   970,  2713,  2688,  1714,  1877,   217,
      41,    42,    43,    44,    45,   562,   104,     8,   106,  1712,
     108,   188,   551,  1344,  1345,   430,   551,   282,   116,  1650,
     435,  1737,  1353,  1354,  1355,  1656,  1588,  1589,   354,   282,
     472,  1662,   527,  1595,  1777,  1597,   195,    38,   557,     1,
     712,  2961,    83,    84,   309,   222,   251,   206,   427,  1876,
    1877,   546,  2731,  1029,  1385,   447,   309,  1619,  1775,   231,
     498,   550,   551,   239,   375,  1944,  1945,    59,    60,  1724,
     229,   276,  1634,   157,  1705,   497,  2007,  2008,  2998,  2999,
     495,   195,  1413,   181,   219,  1764,    48,  3007,  3080,  2020,
    2021,  3011,  3012,   206,   374,   497,   272,   245,   231,  2518,
     259,   188,  2033,  2034,   276,    97,   508,    69,  1084,   551,
     502,  1766,  1767,   248,   286,   507,   229,  1944,  1945,  1719,
    3040,   386,   114,  1953,  1876,  1877,   548,   332,  1880,   334,
     348,   349,   495,   125,   502,   222,   291,  1588,  1589,   507,
     282,   452,   256,   276,  1595,   259,  1597,     8,   110,   367,
    3070,   282,   381,   286,   383,  1606,   197,   198,   199,   200,
     201,   202,   203,   204,   205,  1983,     8,   309,  1619,    60,
     450,  1622,   299,   300,   499,  1806,   501,    38,   309,   277,
     495,  1812,   331,  1634,   464,   465,   466,   144,     9,  1820,
    1641,    12,  1944,  1945,    15,    16,    38,   474,   475,   326,
     327,  2152,  1764,   375,  1642,   381,    97,   484,  2002,   427,
     492,  3059,  3060,   389,  1825,   172,   365,   174,   206,   374,
     502,  1197,   153,   114,   206,   507,   188,  1678,   326,  1972,
    2701,   489,   495,  3081,   125,  1211,  1687,  1688,   497,   388,
     114,   229,   375,  1874,  1875,  1696,   557,   229,   506,   508,
    1226,   125,  3100,   530,   332,   520,   334,  1588,  1589,   492,
     222,   492,  3066,  2090,  1595,  2095,  1597,  2219,  2579,   502,
     132,   502,  1723,  3077,   507,  1606,   507,  2588,  2589,  2590,
     452,  3129,  2213,  2214,  2215,   550,   244,  1618,  1619,   251,
     494,   427,   177,   493,   252,   450,   258,   182,   207,   503,
     209,     9,   502,  1634,   459,  1756,   493,   507,   270,   464,
     465,   466,   178,  1764,   355,   502,   182,   472,  2061,   452,
     507,   446,    23,    24,   422,   450,  1777,   482,   495,   370,
    1781,   427,   504,  2160,   381,  1666,   383,  1668,   317,  1596,
     319,  1598,   272,    51,   274,   191,  1603,  1604,  1605,   845,
     846,   847,  1609,   272,   346,   274,   495,  1688,   350,   351,
     381,   495,   383,  1620,  1621,  1696,   329,  2161,  1344,  1345,
     453,   504,  2303,  2304,   472,   272,   451,   274,  1845,    11,
    1847,  1712,    90,  1714,  2127,   557,    72,  1218,    74,   511,
    1221,   513,  3110,   268,   269,   495,   551,   495,  1229,   107,
     324,   325,  1233,   495,   396,    23,    24,  2038,  1239,  1385,
     531,   532,   533,   534,   531,   532,   533,   534,   495,   381,
     382,   413,   414,   495,   557,   531,   532,   533,   534,   843,
     844,    27,    64,  1764,   396,   495,   398,   900,   901,   902,
     903,   819,   820,   495,  1775,  2070,  2071,  2072,  2073,   433,
    1781,   406,   363,   551,   553,   459,   551,   440,   251,  2201,
     440,    63,   399,   237,   276,   495,    48,   495,   276,   551,
     551,   490,   104,  2104,   106,   551,   108,   551,   299,   300,
     427,    80,   495,   495,   116,   435,   446,    69,    65,    70,
      75,   145,   551,   495,   551,   291,     9,   214,   506,   331,
     506,   209,   210,   211,   212,   326,   327,   506,   506,   506,
     551,   219,   506,   506,   491,  2146,   506,   506,  2149,   506,
    2237,  1972,  2239,   231,   146,  2156,   186,   147,   110,   490,
    2357,   506,   506,   495,   506,   148,   417,  2364,   149,   480,
     381,  2359,   480,   150,   152,   151,   542,   109,   490,   181,
      63,   485,  2183,   506,   155,   263,   264,   265,    50,   439,
     486,   489,   483,   158,   214,   159,   551,   160,   276,   161,
     278,   279,   280,   162,   293,   546,   284,    90,   286,   182,
    2323,    32,   544,   163,  2184,  2185,   164,   120,   214,   551,
     165,   120,   166,   214,   107,   167,   495,  2252,  2253,   427,
     281,   495,   551,   551,   337,   435,   276,   551,   495,  2264,
     115,     4,  1588,  1589,   276,   276,  1947,   512,  1949,  1595,
     328,  1597,   495,   446,   337,   103,    19,   117,   331,   506,
    1606,   490,   335,   551,   551,   551,  2267,    30,  2465,  3058,
     406,   344,   345,  1619,   242,   277,   366,   276,   221,   352,
     294,   404,   355,   319,   182,   548,  1987,   360,  1634,  2477,
     363,   142,   365,   366,   367,   368,   548,   375,   193,   251,
     395,   490,   550,   495,   185,   144,   258,    70,  2496,  2497,
    2507,   247,   385,   247,  2502,  2479,   490,   490,   270,    50,
      50,   214,   247,   495,   326,   427,   275,   210,   211,   212,
     408,   393,    93,   495,  2315,   495,   219,   498,    23,  2040,
     495,   294,  1688,   486,   251,  2458,   434,   276,   231,  2546,
    1696,   367,    79,   426,   551,   428,   495,   487,   431,   256,
     556,   555,   459,   324,   492,   298,   492,   492,  2565,   411,
     492,   492,   492,   492,   452,   492,  2377,   260,   492,   490,
    2201,   264,  2570,  2384,  2385,  2386,  2387,   392,   256,   467,
     468,  2392,   490,   276,  2419,   278,   279,   280,   221,   221,
      17,   284,   486,   286,  2225,   483,   334,   142,   154,   395,
     490,    50,   134,    25,    26,   221,   156,     8,  1764,   214,
     422,   548,   332,  2424,  2621,   459,   504,  2397,   221,   381,
     382,   495,   548,   490,     9,  1781,   514,  2928,     7,   396,
     495,    94,   103,   427,   396,   328,   398,   294,   544,   527,
      22,   331,   479,   526,   207,  2425,  2644,  2610,   536,   353,
      72,    48,    74,   541,    76,   551,   324,    62,   546,  2290,
     472,   549,   550,   551,   547,     8,   239,  2298,   322,   557,
    2301,  2669,  2670,   144,   221,   320,   446,  2488,   250,   447,
    2678,  2679,   375,   495,   544,   544,    50,   207,   207,   111,
     112,   113,  2503,   339,   435,   256,   335,   341,   315,   272,
     144,   338,   355,   121,   427,   495,  2517,   395,   222,   282,
     593,  2709,   478,   222,   546,   408,   551,   276,   551,   421,
     248,   604,   486,   221,   221,   391,  2237,   391,  2239,   110,
       8,    38,    50,   495,   282,   256,  2516,   110,  2736,   551,
     310,   321,   103,  2666,   495,   237,   282,   169,   272,   171,
     500,   427,    58,   538,   551,   495,   178,   505,   251,   452,
     182,   490,   207,   415,   548,   436,   274,   340,    62,   282,
     459,    40,   345,   319,   467,   468,  2827,    36,   661,    50,
      39,   118,   544,   364,  2415,   411,   282,    46,    47,   495,
     316,  2798,   675,   282,   282,   173,   495,   680,    54,   486,
     518,   557,  2433,    27,   490,   434,   446,  2618,   381,   436,
      17,   504,   356,   117,  2445,   364,   389,   527,   214,   512,
     291,   514,   489,   276,   367,   455,   495,  2825,   401,   115,
     437,   490,  2643,   122,   497,  2842,   551,   122,   377,   206,
      99,     7,   397,   536,   454,   495,    31,   490,   541,   242,
     272,   239,   274,   550,   495,   122,   549,   495,   551,   471,
     364,   334,   435,   331,   557,   548,   439,  2790,   263,   256,
     256,   293,   124,   495,   757,   448,   551,   103,   330,   256,
     256,   256,   256,   263,   480,   194,   459,   222,  3020,   138,
      62,   263,   222,   276,   240,   551,   229,  2895,   129,   228,
     548,   214,   338,   374,    50,   346,     7,   461,  2719,    58,
      55,   794,    60,  1162,    53,    52,   214,   176,  1506,  2730,
    2731,   229,   495,   413,  2735,   498,  1159,  1141,   767,  1486,
    2647,  2742,  2396,  2383,  2745,  2746,  2747,  2748,  2570,  2391,
    2756,  2397,  2376,   738,   827,  2564,  2725,  2869,  2767,   208,
    1806,  2351,  3108,  2731,  2672,  2823,  2323,  3094,  3120,  2788,
    3070,  2788,  2773,  2774,  2615,  2937,  2937,   251,  1671,    65,
    1181,   230,  2731,  2784,  1554,   722,  1253,  1264,  2789,   450,
    1669,  2218,  2217,   862,   526,   340,   539,     9,   459,   567,
     912,  1314,  1292,   464,   465,   466,   915,   589,  1963,  1673,
     959,   472,   966,  1977,  1342,  1696,  1976,   626,  1725,   637,
    3017,   482,  1723,  1372,  2936,  1786,  2002,  2011,  2010,  2650,
    1740,  1402,  1042,   368,   907,  1767,   285,   689,   675,  1420,
    1420,   673,  1424,  2043,  1531,  1529,  2688,  2848,  2849,  2850,
    2851,  2852,   895,  1018,  2855,  2856,  2857,  2858,  2859,  2860,
    2861,  2862,  2863,  2864,  2865,  1615,   894,  2868,  3065,  2048,
    2840,  2872,  3069,  2436,   323,   487,  2037,  1614,    90,  2036,
    2308,  2307,  2049,   956,  1640,  1639,  2887,   945,  2889,  1760,
    1760,  1760,  2893,  1760,   343,   107,   363,   661,   385,   769,
     973,  1128,  2135,   976,  1953,  1622,  2727,  2728,  1389,  1880,
    2790,  1495,  1490,   852,   493,  2529,  2354,   458,  2636,  3044,
    1153,  1409,  1716,  2744,  1685,  2831,  1829,   539,  2631,  3061,
    1441,    -1,    -1,  3130,    -1,    -1,    -1,    -1,  2759,   551,
      -1,    -1,    -1,  1016,    -1,    -1,    -1,    -1,    -1,  2770,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,
     409,   410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2790,
      -1,    -1,    -1,    -1,   423,    -1,    -1,    -1,  1051,  2980,
    2981,    -1,    -1,  2804,    -1,    46,  2807,    -1,    -1,    55,
      56,    57,    -1,    -1,    -1,    -1,  2997,   446,   210,   211,
     212,  3002,  3003,    -1,    -1,    -1,  1079,   219,    -1,    -1,
      71,    72,    73,    74,    -1,    -1,    -1,    -1,  3019,   231,
    2841,  3022,  3023,  2844,  2845,  2846,  2847,    -1,    -1,    -1,
      -1,    -1,    -1,  2854,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2867,    -1,  2869,    -1,
      -1,    -1,   264,    -1,    -1,  3056,    -1,    -1,  2879,    -1,
      -1,   127,   128,   129,   276,    -1,   278,   279,   280,    -1,
     519,    -1,   284,    -1,   286,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2904,  2905,  2906,  2907,  2908,  2909,  2910,
    2911,  2912,  2913,  2914,  2915,  2916,  2917,  2918,  2919,  2920,
    2921,  2922,    -1,  1176,    -1,    -1,    -1,    -1,    -1,  2930,
      -1,    -1,    -1,    -1,    -1,  2936,   328,    -1,    -1,    -1,
      -1,    -1,    -1,  2944,    -1,    -1,    -1,  3059,  3060,    -1,
      -1,    -1,    -1,    -1,    -1,  2956,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3081,
      -1,    -1,    -1,    -1,    -1,    -1,  2977,  2978,    -1,    -1,
      -1,    -1,    -1,   375,    -1,    -1,    -1,   228,  3100,  1242,
      -1,    -1,  2993,  1246,    -1,    -1,    -1,    -1,   244,   245,
      -1,    -1,    -1,    -1,    -1,  3006,  1259,     6,    -1,  1262,
       9,   257,    -1,   254,    -1,    -1,   408,  3129,    -1,    -1,
      -1,    -1,    -1,  3024,    -1,    -1,   272,    -1,    -1,    -1,
      -1,   272,    -1,   274,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   288,    -1,    -1,  3046,    -1,    -1,  1301,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3059,  3060,
     452,    -1,    -1,    -1,    -1,    -1,   307,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3075,   467,   468,    -1,    -1,    -1,
    3081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   330,
      -1,    90,  3093,  3094,    -1,    -1,   342,    -1,    -1,  3100,
    1353,  1354,  1355,   102,    -1,  3106,    -1,    -1,   107,    -1,
      -1,   352,   504,    -1,   360,   361,   362,   358,    -1,    -1,
      -1,    -1,   514,    -1,  3125,    -1,    -1,    -1,  3129,    -1,
     376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,   541,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   549,    -1,   551,
    1413,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,    -1,
       9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3059,  3060,
    1433,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   440,
    3081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   450,
      -1,   210,   211,   212,    -1,    -1,    -1,    -1,    -1,  3100,
     219,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,   470,
      -1,    -1,   231,   479,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3129,    -1,
      -1,    90,    -1,    -1,   495,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2790,   264,   265,    -1,   107,    -1,
      -1,    -1,   271,    -1,   273,   516,    -1,   276,    -1,   278,
     279,   280,    -1,    -1,    -1,   284,    -1,   286,  1541,    -1,
      -1,    -1,    -1,    -1,   293,    -1,   537,    -1,    -1,    -1,
      -1,    -1,    -1,   544,    -1,   546,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
      -1,     5,    -1,    -1,    -1,    -1,    10,    -1,   347,  1602,
      -1,    -1,    -1,    -1,    18,    -1,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1618,    -1,    -1,    -1,    -1,
      -1,   210,   211,   212,    -1,    -1,   375,    -1,    -1,    -1,
     219,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    52,    53,
      -1,    -1,   231,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,   408,
      -1,    -1,    -1,  1666,    78,  1668,    -1,    -1,    82,    -1,
      -1,   260,    -1,    -1,    -1,   264,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    -1,    -1,   276,    -1,   278,
     279,   280,    -1,    -1,    -1,   284,   110,   286,    -1,    -1,
      -1,    -1,    -1,   452,    -1,   119,    -1,    -1,    -1,  1712,
      -1,  1714,   126,    -1,    -1,   129,    -1,   131,   467,   468,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,   142,    -1,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   328,
     154,   155,   156,    -1,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   504,    -1,    -1,    -1,   173,
      -1,    -1,    -1,    -1,   178,   514,   180,    -1,   182,    -1,
      -1,   185,  1775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  3059,  3060,  1788,   375,   536,    -1,    -1,
      -1,    -1,   541,    -1,    -1,    -1,    -1,    -1,    -1,   213,
     549,    -1,   551,    -1,    -1,  3081,   220,    -1,   557,   223,
     224,    -1,    -1,  1816,    -1,    -1,    -1,    -1,    -1,   408,
     234,    -1,    -1,    -1,  3100,    -1,    -1,   241,    -1,   243,
      -1,    -1,   246,    -1,    -1,    -1,   250,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  3129,    -1,    -1,    -1,    -1,    -1,    -1,
    1863,  1864,  1865,   452,    -1,    -1,    -1,  1870,  1871,    -1,
      -1,    -1,    -1,  1876,  1877,  1878,   290,  1880,   467,   468,
      -1,   295,    -1,   297,    -1,    33,    -1,    -1,    36,    -1,
      -1,    39,    -1,   307,    -1,    -1,   310,    -1,    -1,    47,
      -1,   315,   316,   317,    -1,   319,   320,   321,   322,    -1,
      -1,    -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,
      -1,    -1,   336,   512,    -1,   514,    -1,    -1,    -1,    -1,
      -1,  1934,    -1,    -1,    -1,   349,    -1,    -1,    -1,    -1,
    1943,  1944,  1945,  1946,  1947,    -1,  1949,   536,    -1,    -1,
      -1,    99,   541,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     549,    -1,   551,    -1,   378,   379,   380,    -1,   557,    -1,
     118,    -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,    -1,
    1983,    -1,    -1,    -1,  1987,    -1,    -1,    -1,     1,   403,
       3,   405,     5,    -1,    -1,    -1,    -1,    10,   412,    -1,
      -1,    -1,   416,    -1,    31,    18,    -1,    -1,    -1,    36,
      -1,   425,    39,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      47,    -1,    -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   447,   448,    -1,    -1,  2040,    -1,    52,
      53,    -1,    -1,   457,    71,    -1,    73,    -1,   462,   463,
      -1,    -1,    -1,    -1,    67,   469,    -1,   471,    -1,    -1,
     208,  2064,    -1,    -1,    -1,    78,    -1,   481,   216,    82,
      -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   495,   230,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   521,    -1,    -1,
      -1,    -1,   526,   126,   262,    -1,   129,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,   548,   283,    -1,   285,    -1,    -1,
     554,    -1,    -1,    -1,    -1,   559,   560,    -1,    -1,   176,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,    -1,
      -1,   309,    -1,   311,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,   185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   208,    -1,    -1,   332,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   343,    -1,    -1,    -1,  2202,
     213,    -1,    -1,   230,    -1,    -1,    -1,   220,    -1,    -1,
     223,   224,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   234,    -1,    -1,   372,   373,    -1,    -1,   241,    -1,
     243,    -1,    -1,   246,  2237,    -1,  2239,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   272,    -1,    -1,    -1,    -1,
      -1,    -1,   400,    -1,    -1,    -1,    -1,  2260,   285,   407,
      -1,   409,   410,    -1,    -1,    -1,    -1,   294,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   423,    -1,   290,    -1,    -1,
     428,    -1,   295,    -1,   297,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   307,    -1,   323,    -1,   446,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,    -1,    -1,
      -1,    -1,    -1,   336,    -1,    -1,    -1,    -1,   476,   356,
      -1,    -1,    -1,    -1,    -1,    -1,   349,    -1,    -1,    -1,
     488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2359,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   378,   379,   380,    -1,    -1,
      -1,   519,  2375,  2376,    -1,    -1,    -1,   390,    -1,    -1,
     407,   529,   409,   410,    -1,    -1,    -1,   535,    -1,    -1,
     403,    -1,   405,    -1,    -1,    -1,   423,    -1,    -1,   412,
      -1,    -1,    -1,   416,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   425,    -1,    -1,    -1,    -1,    -1,    -1,   446,
      -1,    -1,    -1,    -1,   437,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   448,    -1,    -1,     1,    -1,
       3,    -1,     5,    -1,   457,    -1,    -1,    10,    -1,   462,
     463,    -1,    -1,    -1,    -1,    18,   469,    -1,   471,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   481,    -1,
      -1,    -1,    -1,    -1,  2477,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   495,    -1,    -1,    -1,    -1,    -1,    -1,    52,
      53,  2494,   519,  2496,  2497,    -1,    -1,    -1,    -1,  2502,
     513,    -1,    -1,   530,    67,    -1,    -1,    -1,   521,    -1,
      -1,    -1,  2515,   526,    -1,    78,    -1,    -1,    -1,    82,
      -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   554,    -1,    -1,    -1,    -1,   559,   560,    -1,    -1,
      -1,    -1,  2555,  2556,    -1,    -1,   119,    -1,    -1,    -1,
      -1,    -1,    -1,   126,    -1,    -1,   129,  2570,   131,    -1,
      -1,    -1,    -1,  2576,    -1,    -1,  2579,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,  2588,  2589,  2590,  2591,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2603,    -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,   185,    -1,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2644,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     213,    -1,    -1,  2656,    -1,    -1,    -1,   220,    -1,    -1,
     223,   224,    -1,    -1,    -1,    -1,  2669,  2670,    -1,    -1,
    2673,   234,    -1,    -1,    -1,  2678,  2679,    -1,   241,    -1,
     243,    -1,    -1,   246,    -1,  2688,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2709,    -1,    -1,    -1,
      -1,     1,    -1,     3,  2717,     5,    -1,    -1,    -1,    -1,
      10,    -1,    -1,    -1,    -1,    -1,    -1,   290,    18,   210,
     211,   212,   295,  2736,   297,    -1,    -1,    -1,   219,    -1,
      -1,    -1,    -1,    -1,   307,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   336,    -1,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    -1,   264,    -1,    -1,   349,    -1,    78,    -1,
      -1,    -1,    82,    -1,    -1,   276,    -1,   278,   279,   280,
      -1,    -1,    -1,   284,    -1,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   378,   379,   380,    -1,    -1,
      -1,    -1,  2825,    -1,    -1,    -1,    -1,   390,    -1,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,   129,
     403,   131,   405,    -1,    -1,    -1,    -1,   328,    -1,   412,
      -1,   141,    -1,   416,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   425,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   437,    -1,    -1,  2880,   168,    -1,
      -1,    -1,  2885,    -1,    -1,   448,    -1,    -1,   369,    -1,
     180,    -1,  2895,    -1,   457,   185,    -1,    -1,    -1,   462,
     463,    -1,    -1,    -1,    -1,    -1,   469,    -1,   471,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   481,    -1,
      -1,    -1,    -1,   213,    -1,    -1,    -1,   408,    -1,    -1,
     220,    -1,   495,   223,   224,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   234,    -1,    -1,    -1,    -1,    -1,
     513,   241,    -1,   243,    -1,    -1,   246,    -1,   521,    -1,
      -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,  2971,    -1,
      -1,    -1,  2975,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   467,   468,    -1,    -1,
      -1,   554,    -1,    -1,    -1,    -1,   559,   560,    -1,    -1,
     290,    -1,    -1,    -1,    -1,   295,    -1,   297,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   307,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   336,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,    -1,   349,
     541,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   549,    -1,
     551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   378,   379,
     380,    -1,    -1,    -1,    -1,    -1,     3,    -1,     5,    -1,
     390,    -1,    -1,    10,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,    -1,   403,    -1,   405,    -1,    -1,    -1,    -1,
      -1,    -1,   412,    -1,    -1,    -1,   416,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   425,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    52,    53,   437,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   448,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,   457,    -1,    -1,
      -1,    78,   462,   463,    -1,    82,    -1,    -1,    -1,   469,
      -1,   471,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,   481,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     6,    -1,    -1,
       9,    -1,   119,    12,    13,    14,    -1,    -1,    -1,    -1,
      -1,    20,   129,   513,   131,    -1,    -1,    -1,    -1,    -1,
      -1,   521,    -1,    -1,    -1,   142,   526,    -1,   145,   146,
     147,   148,   149,   150,   151,   152,    -1,   154,   155,   156,
      -1,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,   554,    -1,    -1,    66,    -1,   559,
     560,    -1,    -1,   180,    -1,    -1,    -1,    -1,   185,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   213,    -1,   107,    -1,
      -1,    -1,    -1,   220,    -1,    -1,   223,   224,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   234,    -1,    -1,
      -1,    -1,    -1,    -1,   241,    -1,   243,    -1,    -1,   246,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   175,    -1,    -1,    -1,
      -1,    -1,    -1,   290,    -1,    -1,    -1,   186,   295,    -1,
     297,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   210,   211,   212,    -1,    -1,    -1,    -1,    -1,    -1,
     219,    -1,    -1,    -1,    -1,    -1,   225,   226,    -1,   336,
      -1,    -1,   231,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   349,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   251,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   264,   265,   266,   267,    -1,
      -1,   378,   271,    -1,   273,    -1,    -1,   276,    -1,   278,
     279,   280,    -1,   390,    -1,   284,    -1,   286,    -1,    -1,
      -1,    -1,    -1,    -1,   293,    -1,    -1,    -1,   405,    -1,
      -1,    -1,    -1,    -1,    -1,   412,    -1,   306,    -1,   416,
      -1,    -1,    -1,    -1,   313,    -1,    -1,    -1,   425,   318,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   326,    -1,   328,
     437,    -1,    -1,    -1,   333,    -1,    -1,    -1,    -1,   338,
      -1,   448,    -1,    -1,    -1,    -1,    -1,    -1,   347,    -1,
     457,    -1,    -1,    -1,    -1,   462,   463,    -1,   357,    -1,
      -1,    -1,   469,    -1,   471,     6,    -1,    -1,     9,    -1,
      -1,    12,    13,    14,   481,    -1,   375,    -1,    -1,    20,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   495,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   513,    -1,    -1,   408,
      -1,    -1,    -1,    -1,   521,    -1,    -1,    -1,    -1,   526,
      -1,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   554,    -1,    90,
      -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   467,   468,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   492,    -1,   494,    -1,   496,    -1,    -1,
     499,    -1,   501,   502,   503,   504,    -1,   506,   507,    -1,
      -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   175,     6,    -1,   536,     9,    -1,
      -1,    -1,   541,    -1,    -1,   186,    -1,    -1,    -1,    -1,
     549,    -1,   551,    -1,    -1,    -1,    -1,    -1,   557,    -1,
      31,    -1,    -1,    -1,    -1,    36,    -1,    -1,    39,   210,
     211,   212,    -1,    -1,    -1,    46,    47,    -1,   219,    -1,
      -1,    -1,    -1,    -1,   225,   226,    -1,    -1,    -1,    -1,
     231,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    -1,   264,   265,   266,   267,    -1,    99,    -1,
     271,    -1,   273,    -1,    -1,   276,   107,   278,   279,   280,
      -1,    -1,    -1,   284,    -1,   286,    -1,    -1,    -1,    -1,
      -1,    -1,   293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   306,    -1,    -1,    -1,    -1,
      -1,    -1,   313,    -1,    -1,    -1,    -1,   318,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   326,     6,   328,    -1,     9,
      -1,    -1,   333,    -1,    -1,    -1,    -1,   338,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   176,   347,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   357,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    -1,    -1,    -1,   375,    -1,    -1,   208,    -1,   210,
     211,   212,    -1,    -1,    -1,    -1,    -1,    -1,   219,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,    -1,
      90,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,   264,   265,    -1,    -1,    -1,    -1,    -1,
     271,   272,   273,    -1,    -1,   276,    -1,   278,   279,   280,
      -1,   452,    -1,   284,   285,   286,    -1,     6,    -1,    -1,
       9,    -1,   293,   294,    -1,    -1,   467,   468,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     6,    -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,
      -1,   492,   323,   494,    -1,   496,    -1,   328,   499,    -1,
     501,   502,   503,   504,    -1,   506,   507,    -1,    -1,    -1,
      -1,    -1,   343,   514,    -1,    -1,   347,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,
     210,   211,   212,    -1,    -1,   536,    -1,    -1,    -1,   219,
     541,    90,    -1,    -1,   375,    -1,    -1,    -1,   549,    -1,
     551,   231,    -1,    -1,    -1,    -1,   557,    -1,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   407,   408,   409,   410,
      -1,    -1,   107,    -1,   264,   265,    -1,    -1,    -1,    -1,
      -1,   271,   423,   273,    -1,    -1,   276,    -1,   278,   279,
     280,    -1,    -1,    -1,   284,    -1,   286,    -1,     6,    -1,
      -1,     9,    -1,   293,    -1,   446,    -1,    -1,    -1,    -1,
      -1,   452,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   467,   468,    -1,    -1,
      -1,    -1,    -1,   168,    -1,    -1,    -1,    -1,   328,    -1,
      -1,    -1,    -1,   333,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   210,   211,   212,    -1,    -1,    -1,   347,    -1,    -1,
     219,    -1,   221,   504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   231,   514,    -1,   210,   211,   212,   519,    -1,
      -1,    -1,    90,    -1,   219,   375,    -1,    -1,    -1,   530,
      -1,    -1,    -1,    -1,    -1,   536,   231,   387,     6,   107,
     541,     9,    -1,    -1,    -1,   264,   265,    -1,   549,   550,
     551,    -1,   271,    -1,   273,    -1,   557,   276,   408,   278,
     279,   280,    -1,    -1,    -1,   284,    -1,   286,    -1,   264,
     265,    -1,    -1,    -1,   293,    -1,   271,    -1,   273,    -1,
      -1,   276,    -1,   278,   279,   280,    -1,    -1,    -1,   284,
      -1,   286,    -1,   312,    -1,    -1,   446,    -1,   293,    -1,
      -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,    -1,   328,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   467,   468,    -1,
      -1,    -1,    90,    -1,    -1,    -1,    -1,    -1,   347,    -1,
      -1,    -1,    -1,   328,   102,    -1,    -1,    -1,     6,   107,
      -1,     9,   210,   211,   212,    -1,    -1,    -1,    -1,    -1,
      -1,   219,   347,    -1,   504,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,   231,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     375,    -1,    -1,    -1,    -1,    -1,   536,   406,    -1,   408,
      -1,   541,    -1,    -1,   544,    -1,   264,   265,    -1,   549,
      -1,   551,    -1,   271,    -1,   273,    -1,   557,   276,    -1,
     278,   279,   280,   408,    -1,    -1,   284,    -1,   286,    -1,
      -1,    -1,    90,    -1,    -1,   293,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   452,   102,    -1,    -1,    -1,    -1,   107,
      -1,    -1,   210,   211,   212,    -1,    -1,    -1,   467,   468,
      -1,   219,     6,    -1,    -1,     9,    -1,   452,    -1,    -1,
     328,    -1,    -1,   231,    -1,   333,    -1,    -1,    -1,    -1,
      -1,    -1,   467,   468,    -1,    -1,    -1,    -1,    -1,   347,
      -1,    -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,
      -1,     6,    -1,    -1,     9,   514,   264,   265,    -1,    -1,
      -1,    -1,   497,   271,    -1,   273,    -1,   375,   276,   504,
     278,   279,   280,   508,    -1,    -1,   284,   536,   286,   514,
      -1,    -1,   541,    -1,    -1,   293,    -1,    -1,    -1,    -1,
     549,   550,   551,    -1,    -1,    -1,    90,    -1,   557,    -1,
     408,   536,   210,   211,   212,    -1,   541,    -1,    -1,    -1,
      -1,   219,    -1,   107,   549,    -1,   551,    -1,    -1,    -1,
     328,    -1,   557,   231,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,   347,
      -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,   264,   265,    -1,   467,
     468,    -1,    -1,   271,    -1,   273,    -1,   375,   276,    -1,
     278,   279,   280,    -1,    -1,    -1,   284,    -1,   286,    -1,
      -1,    -1,    -1,    -1,    -1,   293,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,
     408,    -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,
      -1,     6,    -1,    -1,     9,    -1,   210,   211,   212,    -1,
     328,    -1,    -1,    -1,    -1,   219,    -1,    -1,   536,    -1,
      -1,    -1,    -1,   541,    -1,    -1,    -1,   231,    -1,   347,
      -1,   549,    -1,   551,   452,    -1,    -1,    -1,    -1,   557,
      -1,    -1,    -1,    -1,    -1,   210,   211,   212,    -1,   467,
     468,    -1,    -1,    -1,   219,    -1,    -1,   375,    -1,    -1,
     264,   265,    -1,    -1,    -1,    -1,   231,   271,    -1,   273,
      -1,    -1,   276,    -1,   278,   279,   280,    -1,    -1,    -1,
     284,    -1,   286,    -1,    -1,    90,   504,    -1,    -1,   293,
     408,    -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,   264,
     265,    -1,   107,    -1,    -1,    -1,   271,    -1,   273,    -1,
      -1,   276,    -1,   278,   279,   280,    -1,    -1,   536,   284,
      -1,   286,    -1,   541,   328,    -1,    -1,    -1,   293,    -1,
      -1,   549,    -1,   551,   452,    -1,    -1,    -1,    -1,   557,
      -1,    -1,    -1,   347,    -1,    -1,    -1,    -1,    -1,   467,
     468,    -1,     6,    -1,    -1,     9,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   328,    -1,    -1,    -1,    -1,   333,    -1,
      -1,   375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   347,    -1,    -1,    -1,   504,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   408,   210,   211,   212,     6,    -1,
     375,     9,    -1,    -1,   219,    -1,    -1,    -1,   536,    -1,
      -1,    -1,    -1,   541,    -1,    -1,   231,    -1,    -1,    -1,
      -1,   549,    -1,   551,    -1,    -1,    90,    -1,    -1,   557,
      -1,    -1,   247,   408,    -1,    -1,    -1,    -1,   452,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,   264,
     265,    -1,    -1,   467,   468,    -1,   271,    -1,   273,    -1,
      -1,   276,    -1,   278,   279,   280,    -1,    -1,    -1,   284,
      -1,   286,    -1,    -1,    -1,    -1,   490,   452,   293,    -1,
      -1,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     504,    -1,   467,   468,    -1,    -1,    -1,    -1,    -1,   107,
     514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     6,   328,    -1,     9,    -1,    -1,    -1,    -1,
      -1,    -1,   536,    -1,    -1,    -1,    -1,   541,    -1,   504,
      -1,    -1,   347,    -1,    -1,   549,    -1,   551,    -1,   514,
      -1,    -1,    -1,   557,    -1,    -1,   210,   211,   212,    -1,
     214,    -1,    -1,    -1,    -1,   219,    -1,    -1,    -1,    -1,
     375,   536,    -1,    -1,    -1,    -1,   541,   231,    -1,    -1,
      -1,    -1,    -1,    -1,   549,    -1,   551,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   408,    -1,    -1,    90,    -1,    -1,    -1,
     264,   265,   210,   211,   212,    -1,    -1,   271,    -1,   273,
      -1,   219,   276,   107,   278,   279,   280,    -1,    -1,    -1,
     284,    -1,   286,   231,    -1,    -1,    -1,    -1,    -1,   293,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   467,   468,    -1,    -1,   264,   265,    -1,    -1,
      -1,    -1,    -1,   271,   328,   273,    -1,    -1,   276,    -1,
     278,   279,   280,    -1,    -1,    -1,   284,    -1,   286,    -1,
      -1,    -1,    -1,   347,    -1,   293,    -1,    -1,    -1,   504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     6,    -1,   514,
       9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   375,    -1,    -1,    -1,    -1,   210,   211,   212,    -1,
     328,   536,    -1,    -1,    -1,   219,   541,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   549,    -1,   551,   231,    -1,   347,
      -1,    -1,   557,    -1,   408,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,
     264,   265,    -1,    -1,    -1,    -1,    -1,   271,    -1,   273,
      -1,    90,   276,    -1,   278,   279,   280,    -1,   452,    -1,
     284,    -1,   286,    -1,    -1,    -1,    -1,    -1,   107,   293,
     408,    -1,    -1,   467,   468,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   328,    -1,    -1,    -1,   446,    90,
     504,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,
     514,    -1,    -1,   347,    -1,    -1,   107,    -1,    -1,   467,
     468,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   536,    -1,    -1,    -1,    -1,   541,    -1,    -1,
      -1,   375,    -1,    -1,    -1,   549,    -1,   551,    -1,    -1,
      -1,    -1,    -1,   557,    -1,    -1,   504,    -1,    -1,    -1,
      -1,   210,   211,   212,    -1,    -1,   514,    -1,    -1,    -1,
     219,    -1,    -1,    -1,   408,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   231,    -1,    -1,    -1,    -1,    -1,   536,    -1,
      -1,    -1,    -1,   541,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   549,    -1,   551,    -1,    -1,    -1,    -1,    -1,   557,
      -1,    -1,    -1,    -1,    -1,   264,   265,    -1,   452,   210,
     211,   212,   271,    -1,   273,    -1,    -1,   276,   219,   278,
     279,   280,    -1,   467,   468,   284,    -1,   286,    -1,    -1,
     231,    -1,    -1,    -1,   293,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     504,    -1,    -1,   264,    -1,    -1,    -1,    -1,    -1,   328,
     514,    -1,    -1,    -1,    -1,   276,    -1,   278,   279,   280,
      -1,    -1,    -1,   284,    -1,   286,    -1,    -1,   347,    -1,
      -1,    -1,   536,    -1,    -1,    -1,    -1,   541,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   549,    -1,   551,    -1,    -1,
      -1,    -1,     1,   557,    -1,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    33,    -1,    -1,    36,    -1,   408,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    81,   452,    83,    84,    85,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,   408,   467,   468,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,
      -1,   452,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   467,   468,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,   541,    -1,    -1,    -1,    -1,   176,    -1,    -1,
     549,    -1,   551,    -1,    -1,    -1,    -1,    -1,   557,    -1,
      -1,    -1,    -1,   504,   193,    -1,    -1,    -1,   197,   198,
     199,   200,   201,   514,    -1,   204,   205,    -1,    -1,   208,
      -1,    -1,    -1,    -1,    -1,   214,    -1,   216,    -1,    -1,
      -1,    -1,    -1,   222,    -1,   536,    -1,    -1,   227,    -1,
     541,   230,    -1,    -1,    -1,    -1,     1,    -1,   549,   238,
     551,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,
      -1,    -1,   251,    -1,    -1,   254,    21,    -1,    -1,    -1,
      -1,   260,     9,   262,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,   272,    -1,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,   285,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     299,    -1,    -1,    -1,    51,    -1,    -1,    -1,    -1,    -1,
     309,    -1,   311,    -1,    -1,    -1,    81,    -1,    83,    84,
      85,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,   330,    -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,   343,   344,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   352,    -1,    -1,   355,    -1,    -1,    -1,
     107,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,   370,    -1,   372,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   400,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,
     409,   410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   188,    -1,    -1,    -1,    -1,   193,   428,
      -1,    -1,   197,   198,   199,   200,   201,    -1,    -1,   204,
     205,   440,    -1,   442,   443,   444,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   222,    -1,    -1,
      -1,    -1,   209,   210,   211,   212,    -1,    -1,    -1,    -1,
      -1,    -1,   219,   238,    -1,    -1,    -1,   476,    -1,    -1,
      -1,    -1,    -1,    -1,   231,    -1,   251,    -1,    -1,   254,
      -1,   490,    -1,    -1,    -1,   260,   495,    -1,    -1,    -1,
      -1,   500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   512,    -1,    -1,   263,   264,   265,    -1,
     519,    -1,    -1,    -1,   523,   524,   525,    -1,    -1,   276,
      -1,   278,   279,   280,   299,    21,    -1,   284,   537,   286,
      -1,    -1,    -1,   542,    -1,   544,    -1,    -1,    -1,    -1,
      -1,    37,   551,    -1,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,   330,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   344,
      -1,   328,    -1,    -1,    -1,    -1,    -1,   352,    -1,    -1,
     355,    -1,    -1,    -1,    -1,    81,    -1,    83,    84,    85,
      86,    87,    88,    89,    -1,   370,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     385,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   408,    -1,    -1,    31,    -1,    33,    -1,    -1,    36,
      -1,    38,    39,    -1,    -1,   440,    -1,   442,   443,   444,
      47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,   452,    73,    -1,   473,    -1,
      -1,   197,   198,   199,   200,   201,    -1,    -1,   204,   205,
     467,   468,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     495,    -1,    99,    -1,    -1,    -1,   483,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,    -1,
      -1,   118,   238,    -1,    -1,    -1,    -1,   504,   523,   524,
     525,    -1,    -1,    -1,    -1,   251,    -1,   514,   254,    -1,
      -1,    -1,   537,    -1,   260,    -1,    -1,    -1,    -1,   544,
     527,    -1,    -1,    -1,    -1,    -1,   551,    -1,    -1,   536,
      -1,    -1,    -1,    -1,   541,    -1,    -1,    -1,    -1,   546,
      -1,    -1,   549,   550,   551,    -1,    -1,    -1,    -1,    -1,
     557,    -1,    -1,   299,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    40,    41,    42,    43,    44,    45,
      -1,   208,    -1,    -1,   330,    -1,    -1,    -1,   215,   216,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   344,    -1,
      -1,    -1,    -1,   230,    -1,    -1,   352,    -1,    -1,   355,
      -1,    -1,    -1,    -1,    -1,    81,    -1,    83,    84,    85,
      86,    87,    88,    89,   370,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   262,    -1,    -1,    -1,   385,
      -1,    -1,    -1,    -1,    -1,   272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   283,    -1,   285,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,   294,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   309,    -1,   311,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   440,    -1,   442,   443,   444,    -1,
      -1,    -1,    -1,    -1,    -1,   332,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   473,    -1,   356,
      -1,   197,   198,   199,   200,   201,    -1,    -1,   204,   205,
      -1,    -1,    -1,    -1,    -1,   372,   373,    -1,    -1,   495,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,    -1,    -1,
      -1,    -1,   238,   400,    -1,    -1,    -1,   523,   524,   525,
     407,    -1,   409,   410,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   537,    -1,    -1,    -1,    -1,   423,    -1,   544,    -1,
      33,   428,    -1,    36,    -1,    -1,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    -1,    -1,    -1,   446,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   299,    -1,    -1,    -1,    -1,    71,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    81,   476,
      83,    84,    85,    86,    87,    88,    89,    -1,    -1,    -1,
     487,   488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   344,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   355,
      -1,    -1,   519,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,   529,    -1,   370,    -1,    -1,    -1,   535,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,   176,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,   197,   198,   199,   200,   201,    -1,
      -1,   204,   205,    -1,    -1,   208,   442,   443,   444,    -1,
      -1,   214,    -1,   216,    -1,    -1,    -1,    -1,    -1,   222,
      -1,    -1,    -1,    -1,   227,    -1,    -1,   230,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   238,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   251,    -1,
      -1,   254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   262,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   272,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   285,    -1,    -1,    -1,    -1,   523,   524,   525,
     210,   211,   212,    -1,    -1,    90,   299,    -1,    -1,   219,
      -1,    -1,    -1,    -1,    -1,    -1,   309,    -1,   311,    -1,
      -1,   231,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   330,    -1,   332,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     343,   344,    -1,    -1,   264,   265,    -1,    -1,    -1,   352,
      -1,   271,   355,   273,    -1,    -1,   276,    -1,   278,   279,
     280,    -1,    -1,    -1,   284,    -1,   286,   370,    -1,   372,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   400,    -1,    -1,
      -1,    -1,    -1,    -1,   407,    -1,   409,   410,   328,    -1,
      -1,    -1,    -1,    -1,    -1,   210,   211,   212,    -1,    -1,
      -1,    -1,    -1,    -1,   219,   428,    -1,   347,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,   231,   440,    -1,   442,
     443,   444,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   264,
     265,    -1,    -1,   476,    -1,    -1,   271,    -1,   273,    -1,
      -1,   276,    -1,   278,   279,   280,    -1,   490,   408,   284,
      -1,   286,   495,    -1,    -1,    -1,    -1,   500,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,
     523,   524,   525,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   452,   328,   537,    -1,    -1,    -1,    -1,   542,
      -1,   544,    90,    -1,    -1,    -1,    -1,   467,   468,   210,
     211,   212,   347,    -1,    -1,    -1,    -1,    -1,   219,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     231,    -1,   492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     375,    -1,   502,    -1,   504,    -1,   506,   507,    -1,    -1,
      -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   264,   265,    -1,    -1,    -1,    -1,    -1,
     271,    -1,   273,   408,    -1,   276,   536,   278,   279,   280,
      -1,   541,    -1,   284,    -1,   286,    -1,    -1,    -1,   549,
      90,   551,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,
      -1,    90,   210,   211,   212,    -1,    -1,   328,    -1,    -1,
      -1,   219,   467,   468,    -1,    -1,    -1,    -1,   107,    -1,
      -1,    -1,    -1,   231,    -1,    -1,   347,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   502,    -1,   504,
      -1,   506,   507,    -1,   375,    -1,   264,   265,    -1,   514,
      -1,    -1,    -1,   271,    -1,   273,    -1,    -1,   276,    -1,
     278,   279,   280,    -1,    -1,    -1,   284,    -1,   286,    -1,
      -1,   536,    -1,    -1,    -1,    -1,   541,   408,    -1,    -1,
     210,   211,   212,    -1,   549,    -1,   551,    -1,    -1,   219,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   231,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     328,   210,   211,   212,    -1,    -1,    -1,    -1,    -1,    -1,
     219,   452,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   347,
      -1,    -1,   231,    -1,   264,   265,   467,   468,    -1,    -1,
      -1,   271,    -1,   273,   475,    -1,   276,    -1,   278,   279,
     280,    -1,    -1,    -1,   284,    -1,   286,   375,    -1,    -1,
      -1,    90,    -1,    -1,    -1,   264,   265,    -1,    -1,    -1,
      -1,   502,   271,   504,   273,   506,   507,   276,   107,   278,
     279,   280,    -1,   514,    -1,   284,    -1,   286,    -1,    -1,
     408,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,    -1,
      -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,
     541,    -1,    -1,    -1,    -1,    -1,    -1,   347,   549,    -1,
     551,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,   328,
      -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,   347,   467,
     468,    -1,    -1,    -1,    -1,    -1,    -1,   475,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,   408,    -1,
      -1,   210,   211,   212,   502,    -1,   504,    -1,   506,   507,
     219,    -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,
      -1,    -1,   231,    90,    -1,    -1,    -1,    -1,    -1,   408,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,
     107,    -1,   452,   541,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   549,    -1,   551,    -1,   264,   265,   467,   468,   557,
      -1,    90,   271,    -1,   273,    -1,    -1,   276,    -1,   278,
     279,   280,    -1,   452,    -1,   284,    -1,   286,   107,    -1,
      -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,   467,   468,
      -1,    -1,   502,    -1,   504,    -1,   506,   507,    -1,    -1,
      -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,
      -1,    -1,    -1,   502,    -1,   504,   536,   506,   507,    -1,
      -1,   541,    -1,    -1,    -1,   514,    -1,    -1,   347,   549,
      -1,   551,    -1,   210,   211,   212,    -1,   557,    -1,    -1,
      -1,    -1,   219,    -1,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,   541,    -1,   231,    -1,   375,    -1,    -1,    -1,
     549,    -1,   551,    -1,    -1,    -1,    -1,    -1,   557,    -1,
      -1,   210,   211,   212,    -1,    -1,    -1,    -1,    -1,    -1,
     219,    -1,    -1,    -1,    -1,    -1,    -1,   264,   265,   408,
      -1,    -1,   231,    -1,   271,    -1,   273,    -1,    -1,   276,
      -1,   278,   279,   280,    -1,    -1,    -1,   284,    -1,   286,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   264,   265,    -1,    -1,    -1,
      -1,    -1,   271,   452,   273,    -1,    -1,   276,    -1,   278,
     279,   280,    -1,    -1,    -1,   284,    -1,   286,   467,   468,
      -1,   328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     347,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   504,    -1,   506,    -1,   328,
      -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,   375,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   347,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,   541,    -1,    -1,   402,    -1,    -1,    -1,    -1,
     549,   408,   551,    -1,    -1,    -1,   375,    -1,   557,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,
      -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     467,   468,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   504,   467,   468,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   514,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,
      -1,    -1,    -1,    -1,   541,   504,    -1,    -1,    -1,    -1,
      -1,    -1,   549,    -1,   551,   514,    -1,    -1,    -1,    -1,
     557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,   541,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     549,    -1,   551,    -1,    -1,    -1,    -1,    -1,   557
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   563,   564,     0,   232,   233,   565,   566,   567,   568,
     569,   570,   571,   577,   132,   132,   567,   169,   576,   590,
     591,   218,   368,   578,   581,   495,   495,   132,   110,   705,
     707,    92,   592,   593,   582,   579,   576,   576,   495,   132,
     364,   887,   890,   498,   708,   427,   245,   655,   656,   329,
     451,   594,   595,   599,   495,   495,   157,   572,   573,   574,
     153,   575,   495,   132,   913,   914,   427,   709,   495,   427,
     191,   657,   495,   495,   453,   616,   599,   595,   276,   369,
     583,   583,   276,   369,   584,   574,   584,    61,   542,   891,
       1,     3,     5,    10,    18,    52,    53,    67,    78,    82,
      96,   119,   126,   129,   131,   141,   168,   180,   185,   213,
     220,   223,   224,   234,   241,   243,   246,   290,   295,   297,
     307,   336,   349,   378,   379,   380,   390,   403,   405,   412,
     416,   425,   437,   448,   457,   462,   463,   469,   471,   481,
     495,   513,   521,   526,   554,   559,   560,   915,   916,   934,
     939,   943,   948,   968,   972,   976,   980,   981,   982,   987,
     992,  1006,  1010,  1012,  1015,  1029,  1033,  1036,  1039,  1043,
    1044,  1048,  1058,  1061,  1079,  1081,  1084,  1088,  1095,  1107,
    1109,  1124,  1125,  1135,  1138,  1139,  1143,  1149,  1150,  1158,
    1165,  1181,  1191,  1200,  1205,  1214,  1218,  1220,  1223,  1226,
    1229,  1256,   915,   495,   190,   424,   706,   710,   711,   713,
     495,   495,   659,   600,   596,   495,    11,    64,   104,   106,
     108,   116,   181,   277,   326,   422,   472,   551,   617,   618,
     619,   620,   621,   627,   636,   638,   643,   646,   647,   649,
     650,   651,   652,   653,   654,    27,   585,   585,   495,   495,
     893,   892,   406,   899,     3,     5,    10,    18,    52,    53,
      67,    78,    82,    96,   119,   129,   131,   142,   145,   146,
     147,   148,   149,   150,   151,   152,   154,   155,   156,   158,
     159,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     180,   185,   213,   220,   223,   224,   234,   241,   243,   246,
     290,   295,   297,   307,   336,   349,   378,   390,   405,   412,
     416,   425,   437,   448,   457,   462,   463,   469,   471,   481,
     495,   513,   521,   526,   554,  1415,  1416,  1417,   917,   935,
     940,   944,   949,   969,   973,   977,   983,   988,   993,  1007,
    1011,  1013,  1016,  1030,  1034,  1037,  1040,   221,   406,   960,
    1032,  1045,  1049,  1059,  1062,  1080,  1082,  1085,   433,  1089,
    1096,  1110,  1126,  1136,  1140,  1144,  1151,  1166,  1182,  1192,
     276,   375,   418,   452,   557,  1204,  1206,  1215,   363,  1219,
    1221,   902,  1224,  1227,  1230,  1257,  1108,  1159,   553,   760,
     762,   763,     1,   551,  1329,   253,   431,   658,   660,    62,
      70,   289,   367,   430,   435,   551,   601,   602,   603,   604,
     605,   606,   607,   609,  1428,  1494,   597,   609,     1,   551,
    1344,  1344,   459,   440,  1463,   251,  1442,  1442,  1442,  1344,
     440,  1442,    63,  1429,   622,   399,   610,   619,   495,   620,
     237,   637,   276,   495,   580,    50,   894,   895,   896,  1427,
     894,   333,   551,   495,   333,   551,   918,   920,  1330,  1375,
    1376,  1379,     6,     9,    90,   102,   107,   210,   211,   212,
     219,   231,   264,   265,   271,   273,   276,   278,   279,   280,
     284,   286,   293,   328,   347,   375,   408,   452,   467,   468,
     504,   514,   536,   541,   549,   551,   557,   936,  1322,  1349,
    1350,  1352,  1375,  1386,  1387,  1388,  1389,  1390,  1391,  1392,
     265,   502,   506,   507,   941,  1317,  1318,  1319,  1320,  1321,
    1322,  1355,  1375,  1387,  1389,   276,   945,   946,  1335,  1336,
    1337,  1379,   293,   458,   460,   950,   952,   276,   369,   970,
     971,  1362,  1375,   974,  1329,     6,   978,  1323,  1324,  1347,
    1377,  1378,  1379,  1387,   498,   984,  1329,   244,   252,   342,
     480,   989,   991,   276,   333,   994,   995,   996,   997,   999,
    1349,  1362,  1375,  1008,  1350,   989,   276,  1014,   497,   508,
    1017,  1018,  1019,  1305,  1306,  1307,   217,   348,   349,   367,
     427,  1031,  1035,  1346,  1347,  1038,  1379,   490,  1041,  1474,
    1350,  1304,  1305,  1050,  1346,   551,  1060,  1331,  1063,  1064,
    1375,  1386,  1389,  1183,  1370,  1371,  1379,   102,  1083,  1350,
    1086,  1350,   187,   244,   252,   342,  1090,  1091,  1092,    51,
     209,   263,   265,   276,   550,   771,  1097,  1101,  1102,  1103,
    1335,  1364,  1375,  1379,  1386,  1389,  1478,  1111,  1329,   551,
    1127,  1330,  1137,  1325,  1379,  1141,  1329,   498,  1145,  1325,
    1326,     9,  1152,  1327,  1379,   169,   259,   293,  1167,  1170,
    1171,  1174,  1175,  1176,  1177,  1178,  1179,  1180,  1332,  1333,
    1346,  1369,  1371,  1379,  1183,  1193,  1329,  1201,  1207,  1208,
    1209,  1350,   102,  1216,  1349,  1222,  1331,   495,   551,   903,
     904,   907,   908,   913,  1225,  1372,  1375,  1228,  1329,  1231,
    1375,  1258,  1326,  1330,  1160,  1330,   427,    80,   741,   140,
     438,   445,   764,   765,   767,   777,   779,   781,  1402,   495,
     712,   495,   313,   337,  1410,   296,   420,   695,   696,   697,
     698,   700,   435,   446,    70,  1442,   495,   603,   495,   551,
     602,    65,  1442,   598,  1478,   628,  1442,  1442,  1442,  1339,
    1379,    75,  1339,  1442,  1442,  1339,   551,   639,   640,   641,
    1345,   276,   332,   334,   623,   625,   626,  1168,  1382,  1442,
     495,   495,   551,   586,  1442,   895,   446,   522,   897,   387,
     544,   888,   237,   331,  1484,   145,   933,   919,   291,  1450,
     214,   506,  1380,  1381,   331,  1454,  1388,  1375,   506,   506,
     506,  1394,   506,  1376,  1387,  1389,  1484,  1484,   506,   506,
     506,   506,  1484,   506,  1394,   146,   938,   490,   937,  1350,
     491,   506,  1393,   506,   506,  1376,  1387,  1389,  1321,  1375,
    1317,  1321,    63,   502,   507,   494,   503,   186,   242,  1405,
     946,   490,  1484,   147,   967,   276,   369,   953,  1363,  1375,
    1389,   971,  1329,   386,   520,   975,  1478,  1490,  1454,   148,
     979,   175,   496,  1324,  1482,   417,  1411,  1380,  1381,   985,
    1329,   149,   986,   381,  1460,   480,  1471,   480,  1330,   150,
    1005,   182,   319,  1271,  1273,  1275,   997,  1348,  1349,   998,
     531,   532,   533,   534,   151,  1009,    50,   247,   542,   954,
     152,  1028,    17,   548,  1020,  1021,  1022,  1024,    12,    13,
      14,    20,    66,   175,   186,   225,   226,   266,   267,   306,
     313,   318,   326,   333,   338,   357,   492,   494,   496,   499,
     501,   502,   503,   506,   507,  1308,  1309,  1310,  1311,  1312,
    1313,  1314,  1350,   109,  1032,  1347,  1334,   485,  1472,  1051,
    1478,  1331,   100,   395,   478,  1065,  1066,  1068,  1069,  1185,
     506,  1380,  1350,   490,   155,  1087,    50,  1091,   439,  1093,
    1454,     1,    41,    42,    43,    44,    45,    83,    84,   197,
     198,   199,   200,   201,   202,   203,   204,   205,   355,   370,
     772,   773,   774,   775,   776,   794,   795,  1376,   772,  1102,
     156,   495,  1098,  1100,   527,   546,   486,   489,   483,   158,
    1123,   307,   359,  1408,   159,  1134,   291,   429,  1128,   214,
    1259,   160,  1142,  1460,   551,   161,  1148,  1259,  1327,   162,
    1157,   546,  1153,  1358,  1360,  1375,  1387,  1389,   182,  1177,
    1179,  1346,   490,  1333,   133,   490,   528,  1169,    32,  1380,
     163,  1199,   195,   256,   259,  1195,   960,  1202,  1350,  1478,
     164,  1213,   247,  1209,   120,  1210,  1375,   165,  1217,   214,
    1331,   427,   495,   495,   214,   381,   383,  1461,   166,  1240,
     120,  1232,   167,  1263,  1259,   214,  1161,  1162,   495,   427,
     281,   822,   551,   769,   769,   769,   765,   495,     1,   193,
     768,   769,   551,   714,   337,  1344,   701,   381,   448,   449,
     699,     1,   495,   697,  1442,   435,  1382,   495,  1442,   551,
    1340,   495,   115,  1442,   231,   276,   286,   375,   452,   504,
     557,   644,   645,  1385,  1339,   276,   276,   512,   640,    22,
     251,  1345,  1443,  1168,   251,   459,  1456,  1442,   104,  1344,
     611,   495,    79,   188,   384,   500,   587,   588,   589,  1442,
     446,   337,   898,   117,   900,  1379,    31,    38,   215,   294,
     921,   922,   923,   925,   928,  1424,  1425,  1478,   103,    25,
      26,    72,    74,    76,   111,   112,   113,   169,   171,   178,
     182,   272,   274,   487,   539,   551,   924,  1333,  1481,  1315,
    1317,   506,  1381,   168,   367,  1356,  1376,   490,  1315,  1317,
    1400,  1315,  1401,   492,  1315,  1396,  1397,   551,   551,  1317,
    1399,  1399,  1399,  1354,  1375,  1387,  1389,  1398,   551,  1354,
    1395,     6,  1323,  1350,  1379,  1387,   221,  1388,  1317,  1354,
    1315,   492,   242,  1406,  1318,  1318,  1319,  1319,  1319,   406,
     942,   366,   947,  1337,   951,   975,   282,   309,   207,  1435,
    1376,  1317,   294,  1412,  1381,  1329,   404,  1116,  1117,  1118,
     990,  1478,   910,   911,   910,  1274,  1275,  1272,  1273,   530,
     925,   928,  1000,  1001,  1002,  1478,  1271,  1271,  1271,  1271,
    1350,  1323,  1350,   955,  1019,    21,   497,   508,  1025,  1026,
    1306,   548,  1022,  1023,   548,   910,  1474,   251,  1309,   122,
    1042,  1335,   142,   910,  1046,     9,    12,    15,    16,   299,
     300,   326,   327,  1052,  1056,   193,  1358,     9,    63,   195,
     260,   512,  1072,  1073,  1074,  1067,  1068,   134,   334,   550,
    1187,  1455,  1493,   490,  1346,  1323,  1350,  1478,  1116,   772,
     495,   910,   185,  1104,  1304,  1105,  1106,  1375,  1335,     8,
      38,  1261,  1460,   247,  1368,  1375,  1386,  1389,   247,  1112,
    1116,   144,  1154,  1375,  1154,   490,   490,   490,  1168,   168,
     497,   508,  1350,    50,    39,    47,   230,   262,   285,   343,
     409,   519,  1172,  1173,  1442,  1194,  1478,  1350,   177,   312,
    1375,  1427,   214,  1323,  1350,   909,  1382,  1358,  1427,   247,
    1235,  1260,  1261,  1375,  1163,  1478,   761,   495,   427,   275,
     824,   780,   782,   393,   495,   495,   766,    93,    48,    69,
     110,   258,   270,   381,   382,   396,   398,   495,   544,   715,
     716,   718,   722,   723,   726,   727,   733,   736,   738,   739,
    1442,   661,   498,  1433,    23,  1421,   495,  1382,   277,   474,
     540,   608,  1340,   294,    29,   136,   231,   276,   286,   302,
     375,   452,   455,   456,   557,   629,   630,   631,   634,   645,
     486,   648,  1478,   434,   276,   642,  1383,  1456,   251,  1344,
    1344,   624,   625,   217,   367,   612,   613,   614,   589,   367,
    1459,    79,    33,   118,  1382,  1442,   551,   495,   889,   557,
    1365,  1369,  1382,  1442,   178,   182,   317,   319,  1264,  1266,
    1267,  1269,  1270,   923,    71,    73,   272,   356,   926,   927,
    1480,   487,    33,    36,    39,    47,    99,   118,   208,   216,
     230,   262,   283,   285,   309,   311,   332,   343,   372,   373,
     400,   407,   409,   410,   423,   428,   446,   476,   488,   519,
     529,   535,   929,   930,   931,   932,  1264,   556,   555,  1358,
    1264,   256,   459,   324,   298,    77,   432,   492,  1316,   493,
    1317,   276,  1357,  1376,  1375,  1316,   492,  1316,   492,   492,
     492,  1354,  1316,   492,   492,   492,  1316,   492,  1316,   492,
    1454,   322,   447,  1276,  1278,  1280,  1380,  1381,  1323,   493,
     492,   492,   490,  1407,   942,  1347,   490,  1335,   954,   411,
     392,  1276,  1442,   209,  1435,   250,   320,  1297,  1298,  1300,
    1302,   256,   912,   104,   105,   361,   551,  1003,  1333,  1001,
      36,    39,    46,    47,    99,   176,   208,   230,   285,   323,
     343,   423,   446,   519,   931,  1004,   221,  1276,   221,   956,
     957,   958,  1427,    17,   486,  1027,   341,  1025,  1455,   910,
     142,   154,  1047,  1474,   395,  1053,  1474,   490,    50,  1073,
    1075,  1358,     9,    63,   260,   512,  1070,  1071,  1358,  1373,
    1375,   134,    70,   435,  1188,  1479,    28,   123,   808,   237,
     339,  1438,  1346,  1276,   221,     9,   309,   378,   694,  1328,
    1329,   156,  1099,     8,   214,  1112,  1375,  1375,   144,   315,
    1286,  1289,  1291,  1146,  1147,  1478,   910,   548,   548,  1155,
    1156,  1358,   332,  1357,  1350,  1168,  1168,  1168,  1168,  1168,
    1168,  1168,  1168,  1173,   313,   318,  1196,  1197,  1198,  1310,
    1409,  1297,   263,   446,  1492,   459,  1468,  1468,  1212,  1478,
     446,  1211,  1350,  1375,  1276,   221,   495,   490,     9,  1233,
    1234,  1403,  1236,  1375,  1212,  1236,  1116,     7,  1418,  1260,
     139,   143,   179,  1375,   762,   742,   495,   427,   396,   826,
     544,   816,   789,   790,  1442,  1379,   784,   770,  1442,    94,
    1430,  1442,   381,   383,  1489,  1489,  1442,  1430,  1442,   294,
    1451,  1442,    22,  1420,   331,   740,  1344,   188,   222,   662,
     479,  1470,  1435,    63,   552,  1488,   631,    17,   486,  1385,
     353,  1383,  1344,     9,   219,   551,   615,   551,     1,   495,
     614,    33,  1382,   901,   902,    48,  1268,  1269,   910,  1265,
    1266,   910,   324,  1452,  1452,  1452,   276,  1366,  1369,  1384,
    1442,  1442,   138,   932,    62,   446,   133,   528,  1442,     8,
    1419,  1264,  1317,   492,  1317,  1411,   475,  1393,   475,  1393,
    1339,  1393,  1393,  1393,  1354,   260,   512,  1393,  1376,   910,
     910,  1279,  1280,  1277,  1278,  1381,  1276,   492,  1317,  1393,
    1393,  1361,  1375,  1386,   959,   960,    35,   303,   304,   305,
     371,   510,   511,   515,  1413,  1317,   910,   910,  1301,  1302,
    1299,  1300,  1362,   913,  1442,   272,   421,   144,   172,   174,
     879,   880,  1432,  1442,   133,   528,  1442,  1323,  1324,  1323,
    1324,   957,   333,   897,    95,   387,   544,  1026,  1305,   910,
    1375,   910,   544,  1054,  1055,  1056,  1057,  1472,   544,  1359,
    1361,  1358,    50,     8,    38,  1076,  1077,  1078,  1071,  1076,
     207,   435,  1184,  1442,   256,  1444,   339,  1323,   341,  1457,
    1457,   335,   410,  1094,  1329,  1478,  1106,  1350,     7,   236,
    1113,  1114,  1115,  1117,  1120,  1147,  1478,   110,   310,  1129,
    1131,  1133,   910,   910,  1290,  1291,  1289,  1297,   282,   309,
    1305,  1304,  1155,  1310,  1375,  1311,  1312,  1313,  1314,  1317,
    1203,  1350,  1203,   321,   505,  1281,  1283,  1285,   355,  1411,
    1323,   905,  1359,   338,  1358,   121,  1237,   478,  1239,  1146,
     346,  1333,  1365,   395,  1164,   743,   823,   495,   427,   421,
     871,  1443,   816,   222,   486,   778,    21,    37,    40,    41,
      42,    43,    44,    45,    46,    81,    85,    86,    87,    88,
      89,   129,   197,   198,   199,   200,   201,   238,   254,   299,
     330,   344,   352,   355,   370,   385,   440,   442,   443,   444,
     473,   523,   524,   525,   537,   785,   786,   787,   790,   791,
     792,   793,   794,   795,   796,   799,   812,   813,   814,   815,
     816,   821,  1442,  1465,    27,   214,   783,  1423,   222,  1382,
     551,   677,  1442,  1420,   551,  1341,  1342,   333,   454,  1485,
     276,  1339,  1343,  1382,   546,  1442,   192,   232,   551,   724,
    1344,     4,    19,    30,   239,   272,   340,   345,   381,   389,
     401,   439,   448,   495,   498,   663,   664,   672,   674,   676,
     678,   679,   680,   681,   684,   685,   686,   687,   688,   690,
     691,   693,  1460,  1479,  1430,  1328,   632,   634,   276,   248,
     585,   219,   248,   585,   495,   902,  1365,  1365,  1365,  1365,
    1365,  1442,  1442,  1303,  1367,  1369,  1382,  1303,  1365,  1366,
     492,  1276,   492,   182,   319,   505,   962,   964,   966,     6,
     247,   314,   333,   504,   961,  1441,   426,   489,  1365,  1454,
     272,   421,  1365,  1303,  1303,  1365,  1276,   391,  1276,   391,
    1351,  1352,  1374,  1376,  1055,   110,  1431,  1474,  1076,  1076,
    1359,   500,  1440,  1440,  1078,  1077,   244,   542,  1189,  1339,
    1186,  1276,   282,   309,    50,  1455,   282,   256,  1121,  1119,
    1120,  1478,   235,   255,   547,   910,   910,  1132,  1133,  1130,
    1131,   282,   910,   910,   910,   910,  1284,  1285,  1282,  1283,
    1442,  1276,  1276,   538,   906,  1241,  1234,   237,  1437,   103,
    1238,  1437,  1281,   173,   316,  1262,  1292,  1294,  1296,  1298,
     272,   274,  1447,   272,  1446,    58,   744,   745,   762,   825,
     495,   427,   771,   818,   819,  1379,   263,   326,   441,   522,
    1464,   522,  1464,   522,  1464,   522,  1464,   522,  1464,   548,
    1476,   415,  1462,   135,   802,  1382,  1376,   251,   261,   415,
    1445,  1442,   188,   260,   512,   551,   771,   490,   721,   207,
     737,  1342,   274,  1449,   490,  1429,  1437,   189,   196,   419,
     517,   543,   545,   734,   735,  1442,  1442,  1451,  1460,   490,
     542,  1475,   436,  1442,  1428,   121,  1444,  1444,   309,   692,
    1382,  1478,   459,   282,    40,  1426,  1442,   702,   703,  1329,
     633,   634,   144,  1362,  1365,   272,   274,  1491,   910,   910,
     910,   965,   966,   963,   964,  1454,  1375,  1324,  1324,    50,
     118,  1076,  1350,  1350,   364,  1328,   221,   342,  1190,  1379,
     411,  1350,   282,  1442,  1122,  1287,  1289,  1291,  1297,   282,
     282,  1375,  1242,   495,  1375,  1437,  1375,   910,   910,  1295,
    1296,  1293,  1294,  1344,   762,   762,   827,   495,   486,   817,
     819,   557,    54,   804,   518,   490,   800,   793,    27,   788,
     434,  1414,  1414,  1382,    63,   383,   717,  1338,  1339,   728,
    1382,   446,  1466,   276,   725,  1379,   725,  1442,  1444,   135,
     188,   669,   389,   685,  1442,  1442,  1442,  1442,    23,    24,
    1422,   694,  1442,  1451,   436,   677,   703,   356,   704,    17,
     117,  1375,  1276,  1276,  1350,  1442,  1328,   364,   527,  1375,
    1290,  1288,  1289,    31,   137,   183,   222,  1243,  1244,  1245,
    1247,  1251,  1253,  1254,  1255,  1424,  1435,  1375,   746,   828,
     872,   771,   548,   820,  1477,  1437,   214,   801,   489,  1473,
    1382,  1473,   276,  1428,  1339,    49,   509,   729,   730,   731,
     732,  1478,  1429,   214,   720,  1436,   135,   376,   436,   673,
    1442,    55,    56,    57,   127,   128,   129,   244,   245,   257,
     272,   288,   342,   360,   361,   362,   376,   479,   665,   666,
     667,   668,  1343,   455,   689,  1339,  1339,  1339,  1442,  1382,
     634,   495,  1101,  1442,  1304,    38,  1419,   367,   115,   747,
     377,   829,   767,   781,   873,   874,   875,   437,   497,   551,
    1382,   490,   800,   122,   122,   803,  1343,  1343,   206,   721,
    1382,   689,   276,   671,  1379,   671,     7,   671,   671,   671,
     276,   670,  1379,   450,   496,    34,   184,   287,   682,  1101,
     397,   454,  1467,   144,   457,  1252,  1455,   495,   748,  1435,
    1331,     1,   768,   875,   495,   490,  1442,   242,   805,  1455,
    1455,   806,   807,  1424,  1429,  1404,  1493,  1433,  1442,  1338,
     550,   683,   683,  1375,   177,   182,  1483,     9,  1248,  1249,
    1336,   342,  1440,   830,   495,   876,   495,   771,   806,  1339,
    1339,   239,   809,   808,  1343,   122,   719,   471,   675,  1338,
     282,   416,   364,  1458,   331,   365,   388,  1250,  1249,   124,
     177,   459,   472,   482,   753,   754,   755,   244,   252,     1,
     831,   877,   809,   806,  1427,  1444,  1455,   548,   334,  1455,
     331,  1379,   103,   477,  1442,   256,   256,   124,   263,   755,
     144,   291,   459,   472,   482,   749,   750,   751,   752,  1375,
    1450,  1469,   144,   291,   459,   482,   756,   757,   758,   759,
    1375,  1469,   495,    68,    97,    98,   346,   495,   832,   833,
     836,  1442,  1502,    33,    36,    39,    46,    47,   176,   208,
     214,   216,   227,   230,   262,   272,   285,   309,   330,   343,
     372,   400,   428,   476,   490,   500,   519,   542,   791,   792,
     796,   812,   814,   816,   878,   885,   886,   930,   931,  1442,
    1480,   809,   551,   810,   811,  1442,  1339,     9,   452,   557,
     635,   296,   381,   383,  1487,   187,   244,   252,   342,  1246,
    1328,  1442,   330,  1375,  1442,  1442,  1442,  1442,   256,   111,
     487,   256,   263,   751,  1375,   103,   374,   450,   464,   465,
     466,   256,   111,   487,   256,   263,   758,  1375,   480,  1442,
    1442,  1420,   268,   269,  1448,   845,   222,   194,   834,  1434,
    1442,   272,   421,   879,   880,  1442,  1368,  1452,  1382,    62,
    1375,  1375,   222,  1452,   811,  1338,  1388,  1487,  1375,  1382,
    1375,  1375,  1375,  1375,  1442,  1442,  1442,  1442,  1442,  1375,
    1442,  1442,  1442,  1442,  1442,  1442,  1442,  1442,  1442,  1442,
    1442,  1375,  1442,  1362,  1442,  1420,   837,  1384,   767,   846,
     835,  1375,  1365,  1365,  1442,  1473,  1442,  1442,  1473,  1375,
    1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,
    1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,   838,   272,
     274,  1486,   768,   769,  1375,   292,   354,   502,   507,   881,
     882,   883,  1362,   881,   882,   884,   809,  1375,  1375,   195,
     206,   229,   259,   839,   840,   841,   842,   843,   844,  1384,
     847,  1365,  1365,  1375,  1375,   114,   125,  1495,  1442,  1442,
      60,    97,  1495,  1496,  1481,   848,  1375,  1442,  1384,  1384,
     229,  1442,  1442,   228,   272,   274,   307,   330,   358,   450,
     470,   495,   516,   537,   546,   791,   796,   797,   812,   814,
     816,   849,   850,   854,   855,   858,   859,   860,   861,   862,
     863,   868,   869,   870,  1480,  1481,  1375,  1384,  1384,  1384,
     240,  1439,   324,   325,  1453,  1420,   228,  1382,   548,  1442,
    1454,  1442,  1442,  1375,   308,   354,   864,   865,  1384,   354,
     866,   867,  1384,  1453,  1420,  1375,  1443,  1442,   800,  1304,
    1355,  1353,  1355,    59,    97,   346,   350,   351,   396,   413,
     414,   851,  1495,  1496,  1497,  1498,  1499,  1500,  1501,   129,
     214,  1382,   865,  1382,   867,  1443,  1375,   865,  1473,  1411,
     402,   856,  1355,   206,   206,   229,   206,   229,   194,   852,
    1375,   852,  1355,  1375,   803,  1455,   338,   853,   853,    50,
     461,   798,   194,   857,  1375,   346,  1355,  1382
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

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
#ifndef	YYINITDEPTH
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
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
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
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
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
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

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

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

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
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
  int yytoken;
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

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

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
      yychar = YYLEX;
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
  *++yyvsp = yylval;

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
     `$$ = $1'.

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

/* Line 1806 of yacc.c  */
#line 2288 "parser.y"
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
    break;

  case 3:

/* Line 1806 of yacc.c  */
#line 2300 "parser.y"
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
    break;

  case 10:

/* Line 1806 of yacc.c  */
#line 2336 "parser.y"
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
    break;

  case 18:

/* Line 1806 of yacc.c  */
#line 2387 "parser.y"
    {
	first_nested_program = 0;
	clean_up_program ((yyvsp[(2) - (3)]), CB_PROGRAM_TYPE);
  }
    break;

  case 19:

/* Line 1806 of yacc.c  */
#line 2395 "parser.y"
    {
	  clean_up_program ((yyvsp[(2) - (3)]), CB_FUNCTION_TYPE);
  }
    break;

  case 24:

/* Line 1806 of yacc.c  */
#line 2418 "parser.y"
    {
	cobc_in_id = 1;
  }
    break;

  case 25:

/* Line 1806 of yacc.c  */
#line 2422 "parser.y"
    {
	if (setup_program ((yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]), CB_PROGRAM_TYPE)) {
		YYABORT;
	}

	setup_prototype ((yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]), CB_PROGRAM_TYPE, 1);
  }
    break;

  case 26:

/* Line 1806 of yacc.c  */
#line 2430 "parser.y"
    {
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
    break;

  case 27:

/* Line 1806 of yacc.c  */
#line 2438 "parser.y"
    {
	cobc_in_id = 1;
  }
    break;

  case 28:

/* Line 1806 of yacc.c  */
#line 2442 "parser.y"
    {
	if (setup_program ((yyvsp[(4) - (6)]), (yyvsp[(5) - (6)]), CB_FUNCTION_TYPE)) {
		YYABORT;
	}
	setup_prototype ((yyvsp[(4) - (6)]), (yyvsp[(5) - (6)]), CB_FUNCTION_TYPE, 1);
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
    break;

  case 29:

/* Line 1806 of yacc.c  */
#line 2454 "parser.y"
    {
	if (CB_REFERENCE_P ((yyvsp[(1) - (1)])) && CB_WORD_COUNT ((yyvsp[(1) - (1)])) > 0) {
		redefinition_error ((yyvsp[(1) - (1)]));
	}
	/*
	  The program name is a key part of defining the current_program, so we
	  mustn't lose it (unlike in undefined_word).
	*/
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 30:

/* Line 1806 of yacc.c  */
#line 2465 "parser.y"
    {
	cb_trim_program_id ((yyvsp[(1) - (1)]));
  }
    break;

  case 32:

/* Line 1806 of yacc.c  */
#line 2473 "parser.y"
    {
	cb_trim_program_id ((yyvsp[(1) - (1)]));
  }
    break;

  case 33:

/* Line 1806 of yacc.c  */
#line 2479 "parser.y"
    { (yyval) = NULL; }
    break;

  case 34:

/* Line 1806 of yacc.c  */
#line 2480 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 37:

/* Line 1806 of yacc.c  */
#line 2489 "parser.y"
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
    break;

  case 38:

/* Line 1806 of yacc.c  */
#line 2498 "parser.y"
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
    break;

  case 40:

/* Line 1806 of yacc.c  */
#line 2508 "parser.y"
    {
	CB_PENDING (_("CALL prototypes"));
  }
    break;

  case 43:

/* Line 1806 of yacc.c  */
#line 2520 "parser.y"
    {
	current_program->flag_initial = 1;
  }
    break;

  case 44:

/* Line 1806 of yacc.c  */
#line 2524 "parser.y"
    {
	current_program->flag_recursive = 1;
  }
    break;

  case 47:

/* Line 1806 of yacc.c  */
#line 2540 "parser.y"
    {
	header_check |= COBC_HD_ENVIRONMENT_DIVISION;
  }
    break;

  case 50:

/* Line 1806 of yacc.c  */
#line 2557 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_CONFIGURATION_SECTION;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "CONFIGURATION SECTION");
	}
  }
    break;

  case 55:

/* Line 1806 of yacc.c  */
#line 2571 "parser.y"
    {
	if (warningopt && (check_comp_duplicate & SYN_CLAUSE_2)) {
		cb_warning (_("phrases in non-standard order"));
	}
  }
    break;

  case 56:

/* Line 1806 of yacc.c  */
#line 2583 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_repeated ("SOURCE-COMPUTER", SYN_CLAUSE_1, &check_comp_duplicate);
  }
    break;

  case 61:

/* Line 1806 of yacc.c  */
#line 2598 "parser.y"
    {
	cb_verify (cb_debugging_line, "DEBUGGING MODE");
	current_program->flag_debugging = 1;
	needs_debug_item = 1;
	cobc_cs_check = 0;
	cb_build_debug_item ();
  }
    break;

  case 62:

/* Line 1806 of yacc.c  */
#line 2611 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_repeated ("OBJECT-COMPUTER", SYN_CLAUSE_2, &check_comp_duplicate);
  }
    break;

  case 74:

/* Line 1806 of yacc.c  */
#line 2640 "parser.y"
    {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
    break;

  case 75:

/* Line 1806 of yacc.c  */
#line 2648 "parser.y"
    {
	current_program->collating_sequence = (yyvsp[(3) - (3)]);
  }
    break;

  case 76:

/* Line 1806 of yacc.c  */
#line 2655 "parser.y"
    {
	/* Ignore */
  }
    break;

  case 77:

/* Line 1806 of yacc.c  */
#line 2662 "parser.y"
    {
	if (current_program->classification) {
		cb_error (_("duplicate CLASSIFICATION clause"));
	} else {
		current_program->classification = (yyvsp[(4) - (4)]);
	}
  }
    break;

  case 78:

/* Line 1806 of yacc.c  */
#line 2673 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 79:

/* Line 1806 of yacc.c  */
#line 2677 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 80:

/* Line 1806 of yacc.c  */
#line 2681 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 81:

/* Line 1806 of yacc.c  */
#line 2685 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 85:

/* Line 1806 of yacc.c  */
#line 2699 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
  }
    break;

  case 86:

/* Line 1806 of yacc.c  */
#line 2704 "parser.y"
    {
	cobc_in_repository = 0;
  }
    break;

  case 89:

/* Line 1806 of yacc.c  */
#line 2712 "parser.y"
    {
	yyerrok;
  }
    break;

  case 92:

/* Line 1806 of yacc.c  */
#line 2724 "parser.y"
    {
	functions_are_all = 1;
  }
    break;

  case 93:

/* Line 1806 of yacc.c  */
#line 2728 "parser.y"
    {
	if ((yyvsp[(2) - (3)]) != cb_error_node) {
		setup_prototype ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), CB_FUNCTION_TYPE, 0);
	}
  }
    break;

  case 95:

/* Line 1806 of yacc.c  */
#line 2735 "parser.y"
    {
	  if ((yyvsp[(2) - (3)]) != cb_error_node
	      && cb_verify (cb_program_prototypes, _("PROGRAM phrase"))) {
		setup_prototype ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), CB_PROGRAM_TYPE, 0);
	}
  }
    break;

  case 96:

/* Line 1806 of yacc.c  */
#line 2745 "parser.y"
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[(1) - (1)]));
  }
    break;

  case 97:

/* Line 1806 of yacc.c  */
#line 2750 "parser.y"
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[(2) - (2)]));
  }
    break;

  case 99:

/* Line 1806 of yacc.c  */
#line 2761 "parser.y"
    {
	check_duplicate = 0;
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	header_check |= COBC_HD_SPECIAL_NAMES;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
  }
    break;

  case 118:

/* Line 1806 of yacc.c  */
#line 2806 "parser.y"
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
		system_name[15] = 0;
		strncpy(system_name, CB_NAME ((yyvsp[(1) - (1)])), 15);
		if (system_name [6] == '_') {
			system_name [6] = ' ';
		}
		/* lookup system name */
		save_tree = lookup_system_name (system_name);
		if (!save_tree) {
			cb_error_x ((yyvsp[(1) - (1)]), _("invalid system-name '%s'"), system_name);
		}
	}
  }
    break;

  case 120:

/* Line 1806 of yacc.c  */
#line 2835 "parser.y"
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_DEVICE_CONSOLE) {
			cb_error_x (save_tree, _("invalid %s clause"), "");
		} else {
			current_program->flag_console_is_crt = 1;
		}
	}
  }
    break;

  case 121:

/* Line 1806 of yacc.c  */
#line 2846 "parser.y"
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (save_tree, _("invalid %s clause"), "SPECIAL NAMES");
		} else if (CB_VALID_TREE ((yyvsp[(3) - (3)]))) {
			CB_SYSTEM_NAME(save_tree)->value = (yyvsp[(1) - (3)]);
			cb_define ((yyvsp[(3) - (3)]), save_tree);
			CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
					(yyvsp[(3) - (3)]), save_tree);
		}
	}
  }
    break;

  case 122:

/* Line 1806 of yacc.c  */
#line 2859 "parser.y"
    {
	if (save_tree && CB_VALID_TREE ((yyvsp[(2) - (3)]))) {
		cb_define ((yyvsp[(2) - (3)]), save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				(yyvsp[(2) - (3)]), save_tree);
	}
  }
    break;

  case 126:

/* Line 1806 of yacc.c  */
#line 2875 "parser.y"
    {
	  check_on_off_duplicate = 0;
  }
    break;

  case 127:

/* Line 1806 of yacc.c  */
#line 2882 "parser.y"
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[(3) - (3)]), save_tree, (yyvsp[(1) - (3)]) == cb_int1);
	if (x) {
		if ((yyvsp[(1) - (3)]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[(3) - (3)]), x);
	}
  }
    break;

  case 128:

/* Line 1806 of yacc.c  */
#line 2897 "parser.y"
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[(4) - (4)]), save_tree, (yyvsp[(2) - (4)]) == cb_int1);
	if (x) {
		if ((yyvsp[(2) - (4)]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[(4) - (4)]), x);
	}
  }
    break;

  case 129:

/* Line 1806 of yacc.c  */
#line 2917 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		(yyval) = NULL;
	} else {
		/* Returns null on error */
		(yyval) = cb_build_alphabet_name ((yyvsp[(2) - (2)]));
	}
  }
    break;

  case 130:

/* Line 1806 of yacc.c  */
#line 2930 "parser.y"
    {
	if ((yyvsp[(3) - (5)])) {
		current_program->alphabet_name_list =
			cb_list_add (current_program->alphabet_name_list, (yyvsp[(3) - (5)]));
	}
	cobc_cs_check = 0;
  }
    break;

  case 131:

/* Line 1806 of yacc.c  */
#line 2941 "parser.y"
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_NATIVE;
	}
  }
    break;

  case 132:

/* Line 1806 of yacc.c  */
#line 2947 "parser.y"
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
    break;

  case 133:

/* Line 1806 of yacc.c  */
#line 2953 "parser.y"
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
    break;

  case 134:

/* Line 1806 of yacc.c  */
#line 2959 "parser.y"
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_EBCDIC;
	}
  }
    break;

  case 135:

/* Line 1806 of yacc.c  */
#line 2965 "parser.y"
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
    break;

  case 136:

/* Line 1806 of yacc.c  */
#line 2971 "parser.y"
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_CUSTOM;
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->custom_list = (yyvsp[(1) - (1)]);
	}
  }
    break;

  case 137:

/* Line 1806 of yacc.c  */
#line 2981 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 138:

/* Line 1806 of yacc.c  */
#line 2985 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 139:

/* Line 1806 of yacc.c  */
#line 2992 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 140:

/* Line 1806 of yacc.c  */
#line 2996 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 141:

/* Line 1806 of yacc.c  */
#line 3000 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (2)]));
  }
    break;

  case 142:

/* Line 1806 of yacc.c  */
#line 3004 "parser.y"
    {
	(yyval) = (yyvsp[(3) - (4)]);
  }
    break;

  case 143:

/* Line 1806 of yacc.c  */
#line 3011 "parser.y"
    {
	cb_list_add ((yyvsp[(0) - (1)]), (yyvsp[(1) - (1)]));
  }
    break;

  case 144:

/* Line 1806 of yacc.c  */
#line 3015 "parser.y"
    {
	cb_list_add ((yyvsp[(0) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 145:

/* Line 1806 of yacc.c  */
#line 3021 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 146:

/* Line 1806 of yacc.c  */
#line 3022 "parser.y"
    { (yyval) = cb_space; }
    break;

  case 147:

/* Line 1806 of yacc.c  */
#line 3023 "parser.y"
    { (yyval) = cb_zero; }
    break;

  case 148:

/* Line 1806 of yacc.c  */
#line 3024 "parser.y"
    { (yyval) = cb_quote; }
    break;

  case 149:

/* Line 1806 of yacc.c  */
#line 3025 "parser.y"
    { (yyval) = cb_norm_high; }
    break;

  case 150:

/* Line 1806 of yacc.c  */
#line 3026 "parser.y"
    { (yyval) = cb_norm_low; }
    break;

  case 151:

/* Line 1806 of yacc.c  */
#line 3030 "parser.y"
    { (yyval) = cb_space; }
    break;

  case 152:

/* Line 1806 of yacc.c  */
#line 3031 "parser.y"
    { (yyval) = cb_zero; }
    break;

  case 153:

/* Line 1806 of yacc.c  */
#line 3039 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else if ((yyvsp[(1) - (2)])) {
		CB_CHAIN_PAIR (current_program->symbolic_char_list, (yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
	}
  }
    break;

  case 154:

/* Line 1806 of yacc.c  */
#line 3053 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 155:

/* Line 1806 of yacc.c  */
#line 3057 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 156:

/* Line 1806 of yacc.c  */
#line 3065 "parser.y"
    {
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 157:

/* Line 1806 of yacc.c  */
#line 3072 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 158:

/* Line 1806 of yacc.c  */
#line 3076 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		(yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
	} else {
		(yyval) = (yyvsp[(1) - (2)]);
	}
  }
    break;

  case 159:

/* Line 1806 of yacc.c  */
#line 3087 "parser.y"
    {
	cb_tree		l1;
	cb_tree		l2;

	if (cb_list_length ((yyvsp[(1) - (3)])) != cb_list_length ((yyvsp[(3) - (3)]))) {
		cb_error (_("invalid %s clause"), "SYMBOLIC");
		(yyval) = NULL;
	} else {
		l1 = (yyvsp[(1) - (3)]);
		l2 = (yyvsp[(3) - (3)]);
		for (; l1; l1 = CB_CHAIN (l1), l2 = CB_CHAIN (l2)) {
			CB_PURPOSE (l1) = CB_VALUE (l2);
		}
		(yyval) = (yyvsp[(1) - (3)]);
	}
  }
    break;

  case 160:

/* Line 1806 of yacc.c  */
#line 3107 "parser.y"
    {
	if ((yyvsp[(1) - (1)]) == NULL) {
		(yyval) = NULL;
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
	}
  }
    break;

  case 161:

/* Line 1806 of yacc.c  */
#line 3115 "parser.y"
    {
	if ((yyvsp[(2) - (2)]) == NULL) {
		(yyval) = (yyvsp[(1) - (2)]);
	} else {
		(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
	}
  }
    break;

  case 162:

/* Line 1806 of yacc.c  */
#line 3125 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 163:

/* Line 1806 of yacc.c  */
#line 3126 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 164:

/* Line 1806 of yacc.c  */
#line 3133 "parser.y"
    {
	cb_tree		x;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		x = cb_build_class_name ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
		if (x) {
			current_program->class_name_list =
				cb_list_add (current_program->class_name_list, x);
		}
	}
  }
    break;

  case 165:

/* Line 1806 of yacc.c  */
#line 3153 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 166:

/* Line 1806 of yacc.c  */
#line 3154 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 167:

/* Line 1806 of yacc.c  */
#line 3159 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 168:

/* Line 1806 of yacc.c  */
#line 3163 "parser.y"
    {
	if (CB_TREE_CLASS ((yyvsp[(1) - (3)])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[(1) - (3)])) && CB_LITERAL ((yyvsp[(1) - (3)]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (CB_TREE_CLASS ((yyvsp[(3) - (3)])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[(3) - (3)])) && CB_LITERAL ((yyvsp[(3) - (3)]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (literal_value ((yyvsp[(1) - (3)])) <= literal_value ((yyvsp[(3) - (3)]))) {
		(yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
	} else {
		(yyval) = CB_BUILD_PAIR ((yyvsp[(3) - (3)]), (yyvsp[(1) - (3)]));
	}
  }
    break;

  case 169:

/* Line 1806 of yacc.c  */
#line 3184 "parser.y"
    {
	cb_tree	l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		l = cb_build_locale_name ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
		if (l) {
			current_program->locale_list =
				cb_list_add (current_program->locale_list, l);
		}
	}
  }
    break;

  case 170:

/* Line 1806 of yacc.c  */
#line 3207 "parser.y"
    {
	unsigned char	*s = CB_LITERAL ((yyvsp[(4) - (5)]))->data;
	unsigned int	error_ind = 0;
	unsigned int	char_seen = 0;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURRENCY", SYN_CLAUSE_1, &check_duplicate);
		if (strcmp("$", (const char *)s) != 0) {
			if ((yyvsp[(5) - (5)]) && CB_LITERAL ((yyvsp[(4) - (5)]))->size != 1) {
				CB_PENDING_X ((yyvsp[(4) - (5)]), _("CURRENCY SIGN longer than one character"));
				error_ind = 1;
			}
			while (*s) {
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
				case '+':
				case '-':
				case ',':
				case '.':
				case '*':
					error_ind = 2;
					break;
				case ' ':
					break;
				default:
					char_seen = 1;
					break;
				}
				s++;
			}
			if (!char_seen) {
				error_ind = 2;
			}
		} else {
			if (error_ind > 1) {;
				CB_PENDING_X ((yyvsp[(4) - (5)]), _("CURRENCY SIGN other than '$'"));
			}
		}
		switch (error_ind) {
		case 0:
		case 1:
			/* FIXME: currency sign/symbol are currently mixed in cobc and libcob */
			/* current_program->currency_sign = CB_LITERAL ($4); */
			break;
		default:
			cb_error_x ((yyvsp[(4) - (5)]), _("invalid CURRENCY SIGN '%s'"), (char*)CB_LITERAL ((yyvsp[(4) - (5)]))->data);
			break;
		}
		if ((yyvsp[(5) - (5)])) {
			set_currency_picture_symbol ((yyvsp[(5) - (5)]));
		} else {
			set_currency_picture_symbol ((yyvsp[(4) - (5)]));
		}
	}
  }
    break;

  case 171:

/* Line 1806 of yacc.c  */
#line 3281 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 172:

/* Line 1806 of yacc.c  */
#line 3285 "parser.y"
    {
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 173:

/* Line 1806 of yacc.c  */
#line 3294 "parser.y"
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
    break;

  case 174:

/* Line 1806 of yacc.c  */
#line 3313 "parser.y"
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
    break;

  case 175:

/* Line 1806 of yacc.c  */
#line 3329 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURSOR", SYN_CLAUSE_3, &check_duplicate);
		current_program->cursor_pos = (yyvsp[(3) - (3)]);
	}
  }
    break;

  case 176:

/* Line 1806 of yacc.c  */
#line 3347 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CRT STATUS", SYN_CLAUSE_4, &check_duplicate);
		current_program->crt_status = (yyvsp[(4) - (4)]);
	}
  }
    break;

  case 177:

/* Line 1806 of yacc.c  */
#line 3365 "parser.y"
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
    break;

  case 178:

/* Line 1806 of yacc.c  */
#line 3382 "parser.y"
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
    break;

  case 179:

/* Line 1806 of yacc.c  */
#line 3403 "parser.y"
    {
	cb_validate_program_environment (current_program);
  }
    break;

  case 181:

/* Line 1806 of yacc.c  */
#line 3410 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_INPUT_OUTPUT_SECTION;
  }
    break;

  case 183:

/* Line 1806 of yacc.c  */
#line 3418 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_FILE_CONTROL;
  }
    break;

  case 185:

/* Line 1806 of yacc.c  */
#line 3427 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_I_O_CONTROL;
  }
    break;

  case 188:

/* Line 1806 of yacc.c  */
#line 3442 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_FILE_CONTROL, 0);
	check_duplicate = 0;
	if (CB_VALID_TREE ((yyvsp[(3) - (3)]))) {
		/* Build new file */
		current_file = build_file ((yyvsp[(3) - (3)]));
		current_file->optional = CB_INTEGER ((yyvsp[(2) - (3)]))->val;

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
    break;

  case 189:

/* Line 1806 of yacc.c  */
#line 3464 "parser.y"
    {
	if (CB_VALID_TREE ((yyvsp[(3) - (6)]))) {
		validate_file (current_file, (yyvsp[(3) - (6)]));
	}
  }
    break;

  case 205:

/* Line 1806 of yacc.c  */
#line 3496 "parser.y"
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
  }
    break;

  case 206:

/* Line 1806 of yacc.c  */
#line 3502 "parser.y"
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[(5) - (5)])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
    break;

  case 207:

/* Line 1806 of yacc.c  */
#line 3512 "parser.y"
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[(5) - (5)])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
    break;

  case 208:

/* Line 1806 of yacc.c  */
#line 3523 "parser.y"
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[(5) - (5)])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdout", (size_t)6);
		current_file->special = COB_SELECT_STDOUT;
	}
  }
    break;

  case 209:

/* Line 1806 of yacc.c  */
#line 3536 "parser.y"
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[(5) - (5)])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdin", (size_t)5);
		current_file->special = COB_SELECT_STDIN;
	}
  }
    break;

  case 210:

/* Line 1806 of yacc.c  */
#line 3549 "parser.y"
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[(5) - (5)])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
	} else {
		/* RM/COBOL always expects an assignment name here - we ignore this
		   for PRINTER + PRINTER-1 as ACUCOBOL allows this for using as alias */
		current_file->flag_ext_assign = 0;
		if ((yyvsp[(4) - (5)]) == cb_int0) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER",	(size_t)7);
		} else if ((yyvsp[(4) - (5)]) == cb_int1) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER-1",	(size_t)9);
		} else {
			current_file->assign =
				cb_build_alphanumeric_literal ("LPT1",	(size_t)4);
		}

	}
  }
    break;

  case 211:

/* Line 1806 of yacc.c  */
#line 3581 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 212:

/* Line 1806 of yacc.c  */
#line 3582 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 213:

/* Line 1806 of yacc.c  */
#line 3583 "parser.y"
    { (yyval) = cb_int4; }
    break;

  case 226:

/* Line 1806 of yacc.c  */
#line 3606 "parser.y"
    {
	current_file->flag_line_adv = 1;
  }
    break;

  case 228:

/* Line 1806 of yacc.c  */
#line 3613 "parser.y"
    {
	current_file->flag_ext_assign = 1;
  }
    break;

  case 232:

/* Line 1806 of yacc.c  */
#line 3626 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 235:

/* Line 1806 of yacc.c  */
#line 3638 "parser.y"
    {
	cobc_cs_check = 0;
	check_repeated ("ACCESS", SYN_CLAUSE_2, &check_duplicate);
  }
    break;

  case 236:

/* Line 1806 of yacc.c  */
#line 3645 "parser.y"
    { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
    break;

  case 237:

/* Line 1806 of yacc.c  */
#line 3646 "parser.y"
    { current_file->access_mode = COB_ACCESS_DYNAMIC; }
    break;

  case 238:

/* Line 1806 of yacc.c  */
#line 3647 "parser.y"
    { current_file->access_mode = COB_ACCESS_RANDOM; }
    break;

  case 239:

/* Line 1806 of yacc.c  */
#line 3655 "parser.y"
    {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	p = cobc_parse_malloc (sizeof (struct cb_alt_key));
	p->key = (yyvsp[(5) - (7)]);
	p->duplicates = CB_INTEGER ((yyvsp[(6) - (7)]))->val;
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
    break;

  case 240:

/* Line 1806 of yacc.c  */
#line 3678 "parser.y"
    { }
    break;

  case 241:

/* Line 1806 of yacc.c  */
#line 3681 "parser.y"
    {
	CB_PENDING ("SUPPRESS WHEN ALL");
  }
    break;

  case 242:

/* Line 1806 of yacc.c  */
#line 3686 "parser.y"
    {
	CB_PENDING ("SUPPRESS WHEN SPACE/ZERO");
  }
    break;

  case 243:

/* Line 1806 of yacc.c  */
#line 3696 "parser.y"
    {
	check_repeated ("COLLATING", SYN_CLAUSE_3, &check_duplicate);
	CB_PENDING ("COLLATING SEQUENCE");
  }
    break;

  case 244:

/* Line 1806 of yacc.c  */
#line 3704 "parser.y"
    {
	  if (CB_ALPHABET_NAME_P (cb_ref ((yyvsp[(1) - (1)])))) {
		  (yyval) = (yyvsp[(1) - (1)]);
	  } else {
		  cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not an alphabet-name"),
			      cb_name ((yyvsp[(1) - (1)])));
		  (yyval) = cb_error_node;
	  }
  }
    break;

  case 245:

/* Line 1806 of yacc.c  */
#line 3719 "parser.y"
    {
	check_repeated ("STATUS", SYN_CLAUSE_4, &check_duplicate);
	current_file->file_status = (yyvsp[(4) - (4)]);
  }
    break;

  case 249:

/* Line 1806 of yacc.c  */
#line 3734 "parser.y"
    {
	check_repeated ("LOCK", SYN_CLAUSE_5, &check_duplicate);
  }
    break;

  case 251:

/* Line 1806 of yacc.c  */
#line 3742 "parser.y"
    {
	current_file->lock_mode |= COB_LOCK_MANUAL;
	cobc_cs_check = 0;
  }
    break;

  case 252:

/* Line 1806 of yacc.c  */
#line 3747 "parser.y"
    {
	current_file->lock_mode |= COB_LOCK_AUTOMATIC;
	cobc_cs_check = 0;
  }
    break;

  case 253:

/* Line 1806 of yacc.c  */
#line 3752 "parser.y"
    {
	current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
	cobc_cs_check = 0;
  }
    break;

  case 256:

/* Line 1806 of yacc.c  */
#line 3761 "parser.y"
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
    break;

  case 257:

/* Line 1806 of yacc.c  */
#line 3765 "parser.y"
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
	CB_PENDING ("WITH ROLLBACK");
  }
    break;

  case 260:

/* Line 1806 of yacc.c  */
#line 3781 "parser.y"
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_INDEXED;
  }
    break;

  case 261:

/* Line 1806 of yacc.c  */
#line 3786 "parser.y"
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_SEQUENTIAL;
  }
    break;

  case 262:

/* Line 1806 of yacc.c  */
#line 3791 "parser.y"
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_RELATIVE;
  }
    break;

  case 263:

/* Line 1806 of yacc.c  */
#line 3796 "parser.y"
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
  }
    break;

  case 264:

/* Line 1806 of yacc.c  */
#line 3807 "parser.y"
    {
	check_repeated ("PADDING", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
    break;

  case 265:

/* Line 1806 of yacc.c  */
#line 3818 "parser.y"
    {
	check_repeated ("RECORD DELIMITER", SYN_CLAUSE_8, &check_duplicate);
  }
    break;

  case 266:

/* Line 1806 of yacc.c  */
#line 3828 "parser.y"
    {
	check_repeated ("RECORD KEY", SYN_CLAUSE_9, &check_duplicate);
	current_file->key = (yyvsp[(4) - (4)]);
  }
    break;

  case 267:

/* Line 1806 of yacc.c  */
#line 3835 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 268:

/* Line 1806 of yacc.c  */
#line 3836 "parser.y"
    { CB_PENDING ("SPLIT KEYS"); }
    break;

  case 269:

/* Line 1806 of yacc.c  */
#line 3837 "parser.y"
    { CB_PENDING ("SPLIT KEYS"); }
    break;

  case 270:

/* Line 1806 of yacc.c  */
#line 3844 "parser.y"
    {
	check_repeated ("RELATIVE KEY", SYN_CLAUSE_10, &check_duplicate);
	current_file->key = (yyvsp[(4) - (4)]);
  }
    break;

  case 271:

/* Line 1806 of yacc.c  */
#line 3855 "parser.y"
    {
	check_repeated ("RESERVE", SYN_CLAUSE_11, &check_duplicate);
  }
    break;

  case 274:

/* Line 1806 of yacc.c  */
#line 3869 "parser.y"
    {
	check_repeated ("SHARING", SYN_CLAUSE_12, &check_duplicate);
	current_file->sharing = (yyvsp[(3) - (3)]);
  }
    break;

  case 275:

/* Line 1806 of yacc.c  */
#line 3876 "parser.y"
    { (yyval) = NULL; }
    break;

  case 276:

/* Line 1806 of yacc.c  */
#line 3877 "parser.y"
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
    break;

  case 277:

/* Line 1806 of yacc.c  */
#line 3878 "parser.y"
    { (yyval) = NULL; }
    break;

  case 280:

/* Line 1806 of yacc.c  */
#line 3887 "parser.y"
    {
	yyerrok;
  }
    break;

  case 285:

/* Line 1806 of yacc.c  */
#line 3906 "parser.y"
    {
	cb_tree l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	switch (CB_INTEGER ((yyvsp[(2) - (5)]))->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = (yyvsp[(5) - (5)]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l))) {
				CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = same_area;
			}
		}
		same_area++;
		break;
	case 2:
		/* SAME SORT-MERGE */
		break;
	}
  }
    break;

  case 286:

/* Line 1806 of yacc.c  */
#line 3933 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 287:

/* Line 1806 of yacc.c  */
#line 3934 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 288:

/* Line 1806 of yacc.c  */
#line 3935 "parser.y"
    { (yyval) = cb_int2; }
    break;

  case 289:

/* Line 1806 of yacc.c  */
#line 3936 "parser.y"
    { (yyval) = cb_int2; }
    break;

  case 290:

/* Line 1806 of yacc.c  */
#line 3943 "parser.y"
    {
	/* Fake for TAPE */
	cobc_cs_check = CB_CS_ASSIGN;
  }
    break;

  case 291:

/* Line 1806 of yacc.c  */
#line 3948 "parser.y"
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
	cobc_cs_check = 0;
  }
    break;

  case 297:

/* Line 1806 of yacc.c  */
#line 3977 "parser.y"
    {
	current_storage = CB_STORAGE_WORKING;
  }
    break;

  case 298:

/* Line 1806 of yacc.c  */
#line 3986 "parser.y"
    {
	cb_validate_program_data (current_program);
  }
    break;

  case 300:

/* Line 1806 of yacc.c  */
#line 3993 "parser.y"
    {
	header_check |= COBC_HD_DATA_DIVISION;
  }
    break;

  case 302:

/* Line 1806 of yacc.c  */
#line 4002 "parser.y"
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_FILE_SECTION;
  }
    break;

  case 305:

/* Line 1806 of yacc.c  */
#line 4016 "parser.y"
    {
	if (CB_VALID_TREE (current_file)) {
		if (CB_VALID_TREE ((yyvsp[(2) - (2)]))) {
			if (current_file->reports) {
				cb_error (_("RECORD description invalid with REPORT"));
			} else {
				finalize_file (current_file, CB_FIELD ((yyvsp[(2) - (2)])));
			}
		} else if (!current_file->reports) {
			cb_error (_("RECORD description missing or invalid"));
		}
	}
  }
    break;

  case 306:

/* Line 1806 of yacc.c  */
#line 4035 "parser.y"
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION,
			       COBC_HD_FILE_SECTION, 0, 0);
	check_duplicate = 0;
	if (CB_INVALID_TREE ((yyvsp[(2) - (2)])) || cb_ref ((yyvsp[(2) - (2)])) == cb_error_node) {
		YYERROR;
	}
	current_file = CB_FILE (cb_ref ((yyvsp[(2) - (2)])));
	if (CB_VALID_TREE (current_file)) {
		if ((yyvsp[(1) - (2)])) {
			current_file->organization = COB_ORG_SORT;
		}
	}
  }
    break;

  case 308:

/* Line 1806 of yacc.c  */
#line 4052 "parser.y"
    {
	yyerrok;
  }
    break;

  case 309:

/* Line 1806 of yacc.c  */
#line 4059 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 310:

/* Line 1806 of yacc.c  */
#line 4063 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 313:

/* Line 1806 of yacc.c  */
#line 4074 "parser.y"
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_1, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_global) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	current_file->flag_external = 1;
  }
    break;

  case 314:

/* Line 1806 of yacc.c  */
#line 4084 "parser.y"
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
    break;

  case 324:

/* Line 1806 of yacc.c  */
#line 4114 "parser.y"
    {
	check_repeated ("BLOCK", SYN_CLAUSE_3, &check_duplicate);
	/* ignore */
  }
    break;

  case 328:

/* Line 1806 of yacc.c  */
#line 4127 "parser.y"
    {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		if (warningopt) {
			cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
		}
	} else {
		current_file->record_max = cb_get_int ((yyvsp[(3) - (4)]));
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
    break;

  case 329:

/* Line 1806 of yacc.c  */
#line 4147 "parser.y"
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		if (warningopt) {
			cb_warning (_("RECORD clause ignored for LINE SEQUENTIAL"));
		}
	} else {
		current_file->record_min = cb_get_int ((yyvsp[(3) - (6)]));
		current_file->record_max = cb_get_int ((yyvsp[(5) - (6)]));
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
    break;

  case 330:

/* Line 1806 of yacc.c  */
#line 4182 "parser.y"
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	current_file->record_min = (yyvsp[(6) - (9)]) ? cb_get_int ((yyvsp[(6) - (9)])) : 0;
	current_file->record_max = (yyvsp[(7) - (9)]) ? cb_get_int ((yyvsp[(7) - (9)])) : 0;
	if ((yyvsp[(6) - (9)]) && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if ((yyvsp[(7) - (9)]) && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if ((yyvsp[(7) - (9)]) && current_file->record_max > MAX_FD_RECORD)  {
		current_file->record_max = MAX_FD_RECORD;
		cb_error (_("RECORD size exceeds maximum allowed (%d)"),
			  MAX_FD_RECORD);
		error_ind = 1;
	}
	if (((yyvsp[(6) - (9)]) || (yyvsp[(7) - (9)])) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
    break;

  case 332:

/* Line 1806 of yacc.c  */
#line 4213 "parser.y"
    {
	current_file->record_depending = (yyvsp[(3) - (3)]);
  }
    break;

  case 333:

/* Line 1806 of yacc.c  */
#line 4219 "parser.y"
    { (yyval) = NULL; }
    break;

  case 334:

/* Line 1806 of yacc.c  */
#line 4220 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 335:

/* Line 1806 of yacc.c  */
#line 4224 "parser.y"
    { (yyval) = NULL; }
    break;

  case 336:

/* Line 1806 of yacc.c  */
#line 4225 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 337:

/* Line 1806 of yacc.c  */
#line 4233 "parser.y"
    {
	check_repeated ("LABEL", SYN_CLAUSE_5, &check_duplicate);
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
    break;

  case 338:

/* Line 1806 of yacc.c  */
#line 4244 "parser.y"
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
    break;

  case 339:

/* Line 1806 of yacc.c  */
#line 4249 "parser.y"
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[(5) - (5)]));
	}
  }
    break;

  case 344:

/* Line 1806 of yacc.c  */
#line 4272 "parser.y"
    {
	check_repeated ("DATA", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
    break;

  case 345:

/* Line 1806 of yacc.c  */
#line 4284 "parser.y"
    {
	check_repeated ("LINAGE", SYN_CLAUSE_8, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
	} else {
		current_file->linage = (yyvsp[(3) - (5)]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
    break;

  case 351:

/* Line 1806 of yacc.c  */
#line 4312 "parser.y"
    {
	current_file->latfoot = (yyvsp[(4) - (4)]);
  }
    break;

  case 352:

/* Line 1806 of yacc.c  */
#line 4319 "parser.y"
    {
	current_file->lattop = (yyvsp[(2) - (2)]);
  }
    break;

  case 353:

/* Line 1806 of yacc.c  */
#line 4326 "parser.y"
    {
	current_file->latbot = (yyvsp[(2) - (2)]);
  }
    break;

  case 354:

/* Line 1806 of yacc.c  */
#line 4335 "parser.y"
    {
	cobc_cs_check = 0;
	check_repeated ("RECORDING", SYN_CLAUSE_9, &check_duplicate);
	/* ignore */
  }
    break;

  case 359:

/* Line 1806 of yacc.c  */
#line 4348 "parser.y"
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORDING MODE U or S can only be used with RECORD SEQUENTIAL files"));
	}
  }
    break;

  case 362:

/* Line 1806 of yacc.c  */
#line 4364 "parser.y"
    {
	struct cb_alphabet_name	*al;

	check_repeated ("CODE SET", SYN_CLAUSE_10, &check_duplicate);

	if (CB_VALID_TREE ((yyvsp[(3) - (4)]))) {
		al = CB_ALPHABET_NAME (cb_ref ((yyvsp[(3) - (4)])));
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
			if (warningopt && CB_VALID_TREE ((yyvsp[(3) - (4)]))) {
				cb_warning_x ((yyvsp[(3) - (4)]), _("ignoring CODE-SET '%s'"),
						  cb_name ((yyvsp[(3) - (4)])));
			}
			break;
		}
	}

	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("CODE-SET clause invalid for file type"));
	}

	if (warningopt) {
		CB_PENDING ("CODE-SET");
	}
  }
    break;

  case 364:

/* Line 1806 of yacc.c  */
#line 4402 "parser.y"
    {
	  if (warningopt) {
		  CB_PENDING ("FOR sub-records");
	  }

	  current_file->code_set_items = CB_LIST ((yyvsp[(2) - (2)]));
  }
    break;

  case 365:

/* Line 1806 of yacc.c  */
#line 4415 "parser.y"
    {
	check_repeated ("REPORT", SYN_CLAUSE_11, &check_duplicate);
	CB_PENDING("REPORT WRITER");
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("REPORT clause with wrong file type"));
	} else {
		current_file->reports = (yyvsp[(2) - (2)]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	}
  }
    break;

  case 368:

/* Line 1806 of yacc.c  */
#line 4435 "parser.y"
    {
	current_report = build_report ((yyvsp[(1) - (1)]));
	current_report->file = current_file;
	CB_ADD_TO_CHAIN (CB_TREE (current_report), current_program->report_list);
	if (report_count == 0) {
		report_instance = current_report;
	}
	report_count++;
  }
    break;

  case 369:

/* Line 1806 of yacc.c  */
#line 4445 "parser.y"
    {
	current_report = build_report ((yyvsp[(2) - (2)]));
	CB_ADD_TO_CHAIN (CB_TREE (current_report), current_program->report_list);
	if (report_count == 0) {
		report_instance = current_report;
	}
	report_count++;
  }
    break;

  case 371:

/* Line 1806 of yacc.c  */
#line 4459 "parser.y"
    {
	current_storage = CB_STORAGE_COMMUNICATION;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_COMMUNICATION_SECTION;
	/* add a compiler configuration if either */
	if (cb_std_define > CB_STD_85) {
		cb_verify (CB_UNCONFORMABLE, _ ("COMMUNICATION SECTION"));
	} else if (cb_verify (CB_OBSOLETE, _("COMMUNICATION SECTION"))) {
		CB_PENDING ("COMMUNICATION SECTION");
	}
  }
    break;

  case 375:

/* Line 1806 of yacc.c  */
#line 4480 "parser.y"
    {
	if (CB_VALID_TREE (current_cd)) {
		if (CB_VALID_TREE ((yyvsp[(2) - (2)]))) {
			cb_finalize_cd (current_cd, CB_FIELD ((yyvsp[(2) - (2)])));
		} else if (!current_cd->record) {
			cb_error (_("CD record missing"));
		}
	}
  }
    break;

  case 376:

/* Line 1806 of yacc.c  */
#line 4495 "parser.y"
    {
	/* CD internally defines a new file */
	if (CB_VALID_TREE ((yyvsp[(2) - (2)]))) {
		current_cd = cb_build_cd ((yyvsp[(2) - (2)]));

		CB_ADD_TO_CHAIN (CB_TREE (current_cd),
				 current_program->cd_list);
	} else {
		current_cd = NULL;
		/* TO-DO: Is this necessary? */
		if (current_program->cd_list) {
			current_program->cd_list
				= CB_CHAIN (current_program->cd_list);
		}
	}
	check_duplicate = 0;
  }
    break;

  case 424:

/* Line 1806 of yacc.c  */
#line 4603 "parser.y"
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_WORKING_STORAGE_SECTION;
	current_storage = CB_STORAGE_WORKING;
  }
    break;

  case 425:

/* Line 1806 of yacc.c  */
#line 4609 "parser.y"
    {
	if ((yyvsp[(5) - (5)])) {
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD ((yyvsp[(5) - (5)])));
	}
  }
    break;

  case 426:

/* Line 1806 of yacc.c  */
#line 4618 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 427:

/* Line 1806 of yacc.c  */
#line 4621 "parser.y"
    {
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
    break;

  case 428:

/* Line 1806 of yacc.c  */
#line 4627 "parser.y"
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	(yyval) = CB_TREE (description_field);
  }
    break;

  case 434:

/* Line 1806 of yacc.c  */
#line 4647 "parser.y"
    {
	if (set_current_field ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]))) {
		YYERROR;
	}
  }
    break;

  case 435:

/* Line 1806 of yacc.c  */
#line 4653 "parser.y"
    {
	if (!qualifier) {
		current_field->flag_filler = 1;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
    break;

  case 436:

/* Line 1806 of yacc.c  */
#line 4662 "parser.y"
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[(1) - (3)]));
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
    break;

  case 437:

/* Line 1806 of yacc.c  */
#line 4675 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 438:

/* Line 1806 of yacc.c  */
#line 4682 "parser.y"
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
    break;

  case 439:

/* Line 1806 of yacc.c  */
#line 4688 "parser.y"
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	non_const_word = 0;
  }
    break;

  case 441:

/* Line 1806 of yacc.c  */
#line 4698 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	qualifier = (yyvsp[(1) - (1)]);
	non_const_word = 0;
  }
    break;

  case 442:

/* Line 1806 of yacc.c  */
#line 4707 "parser.y"
    {
	(yyval)= NULL;
  }
    break;

  case 443:

/* Line 1806 of yacc.c  */
#line 4711 "parser.y"
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
		(yyval)= NULL;
	} else {
		(yyval) = cb_null;
	}
  }
    break;

  case 444:

/* Line 1806 of yacc.c  */
#line 4722 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 445:

/* Line 1806 of yacc.c  */
#line 4723 "parser.y"
    { (yyval) = cb_build_const_length ((yyvsp[(2) - (2)])); }
    break;

  case 446:

/* Line 1806 of yacc.c  */
#line 4724 "parser.y"
    { (yyval) = cb_build_const_length ((yyvsp[(2) - (2)])); }
    break;

  case 447:

/* Line 1806 of yacc.c  */
#line 4725 "parser.y"
    { (yyval) = cb_build_const_length ((yyvsp[(3) - (3)])); }
    break;

  case 448:

/* Line 1806 of yacc.c  */
#line 4730 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 449:

/* Line 1806 of yacc.c  */
#line 4734 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 450:

/* Line 1806 of yacc.c  */
#line 4738 "parser.y"
    {
	(yyval) = cb_int2;
  }
    break;

  case 451:

/* Line 1806 of yacc.c  */
#line 4742 "parser.y"
    {
	(yyval) = cb_int4;
  }
    break;

  case 452:

/* Line 1806 of yacc.c  */
#line 4746 "parser.y"
    {
	(yyval) = cb_int (8);
  }
    break;

  case 453:

/* Line 1806 of yacc.c  */
#line 4750 "parser.y"
    {
	(yyval) = cb_int ((int)sizeof(long));
  }
    break;

  case 454:

/* Line 1806 of yacc.c  */
#line 4754 "parser.y"
    {
	(yyval) = cb_int ((int)sizeof(void *));
  }
    break;

  case 455:

/* Line 1806 of yacc.c  */
#line 4758 "parser.y"
    {
	(yyval) = cb_int ((int)sizeof(float));
  }
    break;

  case 456:

/* Line 1806 of yacc.c  */
#line 4762 "parser.y"
    {
	(yyval) = cb_int ((int)sizeof(double));
  }
    break;

  case 457:

/* Line 1806 of yacc.c  */
#line 4766 "parser.y"
    {
	(yyval) = cb_int (4);
  }
    break;

  case 458:

/* Line 1806 of yacc.c  */
#line 4770 "parser.y"
    {
	(yyval) = cb_int (8);
  }
    break;

  case 459:

/* Line 1806 of yacc.c  */
#line 4774 "parser.y"
    {
	(yyval) = cb_int (16);
  }
    break;

  case 460:

/* Line 1806 of yacc.c  */
#line 4778 "parser.y"
    {
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
    break;

  case 470:

/* Line 1806 of yacc.c  */
#line 4810 "parser.y"
    {
	if (set_current_field ((yyvsp[(1) - (5)]), (yyvsp[(2) - (5)]))) {
		YYERROR;
	}

	if (cb_ref ((yyvsp[(4) - (5)])) != cb_error_node) {
		error_if_invalid_level_for_renames ((yyvsp[(4) - (5)]));
		current_field->redefines = CB_FIELD (cb_ref ((yyvsp[(4) - (5)])));
	}

	if ((yyvsp[(5) - (5)])) {
		error_if_invalid_level_for_renames ((yyvsp[(5) - (5)]));
		current_field->rename_thru = CB_FIELD (cb_ref ((yyvsp[(5) - (5)])));
	} else {
		/* If there is no THRU clause, RENAMES acts like REDEFINES. */
		current_field->pic = current_field->redefines->pic;
	}

	cb_validate_renames_item (current_field);
  }
    break;

  case 471:

/* Line 1806 of yacc.c  */
#line 4834 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 472:

/* Line 1806 of yacc.c  */
#line 4838 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]) == cb_error_node ? NULL : (yyvsp[(2) - (2)]);
  }
    break;

  case 473:

/* Line 1806 of yacc.c  */
#line 4845 "parser.y"
    {
	if (set_current_field ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]))) {
		YYERROR;
	}
  }
    break;

  case 474:

/* Line 1806 of yacc.c  */
#line 4851 "parser.y"
    {
	cb_validate_88_item (current_field);
  }
    break;

  case 475:

/* Line 1806 of yacc.c  */
#line 4858 "parser.y"
    {
	cb_tree x;
	int	level;

	cobc_cs_check = 0;
	level = cb_get_level ((yyvsp[(1) - (5)]));
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[(1) - (5)]));
	if (level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	} else if ((yyvsp[(5) - (5)])) {
		x = cb_build_constant ((yyvsp[(2) - (5)]), (yyvsp[(5) - (5)]));
		CB_FIELD (x)->flag_item_78 = 1;
		CB_FIELD (x)->level = 1;
		cb_needs_01 = 1;
		if ((yyvsp[(4) - (5)])) {
			CB_FIELD (x)->flag_is_global = 1;
		}
		/* Ignore return value */
		(void)cb_validate_78_item (CB_FIELD (x), 0);
	}
  }
    break;

  case 476:

/* Line 1806 of yacc.c  */
#line 4881 "parser.y"
    {
	if (set_current_field ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]))) {
		YYERROR;
	}
  }
    break;

  case 477:

/* Line 1806 of yacc.c  */
#line 4887 "parser.y"
    {
	/* Reset to last non-78 item */
	current_field = cb_validate_78_item (current_field, 0);
  }
    break;

  case 478:

/* Line 1806 of yacc.c  */
#line 4895 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 479:

/* Line 1806 of yacc.c  */
#line 4899 "parser.y"
    {
	CB_PENDING ("CONSTANT FROM");
	(yyval) = NULL;
  }
    break;

  case 480:

/* Line 1806 of yacc.c  */
#line 4907 "parser.y"
    {
	/* Required to check redefines */
	(yyval) = NULL;
  }
    break;

  case 481:

/* Line 1806 of yacc.c  */
#line 4913 "parser.y"
    {
	/* Required to check redefines */
	(yyval) = cb_true;
  }
    break;

  case 495:

/* Line 1806 of yacc.c  */
#line 4940 "parser.y"
    {
	check_repeated ("REDEFINES", SYN_CLAUSE_1, &check_pic_duplicate);
	if ((yyvsp[(0) - (2)]) != NULL) {
		if (cb_relaxed_syntax_checks) {
			cb_warning_x ((yyvsp[(2) - (2)]), _("REDEFINES clause should follow entry-name"));
		} else {
			cb_error_x ((yyvsp[(2) - (2)]), _("REDEFINES clause must follow entry-name"));
		}
	}

	current_field->redefines = cb_resolve_redefines (current_field, (yyvsp[(2) - (2)]));
	if (current_field->redefines == NULL) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		YYERROR;
	}
  }
    break;

  case 496:

/* Line 1806 of yacc.c  */
#line 4964 "parser.y"
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
    break;

  case 497:

/* Line 1806 of yacc.c  */
#line 4991 "parser.y"
    {
	current_field->ename = cb_to_cname (current_field->name);
  }
    break;

  case 498:

/* Line 1806 of yacc.c  */
#line 4995 "parser.y"
    {
	current_field->ename = cb_to_cname ((const char *)CB_LITERAL ((yyvsp[(2) - (2)]))->data);
  }
    break;

  case 501:

/* Line 1806 of yacc.c  */
#line 5008 "parser.y"
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
    break;

  case 502:

/* Line 1806 of yacc.c  */
#line 5033 "parser.y"
    {
	check_repeated ("PICTURE", SYN_CLAUSE_4, &check_pic_duplicate);
	current_field->pic = CB_PICTURE ((yyvsp[(1) - (1)]));
  }
    break;

  case 505:

/* Line 1806 of yacc.c  */
#line 5049 "parser.y"
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
    break;

  case 506:

/* Line 1806 of yacc.c  */
#line 5053 "parser.y"
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
    break;

  case 507:

/* Line 1806 of yacc.c  */
#line 5057 "parser.y"
    {
	check_and_set_usage (CB_USAGE_FLOAT);
  }
    break;

  case 508:

/* Line 1806 of yacc.c  */
#line 5061 "parser.y"
    {
	check_and_set_usage (CB_USAGE_DOUBLE);
  }
    break;

  case 509:

/* Line 1806 of yacc.c  */
#line 5065 "parser.y"
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
    break;

  case 510:

/* Line 1806 of yacc.c  */
#line 5069 "parser.y"
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
    break;

  case 511:

/* Line 1806 of yacc.c  */
#line 5073 "parser.y"
    {
	check_and_set_usage (CB_USAGE_COMP_5);
  }
    break;

  case 512:

/* Line 1806 of yacc.c  */
#line 5077 "parser.y"
    {
	check_and_set_usage (CB_USAGE_COMP_6);
  }
    break;

  case 513:

/* Line 1806 of yacc.c  */
#line 5081 "parser.y"
    {
	check_and_set_usage (CB_USAGE_COMP_X);
  }
    break;

  case 514:

/* Line 1806 of yacc.c  */
#line 5085 "parser.y"
    {
	check_and_set_usage (CB_USAGE_DISPLAY);
  }
    break;

  case 515:

/* Line 1806 of yacc.c  */
#line 5089 "parser.y"
    {
	check_and_set_usage (CB_USAGE_INDEX);
  }
    break;

  case 516:

/* Line 1806 of yacc.c  */
#line 5093 "parser.y"
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
    break;

  case 517:

/* Line 1806 of yacc.c  */
#line 5097 "parser.y"
    {
	check_and_set_usage (CB_USAGE_POINTER);
	current_field->flag_is_pointer = 1;
  }
    break;

  case 518:

/* Line 1806 of yacc.c  */
#line 5102 "parser.y"
    {
	check_and_set_usage (CB_USAGE_PROGRAM_POINTER);
	current_field->flag_is_pointer = 1;
  }
    break;

  case 519:

/* Line 1806 of yacc.c  */
#line 5107 "parser.y"
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
    break;

  case 520:

/* Line 1806 of yacc.c  */
#line 5111 "parser.y"
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
    break;

  case 521:

/* Line 1806 of yacc.c  */
#line 5115 "parser.y"
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
    break;

  case 522:

/* Line 1806 of yacc.c  */
#line 5123 "parser.y"
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
    break;

  case 523:

/* Line 1806 of yacc.c  */
#line 5127 "parser.y"
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
    break;

  case 524:

/* Line 1806 of yacc.c  */
#line 5131 "parser.y"
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
    break;

  case 525:

/* Line 1806 of yacc.c  */
#line 5139 "parser.y"
    {
	check_and_set_usage (CB_USAGE_SIGNED_CHAR);
  }
    break;

  case 526:

/* Line 1806 of yacc.c  */
#line 5143 "parser.y"
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_CHAR);
  }
    break;

  case 527:

/* Line 1806 of yacc.c  */
#line 5147 "parser.y"
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
    break;

  case 528:

/* Line 1806 of yacc.c  */
#line 5151 "parser.y"
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
    break;

  case 529:

/* Line 1806 of yacc.c  */
#line 5155 "parser.y"
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
    break;

  case 530:

/* Line 1806 of yacc.c  */
#line 5159 "parser.y"
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
    break;

  case 531:

/* Line 1806 of yacc.c  */
#line 5163 "parser.y"
    {
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
  }
    break;

  case 532:

/* Line 1806 of yacc.c  */
#line 5167 "parser.y"
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
  }
    break;

  case 533:

/* Line 1806 of yacc.c  */
#line 5171 "parser.y"
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
    break;

  case 534:

/* Line 1806 of yacc.c  */
#line 5179 "parser.y"
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
    break;

  case 535:

/* Line 1806 of yacc.c  */
#line 5187 "parser.y"
    {
	check_and_set_usage (CB_USAGE_FP_BIN32);
  }
    break;

  case 536:

/* Line 1806 of yacc.c  */
#line 5191 "parser.y"
    {
	check_and_set_usage (CB_USAGE_FP_BIN64);
  }
    break;

  case 537:

/* Line 1806 of yacc.c  */
#line 5195 "parser.y"
    {
	check_and_set_usage (CB_USAGE_FP_BIN128);
  }
    break;

  case 538:

/* Line 1806 of yacc.c  */
#line 5199 "parser.y"
    {
	check_and_set_usage (CB_USAGE_FP_DEC64);
  }
    break;

  case 539:

/* Line 1806 of yacc.c  */
#line 5203 "parser.y"
    {
	check_and_set_usage (CB_USAGE_FP_DEC128);
  }
    break;

  case 540:

/* Line 1806 of yacc.c  */
#line 5207 "parser.y"
    {
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	CB_UNFINISHED ("USAGE NATIONAL");
  }
    break;

  case 545:

/* Line 1806 of yacc.c  */
#line 5227 "parser.y"
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[(3) - (3)]) ? 1 : 0);
	current_field->flag_sign_leading  = 1;
  }
    break;

  case 546:

/* Line 1806 of yacc.c  */
#line 5234 "parser.y"
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[(3) - (3)]) ? 1 : 0);
	current_field->flag_sign_leading  = 0;
  }
    break;

  case 547:

/* Line 1806 of yacc.c  */
#line 5248 "parser.y"
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[(2) - (6)]), (yyvsp[(3) - (6)]));
  }
    break;

  case 549:

/* Line 1806 of yacc.c  */
#line 5257 "parser.y"
    {
	current_field->step_count = cb_get_int ((yyvsp[(2) - (2)]));
  }
    break;

  case 550:

/* Line 1806 of yacc.c  */
#line 5267 "parser.y"
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]));
  }
    break;

  case 551:

/* Line 1806 of yacc.c  */
#line 5274 "parser.y"
    {
	current_field->flag_unbounded = 1;
	if (current_field->parent) {
		current_field->parent->flag_unbounded = 1;
	}
	current_field->depending = (yyvsp[(7) - (9)]);
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[(2) - (9)]), cb_int0);
  }
    break;

  case 552:

/* Line 1806 of yacc.c  */
#line 5286 "parser.y"
    {
	setup_occurs ();
	current_field->occurs_min = (yyvsp[(4) - (8)]) ? cb_get_int ((yyvsp[(4) - (8)])) : 0;
	if ((yyvsp[(5) - (8)])) {
		current_field->occurs_max = cb_get_int ((yyvsp[(5) - (8)]));
		if (current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_max = 0;
	}
	CB_PENDING("OCCURS DYNAMIC");
  }
    break;

  case 553:

/* Line 1806 of yacc.c  */
#line 5302 "parser.y"
    { (yyval) = NULL; }
    break;

  case 554:

/* Line 1806 of yacc.c  */
#line 5303 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 555:

/* Line 1806 of yacc.c  */
#line 5307 "parser.y"
    { (yyval) = NULL; }
    break;

  case 556:

/* Line 1806 of yacc.c  */
#line 5308 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 557:

/* Line 1806 of yacc.c  */
#line 5312 "parser.y"
    { (yyval) = NULL; }
    break;

  case 558:

/* Line 1806 of yacc.c  */
#line 5313 "parser.y"
    { (yyval) = (yyvsp[(1) - (2)]); }
    break;

  case 560:

/* Line 1806 of yacc.c  */
#line 5318 "parser.y"
    {
	current_field->depending = (yyvsp[(3) - (3)]);
  }
    break;

  case 562:

/* Line 1806 of yacc.c  */
#line 5324 "parser.y"
    {
	(yyval) = cb_build_index ((yyvsp[(3) - (3)]), cb_zero, 0, current_field);
	CB_FIELD_PTR ((yyval))->special_index = 1;
  }
    break;

  case 564:

/* Line 1806 of yacc.c  */
#line 5332 "parser.y"
    {
	/* current_field->initialized = 1; */
  }
    break;

  case 565:

/* Line 1806 of yacc.c  */
#line 5339 "parser.y"
    {
	if ((yyvsp[(1) - (1)])) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = (yyvsp[(1) - (1)]);
		nkeys = cb_list_length ((yyvsp[(1) - (1)]));
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
    break;

  case 566:

/* Line 1806 of yacc.c  */
#line 5362 "parser.y"
    { (yyval) = NULL; }
    break;

  case 567:

/* Line 1806 of yacc.c  */
#line 5365 "parser.y"
    {
	cb_tree l;

	for (l = (yyvsp[(5) - (5)]); l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[(2) - (5)]);
		if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
		    strcasecmp (CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
			CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
		}
	}
	(yyval) = cb_list_append ((yyvsp[(1) - (5)]), (yyvsp[(5) - (5)]));
  }
    break;

  case 568:

/* Line 1806 of yacc.c  */
#line 5380 "parser.y"
    { (yyval) = cb_int (COB_ASCENDING); }
    break;

  case 569:

/* Line 1806 of yacc.c  */
#line 5381 "parser.y"
    { (yyval) = cb_int (COB_DESCENDING); }
    break;

  case 571:

/* Line 1806 of yacc.c  */
#line 5386 "parser.y"
    {
	current_field->index_list = (yyvsp[(3) - (3)]);
  }
    break;

  case 572:

/* Line 1806 of yacc.c  */
#line 5392 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 573:

/* Line 1806 of yacc.c  */
#line 5394 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 574:

/* Line 1806 of yacc.c  */
#line 5399 "parser.y"
    {
	(yyval) = cb_build_index ((yyvsp[(1) - (1)]), cb_int1, 1U, current_field);
	CB_FIELD_PTR ((yyval))->special_index = 1;
  }
    break;

  case 575:

/* Line 1806 of yacc.c  */
#line 5410 "parser.y"
    {
	check_repeated ("JUSTIFIED", SYN_CLAUSE_8, &check_pic_duplicate);
	current_field->flag_justified = 1;
  }
    break;

  case 576:

/* Line 1806 of yacc.c  */
#line 5421 "parser.y"
    {
	check_repeated ("SYNCHRONIZED", SYN_CLAUSE_9, &check_pic_duplicate);
	current_field->flag_synchronized = 1;
  }
    break;

  case 577:

/* Line 1806 of yacc.c  */
#line 5432 "parser.y"
    {
	check_repeated ("BLANK", SYN_CLAUSE_10, &check_pic_duplicate);
	current_field->flag_blank_zero = 1;
  }
    break;

  case 578:

/* Line 1806 of yacc.c  */
#line 5443 "parser.y"
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
    break;

  case 579:

/* Line 1806 of yacc.c  */
#line 5471 "parser.y"
    {
	check_repeated ("VALUE", SYN_CLAUSE_12, &check_pic_duplicate);
	current_field->values = (yyvsp[(3) - (3)]);
  }
    break;

  case 581:

/* Line 1806 of yacc.c  */
#line 5479 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 582:

/* Line 1806 of yacc.c  */
#line 5480 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 583:

/* Line 1806 of yacc.c  */
#line 5484 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 584:

/* Line 1806 of yacc.c  */
#line 5485 "parser.y"
    { (yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 586:

/* Line 1806 of yacc.c  */
#line 5490 "parser.y"
    {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = CB_LIST_INIT ((yyvsp[(4) - (4)]));
  }
    break;

  case 587:

/* Line 1806 of yacc.c  */
#line 5502 "parser.y"
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else {
		current_field->flag_any_length = 1;
	}
  }
    break;

  case 588:

/* Line 1806 of yacc.c  */
#line 5511 "parser.y"
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY NUMERIC");
	} else {
		current_field->flag_any_length = 1;
		current_field->flag_any_numeric = 1;
	}
  }
    break;

  case 590:

/* Line 1806 of yacc.c  */
#line 5526 "parser.y"
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LOCAL_STORAGE_SECTION;
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "LOCAL-STORAGE");
	}
  }
    break;

  case 591:

/* Line 1806 of yacc.c  */
#line 5535 "parser.y"
    {
	if ((yyvsp[(5) - (5)])) {
		current_program->local_storage = CB_FIELD ((yyvsp[(5) - (5)]));
	}
  }
    break;

  case 593:

/* Line 1806 of yacc.c  */
#line 5547 "parser.y"
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LINKAGE_SECTION;
	current_storage = CB_STORAGE_LINKAGE;
  }
    break;

  case 594:

/* Line 1806 of yacc.c  */
#line 5553 "parser.y"
    {
	if ((yyvsp[(5) - (5)])) {
		current_program->linkage_storage = CB_FIELD ((yyvsp[(5) - (5)]));
	}
  }
    break;

  case 596:

/* Line 1806 of yacc.c  */
#line 5564 "parser.y"
    {
	CB_PENDING("REPORT SECTION");
	current_storage = CB_STORAGE_REPORT;
	cb_clear_real_field ();
  }
    break;

  case 600:

/* Line 1806 of yacc.c  */
#line 5580 "parser.y"
    {
	if (CB_INVALID_TREE ((yyvsp[(2) - (2)]))) {
		YYERROR;
	} else {
		current_report = CB_REPORT (cb_ref ((yyvsp[(2) - (2)])));
	}
	check_duplicate = 0;
  }
    break;

  case 604:

/* Line 1806 of yacc.c  */
#line 5595 "parser.y"
    {
	yyerrok;
  }
    break;

  case 605:

/* Line 1806 of yacc.c  */
#line 5602 "parser.y"
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_1, &check_duplicate);
	cb_error (_("GLOBAL is not allowed with RD"));
  }
    break;

  case 606:

/* Line 1806 of yacc.c  */
#line 5607 "parser.y"
    {
	check_repeated ("CODE", SYN_CLAUSE_2, &check_duplicate);
  }
    break;

  case 609:

/* Line 1806 of yacc.c  */
#line 5618 "parser.y"
    {
	check_repeated ("CONTROL", SYN_CLAUSE_3, &check_duplicate);
  }
    break;

  case 613:

/* Line 1806 of yacc.c  */
#line 5637 "parser.y"
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
    break;

  case 614:

/* Line 1806 of yacc.c  */
#line 5673 "parser.y"
    {
	current_report->lines = cb_get_int ((yyvsp[(1) - (1)]));
  }
    break;

  case 615:

/* Line 1806 of yacc.c  */
#line 5677 "parser.y"
    {
	current_report->lines = cb_get_int ((yyvsp[(1) - (4)]));
	current_report->columns = cb_get_int ((yyvsp[(3) - (4)]));
  }
    break;

  case 616:

/* Line 1806 of yacc.c  */
#line 5682 "parser.y"
    {
	current_report->lines = cb_get_int ((yyvsp[(1) - (2)]));
  }
    break;

  case 624:

/* Line 1806 of yacc.c  */
#line 5702 "parser.y"
    {
	current_report->heading = cb_get_int ((yyvsp[(3) - (3)]));
  }
    break;

  case 625:

/* Line 1806 of yacc.c  */
#line 5709 "parser.y"
    {
	current_report->first_detail = cb_get_int ((yyvsp[(4) - (4)]));
  }
    break;

  case 626:

/* Line 1806 of yacc.c  */
#line 5716 "parser.y"
    {
	current_report->last_control = cb_get_int ((yyvsp[(4) - (4)]));
  }
    break;

  case 627:

/* Line 1806 of yacc.c  */
#line 5723 "parser.y"
    {
	current_report->last_detail = cb_get_int ((yyvsp[(4) - (4)]));
  }
    break;

  case 628:

/* Line 1806 of yacc.c  */
#line 5730 "parser.y"
    {
	current_report->footing = cb_get_int ((yyvsp[(3) - (3)]));
  }
    break;

  case 631:

/* Line 1806 of yacc.c  */
#line 5741 "parser.y"
    {
	check_pic_duplicate = 0;
  }
    break;

  case 651:

/* Line 1806 of yacc.c  */
#line 5772 "parser.y"
    {
	check_repeated ("TYPE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
    break;

  case 664:

/* Line 1806 of yacc.c  */
#line 5798 "parser.y"
    {
	check_repeated ("NEXT GROUP", SYN_CLAUSE_17, &check_pic_duplicate);
  }
    break;

  case 665:

/* Line 1806 of yacc.c  */
#line 5805 "parser.y"
    {
	check_repeated ("SUM", SYN_CLAUSE_19, &check_pic_duplicate);
  }
    break;

  case 670:

/* Line 1806 of yacc.c  */
#line 5821 "parser.y"
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
  }
    break;

  case 672:

/* Line 1806 of yacc.c  */
#line 5832 "parser.y"
    {
	check_repeated ("LINE", SYN_CLAUSE_21, &check_pic_duplicate);
  }
    break;

  case 675:

/* Line 1806 of yacc.c  */
#line 5844 "parser.y"
    {
	check_repeated ("COLUMN", SYN_CLAUSE_18, &check_pic_duplicate);
  }
    break;

  case 687:

/* Line 1806 of yacc.c  */
#line 5877 "parser.y"
    {
	check_repeated ("SOURCE", SYN_CLAUSE_22, &check_pic_duplicate);
  }
    break;

  case 688:

/* Line 1806 of yacc.c  */
#line 5884 "parser.y"
    {
	check_repeated ("GROUP", SYN_CLAUSE_23, &check_pic_duplicate);
  }
    break;

  case 689:

/* Line 1806 of yacc.c  */
#line 5891 "parser.y"
    {
	check_repeated ("USAGE", SYN_CLAUSE_24, &check_pic_duplicate);
  }
    break;

  case 691:

/* Line 1806 of yacc.c  */
#line 5900 "parser.y"
    {
	current_storage = CB_STORAGE_SCREEN;
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
    break;

  case 692:

/* Line 1806 of yacc.c  */
#line 5907 "parser.y"
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
    break;

  case 698:

/* Line 1806 of yacc.c  */
#line 5932 "parser.y"
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[(1) - (2)]));
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
    break;

  case 699:

/* Line 1806 of yacc.c  */
#line 5952 "parser.y"
    {
	cob_flags_t	flags;

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
    break;

  case 700:

/* Line 1806 of yacc.c  */
#line 5992 "parser.y"
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[(1) - (3)]));
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
    break;

  case 703:

/* Line 1806 of yacc.c  */
#line 6015 "parser.y"
    {
	set_screen_attr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				       "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
    break;

  case 704:

/* Line 1806 of yacc.c  */
#line 6020 "parser.y"
    {
	set_screen_attr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				       "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
    break;

  case 705:

/* Line 1806 of yacc.c  */
#line 6025 "parser.y"
    {
	set_screen_attr ("BELL", COB_SCREEN_BELL);
  }
    break;

  case 706:

/* Line 1806 of yacc.c  */
#line 6029 "parser.y"
    {
	set_screen_attr ("BLINK", COB_SCREEN_BLINK);
  }
    break;

  case 707:

/* Line 1806 of yacc.c  */
#line 6033 "parser.y"
    {
	set_screen_attr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				       "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
    break;

  case 708:

/* Line 1806 of yacc.c  */
#line 6038 "parser.y"
    {
	set_screen_attr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				       "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
    break;

  case 709:

/* Line 1806 of yacc.c  */
#line 6043 "parser.y"
    {
	set_screen_attr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				       "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
    break;

  case 710:

/* Line 1806 of yacc.c  */
#line 6048 "parser.y"
    {
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
    break;

  case 711:

/* Line 1806 of yacc.c  */
#line 6053 "parser.y"
    {
	set_screen_attr ("REVERSE-VIDEO", COB_SCREEN_REVERSE);
  }
    break;

  case 712:

/* Line 1806 of yacc.c  */
#line 6057 "parser.y"
    {
	set_screen_attr ("UNDERLINE", COB_SCREEN_UNDERLINE);
  }
    break;

  case 713:

/* Line 1806 of yacc.c  */
#line 6061 "parser.y"
    {
	set_screen_attr ("OVERLINE", COB_SCREEN_OVERLINE);
	CB_PENDING ("OVERLINE");
  }
    break;

  case 714:

/* Line 1806 of yacc.c  */
#line 6066 "parser.y"
    {
	set_screen_attr ("GRID", COB_SCREEN_GRID);
	CB_PENDING ("GRID");
  }
    break;

  case 715:

/* Line 1806 of yacc.c  */
#line 6071 "parser.y"
    {
	set_screen_attr ("LEFTLINE", COB_SCREEN_LEFTLINE);
	CB_PENDING ("LEFTLINE");
  }
    break;

  case 716:

/* Line 1806 of yacc.c  */
#line 6076 "parser.y"
    {
	set_screen_attr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				       "TAB", COB_SCREEN_TAB);
  }
    break;

  case 717:

/* Line 1806 of yacc.c  */
#line 6081 "parser.y"
    {
	set_screen_attr_with_conflict ("TAB", COB_SCREEN_TAB,
				       "AUTO", COB_SCREEN_AUTO);
  }
    break;

  case 718:

/* Line 1806 of yacc.c  */
#line 6086 "parser.y"
    {
	set_screen_attr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				       "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
    break;

  case 719:

/* Line 1806 of yacc.c  */
#line 6091 "parser.y"
    {
	if (cb_no_echo_means_secure) {
		set_screen_attr ("SECURE", COB_SCREEN_SECURE);
	} else {
		set_screen_attr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					       "SECURE", COB_SCREEN_SECURE);
	}
  }
    break;

  case 720:

/* Line 1806 of yacc.c  */
#line 6100 "parser.y"
    {
	set_screen_attr ("REQUIRED", COB_SCREEN_REQUIRED);
  }
    break;

  case 721:

/* Line 1806 of yacc.c  */
#line 6104 "parser.y"
    {
	set_screen_attr ("FULL", COB_SCREEN_FULL);
  }
    break;

  case 722:

/* Line 1806 of yacc.c  */
#line 6108 "parser.y"
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
	current_field->screen_prompt = (yyvsp[(4) - (4)]);
  }
    break;

  case 723:

/* Line 1806 of yacc.c  */
#line 6113 "parser.y"
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
  }
    break;

  case 724:

/* Line 1806 of yacc.c  */
#line 6117 "parser.y"
    {
	set_screen_attr ("INITIAL", COB_SCREEN_INITIAL);
  }
    break;

  case 725:

/* Line 1806 of yacc.c  */
#line 6121 "parser.y"
    {
	check_repeated ("LINE", SYN_CLAUSE_16, &check_pic_duplicate);
	current_field->screen_line = (yyvsp[(5) - (5)]);
  }
    break;

  case 726:

/* Line 1806 of yacc.c  */
#line 6126 "parser.y"
    {
	check_repeated ("COLUMN", SYN_CLAUSE_17, &check_pic_duplicate);
	current_field->screen_column = (yyvsp[(5) - (5)]);
  }
    break;

  case 727:

/* Line 1806 of yacc.c  */
#line 6131 "parser.y"
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_18, &check_pic_duplicate);
	current_field->screen_foreg = (yyvsp[(3) - (3)]);
  }
    break;

  case 728:

/* Line 1806 of yacc.c  */
#line 6136 "parser.y"
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->screen_backg = (yyvsp[(3) - (3)]);
  }
    break;

  case 737:

/* Line 1806 of yacc.c  */
#line 6149 "parser.y"
    {
	check_not_88_level ((yyvsp[(2) - (2)]));

	check_repeated ("USING", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[(2) - (2)]);
	current_field->screen_to = (yyvsp[(2) - (2)]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
    break;

  case 738:

/* Line 1806 of yacc.c  */
#line 6158 "parser.y"
    {
	check_repeated ("FROM", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[(2) - (2)]);
  }
    break;

  case 739:

/* Line 1806 of yacc.c  */
#line 6163 "parser.y"
    {
	check_not_88_level ((yyvsp[(2) - (2)]));

	check_repeated ("TO", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->screen_to = (yyvsp[(2) - (2)]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
    break;

  case 748:

/* Line 1806 of yacc.c  */
#line 6194 "parser.y"
    {
	/* Nothing */
  }
    break;

  case 749:

/* Line 1806 of yacc.c  */
#line 6198 "parser.y"
    {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
    break;

  case 750:

/* Line 1806 of yacc.c  */
#line 6202 "parser.y"
    {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
    break;

  case 751:

/* Line 1806 of yacc.c  */
#line 6209 "parser.y"
    {
	/* Nothing */
  }
    break;

  case 752:

/* Line 1806 of yacc.c  */
#line 6213 "parser.y"
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
    break;

  case 753:

/* Line 1806 of yacc.c  */
#line 6217 "parser.y"
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
    break;

  case 754:

/* Line 1806 of yacc.c  */
#line 6225 "parser.y"
    {
	check_repeated ("OCCURS", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->occurs_max = cb_get_int ((yyvsp[(2) - (3)]));
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
    break;

  case 755:

/* Line 1806 of yacc.c  */
#line 6236 "parser.y"
    {
	cb_error (_("GLOBAL is not allowed with screen items"));
  }
    break;

  case 757:

/* Line 1806 of yacc.c  */
#line 6245 "parser.y"
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	cb_set_system_names ();
	header_check |= COBC_HD_PROCEDURE_DIVISION;
  }
    break;

  case 758:

/* Line 1806 of yacc.c  */
#line 6255 "parser.y"
    {
	if (current_program->flag_main && !current_program->flag_chained && (yyvsp[(3) - (7)])) {
		cb_error (_("executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	/* Main entry point */
	emit_entry (current_program->program_id, 0, (yyvsp[(3) - (7)]));
	current_program->num_proc_params = cb_list_length ((yyvsp[(3) - (7)]));
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, (yyvsp[(3) - (7)]));
	}
  }
    break;

  case 759:

/* Line 1806 of yacc.c  */
#line 6267 "parser.y"
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
    break;

  case 760:

/* Line 1806 of yacc.c  */
#line 6282 "parser.y"
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
	current_section->xref.skip = 1;
	CB_TREE (current_section)->source_file = cb_source_file;
	CB_TREE (current_section)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_section));
	label = cb_build_reference ("MAIN PARAGRAPH");
	current_paragraph = CB_LABEL (cb_build_label (label, NULL));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_dummy_paragraph = 1;
	current_paragraph->xref.skip = 1;
	CB_TREE (current_paragraph)->source_file = cb_source_file;
	CB_TREE (current_paragraph)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_paragraph));
	cb_set_system_names ();
  }
    break;

  case 762:

/* Line 1806 of yacc.c  */
#line 6317 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 763:

/* Line 1806 of yacc.c  */
#line 6321 "parser.y"
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
    break;

  case 764:

/* Line 1806 of yacc.c  */
#line 6326 "parser.y"
    {
	if (cb_list_length ((yyvsp[(3) - (3)])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 765:

/* Line 1806 of yacc.c  */
#line 6334 "parser.y"
    {
	call_mode = CB_CALL_BY_REFERENCE;
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("CHAINING invalid in user FUNCTION"));
	} else {
		current_program->flag_chained = 1;
	}
  }
    break;

  case 766:

/* Line 1806 of yacc.c  */
#line 6343 "parser.y"
    {
	if (cb_list_length ((yyvsp[(3) - (3)])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 767:

/* Line 1806 of yacc.c  */
#line 6353 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 768:

/* Line 1806 of yacc.c  */
#line 6355 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 769:

/* Line 1806 of yacc.c  */
#line 6360 "parser.y"
    {
	cb_tree		x;
	struct cb_field	*f;

	x = cb_build_identifier ((yyvsp[(4) - (4)]), 0);
	if ((yyvsp[(3) - (4)]) == cb_int1 && CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
		f = CB_FIELD (cb_ref (x));
		f->flag_is_pdiv_opt = 1;
	}

	if (call_mode == CB_CALL_BY_VALUE
	    && CB_REFERENCE_P ((yyvsp[(4) - (4)]))
	    && CB_FIELD (cb_ref ((yyvsp[(4) - (4)])))->flag_any_length) {
		cb_error_x ((yyvsp[(4) - (4)]), _("ANY LENGTH items may only be BY REFERENCE formal parameters"));
	}

	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), x);
	CB_SIZES ((yyval)) = size_mode;
  }
    break;

  case 771:

/* Line 1806 of yacc.c  */
#line 6384 "parser.y"
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
    break;

  case 772:

/* Line 1806 of yacc.c  */
#line 6388 "parser.y"
    {
	if (current_program->flag_chained) {
		cb_error (_("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		CB_UNFINISHED (_("parameters passed BY VALUE"));
		call_mode = CB_CALL_BY_VALUE;
	}
  }
    break;

  case 774:

/* Line 1806 of yacc.c  */
#line 6401 "parser.y"
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
    break;

  case 775:

/* Line 1806 of yacc.c  */
#line 6409 "parser.y"
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
    break;

  case 776:

/* Line 1806 of yacc.c  */
#line 6417 "parser.y"
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO | CB_SIZE_UNSIGNED;
	}
  }
    break;

  case 777:

/* Line 1806 of yacc.c  */
#line 6425 "parser.y"
    {
	unsigned char *s = CB_LITERAL ((yyvsp[(4) - (4)]))->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[(4) - (4)]))->size != 1) {
		cb_error_x ((yyvsp[(4) - (4)]), _("invalid value for SIZE"));
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
			cb_error_x ((yyvsp[(4) - (4)]), _("invalid value for SIZE"));
			break;
		}
	}
  }
    break;

  case 778:

/* Line 1806 of yacc.c  */
#line 6454 "parser.y"
    {
	unsigned char *s = CB_LITERAL ((yyvsp[(3) - (3)]))->data;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[(3) - (3)]))->size != 1) {
		cb_error_x ((yyvsp[(3) - (3)]), _("invalid value for SIZE"));
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
			cb_error_x ((yyvsp[(3) - (3)]), _("invalid value for SIZE"));
			break;
		}
	}
  }
    break;

  case 779:

/* Line 1806 of yacc.c  */
#line 6486 "parser.y"
    {
	(yyval) = cb_int0;
  }
    break;

  case 780:

/* Line 1806 of yacc.c  */
#line 6490 "parser.y"
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
		(yyval) = cb_int0;
	} else {
		(yyval) = cb_int1;
	}
  }
    break;

  case 781:

/* Line 1806 of yacc.c  */
#line 6502 "parser.y"
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
    break;

  case 782:

/* Line 1806 of yacc.c  */
#line 6508 "parser.y"
    {
	if (current_program->flag_main) {
		cb_error (_("RETURNING clause cannot be OMITTED for main program"));
	}
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause cannot be OMITTED for a FUNCTION"));
	}
	current_program->flag_void = 1;
  }
    break;

  case 783:

/* Line 1806 of yacc.c  */
#line 6518 "parser.y"
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[(2) - (2)])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[(2) - (2)]));
/* RXWRXW
		if (f->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		} else if (f->level != 1 && f->level != 77) {
*/
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01"));
		} else if (f->flag_occurs) {
			cb_error (_("RETURNING item should not have OCCURS"));
		} else if (f->storage == CB_STORAGE_LOCAL) {
			cb_error (_("RETURNING item should not be in LOCAL-STORAGE"));
		} else {
			if (current_program->prog_type == CB_FUNCTION_TYPE) {
				if (f->flag_any_length) {
					cb_error (_("function RETURNING item may not be ANY LENGTH"));
				}

				f->flag_is_returning = 1;
			}
			current_program->returning = (yyvsp[(2) - (2)]);
		}
	}
  }
    break;

  case 785:

/* Line 1806 of yacc.c  */
#line 6550 "parser.y"
    {
	in_declaratives = 1;
	emit_statement (cb_build_comment ("DECLARATIVES"));
  }
    break;

  case 786:

/* Line 1806 of yacc.c  */
#line 6556 "parser.y"
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
    break;

  case 791:

/* Line 1806 of yacc.c  */
#line 6594 "parser.y"
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
    break;

  case 793:

/* Line 1806 of yacc.c  */
#line 6612 "parser.y"
    {
	/* check_unreached = 0; */
  }
    break;

  case 794:

/* Line 1806 of yacc.c  */
#line 6622 "parser.y"
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[(1) - (4)]), 0) == cb_error_node) {
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
	current_section = CB_LABEL (cb_build_label ((yyvsp[(1) - (4)]), NULL));
	if ((yyvsp[(3) - (4)])) {
		current_section->segment = cb_get_int ((yyvsp[(3) - (4)]));
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
    break;

  case 795:

/* Line 1806 of yacc.c  */
#line 6666 "parser.y"
    {
	emit_statement (CB_TREE (current_section));
  }
    break;

  case 798:

/* Line 1806 of yacc.c  */
#line 6677 "parser.y"
    {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[(1) - (2)]), 1) == cb_error_node) {
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
		current_section->xref.skip = 1;
		CB_TREE (current_section)->source_file = cb_source_file;
		CB_TREE (current_section)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_section));
	}
	current_paragraph = CB_LABEL (cb_build_label ((yyvsp[(1) - (2)]), current_section));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_real_label = !in_debugging;
	current_paragraph->segment = current_section->segment;
	CB_TREE (current_paragraph)->source_file = cb_source_file;
	CB_TREE (current_paragraph)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_paragraph));
  }
    break;

  case 799:

/* Line 1806 of yacc.c  */
#line 6726 "parser.y"
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[(1) - (1)]), 0) != cb_error_node) {
		if (is_reserved_word (CB_NAME ((yyvsp[(1) - (1)])))) {
			cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a statement"), CB_NAME ((yyvsp[(1) - (1)])));
		} else if (is_default_reserved_word (CB_NAME ((yyvsp[(1) - (1)])))) {
			cb_error_x ((yyvsp[(1) - (1)]), _("unknown statement '%s'; it may exist in another dialect"),
				    CB_NAME ((yyvsp[(1) - (1)])));
		} else {
			cb_error_x ((yyvsp[(1) - (1)]), _("unknown statement '%s'"), CB_NAME ((yyvsp[(1) - (1)])));
		}
	}
	YYERROR;
  }
    break;

  case 800:

/* Line 1806 of yacc.c  */
#line 6745 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 801:

/* Line 1806 of yacc.c  */
#line 6749 "parser.y"
    {
	if (in_declaratives) {
		cb_error (_("SECTION segment invalid within DECLARATIVE"));
	}
	if (cb_verify (cb_section_segments, _("SECTION segment"))) {
		current_program->flag_segments = 1;
		(yyval) = (yyvsp[(1) - (1)]);
	} else {
		(yyval) = NULL;
	}
  }
    break;

  case 802:

/* Line 1806 of yacc.c  */
#line 6767 "parser.y"
    {
	(yyval) = current_program->exec_list;
	current_program->exec_list = NULL;
	check_unreached = 0;
  }
    break;

  case 803:

/* Line 1806 of yacc.c  */
#line 6772 "parser.y"
    {
	(yyval) = CB_TREE (current_statement);
	current_statement = NULL;
  }
    break;

  case 804:

/* Line 1806 of yacc.c  */
#line 6777 "parser.y"
    {
	(yyval) = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = (yyvsp[(1) - (3)]);
	current_statement = CB_STATEMENT ((yyvsp[(2) - (3)]));
  }
    break;

  case 805:

/* Line 1806 of yacc.c  */
#line 6785 "parser.y"
    {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_skip_label = !!skip_statements;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->xref.skip = 1;
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
		current_paragraph->xref.skip = 1;
		CB_TREE (current_paragraph)->source_file = cb_source_file;
		CB_TREE (current_paragraph)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_paragraph));
	}
	check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0);
  }
    break;

  case 806:

/* Line 1806 of yacc.c  */
#line 6814 "parser.y"
    {
	cobc_cs_check = 0;
  }
    break;

  case 807:

/* Line 1806 of yacc.c  */
#line 6818 "parser.y"
    {
	cobc_cs_check = 0;
  }
    break;

  case 862:

/* Line 1806 of yacc.c  */
#line 6879 "parser.y"
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
    break;

  case 863:

/* Line 1806 of yacc.c  */
#line 6893 "parser.y"
    {
	yyerrok;
	cobc_cs_check = 0;
  }
    break;

  case 864:

/* Line 1806 of yacc.c  */
#line 6904 "parser.y"
    {
	begin_statement ("ACCEPT", TERM_ACCEPT);
	cobc_cs_check = CB_CS_ACCEPT;
  }
    break;

  case 866:

/* Line 1806 of yacc.c  */
#line 6914 "parser.y"
    {
	  check_duplicate = 0;
	  check_line_col_duplicate = 0;
	  line_column = NULL;
  }
    break;

  case 867:

/* Line 1806 of yacc.c  */
#line 6920 "parser.y"
    {
	/* Check for invalid use of screen clauses */
	if (current_statement->attr_ptr
	    || (!is_screen_field ((yyvsp[(1) - (4)])) && line_column)) {
		cb_verify_x ((yyvsp[(1) - (4)]), cb_accept_display_extensions,
			     _("non-standard ACCEPT"));
	}

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	if ((yyvsp[(1) - (4)]) == cb_null && current_statement->attr_ptr) {
		if (current_statement->attr_ptr->prompt) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("PROMPT clause"));
		}
		if (current_statement->attr_ptr->size_is) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("SIZE IS clause"));
		}
	}
	cobc_cs_check = 0;
	cb_emit_accept ((yyvsp[(1) - (4)]), line_column, current_statement->attr_ptr);
  }
    break;

  case 868:

/* Line 1806 of yacc.c  */
#line 6948 "parser.y"
    {
	cb_emit_accept_line_or_col ((yyvsp[(1) - (3)]), 0);
  }
    break;

  case 869:

/* Line 1806 of yacc.c  */
#line 6952 "parser.y"
    {
	cb_emit_accept_line_or_col ((yyvsp[(1) - (3)]), 1);
  }
    break;

  case 870:

/* Line 1806 of yacc.c  */
#line 6956 "parser.y"
    {
	cobc_cs_check = 0;
	cb_emit_accept_date_yyyymmdd ((yyvsp[(1) - (4)]));
  }
    break;

  case 871:

/* Line 1806 of yacc.c  */
#line 6961 "parser.y"
    {
	cobc_cs_check = 0;
	cb_emit_accept_date ((yyvsp[(1) - (3)]));
  }
    break;

  case 872:

/* Line 1806 of yacc.c  */
#line 6966 "parser.y"
    {
	cobc_cs_check = 0;
	cb_emit_accept_day_yyyyddd ((yyvsp[(1) - (4)]));
  }
    break;

  case 873:

/* Line 1806 of yacc.c  */
#line 6971 "parser.y"
    {
	cobc_cs_check = 0;
	cb_emit_accept_day ((yyvsp[(1) - (3)]));
  }
    break;

  case 874:

/* Line 1806 of yacc.c  */
#line 6976 "parser.y"
    {
	cb_emit_accept_day_of_week ((yyvsp[(1) - (3)]));
  }
    break;

  case 875:

/* Line 1806 of yacc.c  */
#line 6980 "parser.y"
    {
	cb_emit_accept_escape_key ((yyvsp[(1) - (4)]));
  }
    break;

  case 876:

/* Line 1806 of yacc.c  */
#line 6984 "parser.y"
    {
	cb_emit_accept_exception_status ((yyvsp[(1) - (4)]));
  }
    break;

  case 877:

/* Line 1806 of yacc.c  */
#line 6988 "parser.y"
    {
	cb_emit_accept_time ((yyvsp[(1) - (3)]));
  }
    break;

  case 878:

/* Line 1806 of yacc.c  */
#line 6992 "parser.y"
    {
	cobc_cs_check = 0;
	cb_emit_accept_user_name ((yyvsp[(1) - (4)]));
  }
    break;

  case 879:

/* Line 1806 of yacc.c  */
#line 6997 "parser.y"
    {
	cb_emit_accept_command_line ((yyvsp[(1) - (3)]));
  }
    break;

  case 880:

/* Line 1806 of yacc.c  */
#line 7001 "parser.y"
    {
	cb_emit_accept_environment ((yyvsp[(1) - (4)]));
  }
    break;

  case 881:

/* Line 1806 of yacc.c  */
#line 7005 "parser.y"
    {
	cb_emit_get_environment ((yyvsp[(4) - (5)]), (yyvsp[(1) - (5)]));
  }
    break;

  case 882:

/* Line 1806 of yacc.c  */
#line 7009 "parser.y"
    {
	cb_emit_accept_arg_number ((yyvsp[(1) - (3)]));
  }
    break;

  case 883:

/* Line 1806 of yacc.c  */
#line 7013 "parser.y"
    {
	cb_emit_accept_arg_value ((yyvsp[(1) - (4)]));
  }
    break;

  case 884:

/* Line 1806 of yacc.c  */
#line 7017 "parser.y"
    {
	cb_emit_accept_mnemonic ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 885:

/* Line 1806 of yacc.c  */
#line 7021 "parser.y"
    {
	cb_emit_accept_name ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 886:

/* Line 1806 of yacc.c  */
#line 7025 "parser.y"
    {
	CB_PENDING ("ACCEPT MESSAGE COUNT");
  }
    break;

  case 888:

/* Line 1806 of yacc.c  */
#line 7033 "parser.y"
    {
	(yyval) = cb_null;
  }
    break;

  case 894:

/* Line 1806 of yacc.c  */
#line 7051 "parser.y"
    {
	  check_repeated ("FROM CRT", SYN_CLAUSE_2, &check_duplicate);
  }
    break;

  case 895:

/* Line 1806 of yacc.c  */
#line 7055 "parser.y"
    {
	  check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
    break;

  case 897:

/* Line 1806 of yacc.c  */
#line 7060 "parser.y"
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[(3) - (3)]), NULL, NULL, 0);
  }
    break;

  case 900:

/* Line 1806 of yacc.c  */
#line 7074 "parser.y"
    {
	set_attr_with_conflict ("LINE", SYN_CLAUSE_1,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[(2) - (2)])) && cb_get_int ((yyvsp[(2) - (2)])) == 0) || (yyvsp[(2) - (2)]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "LINE 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR ((yyvsp[(2) - (2)]), cb_int0);
	} else {
		CB_PAIR_X (line_column) = (yyvsp[(2) - (2)]);
	}
  }
    break;

  case 901:

/* Line 1806 of yacc.c  */
#line 7090 "parser.y"
    {
	set_attr_with_conflict ("COLUMN", SYN_CLAUSE_2,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[(2) - (2)])) && cb_get_int ((yyvsp[(2) - (2)])) == 0) || (yyvsp[(2) - (2)]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "COLUMN 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (cb_int0, (yyvsp[(2) - (2)]));
	} else {
		CB_PAIR_Y (line_column) = (yyvsp[(2) - (2)]);
	}
  }
    break;

  case 902:

/* Line 1806 of yacc.c  */
#line 7106 "parser.y"
    {
	set_attr_with_conflict (_("AT screen-location"), SYN_CLAUSE_3,
				_("LINE or COLUMN"), SYN_CLAUSE_1 | SYN_CLAUSE_2,
				1, &check_line_col_duplicate);

	cb_verify (cb_accept_display_extensions, "AT clause");

	line_column = (yyvsp[(2) - (2)]);
  }
    break;

  case 903:

/* Line 1806 of yacc.c  */
#line 7118 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 904:

/* Line 1806 of yacc.c  */
#line 7122 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 905:

/* Line 1806 of yacc.c  */
#line 7123 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 906:

/* Line 1806 of yacc.c  */
#line 7128 "parser.y"
    {
	cobc_cs_check = 0;
  }
    break;

  case 907:

/* Line 1806 of yacc.c  */
#line 7135 "parser.y"
    {
	check_repeated ("AUTO", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				    "TAB", COB_SCREEN_TAB);
  }
    break;

  case 908:

/* Line 1806 of yacc.c  */
#line 7141 "parser.y"
    {
	check_repeated ("TAB", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("TAB", COB_SCREEN_TAB,
				    "AUTO", COB_SCREEN_AUTO);
  }
    break;

  case 909:

/* Line 1806 of yacc.c  */
#line 7147 "parser.y"
    {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
    break;

  case 910:

/* Line 1806 of yacc.c  */
#line 7152 "parser.y"
    {
        check_repeated ("BLINK", SYN_CLAUSE_8, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
    break;

  case 911:

/* Line 1806 of yacc.c  */
#line 7157 "parser.y"
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_9, &check_duplicate);
	CB_PENDING ("ACCEPT CONVERSION");
  }
    break;

  case 912:

/* Line 1806 of yacc.c  */
#line 7162 "parser.y"
    {
	check_repeated ("FULL", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr (COB_SCREEN_FULL);
  }
    break;

  case 913:

/* Line 1806 of yacc.c  */
#line 7167 "parser.y"
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
    break;

  case 914:

/* Line 1806 of yacc.c  */
#line 7173 "parser.y"
    {
	check_repeated ("LEFTLINE", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr (COB_SCREEN_LEFTLINE);
  }
    break;

  case 915:

/* Line 1806 of yacc.c  */
#line 7178 "parser.y"
    {
	check_repeated ("LOWER", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr_with_conflict ("LOWER", COB_SCREEN_LOWER,
				    "UPPER", COB_SCREEN_UPPER);
  }
    break;

  case 916:

/* Line 1806 of yacc.c  */
#line 7184 "parser.y"
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
    break;

  case 917:

/* Line 1806 of yacc.c  */
#line 7190 "parser.y"
    {
	if (cb_no_echo_means_secure) {
		check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
		set_dispattr (COB_SCREEN_SECURE);
	} else {
		check_repeated ("NO-ECHO", SYN_CLAUSE_15, &check_duplicate);
		set_dispattr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					    "SECURE", COB_SCREEN_SECURE);
	}
  }
    break;

  case 918:

/* Line 1806 of yacc.c  */
#line 7201 "parser.y"
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
    break;

  case 919:

/* Line 1806 of yacc.c  */
#line 7206 "parser.y"
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, (yyvsp[(4) - (4)]), NULL, COB_SCREEN_PROMPT);
  }
    break;

  case 920:

/* Line 1806 of yacc.c  */
#line 7211 "parser.y"
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_dispattr (COB_SCREEN_PROMPT);
  }
    break;

  case 921:

/* Line 1806 of yacc.c  */
#line 7216 "parser.y"
    {
	check_repeated ("REQUIRED", SYN_CLAUSE_18, &check_duplicate);
	set_dispattr (COB_SCREEN_REQUIRED);
  }
    break;

  case 922:

/* Line 1806 of yacc.c  */
#line 7221 "parser.y"
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_19, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
    break;

  case 923:

/* Line 1806 of yacc.c  */
#line 7226 "parser.y"
    {
	check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
	set_dispattr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				    "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
    break;

  case 924:

/* Line 1806 of yacc.c  */
#line 7232 "parser.y"
    {
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[(4) - (4)]), 0);
  }
    break;

  case 925:

/* Line 1806 of yacc.c  */
#line 7237 "parser.y"
    {
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[(3) - (3)]), 0);
  }
    break;

  case 926:

/* Line 1806 of yacc.c  */
#line 7242 "parser.y"
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_22, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
    break;

  case 927:

/* Line 1806 of yacc.c  */
#line 7247 "parser.y"
    {
	check_repeated ("NO UPDATE", SYN_CLAUSE_23, &check_duplicate);
	set_dispattr_with_conflict ("NO UPDATE", COB_SCREEN_NO_UPDATE,
				    "UPDATE", COB_SCREEN_UPDATE);
  }
    break;

  case 928:

/* Line 1806 of yacc.c  */
#line 7253 "parser.y"
    {
	check_repeated ("UPDATE", SYN_CLAUSE_24, &check_duplicate);
	set_dispattr_with_conflict ("UPDATE", COB_SCREEN_UPDATE,
				    "NO UPDATE", COB_SCREEN_NO_UPDATE);
  }
    break;

  case 929:

/* Line 1806 of yacc.c  */
#line 7259 "parser.y"
    {
	check_repeated ("UPPER", SYN_CLAUSE_25, &check_duplicate);
	set_dispattr_with_conflict ("UPPER", COB_SCREEN_UPPER,
				    "LOWER", COB_SCREEN_LOWER);
  }
    break;

  case 930:

/* Line 1806 of yacc.c  */
#line 7265 "parser.y"
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	set_attribs ((yyvsp[(3) - (3)]), NULL, NULL, NULL, NULL, NULL, 0);
  }
    break;

  case 931:

/* Line 1806 of yacc.c  */
#line 7270 "parser.y"
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	set_attribs (NULL, (yyvsp[(3) - (3)]), NULL, NULL, NULL, NULL, 0);
  }
    break;

  case 932:

/* Line 1806 of yacc.c  */
#line 7275 "parser.y"
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_28, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[(3) - (3)]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
    break;

  case 933:

/* Line 1806 of yacc.c  */
#line 7282 "parser.y"
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[(3) - (3)]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
    break;

  case 934:

/* Line 1806 of yacc.c  */
#line 7289 "parser.y"
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[(3) - (3)]), NULL, NULL, 0);
  }
    break;

  case 943:

/* Line 1806 of yacc.c  */
#line 7315 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ACCEPT);
  }
    break;

  case 944:

/* Line 1806 of yacc.c  */
#line 7319 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ACCEPT);
# if 0 /* activate only for debugging purposes for attribs */
	if (current_statement->attr_ptr) {
		print_bits (current_statement->attr_ptr->dispattrs);
	} else {
		fprintf(stderr, "No Attribs\n");
	}
#endif
  }
    break;

  case 945:

/* Line 1806 of yacc.c  */
#line 7336 "parser.y"
    {
	begin_statement ("ADD", TERM_ADD);
  }
    break;

  case 947:

/* Line 1806 of yacc.c  */
#line 7345 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(3) - (4)]), '+', cb_build_binary_list ((yyvsp[(1) - (4)]), '+'));
  }
    break;

  case 948:

/* Line 1806 of yacc.c  */
#line 7349 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(4) - (5)]), 0, cb_build_binary_list ((yyvsp[(1) - (5)]), '+'));
  }
    break;

  case 949:

/* Line 1806 of yacc.c  */
#line 7353 "parser.y"
    {
	cb_emit_corresponding (cb_build_add, (yyvsp[(4) - (6)]), (yyvsp[(2) - (6)]), (yyvsp[(5) - (6)]));
  }
    break;

  case 951:

/* Line 1806 of yacc.c  */
#line 7360 "parser.y"
    {
	cb_list_add ((yyvsp[(0) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 952:

/* Line 1806 of yacc.c  */
#line 7367 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ADD);
  }
    break;

  case 953:

/* Line 1806 of yacc.c  */
#line 7371 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ADD);
  }
    break;

  case 954:

/* Line 1806 of yacc.c  */
#line 7381 "parser.y"
    {
	begin_statement ("ALLOCATE", 0);
	current_statement->flag_no_based = 1;
  }
    break;

  case 956:

/* Line 1806 of yacc.c  */
#line 7390 "parser.y"
    {
	cb_emit_allocate ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), NULL, (yyvsp[(2) - (3)]));
  }
    break;

  case 957:

/* Line 1806 of yacc.c  */
#line 7394 "parser.y"
    {
	if ((yyvsp[(4) - (4)]) == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("ALLOCATE CHARACTERS requires RETURNING clause"));
	} else {
		cb_emit_allocate (NULL, (yyvsp[(4) - (4)]), (yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
	}
  }
    break;

  case 958:

/* Line 1806 of yacc.c  */
#line 7405 "parser.y"
    { (yyval) = NULL; }
    break;

  case 959:

/* Line 1806 of yacc.c  */
#line 7406 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 960:

/* Line 1806 of yacc.c  */
#line 7414 "parser.y"
    {
	begin_statement ("ALTER", 0);
	cb_verify (cb_alter_statement, "ALTER");
  }
    break;

  case 964:

/* Line 1806 of yacc.c  */
#line 7428 "parser.y"
    {
	cb_emit_alter ((yyvsp[(1) - (4)]), (yyvsp[(4) - (4)]));
  }
    break;

  case 967:

/* Line 1806 of yacc.c  */
#line 7440 "parser.y"
    {
	begin_statement ("CALL", TERM_CALL);
	cobc_cs_check = CB_CS_CALL;
	call_nothing = 0;
	cobc_allow_program_name = 1;
  }
    break;

  case 969:

/* Line 1806 of yacc.c  */
#line 7452 "parser.y"
    {
	cobc_allow_program_name = 0;
  }
    break;

  case 970:

/* Line 1806 of yacc.c  */
#line 7458 "parser.y"
    {
	cb_tree	call_conv_bit;

	if (current_program->prog_type == CB_PROGRAM_TYPE
	    && !current_program->flag_recursive
	    && is_recursive_call ((yyvsp[(2) - (6)]))) {
		cb_warning_x ((yyvsp[(2) - (6)]), _("recursive program call - assuming RECURSIVE attribute"));
		current_program->flag_recursive = 1;
	}
	/* For CALL ... RETURNING NOTHING, set the call convention bit */
	if (call_nothing) {
		if ((yyvsp[(1) - (6)]) && CB_INTEGER_P ((yyvsp[(1) - (6)]))) {
			call_conv_bit = cb_int ((CB_INTEGER ((yyvsp[(1) - (6)]))->val)
						| CB_CONV_NO_RET_UPD);
		} else {
			call_conv_bit = cb_int (CB_CONV_NO_RET_UPD);
		}
	} else {
		call_conv_bit = (yyvsp[(1) - (6)]);
	}
	cb_emit_call ((yyvsp[(2) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(5) - (6)]), CB_PAIR_X ((yyvsp[(6) - (6)])), CB_PAIR_Y ((yyvsp[(6) - (6)])),
		      call_conv_bit);
  }
    break;

  case 971:

/* Line 1806 of yacc.c  */
#line 7485 "parser.y"
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
    break;

  case 972:

/* Line 1806 of yacc.c  */
#line 7490 "parser.y"
    {
	(yyval) = cb_int (CB_CONV_STATIC_LINK);
	cobc_cs_check = 0;
  }
    break;

  case 973:

/* Line 1806 of yacc.c  */
#line 7495 "parser.y"
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
	cobc_cs_check = 0;
  }
    break;

  case 974:

/* Line 1806 of yacc.c  */
#line 7500 "parser.y"
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[(1) - (1)]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME(x)->token != CB_FEATURE_CONVENTION) {
			cb_error_x ((yyvsp[(1) - (1)]), _("invalid mnemonic name"));
			(yyval) = NULL;
		} else {
			(yyval) = CB_SYSTEM_NAME(x)->value;
		}
	} else {
		(yyval) = NULL;
	}
	cobc_cs_check = 0;
  }
    break;

  case 975:

/* Line 1806 of yacc.c  */
#line 7520 "parser.y"
    {
	if (CB_LITERAL_P ((yyvsp[(1) - (1)]))) {
		cb_trim_program_id ((yyvsp[(1) - (1)]));
	}
  }
    break;

  case 976:

/* Line 1806 of yacc.c  */
#line 7526 "parser.y"
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
    break;

  case 977:

/* Line 1806 of yacc.c  */
#line 7533 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 978:

/* Line 1806 of yacc.c  */
#line 7537 "parser.y"
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
    break;

  case 979:

/* Line 1806 of yacc.c  */
#line 7542 "parser.y"
    {
	if (cb_list_length ((yyvsp[(3) - (3)])) > MAX_CALL_FIELD_PARAMS) {
		cb_error_x (CB_TREE (current_statement),
			    _("number of parameters exceeds maximum %d"),
			    MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 980:

/* Line 1806 of yacc.c  */
#line 7553 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 981:

/* Line 1806 of yacc.c  */
#line 7555 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 982:

/* Line 1806 of yacc.c  */
#line 7560 "parser.y"
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error_x (CB_TREE (current_statement),
			    _("OMITTED only allowed when parameters are passed BY REFERENCE"));
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), cb_null);
  }
    break;

  case 983:

/* Line 1806 of yacc.c  */
#line 7568 "parser.y"
    {
	int	save_mode;

	save_mode = call_mode;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P ((yyvsp[(3) - (3)])) || (CB_REFERENCE_P ((yyvsp[(3) - (3)])) &&
		    CB_FILE_P (CB_REFERENCE ((yyvsp[(3) - (3)]))->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			if (cb_category_is_alpha ((yyvsp[(3) - (3)]))) {
				cb_warning_x ((yyvsp[(3) - (3)]),
					      _("BY CONTENT assumed for alphanumeric item"));
				save_mode = CB_CALL_BY_CONTENT;
			}
		}
	}
	(yyval) = CB_BUILD_PAIR (cb_int (save_mode), (yyvsp[(3) - (3)]));
	CB_SIZES ((yyval)) = size_mode;
	call_mode = save_mode;
  }
    break;

  case 985:

/* Line 1806 of yacc.c  */
#line 7594 "parser.y"
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
    break;

  case 986:

/* Line 1806 of yacc.c  */
#line 7598 "parser.y"
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY CONTENT");
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
    break;

  case 987:

/* Line 1806 of yacc.c  */
#line 7607 "parser.y"
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
    break;

  case 988:

/* Line 1806 of yacc.c  */
#line 7619 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 989:

/* Line 1806 of yacc.c  */
#line 7623 "parser.y"
    {
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 990:

/* Line 1806 of yacc.c  */
#line 7627 "parser.y"
    {
	(yyval) = cb_null;
  }
    break;

  case 991:

/* Line 1806 of yacc.c  */
#line 7631 "parser.y"
    {
	call_nothing = CB_CONV_NO_RET_UPD;
	(yyval) = cb_null;
  }
    break;

  case 992:

/* Line 1806 of yacc.c  */
#line 7636 "parser.y"
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[(4) - (4)])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[(4) - (4)]));
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01 or 77"));
			(yyval) = NULL;
		} else if (f->storage != CB_STORAGE_LINKAGE &&
			   !f->flag_item_based) {
			cb_error (_("RETURNING item must be a LINKAGE SECTION item or have BASED clause"));
			(yyval) = NULL;
		} else {
			(yyval) = cb_build_address ((yyvsp[(4) - (4)]));
		}
	} else {
		(yyval) = NULL;
	}
  }
    break;

  case 997:

/* Line 1806 of yacc.c  */
#line 7669 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR (NULL, NULL);
  }
    break;

  case 998:

/* Line 1806 of yacc.c  */
#line 7673 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 999:

/* Line 1806 of yacc.c  */
#line 7677 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
	(yyval) = CB_BUILD_PAIR ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)]));
  }
    break;

  case 1000:

/* Line 1806 of yacc.c  */
#line 7688 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1001:

/* Line 1806 of yacc.c  */
#line 7692 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1002:

/* Line 1806 of yacc.c  */
#line 7699 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1003:

/* Line 1806 of yacc.c  */
#line 7703 "parser.y"
    {
	cb_verify (cb_call_overflow, "ON OVERFLOW");
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1004:

/* Line 1806 of yacc.c  */
#line 7711 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1005:

/* Line 1806 of yacc.c  */
#line 7715 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1006:

/* Line 1806 of yacc.c  */
#line 7722 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1007:

/* Line 1806 of yacc.c  */
#line 7729 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), CALL);
  }
    break;

  case 1008:

/* Line 1806 of yacc.c  */
#line 7733 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), CALL);
  }
    break;

  case 1009:

/* Line 1806 of yacc.c  */
#line 7743 "parser.y"
    {
	begin_statement ("CANCEL", 0);
	cobc_allow_program_name = 1;
  }
    break;

  case 1010:

/* Line 1806 of yacc.c  */
#line 7748 "parser.y"
    {
	cobc_allow_program_name = 0;
  }
    break;

  case 1011:

/* Line 1806 of yacc.c  */
#line 7755 "parser.y"
    {
	cb_emit_cancel ((yyvsp[(1) - (1)]));
  }
    break;

  case 1012:

/* Line 1806 of yacc.c  */
#line 7759 "parser.y"
    {
	cb_emit_cancel ((yyvsp[(2) - (2)]));
  }
    break;

  case 1014:

/* Line 1806 of yacc.c  */
#line 7767 "parser.y"
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
    break;

  case 1015:

/* Line 1806 of yacc.c  */
#line 7776 "parser.y"
    {
	begin_statement ("CLOSE", 0);
  }
    break;

  case 1017:

/* Line 1806 of yacc.c  */
#line 7784 "parser.y"
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1018:

/* Line 1806 of yacc.c  */
#line 7789 "parser.y"
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1019:

/* Line 1806 of yacc.c  */
#line 7796 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_NORMAL); }
    break;

  case 1020:

/* Line 1806 of yacc.c  */
#line 7797 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_UNIT); }
    break;

  case 1021:

/* Line 1806 of yacc.c  */
#line 7798 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_UNIT_REMOVAL); }
    break;

  case 1022:

/* Line 1806 of yacc.c  */
#line 7799 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_NO_REWIND); }
    break;

  case 1023:

/* Line 1806 of yacc.c  */
#line 7800 "parser.y"
    { (yyval) = cb_int (COB_CLOSE_LOCK); }
    break;

  case 1024:

/* Line 1806 of yacc.c  */
#line 7808 "parser.y"
    {
	begin_statement ("COMPUTE", TERM_COMPUTE);
  }
    break;

  case 1026:

/* Line 1806 of yacc.c  */
#line 7817 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(1) - (4)]), 0, (yyvsp[(3) - (4)]));
  }
    break;

  case 1027:

/* Line 1806 of yacc.c  */
#line 7824 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), COMPUTE);
  }
    break;

  case 1028:

/* Line 1806 of yacc.c  */
#line 7828 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), COMPUTE);
  }
    break;

  case 1029:

/* Line 1806 of yacc.c  */
#line 7838 "parser.y"
    {
	begin_statement ("COMMIT", 0);
	cb_emit_commit ();
  }
    break;

  case 1030:

/* Line 1806 of yacc.c  */
#line 7849 "parser.y"
    {
	size_t	save_unreached;

	/* Do not check unreached for CONTINUE */
	save_unreached = check_unreached;
	check_unreached = 0;
	begin_statement ("CONTINUE", 0);
	cb_emit_continue ();
	check_unreached = (unsigned int) save_unreached;
  }
    break;

  case 1031:

/* Line 1806 of yacc.c  */
#line 7866 "parser.y"
    {
	begin_statement ("DELETE", TERM_DELETE);
  }
    break;

  case 1033:

/* Line 1806 of yacc.c  */
#line 7875 "parser.y"
    {
	cb_emit_delete ((yyvsp[(1) - (4)]));
  }
    break;

  case 1035:

/* Line 1806 of yacc.c  */
#line 7883 "parser.y"
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[(1) - (1)]));
  }
    break;

  case 1036:

/* Line 1806 of yacc.c  */
#line 7888 "parser.y"
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[(2) - (2)]));
  }
    break;

  case 1037:

/* Line 1806 of yacc.c  */
#line 7896 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DELETE);
  }
    break;

  case 1038:

/* Line 1806 of yacc.c  */
#line 7900 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DELETE);
  }
    break;

  case 1039:

/* Line 1806 of yacc.c  */
#line 7910 "parser.y"
    {
	begin_statement ("DISABLE", 0);
  }
    break;

  case 1043:

/* Line 1806 of yacc.c  */
#line 7924 "parser.y"
    {
	  /* Add cb_verify for <= COBOL-85 */
  }
    break;

  case 1049:

/* Line 1806 of yacc.c  */
#line 7942 "parser.y"
    {
	begin_statement ("DISPLAY", TERM_DISPLAY);
	cobc_cs_check = CB_CS_DISPLAY;
	display_type = UNKNOWN_DISPLAY;
	is_first_display_item = 1;
  }
    break;

  case 1051:

/* Line 1806 of yacc.c  */
#line 7954 "parser.y"
    {
	cb_emit_env_name ((yyvsp[(1) - (3)]));
  }
    break;

  case 1052:

/* Line 1806 of yacc.c  */
#line 7958 "parser.y"
    {
	cb_emit_env_value ((yyvsp[(1) - (3)]));
  }
    break;

  case 1053:

/* Line 1806 of yacc.c  */
#line 7962 "parser.y"
    {
	cb_emit_arg_number ((yyvsp[(1) - (3)]));
  }
    break;

  case 1054:

/* Line 1806 of yacc.c  */
#line 7966 "parser.y"
    {
	cb_emit_command_line ((yyvsp[(1) - (3)]));
  }
    break;

  case 1056:

/* Line 1806 of yacc.c  */
#line 7974 "parser.y"
    {
	if ((yyvsp[(2) - (2)]) != NULL) {
		error_if_different_display_type ((yyvsp[(2) - (2)]), NULL, NULL, NULL);
		cb_emit_display ((yyvsp[(2) - (2)]), NULL, cb_int1, NULL, NULL, 0,
				 display_type);
	}
  }
    break;

  case 1057:

/* Line 1806 of yacc.c  */
#line 7982 "parser.y"
    {
	set_display_type ((yyvsp[(1) - (1)]), NULL, NULL, NULL);
	cb_emit_display ((yyvsp[(1) - (1)]), NULL, cb_int1, NULL, NULL, 1,
			 display_type);
  }
    break;

  case 1060:

/* Line 1806 of yacc.c  */
#line 7996 "parser.y"
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
  	advancing_value = cb_int1;
	upon_value = NULL;
	line_column = NULL;
  }
    break;

  case 1061:

/* Line 1806 of yacc.c  */
#line 8004 "parser.y"
    {
	if ((yyvsp[(1) - (3)]) == cb_null) {
		/* Emit DISPLAY OMITTED. */
		CB_UNFINISHED ("DISPLAY OMITTED");
		error_if_no_advancing_in_screen_display (advancing_value);
		(yyvsp[(1) - (3)]) = cb_low;
	}

	/* Emit device or screen DISPLAY. */

	/*
	  Check that disp_list does not contain an invalid mix of fields.
	*/
	if (display_type == UNKNOWN_DISPLAY) {
		set_display_type ((yyvsp[(1) - (3)]), upon_value, line_column,
				  current_statement->attr_ptr);
	} else {
		error_if_different_display_type ((yyvsp[(1) - (3)]), upon_value,
						 line_column,
						 current_statement->attr_ptr);
	}

	if (display_type == SCREEN_DISPLAY
	    || display_type == FIELD_ON_SCREEN_DISPLAY) {
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	cb_emit_display ((yyvsp[(1) - (3)]), upon_value, advancing_value, line_column,
			 current_statement->attr_ptr,
			 is_first_display_item, display_type);

	is_first_display_item = 0;
  }
    break;

  case 1062:

/* Line 1806 of yacc.c  */
#line 8041 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1063:

/* Line 1806 of yacc.c  */
#line 8045 "parser.y"
    {
	CB_PENDING ("DISPLAY OMITTED");
	(yyval) = cb_null;
  }
    break;

  case 1066:

/* Line 1806 of yacc.c  */
#line 8058 "parser.y"
    {
	check_repeated ("UPON", SYN_CLAUSE_1, &check_duplicate);
  }
    break;

  case 1067:

/* Line 1806 of yacc.c  */
#line 8062 "parser.y"
    {
 	check_repeated ("NO ADVANCING", SYN_CLAUSE_2, &check_duplicate);
	advancing_value = cb_int0;
  }
    break;

  case 1068:

/* Line 1806 of yacc.c  */
#line 8067 "parser.y"
    {
	check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
    break;

  case 1071:

/* Line 1806 of yacc.c  */
#line 8076 "parser.y"
    {
	upon_value = cb_build_display_mnemonic ((yyvsp[(2) - (2)]));
  }
    break;

  case 1072:

/* Line 1806 of yacc.c  */
#line 8080 "parser.y"
    {
	upon_value = cb_build_display_name ((yyvsp[(2) - (2)]));
  }
    break;

  case 1073:

/* Line 1806 of yacc.c  */
#line 8084 "parser.y"
    {
	upon_value = cb_int0;
  }
    break;

  case 1074:

/* Line 1806 of yacc.c  */
#line 8088 "parser.y"
    {
	upon_value = cb_null;
  }
    break;

  case 1077:

/* Line 1806 of yacc.c  */
#line 8100 "parser.y"
    {
	check_repeated ("BELL", SYN_CLAUSE_4, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
    break;

  case 1078:

/* Line 1806 of yacc.c  */
#line 8105 "parser.y"
    {
	check_repeated ("BLANK LINE", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				    "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
    break;

  case 1079:

/* Line 1806 of yacc.c  */
#line 8111 "parser.y"
    {
	check_repeated ("BLANK SCREEN", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				    "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
    break;

  case 1080:

/* Line 1806 of yacc.c  */
#line 8117 "parser.y"
    {
	check_repeated ("BLINK", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
    break;

  case 1081:

/* Line 1806 of yacc.c  */
#line 8122 "parser.y"
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_8, &check_duplicate);
	cb_warning (_("ignoring CONVERSION"));
  }
    break;

  case 1082:

/* Line 1806 of yacc.c  */
#line 8127 "parser.y"
    {
	check_repeated ("ERASE EOL", SYN_CLAUSE_9, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				    "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
    break;

  case 1083:

/* Line 1806 of yacc.c  */
#line 8133 "parser.y"
    {
	check_repeated ("ERASE EOS", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
    break;

  case 1084:

/* Line 1806 of yacc.c  */
#line 8139 "parser.y"
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
    break;

  case 1085:

/* Line 1806 of yacc.c  */
#line 8145 "parser.y"
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
    break;

  case 1086:

/* Line 1806 of yacc.c  */
#line 8151 "parser.y"
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
    break;

  case 1087:

/* Line 1806 of yacc.c  */
#line 8156 "parser.y"
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
    break;

  case 1088:

/* Line 1806 of yacc.c  */
#line 8161 "parser.y"
    {
	check_repeated ("SIZE", SYN_CLAUSE_15, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[(3) - (3)]), 0);
  }
    break;

  case 1089:

/* Line 1806 of yacc.c  */
#line 8166 "parser.y"
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
    break;

  case 1090:

/* Line 1806 of yacc.c  */
#line 8171 "parser.y"
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	set_attribs ((yyvsp[(3) - (3)]), NULL, NULL, NULL, NULL, NULL, 0);
  }
    break;

  case 1091:

/* Line 1806 of yacc.c  */
#line 8176 "parser.y"
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	set_attribs (NULL, (yyvsp[(3) - (3)]), NULL, NULL, NULL, NULL, 0);
  }
    break;

  case 1092:

/* Line 1806 of yacc.c  */
#line 8181 "parser.y"
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[(3) - (3)]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
    break;

  case 1093:

/* Line 1806 of yacc.c  */
#line 8188 "parser.y"
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_20, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[(3) - (3)]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
    break;

  case 1094:

/* Line 1806 of yacc.c  */
#line 8198 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DISPLAY);
  }
    break;

  case 1095:

/* Line 1806 of yacc.c  */
#line 8202 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DISPLAY);
  }
    break;

  case 1096:

/* Line 1806 of yacc.c  */
#line 8212 "parser.y"
    {
	begin_statement ("DIVIDE", TERM_DIVIDE);
  }
    break;

  case 1098:

/* Line 1806 of yacc.c  */
#line 8221 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(3) - (4)]), '/', (yyvsp[(1) - (4)]));
  }
    break;

  case 1099:

/* Line 1806 of yacc.c  */
#line 8225 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_op ((yyvsp[(3) - (6)]), '/', (yyvsp[(1) - (6)])));
  }
    break;

  case 1100:

/* Line 1806 of yacc.c  */
#line 8229 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_op ((yyvsp[(1) - (6)]), '/', (yyvsp[(3) - (6)])));
  }
    break;

  case 1101:

/* Line 1806 of yacc.c  */
#line 8233 "parser.y"
    {
	cb_emit_divide ((yyvsp[(3) - (8)]), (yyvsp[(1) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(7) - (8)]));
  }
    break;

  case 1102:

/* Line 1806 of yacc.c  */
#line 8237 "parser.y"
    {
	cb_emit_divide ((yyvsp[(1) - (8)]), (yyvsp[(3) - (8)]), (yyvsp[(5) - (8)]), (yyvsp[(7) - (8)]));
  }
    break;

  case 1103:

/* Line 1806 of yacc.c  */
#line 8244 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DIVIDE);
  }
    break;

  case 1104:

/* Line 1806 of yacc.c  */
#line 8248 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DIVIDE);
  }
    break;

  case 1105:

/* Line 1806 of yacc.c  */
#line 8258 "parser.y"
    {
	begin_statement ("ENABLE", 0);
  }
    break;

  case 1107:

/* Line 1806 of yacc.c  */
#line 8269 "parser.y"
    {
	check_unreached = 0;
	begin_statement ("ENTRY", 0);
  }
    break;

  case 1109:

/* Line 1806 of yacc.c  */
#line 8278 "parser.y"
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "ENTRY");
	} else if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "ENTRY");
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (!cobc_check_valid_name ((char *)(CB_LITERAL ((yyvsp[(1) - (2)]))->data), ENTRY_NAME)) {
			emit_entry ((char *)(CB_LITERAL ((yyvsp[(1) - (2)]))->data), 1, (yyvsp[(2) - (2)]));
		}
	}
  }
    break;

  case 1110:

/* Line 1806 of yacc.c  */
#line 8296 "parser.y"
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
    break;

  case 1112:

/* Line 1806 of yacc.c  */
#line 8320 "parser.y"
    {
	cb_emit_evaluate ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
	eval_level--;
  }
    break;

  case 1113:

/* Line 1806 of yacc.c  */
#line 8327 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1114:

/* Line 1806 of yacc.c  */
#line 8329 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 1115:

/* Line 1806 of yacc.c  */
#line 8334 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	eval_check[eval_level][eval_inc++] = (yyvsp[(1) - (1)]);
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
    break;

  case 1116:

/* Line 1806 of yacc.c  */
#line 8345 "parser.y"
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
    break;

  case 1117:

/* Line 1806 of yacc.c  */
#line 8356 "parser.y"
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
    break;

  case 1118:

/* Line 1806 of yacc.c  */
#line 8370 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1119:

/* Line 1806 of yacc.c  */
#line 8374 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1120:

/* Line 1806 of yacc.c  */
#line 8380 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1121:

/* Line 1806 of yacc.c  */
#line 8382 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1122:

/* Line 1806 of yacc.c  */
#line 8388 "parser.y"
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)]));
	eval_inc2 = 0;
  }
    break;

  case 1123:

/* Line 1806 of yacc.c  */
#line 8397 "parser.y"
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[(3) - (3)]), NULL);
	eval_inc2 = 0;
  }
    break;

  case 1124:

/* Line 1806 of yacc.c  */
#line 8405 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(2) - (2)]));
	eval_inc2 = 0;
  }
    break;

  case 1125:

/* Line 1806 of yacc.c  */
#line 8411 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
	eval_inc2 = 0;
  }
    break;

  case 1126:

/* Line 1806 of yacc.c  */
#line 8418 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1127:

/* Line 1806 of yacc.c  */
#line 8420 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 1128:

/* Line 1806 of yacc.c  */
#line 8425 "parser.y"
    {
	cb_tree	not0;
	cb_tree	e1;
	cb_tree	e2;
	cb_tree	x;
	cb_tree	parm1;

	not0 = cb_int0;
	e2 = (yyvsp[(2) - (2)]);
	x = NULL;
	parm1 = (yyvsp[(1) - (2)]);
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
    break;

  case 1129:

/* Line 1806 of yacc.c  */
#line 8486 "parser.y"
    { (yyval) = cb_any; eval_inc2++; }
    break;

  case 1130:

/* Line 1806 of yacc.c  */
#line 8487 "parser.y"
    { (yyval) = cb_true; eval_inc2++; }
    break;

  case 1131:

/* Line 1806 of yacc.c  */
#line 8488 "parser.y"
    { (yyval) = cb_false; eval_inc2++; }
    break;

  case 1132:

/* Line 1806 of yacc.c  */
#line 8492 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1133:

/* Line 1806 of yacc.c  */
#line 8493 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 1134:

/* Line 1806 of yacc.c  */
#line 8498 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), EVALUATE);
  }
    break;

  case 1135:

/* Line 1806 of yacc.c  */
#line 8502 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), EVALUATE);
  }
    break;

  case 1136:

/* Line 1806 of yacc.c  */
#line 8512 "parser.y"
    {
	begin_statement ("EXIT", 0);
	cobc_cs_check = CB_CS_EXIT;
  }
    break;

  case 1137:

/* Line 1806 of yacc.c  */
#line 8517 "parser.y"
    {
	cobc_cs_check = 0;
  }
    break;

  case 1139:

/* Line 1806 of yacc.c  */
#line 8525 "parser.y"
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
	if ((yyvsp[(2) - (2)]) != NULL) {
		cb_emit_move ((yyvsp[(2) - (2)]), CB_LIST_INIT (current_program->cb_return_code));
	}
	current_statement->name = (const char *)"EXIT PROGRAM";
	cb_emit_exit (0);
  }
    break;

  case 1140:

/* Line 1806 of yacc.c  */
#line 8546 "parser.y"
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
    break;

  case 1141:

/* Line 1806 of yacc.c  */
#line 8560 "parser.y"
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
    break;

  case 1142:

/* Line 1806 of yacc.c  */
#line 8582 "parser.y"
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
    break;

  case 1143:

/* Line 1806 of yacc.c  */
#line 8604 "parser.y"
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
    break;

  case 1144:

/* Line 1806 of yacc.c  */
#line 8624 "parser.y"
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
    break;

  case 1145:

/* Line 1806 of yacc.c  */
#line 8646 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1146:

/* Line 1806 of yacc.c  */
#line 8647 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 1147:

/* Line 1806 of yacc.c  */
#line 8655 "parser.y"
    {
	begin_statement ("FREE", 0);
	current_statement->flag_no_based = 1;
  }
    break;

  case 1149:

/* Line 1806 of yacc.c  */
#line 8664 "parser.y"
    {
	cb_emit_free ((yyvsp[(1) - (1)]));
  }
    break;

  case 1150:

/* Line 1806 of yacc.c  */
#line 8674 "parser.y"
    {
	begin_statement ("GENERATE", 0);
	CB_PENDING("GENERATE");
  }
    break;

  case 1153:

/* Line 1806 of yacc.c  */
#line 8690 "parser.y"
    {
	if (!current_paragraph->flag_statement) {
		current_paragraph->flag_first_is_goto = 1;
	}
	begin_statement ("GO TO", 0);
	save_debug = start_debug;
	start_debug = 0;
  }
    break;

  case 1155:

/* Line 1806 of yacc.c  */
#line 8703 "parser.y"
    {
	cb_emit_goto ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
	start_debug = save_debug;
  }
    break;

  case 1156:

/* Line 1806 of yacc.c  */
#line 8711 "parser.y"
    {
	check_unreached = 1;
	(yyval) = NULL;
  }
    break;

  case 1157:

/* Line 1806 of yacc.c  */
#line 8716 "parser.y"
    {
	check_unreached = 0;
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 1158:

/* Line 1806 of yacc.c  */
#line 8727 "parser.y"
    {
	begin_statement ("GOBACK", 0);
	check_unreached = 1;
	if ((yyvsp[(2) - (2)]) != NULL) {
		cb_emit_move ((yyvsp[(2) - (2)]), CB_LIST_INIT (current_program->cb_return_code));
	}
	cb_emit_exit (1U);
  }
    break;

  case 1159:

/* Line 1806 of yacc.c  */
#line 8742 "parser.y"
    {
	begin_statement ("IF", TERM_IF);
  }
    break;

  case 1161:

/* Line 1806 of yacc.c  */
#line 8751 "parser.y"
    {
	cb_emit_if ((yyvsp[(-1) - (3)]), (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1162:

/* Line 1806 of yacc.c  */
#line 8755 "parser.y"
    {
	cb_emit_if ((yyvsp[(-1) - (2)]), NULL, (yyvsp[(2) - (2)]));
  }
    break;

  case 1163:

/* Line 1806 of yacc.c  */
#line 8759 "parser.y"
    {
	cb_emit_if ((yyvsp[(-1) - (1)]), (yyvsp[(1) - (1)]), NULL);
  }
    break;

  case 1164:

/* Line 1806 of yacc.c  */
#line 8766 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), IF);
  }
    break;

  case 1165:

/* Line 1806 of yacc.c  */
#line 8770 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), IF);
  }
    break;

  case 1166:

/* Line 1806 of yacc.c  */
#line 8780 "parser.y"
    {
	begin_statement ("INITIALIZE", 0);
  }
    break;

  case 1168:

/* Line 1806 of yacc.c  */
#line 8789 "parser.y"
    {
	cb_emit_initialize ((yyvsp[(1) - (5)]), (yyvsp[(2) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]));
  }
    break;

  case 1169:

/* Line 1806 of yacc.c  */
#line 8795 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1170:

/* Line 1806 of yacc.c  */
#line 8796 "parser.y"
    { (yyval) = cb_true; }
    break;

  case 1171:

/* Line 1806 of yacc.c  */
#line 8800 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1172:

/* Line 1806 of yacc.c  */
#line 8801 "parser.y"
    { (yyval) = cb_true; }
    break;

  case 1173:

/* Line 1806 of yacc.c  */
#line 8802 "parser.y"
    { (yyval) = (yyvsp[(1) - (3)]); }
    break;

  case 1174:

/* Line 1806 of yacc.c  */
#line 8807 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1175:

/* Line 1806 of yacc.c  */
#line 8811 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1176:

/* Line 1806 of yacc.c  */
#line 8818 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1177:

/* Line 1806 of yacc.c  */
#line 8823 "parser.y"
    {
	(yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1178:

/* Line 1806 of yacc.c  */
#line 8830 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (4)]), (yyvsp[(4) - (4)]));
  }
    break;

  case 1179:

/* Line 1806 of yacc.c  */
#line 8836 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_ALPHABETIC); }
    break;

  case 1180:

/* Line 1806 of yacc.c  */
#line 8837 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC); }
    break;

  case 1181:

/* Line 1806 of yacc.c  */
#line 8838 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC); }
    break;

  case 1182:

/* Line 1806 of yacc.c  */
#line 8839 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
    break;

  case 1183:

/* Line 1806 of yacc.c  */
#line 8840 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
    break;

  case 1184:

/* Line 1806 of yacc.c  */
#line 8841 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL); }
    break;

  case 1185:

/* Line 1806 of yacc.c  */
#line 8842 "parser.y"
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
    break;

  case 1186:

/* Line 1806 of yacc.c  */
#line 8854 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1187:

/* Line 1806 of yacc.c  */
#line 8858 "parser.y"
    {
	(yyval) = cb_true;
  }
    break;

  case 1188:

/* Line 1806 of yacc.c  */
#line 8867 "parser.y"
    {
	begin_statement ("INITIATE", 0);
	CB_PENDING("INITIATE");
  }
    break;

  case 1190:

/* Line 1806 of yacc.c  */
#line 8876 "parser.y"
    {
	begin_implicit_statement ();
	if ((yyvsp[(1) - (1)]) != cb_error_node) {
	}
  }
    break;

  case 1191:

/* Line 1806 of yacc.c  */
#line 8882 "parser.y"
    {
	begin_implicit_statement ();
	if ((yyvsp[(2) - (2)]) != cb_error_node) {
	}
  }
    break;

  case 1192:

/* Line 1806 of yacc.c  */
#line 8893 "parser.y"
    {
	begin_statement ("INSPECT", 0);
	inspect_keyword = 0;
  }
    break;

  case 1202:

/* Line 1806 of yacc.c  */
#line 8921 "parser.y"
    {
	previous_tallying_phrase = NO_PHRASE;
	cb_init_tallying ();
  }
    break;

  case 1203:

/* Line 1806 of yacc.c  */
#line 8926 "parser.y"
    {
	if (!(previous_tallying_phrase == CHARACTERS_PHRASE
	      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
		cb_error (_("TALLYING clause is incomplete"));
	} else {
		cb_emit_inspect ((yyvsp[(0) - (3)]), (yyvsp[(3) - (3)]), TALLYING_CLAUSE);
	}

	(yyval) = (yyvsp[(0) - (3)]);
  }
    break;

  case 1204:

/* Line 1806 of yacc.c  */
#line 8942 "parser.y"
    {
	cb_emit_inspect ((yyvsp[(0) - (2)]), (yyvsp[(2) - (2)]), REPLACING_CLAUSE);
	inspect_keyword = 0;
  }
    break;

  case 1205:

/* Line 1806 of yacc.c  */
#line 8952 "parser.y"
    {
	cb_tree		x;
	x = cb_build_converting ((yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]), (yyvsp[(5) - (5)]));
	cb_emit_inspect ((yyvsp[(0) - (5)]), x, CONVERTING_CLAUSE);
  }
    break;

  case 1206:

/* Line 1806 of yacc.c  */
#line 8961 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1207:

/* Line 1806 of yacc.c  */
#line 8965 "parser.y"
    {
	(yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1208:

/* Line 1806 of yacc.c  */
#line 8972 "parser.y"
    {
	check_preceding_tallying_phrases (FOR_PHRASE);
	(yyval) = cb_build_tallying_data ((yyvsp[(1) - (2)]));
  }
    break;

  case 1209:

/* Line 1806 of yacc.c  */
#line 8977 "parser.y"
    {
	check_preceding_tallying_phrases (CHARACTERS_PHRASE);
	(yyval) = cb_build_tallying_characters ((yyvsp[(2) - (2)]));
  }
    break;

  case 1210:

/* Line 1806 of yacc.c  */
#line 8982 "parser.y"
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_all ();
  }
    break;

  case 1211:

/* Line 1806 of yacc.c  */
#line 8987 "parser.y"
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_leading ();
  }
    break;

  case 1212:

/* Line 1806 of yacc.c  */
#line 8992 "parser.y"
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_trailing ();
  }
    break;

  case 1213:

/* Line 1806 of yacc.c  */
#line 8997 "parser.y"
    {
	check_preceding_tallying_phrases (VALUE_REGION_PHRASE);
	(yyval) = cb_build_tallying_value ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1214:

/* Line 1806 of yacc.c  */
#line 9004 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1215:

/* Line 1806 of yacc.c  */
#line 9005 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1216:

/* Line 1806 of yacc.c  */
#line 9010 "parser.y"
    {
	(yyval) = cb_build_replacing_characters ((yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
	inspect_keyword = 0;
  }
    break;

  case 1217:

/* Line 1806 of yacc.c  */
#line 9015 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1219:

/* Line 1806 of yacc.c  */
#line 9022 "parser.y"
    { inspect_keyword = 1; }
    break;

  case 1220:

/* Line 1806 of yacc.c  */
#line 9023 "parser.y"
    { inspect_keyword = 2; }
    break;

  case 1221:

/* Line 1806 of yacc.c  */
#line 9024 "parser.y"
    { inspect_keyword = 3; }
    break;

  case 1222:

/* Line 1806 of yacc.c  */
#line 9025 "parser.y"
    { inspect_keyword = 4; }
    break;

  case 1223:

/* Line 1806 of yacc.c  */
#line 9030 "parser.y"
    {
	switch (inspect_keyword) {
		case 1:
			(yyval) = cb_build_replacing_all ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
			break;
		case 2:
			(yyval) = cb_build_replacing_leading ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
			break;
		case 3:
			(yyval) = cb_build_replacing_first ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
			break;
		case 4:
			(yyval) = cb_build_replacing_trailing ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
			break;
		default:
			cb_error_x (CB_TREE (current_statement),
				    _("INSPECT missing ALL/FIRST/LEADING/TRAILING"));
			(yyval) = cb_build_replacing_all ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
			break;
	}
  }
    break;

  case 1224:

/* Line 1806 of yacc.c  */
#line 9057 "parser.y"
    {
	(yyval) = cb_build_inspect_region_start ();
  }
    break;

  case 1225:

/* Line 1806 of yacc.c  */
#line 9061 "parser.y"
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[(1) - (1)]));
  }
    break;

  case 1226:

/* Line 1806 of yacc.c  */
#line 9065 "parser.y"
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[(1) - (1)]));
  }
    break;

  case 1227:

/* Line 1806 of yacc.c  */
#line 9069 "parser.y"
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[(1) - (2)])), (yyvsp[(2) - (2)]));
  }
    break;

  case 1228:

/* Line 1806 of yacc.c  */
#line 9073 "parser.y"
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[(1) - (2)])), (yyvsp[(2) - (2)]));
  }
    break;

  case 1229:

/* Line 1806 of yacc.c  */
#line 9080 "parser.y"
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_before", (yyvsp[(3) - (3)]));
  }
    break;

  case 1230:

/* Line 1806 of yacc.c  */
#line 9087 "parser.y"
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_after", (yyvsp[(3) - (3)]));
  }
    break;

  case 1231:

/* Line 1806 of yacc.c  */
#line 9096 "parser.y"
    {
	begin_statement ("MERGE", 0);
	current_statement->flag_merge = 1;
  }
    break;

  case 1233:

/* Line 1806 of yacc.c  */
#line 9108 "parser.y"
    {
	begin_statement ("MOVE", 0);
  }
    break;

  case 1235:

/* Line 1806 of yacc.c  */
#line 9116 "parser.y"
    {
	cb_emit_move ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1236:

/* Line 1806 of yacc.c  */
#line 9120 "parser.y"
    {
	cb_emit_move_corresponding ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
  }
    break;

  case 1237:

/* Line 1806 of yacc.c  */
#line 9130 "parser.y"
    {
	begin_statement ("MULTIPLY", TERM_MULTIPLY);
  }
    break;

  case 1239:

/* Line 1806 of yacc.c  */
#line 9139 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(3) - (4)]), '*', (yyvsp[(1) - (4)]));
  }
    break;

  case 1240:

/* Line 1806 of yacc.c  */
#line 9143 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_op ((yyvsp[(1) - (6)]), '*', (yyvsp[(3) - (6)])));
  }
    break;

  case 1241:

/* Line 1806 of yacc.c  */
#line 9150 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MULTIPLY);
  }
    break;

  case 1242:

/* Line 1806 of yacc.c  */
#line 9154 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MULTIPLY);
  }
    break;

  case 1243:

/* Line 1806 of yacc.c  */
#line 9164 "parser.y"
    {
	begin_statement ("OPEN", 0);
  }
    break;

  case 1247:

/* Line 1806 of yacc.c  */
#line 9177 "parser.y"
    {
	cb_tree l;
	cb_tree x;

	if ((yyvsp[(2) - (5)]) && (yyvsp[(5) - (5)])) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if ((yyvsp[(5) - (5)])) {
		x = (yyvsp[(5) - (5)]);
	} else {
		x = (yyvsp[(2) - (5)]);
	}

	for (l = (yyvsp[(4) - (5)]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), (yyvsp[(1) - (5)]), x);
		}
	}
  }
    break;

  case 1248:

/* Line 1806 of yacc.c  */
#line 9201 "parser.y"
    { (yyval) = cb_int (COB_OPEN_INPUT); }
    break;

  case 1249:

/* Line 1806 of yacc.c  */
#line 9202 "parser.y"
    { (yyval) = cb_int (COB_OPEN_OUTPUT); }
    break;

  case 1250:

/* Line 1806 of yacc.c  */
#line 9203 "parser.y"
    { (yyval) = cb_int (COB_OPEN_I_O); }
    break;

  case 1251:

/* Line 1806 of yacc.c  */
#line 9204 "parser.y"
    { (yyval) = cb_int (COB_OPEN_EXTEND); }
    break;

  case 1252:

/* Line 1806 of yacc.c  */
#line 9208 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1253:

/* Line 1806 of yacc.c  */
#line 9209 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1254:

/* Line 1806 of yacc.c  */
#line 9213 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1255:

/* Line 1806 of yacc.c  */
#line 9214 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1256:

/* Line 1806 of yacc.c  */
#line 9215 "parser.y"
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
    break;

  case 1257:

/* Line 1806 of yacc.c  */
#line 9217 "parser.y"
    {
	(void)cb_verify (CB_OBSOLETE, "REVERSED");
	(yyval) = NULL;
  }
    break;

  case 1258:

/* Line 1806 of yacc.c  */
#line 9228 "parser.y"
    {
	begin_statement ("PERFORM", TERM_PERFORM);
	/* Turn off field debug - PERFORM is special */
	save_debug = start_debug;
	start_debug = 0;
	cobc_cs_check = CB_CS_PERFORM;
  }
    break;

  case 1260:

/* Line 1806 of yacc.c  */
#line 9240 "parser.y"
    {
	cb_emit_perform ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
    break;

  case 1261:

/* Line 1806 of yacc.c  */
#line 9246 "parser.y"
    {
	CB_ADD_TO_CHAIN ((yyvsp[(1) - (1)]), perform_stack);
	/* Restore field debug before inline statements */
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
    break;

  case 1262:

/* Line 1806 of yacc.c  */
#line 9253 "parser.y"
    {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
  }
    break;

  case 1263:

/* Line 1806 of yacc.c  */
#line 9258 "parser.y"
    {
	cb_emit_perform ((yyvsp[(1) - (2)]), NULL);
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
    break;

  case 1264:

/* Line 1806 of yacc.c  */
#line 9267 "parser.y"
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-4) - (0)]), PERFORM);
	}
  }
    break;

  case 1265:

/* Line 1806 of yacc.c  */
#line 9275 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), PERFORM);
  }
    break;

  case 1266:

/* Line 1806 of yacc.c  */
#line 9282 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), PERFORM);
  }
    break;

  case 1267:

/* Line 1806 of yacc.c  */
#line 9286 "parser.y"
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-2) - (1)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-2) - (1)]), PERFORM);
	}
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
  }
    break;

  case 1268:

/* Line 1806 of yacc.c  */
#line 9299 "parser.y"
    {
	/* Return from $1 */
	CB_REFERENCE ((yyvsp[(1) - (1)]))->length = cb_true;
	CB_REFERENCE ((yyvsp[(1) - (1)]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (1)]), (yyvsp[(1) - (1)]));
  }
    break;

  case 1269:

/* Line 1806 of yacc.c  */
#line 9306 "parser.y"
    {
	/* Return from $3 */
	CB_REFERENCE ((yyvsp[(3) - (3)]))->length = cb_true;
	CB_REFERENCE ((yyvsp[(1) - (3)]))->flag_decl_ok = 1;
	CB_REFERENCE ((yyvsp[(3) - (3)]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1270:

/* Line 1806 of yacc.c  */
#line 9317 "parser.y"
    {
	(yyval) = cb_build_perform_once (NULL);
  }
    break;

  case 1271:

/* Line 1806 of yacc.c  */
#line 9321 "parser.y"
    {
	(yyval) = cb_build_perform_times ((yyvsp[(1) - (2)]));
	current_program->loop_counter++;
  }
    break;

  case 1272:

/* Line 1806 of yacc.c  */
#line 9326 "parser.y"
    {
	(yyval) = cb_build_perform_forever (NULL);
  }
    break;

  case 1273:

/* Line 1806 of yacc.c  */
#line 9330 "parser.y"
    {
	cb_tree varying;

	if (!(yyvsp[(3) - (3)])) {
		(yyval) = cb_build_perform_forever (NULL);
	} else {
		varying = CB_LIST_INIT (cb_build_perform_varying (NULL, NULL, NULL, (yyvsp[(3) - (3)])));
		(yyval) = cb_build_perform_until ((yyvsp[(1) - (3)]), varying);
	}
  }
    break;

  case 1274:

/* Line 1806 of yacc.c  */
#line 9341 "parser.y"
    {
	(yyval) = cb_build_perform_until ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1275:

/* Line 1806 of yacc.c  */
#line 9347 "parser.y"
    { (yyval) = CB_BEFORE; }
    break;

  case 1276:

/* Line 1806 of yacc.c  */
#line 9348 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1277:

/* Line 1806 of yacc.c  */
#line 9352 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1278:

/* Line 1806 of yacc.c  */
#line 9353 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1279:

/* Line 1806 of yacc.c  */
#line 9356 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1280:

/* Line 1806 of yacc.c  */
#line 9358 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 1281:

/* Line 1806 of yacc.c  */
#line 9363 "parser.y"
    {
	(yyval) = cb_build_perform_varying ((yyvsp[(1) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(7) - (7)]));
  }
    break;

  case 1282:

/* Line 1806 of yacc.c  */
#line 9372 "parser.y"
    {
	begin_statement ("PURGE", 0);
  }
    break;

  case 1283:

/* Line 1806 of yacc.c  */
#line 9376 "parser.y"
    {
  }
    break;

  case 1284:

/* Line 1806 of yacc.c  */
#line 9384 "parser.y"
    {
	begin_statement ("READ", TERM_READ);
  }
    break;

  case 1286:

/* Line 1806 of yacc.c  */
#line 9393 "parser.y"
    {
	cobc_cs_check = 0;

	if (CB_VALID_TREE ((yyvsp[(1) - (7)]))) {
		struct cb_file	*cf;

		cf = CB_FILE(cb_ref ((yyvsp[(1) - (7)])));
		if ((yyvsp[(5) - (7)]) && (cf->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error_x (CB_TREE (current_statement),
				    _("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if ((yyvsp[(6) - (7)]) &&
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
			cb_emit_read ((yyvsp[(1) - (7)]), (yyvsp[(2) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(5) - (7)]));
		}
	}
  }
    break;

  case 1287:

/* Line 1806 of yacc.c  */
#line 9421 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1288:

/* Line 1806 of yacc.c  */
#line 9422 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 1289:

/* Line 1806 of yacc.c  */
#line 9427 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1290:

/* Line 1806 of yacc.c  */
#line 9431 "parser.y"
    {
	(yyval) = cb_int3;
  }
    break;

  case 1291:

/* Line 1806 of yacc.c  */
#line 9435 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1292:

/* Line 1806 of yacc.c  */
#line 9439 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1295:

/* Line 1806 of yacc.c  */
#line 9451 "parser.y"
    {
	CB_PENDING ("ADVANCING ON LOCK");
  }
    break;

  case 1299:

/* Line 1806 of yacc.c  */
#line 9464 "parser.y"
    {
	CB_PENDING ("RETRY");
	cobc_cs_check = 0;
  }
    break;

  case 1305:

/* Line 1806 of yacc.c  */
#line 9484 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1306:

/* Line 1806 of yacc.c  */
#line 9488 "parser.y"
    {
   (yyval) = cb_int5;
  }
    break;

  case 1307:

/* Line 1806 of yacc.c  */
#line 9492 "parser.y"
    {
	/* TO-DO: Merge with RETRY phrase */
	(yyval) = cb_int4;
  }
    break;

  case 1308:

/* Line 1806 of yacc.c  */
#line 9499 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1309:

/* Line 1806 of yacc.c  */
#line 9500 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1312:

/* Line 1806 of yacc.c  */
#line 9510 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), READ);
  }
    break;

  case 1313:

/* Line 1806 of yacc.c  */
#line 9514 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), READ);
  }
    break;

  case 1314:

/* Line 1806 of yacc.c  */
#line 9524 "parser.y"
    {
	begin_statement ("READY TRACE", 0);
	cb_emit_ready_trace ();
  }
    break;

  case 1315:

/* Line 1806 of yacc.c  */
#line 9534 "parser.y"
    {
	begin_statement ("RECEIVE", TERM_RECEIVE);
  }
    break;

  case 1329:

/* Line 1806 of yacc.c  */
#line 9577 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RECEIVE);
  }
    break;

  case 1330:

/* Line 1806 of yacc.c  */
#line 9581 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RECEIVE);
  }
    break;

  case 1331:

/* Line 1806 of yacc.c  */
#line 9590 "parser.y"
    {
	begin_statement ("RELEASE", 0);
  }
    break;

  case 1333:

/* Line 1806 of yacc.c  */
#line 9598 "parser.y"
    {
	cb_emit_release ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1334:

/* Line 1806 of yacc.c  */
#line 9608 "parser.y"
    {
	begin_statement ("RESET TRACE", 0);
	cb_emit_reset_trace ();
  }
    break;

  case 1335:

/* Line 1806 of yacc.c  */
#line 9618 "parser.y"
    {
	begin_statement ("RETURN", TERM_RETURN);
  }
    break;

  case 1337:

/* Line 1806 of yacc.c  */
#line 9627 "parser.y"
    {
	cb_emit_return ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
  }
    break;

  case 1338:

/* Line 1806 of yacc.c  */
#line 9634 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RETURN);
  }
    break;

  case 1339:

/* Line 1806 of yacc.c  */
#line 9638 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RETURN);
  }
    break;

  case 1340:

/* Line 1806 of yacc.c  */
#line 9648 "parser.y"
    {
	begin_statement ("REWRITE", TERM_REWRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
    break;

  case 1342:

/* Line 1806 of yacc.c  */
#line 9660 "parser.y"
    {
	cb_emit_rewrite ((yyvsp[(1) - (5)]), (yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]));
	start_debug = save_debug;
  }
    break;

  case 1343:

/* Line 1806 of yacc.c  */
#line 9668 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1345:

/* Line 1806 of yacc.c  */
#line 9676 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1346:

/* Line 1806 of yacc.c  */
#line 9680 "parser.y"
    {
	(yyval) = cb_int2;
  }
    break;

  case 1347:

/* Line 1806 of yacc.c  */
#line 9687 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), REWRITE);
  }
    break;

  case 1348:

/* Line 1806 of yacc.c  */
#line 9691 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), REWRITE);
  }
    break;

  case 1349:

/* Line 1806 of yacc.c  */
#line 9701 "parser.y"
    {
	begin_statement ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
    break;

  case 1350:

/* Line 1806 of yacc.c  */
#line 9712 "parser.y"
    {
	begin_statement ("SEARCH", TERM_SEARCH);
  }
    break;

  case 1352:

/* Line 1806 of yacc.c  */
#line 9721 "parser.y"
    {
	cb_emit_search ((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]));
  }
    break;

  case 1353:

/* Line 1806 of yacc.c  */
#line 9726 "parser.y"
    {
	current_statement->name = (const char *)"SEARCH ALL";
	cb_emit_search_all ((yyvsp[(2) - (6)]), (yyvsp[(3) - (6)]), (yyvsp[(5) - (6)]), (yyvsp[(6) - (6)]));
  }
    break;

  case 1354:

/* Line 1806 of yacc.c  */
#line 9733 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1355:

/* Line 1806 of yacc.c  */
#line 9734 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 1356:

/* Line 1806 of yacc.c  */
#line 9739 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1357:

/* Line 1806 of yacc.c  */
#line 9744 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1358:

/* Line 1806 of yacc.c  */
#line 9751 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1359:

/* Line 1806 of yacc.c  */
#line 9755 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)]));
  }
    break;

  case 1360:

/* Line 1806 of yacc.c  */
#line 9763 "parser.y"
    {
	(yyval) = cb_build_if_check_break ((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1361:

/* Line 1806 of yacc.c  */
#line 9770 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SEARCH);
  }
    break;

  case 1362:

/* Line 1806 of yacc.c  */
#line 9774 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SEARCH);
  }
    break;

  case 1363:

/* Line 1806 of yacc.c  */
#line 9784 "parser.y"
    {
	begin_statement ("SEND", 0);
  }
    break;

  case 1365:

/* Line 1806 of yacc.c  */
#line 9792 "parser.y"
    {
  }
    break;

  case 1366:

/* Line 1806 of yacc.c  */
#line 9795 "parser.y"
    {
  }
    break;

  case 1369:

/* Line 1806 of yacc.c  */
#line 9806 "parser.y"
    {
  }
    break;

  case 1376:

/* Line 1806 of yacc.c  */
#line 9826 "parser.y"
    {
	begin_statement ("SET", 0);
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	cobc_cs_check = CB_CS_SET;
  }
    break;

  case 1377:

/* Line 1806 of yacc.c  */
#line 9833 "parser.y"
    {
	cobc_cs_check = 0;
  }
    break;

  case 1385:

/* Line 1806 of yacc.c  */
#line 9849 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1386:

/* Line 1806 of yacc.c  */
#line 9850 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 1387:

/* Line 1806 of yacc.c  */
#line 9854 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 1388:

/* Line 1806 of yacc.c  */
#line 9855 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1389:

/* Line 1806 of yacc.c  */
#line 9862 "parser.y"
    {
	cb_emit_setenv ((yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
  }
    break;

  case 1390:

/* Line 1806 of yacc.c  */
#line 9871 "parser.y"
    {
	cb_emit_set_attribute ((yyvsp[(1) - (3)]), set_attr_val_on, set_attr_val_off);
  }
    break;

  case 1393:

/* Line 1806 of yacc.c  */
#line 9883 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_BELL);
  }
    break;

  case 1394:

/* Line 1806 of yacc.c  */
#line 9887 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_BLINK);
  }
    break;

  case 1395:

/* Line 1806 of yacc.c  */
#line 9891 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_HIGHLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_HIGHLIGHT);
  }
    break;

  case 1396:

/* Line 1806 of yacc.c  */
#line 9897 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_LOWLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_LOWLIGHT);
  }
    break;

  case 1397:

/* Line 1806 of yacc.c  */
#line 9903 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_REVERSE);
  }
    break;

  case 1398:

/* Line 1806 of yacc.c  */
#line 9907 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_UNDERLINE);
  }
    break;

  case 1399:

/* Line 1806 of yacc.c  */
#line 9911 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_LEFTLINE);
  }
    break;

  case 1400:

/* Line 1806 of yacc.c  */
#line 9915 "parser.y"
    {
	bit_set_attr ((yyvsp[(2) - (2)]), COB_SCREEN_OVERLINE);
  }
    break;

  case 1401:

/* Line 1806 of yacc.c  */
#line 9924 "parser.y"
    {
	cb_emit_set_to ((yyvsp[(1) - (4)]), cb_build_ppointer ((yyvsp[(4) - (4)])));
  }
    break;

  case 1402:

/* Line 1806 of yacc.c  */
#line 9928 "parser.y"
    {
	cb_emit_set_to ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1403:

/* Line 1806 of yacc.c  */
#line 9937 "parser.y"
    {
	cb_emit_set_up_down ((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
  }
    break;

  case 1406:

/* Line 1806 of yacc.c  */
#line 9951 "parser.y"
    {
	cb_emit_set_on_off ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1409:

/* Line 1806 of yacc.c  */
#line 9965 "parser.y"
    {
	cb_emit_set_true ((yyvsp[(1) - (3)]));
  }
    break;

  case 1410:

/* Line 1806 of yacc.c  */
#line 9969 "parser.y"
    {
	cb_emit_set_false ((yyvsp[(1) - (3)]));
  }
    break;

  case 1411:

/* Line 1806 of yacc.c  */
#line 9978 "parser.y"
    {
	  cb_emit_set_last_exception_to_off ();
  }
    break;

  case 1412:

/* Line 1806 of yacc.c  */
#line 9987 "parser.y"
    {
	begin_statement ("SORT", 0);
  }
    break;

  case 1414:

/* Line 1806 of yacc.c  */
#line 9995 "parser.y"
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[(1) - (4)]));
	if (CB_VALID_TREE (x)) {
		if (CB_INVALID_TREE ((yyvsp[(2) - (4)]))) {
			if (CB_FILE_P (x)) {
				cb_error (_("file sort requires KEY phrase"));
			} else {
				/* FIXME: use key definition from OCCURS */
				cb_error (_("%s is not implemented"), _("table SORT without keys"));
			}
			(yyval) = NULL;
		} else {
			cb_emit_sort_init ((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(4) - (4)]));
			(yyval)= (yyvsp[(1) - (4)]);
		}
	} else {
		(yyval) = NULL;
	}
  }
    break;

  case 1415:

/* Line 1806 of yacc.c  */
#line 10017 "parser.y"
    {
	if ((yyvsp[(5) - (7)]) && CB_VALID_TREE ((yyvsp[(1) - (7)]))) {
		cb_emit_sort_finish ((yyvsp[(1) - (7)]));
	}
  }
    break;

  case 1416:

/* Line 1806 of yacc.c  */
#line 10026 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1417:

/* Line 1806 of yacc.c  */
#line 10031 "parser.y"
    {
	cb_tree l;
	cb_tree lparm;

	if ((yyvsp[(5) - (5)]) == NULL) {
		l = CB_LIST_INIT (NULL);
	} else {
		l = (yyvsp[(5) - (5)]);
	}
	lparm = l;
	for (; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[(3) - (5)]);
	}
	(yyval) = cb_list_append ((yyvsp[(1) - (5)]), lparm);
  }
    break;

  case 1418:

/* Line 1806 of yacc.c  */
#line 10049 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1419:

/* Line 1806 of yacc.c  */
#line 10050 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1421:

/* Line 1806 of yacc.c  */
#line 10055 "parser.y"
    {
	/* The OC sort is a stable sort. ie. Dups are per default in order */
	/* Therefore nothing to do here */
  }
    break;

  case 1422:

/* Line 1806 of yacc.c  */
#line 10062 "parser.y"
    { (yyval) = cb_null; }
    break;

  case 1423:

/* Line 1806 of yacc.c  */
#line 10063 "parser.y"
    { (yyval) = cb_ref ((yyvsp[(3) - (3)])); }
    break;

  case 1424:

/* Line 1806 of yacc.c  */
#line 10068 "parser.y"
    {
	if ((yyvsp[(0) - (0)]) && CB_FILE_P (cb_ref ((yyvsp[(0) - (0)])))) {
		cb_error (_("file sort requires USING or INPUT PROCEDURE"));
	}
  }
    break;

  case 1425:

/* Line 1806 of yacc.c  */
#line 10074 "parser.y"
    {
	if ((yyvsp[(0) - (2)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(0) - (2)])))) {
			cb_error (_("USING invalid with table SORT"));
		} else {
			cb_emit_sort_using ((yyvsp[(0) - (2)]), (yyvsp[(2) - (2)]));
		}
	}
  }
    break;

  case 1426:

/* Line 1806 of yacc.c  */
#line 10084 "parser.y"
    {
	if ((yyvsp[(0) - (4)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(0) - (4)])))) {
			cb_error (_("INPUT PROCEDURE invalid with table SORT"));
		} else if (current_statement->flag_merge) {
			cb_error (_("INPUT PROCEDURE invalid with MERGE"));
		} else {
			cb_emit_sort_input ((yyvsp[(4) - (4)]));
		}
	}
  }
    break;

  case 1427:

/* Line 1806 of yacc.c  */
#line 10099 "parser.y"
    {
	if ((yyvsp[(-1) - (0)]) && CB_FILE_P (cb_ref ((yyvsp[(-1) - (0)])))) {
		cb_error (_("file sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
    break;

  case 1428:

/* Line 1806 of yacc.c  */
#line 10105 "parser.y"
    {
	if ((yyvsp[(-1) - (2)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (2)])))) {
			cb_error (_("GIVING invalid with table SORT"));
		} else {
			cb_emit_sort_giving ((yyvsp[(-1) - (2)]), (yyvsp[(2) - (2)]));
		}
	}
  }
    break;

  case 1429:

/* Line 1806 of yacc.c  */
#line 10115 "parser.y"
    {
	if ((yyvsp[(-1) - (4)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (4)])))) {
			cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
		} else {
			cb_emit_sort_output ((yyvsp[(4) - (4)]));
		}
	}
  }
    break;

  case 1430:

/* Line 1806 of yacc.c  */
#line 10131 "parser.y"
    {
	begin_statement ("START", TERM_START);
	start_tree = cb_int (COB_EQ);
  }
    break;

  case 1432:

/* Line 1806 of yacc.c  */
#line 10141 "parser.y"
    {
	if ((yyvsp[(3) - (4)]) && !(yyvsp[(2) - (4)])) {
		cb_error_x (CB_TREE (current_statement),
			    _("SIZE/LENGTH invalid here"));
	} else {
		cb_emit_start ((yyvsp[(1) - (4)]), start_tree, (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]));
	}
  }
    break;

  case 1433:

/* Line 1806 of yacc.c  */
#line 10153 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1434:

/* Line 1806 of yacc.c  */
#line 10157 "parser.y"
    {
	(yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 1435:

/* Line 1806 of yacc.c  */
#line 10164 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1436:

/* Line 1806 of yacc.c  */
#line 10168 "parser.y"
    {
	start_tree = (yyvsp[(3) - (4)]);
	(yyval) = (yyvsp[(4) - (4)]);
  }
    break;

  case 1437:

/* Line 1806 of yacc.c  */
#line 10173 "parser.y"
    {
	start_tree = cb_int (COB_FI);
	(yyval) = NULL;
  }
    break;

  case 1438:

/* Line 1806 of yacc.c  */
#line 10178 "parser.y"
    {
	start_tree = cb_int (COB_LA);
	(yyval) = NULL;
  }
    break;

  case 1439:

/* Line 1806 of yacc.c  */
#line 10185 "parser.y"
    { (yyval) = cb_int (COB_EQ); }
    break;

  case 1440:

/* Line 1806 of yacc.c  */
#line 10186 "parser.y"
    { (yyval) = cb_int ((yyvsp[(1) - (2)]) ? COB_LE : COB_GT); }
    break;

  case 1441:

/* Line 1806 of yacc.c  */
#line 10187 "parser.y"
    { (yyval) = cb_int ((yyvsp[(1) - (2)]) ? COB_GE : COB_LT); }
    break;

  case 1442:

/* Line 1806 of yacc.c  */
#line 10188 "parser.y"
    { (yyval) = cb_int ((yyvsp[(1) - (2)]) ? COB_LT : COB_GE); }
    break;

  case 1443:

/* Line 1806 of yacc.c  */
#line 10189 "parser.y"
    { (yyval) = cb_int ((yyvsp[(1) - (2)]) ? COB_GT : COB_LE); }
    break;

  case 1444:

/* Line 1806 of yacc.c  */
#line 10190 "parser.y"
    { (yyval) = cb_int (COB_NE); }
    break;

  case 1445:

/* Line 1806 of yacc.c  */
#line 10195 "parser.y"
    {
	cb_error_x (CB_TREE (current_statement),
		    _("NOT EQUAL condition not allowed on START statement"));
  }
    break;

  case 1448:

/* Line 1806 of yacc.c  */
#line 10208 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), START);
  }
    break;

  case 1449:

/* Line 1806 of yacc.c  */
#line 10212 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), START);
  }
    break;

  case 1450:

/* Line 1806 of yacc.c  */
#line 10222 "parser.y"
    {
	begin_statement ("STOP RUN", 0);
  }
    break;

  case 1451:

/* Line 1806 of yacc.c  */
#line 10226 "parser.y"
    {
	cb_emit_stop_run ((yyvsp[(4) - (4)]));
	check_unreached = 1;
	cobc_cs_check = 0;
  }
    break;

  case 1452:

/* Line 1806 of yacc.c  */
#line 10232 "parser.y"
    {
	begin_statement ("STOP", 0);
	cb_verify (cb_stop_literal_statement, "STOP literal");
	cb_emit_display (CB_LIST_INIT ((yyvsp[(2) - (2)])), cb_int0, cb_int1, NULL,
			 NULL, 1, DEVICE_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
	cobc_cs_check = 0;
  }
    break;

  case 1453:

/* Line 1806 of yacc.c  */
#line 10244 "parser.y"
    {
	(yyval) = current_program->cb_return_code;
  }
    break;

  case 1454:

/* Line 1806 of yacc.c  */
#line 10248 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1455:

/* Line 1806 of yacc.c  */
#line 10252 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1456:

/* Line 1806 of yacc.c  */
#line 10256 "parser.y"
    {
	if ((yyvsp[(4) - (4)])) {
		(yyval) = (yyvsp[(4) - (4)]);
	} else {
		(yyval) = cb_int1;
	}
  }
    break;

  case 1457:

/* Line 1806 of yacc.c  */
#line 10264 "parser.y"
    {
	if ((yyvsp[(4) - (4)])) {
		(yyval) = (yyvsp[(4) - (4)]);
	} else {
		(yyval) = cb_int0;
	}
  }
    break;

  case 1458:

/* Line 1806 of yacc.c  */
#line 10275 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1459:

/* Line 1806 of yacc.c  */
#line 10279 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1460:

/* Line 1806 of yacc.c  */
#line 10285 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1461:

/* Line 1806 of yacc.c  */
#line 10286 "parser.y"
    { (yyval) = cb_space; }
    break;

  case 1462:

/* Line 1806 of yacc.c  */
#line 10287 "parser.y"
    { (yyval) = cb_zero; }
    break;

  case 1463:

/* Line 1806 of yacc.c  */
#line 10288 "parser.y"
    { (yyval) = cb_quote; }
    break;

  case 1464:

/* Line 1806 of yacc.c  */
#line 10295 "parser.y"
    {
	begin_statement ("STRING", TERM_STRING);
	save_tree = NULL;
  }
    break;

  case 1466:

/* Line 1806 of yacc.c  */
#line 10305 "parser.y"
    {
	cb_emit_string (save_tree, (yyvsp[(3) - (5)]), (yyvsp[(4) - (5)]));
  }
    break;

  case 1469:

/* Line 1806 of yacc.c  */
#line 10317 "parser.y"
    {
    if (!save_tree) {
		save_tree = CB_LIST_INIT ((yyvsp[(1) - (2)]));
	} else {
		save_tree = cb_list_add (save_tree, (yyvsp[(1) - (2)]));
	}
	if ((yyvsp[(2) - (2)])) {
		save_tree = cb_list_add (save_tree, (yyvsp[(2) - (2)]));
	}
  }
    break;

  case 1470:

/* Line 1806 of yacc.c  */
#line 10331 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1471:

/* Line 1806 of yacc.c  */
#line 10333 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1472:

/* Line 1806 of yacc.c  */
#line 10337 "parser.y"
    { (yyval) = CB_BUILD_PAIR (cb_int0, NULL); }
    break;

  case 1473:

/* Line 1806 of yacc.c  */
#line 10338 "parser.y"
    { (yyval) = CB_BUILD_PAIR ((yyvsp[(1) - (1)]), NULL); }
    break;

  case 1474:

/* Line 1806 of yacc.c  */
#line 10342 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1475:

/* Line 1806 of yacc.c  */
#line 10343 "parser.y"
    { (yyval) = (yyvsp[(4) - (4)]); }
    break;

  case 1476:

/* Line 1806 of yacc.c  */
#line 10348 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), STRING);
  }
    break;

  case 1477:

/* Line 1806 of yacc.c  */
#line 10352 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), STRING);
  }
    break;

  case 1478:

/* Line 1806 of yacc.c  */
#line 10362 "parser.y"
    {
	begin_statement ("SUBTRACT", TERM_SUBTRACT);
  }
    break;

  case 1480:

/* Line 1806 of yacc.c  */
#line 10371 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(3) - (4)]), '-', cb_build_binary_list ((yyvsp[(1) - (4)]), '+'));
  }
    break;

  case 1481:

/* Line 1806 of yacc.c  */
#line 10375 "parser.y"
    {
	cb_emit_arithmetic ((yyvsp[(5) - (6)]), 0, cb_build_binary_list (CB_BUILD_CHAIN ((yyvsp[(3) - (6)]), (yyvsp[(1) - (6)])), '-'));
  }
    break;

  case 1482:

/* Line 1806 of yacc.c  */
#line 10379 "parser.y"
    {
	cb_emit_corresponding (cb_build_sub, (yyvsp[(4) - (6)]), (yyvsp[(2) - (6)]), (yyvsp[(5) - (6)]));
  }
    break;

  case 1483:

/* Line 1806 of yacc.c  */
#line 10386 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SUBTRACT);
  }
    break;

  case 1484:

/* Line 1806 of yacc.c  */
#line 10390 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SUBTRACT);
  }
    break;

  case 1485:

/* Line 1806 of yacc.c  */
#line 10400 "parser.y"
    {
	begin_statement ("SUPPRESS", 0);
	if (!in_declaratives) {
		cb_error_x (CB_TREE (current_statement),
			    _("SUPPRESS statement must be within DECLARATIVES"));
	}
	CB_PENDING("SUPPRESS");
  }
    break;

  case 1488:

/* Line 1806 of yacc.c  */
#line 10418 "parser.y"
    {
	begin_statement ("TERMINATE", 0);
	CB_PENDING("TERMINATE");
  }
    break;

  case 1490:

/* Line 1806 of yacc.c  */
#line 10427 "parser.y"
    {
	begin_implicit_statement ();
	if ((yyvsp[(1) - (1)]) != cb_error_node) {
	}
  }
    break;

  case 1491:

/* Line 1806 of yacc.c  */
#line 10433 "parser.y"
    {
	begin_implicit_statement ();
	if ((yyvsp[(2) - (2)]) != cb_error_node) {
	}
  }
    break;

  case 1492:

/* Line 1806 of yacc.c  */
#line 10444 "parser.y"
    {
	begin_statement ("TRANSFORM", 0);
  }
    break;

  case 1494:

/* Line 1806 of yacc.c  */
#line 10452 "parser.y"
    {
	cb_tree		x;

	x = cb_build_converting ((yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), cb_build_inspect_region_start ());
	cb_emit_inspect ((yyvsp[(1) - (5)]), x, TRANSFORM_STATEMENT);
  }
    break;

  case 1495:

/* Line 1806 of yacc.c  */
#line 10465 "parser.y"
    {
	begin_statement ("UNLOCK", 0);
  }
    break;

  case 1497:

/* Line 1806 of yacc.c  */
#line 10473 "parser.y"
    {
	if (CB_VALID_TREE ((yyvsp[(1) - (2)]))) {
		if (CB_FILE (cb_ref ((yyvsp[(1) - (2)])))->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("UNLOCK invalid for SORT files"));
		} else {
			cb_emit_unlock ((yyvsp[(1) - (2)]));
		}
	}
  }
    break;

  case 1498:

/* Line 1806 of yacc.c  */
#line 10489 "parser.y"
    {
	begin_statement ("UNSTRING", TERM_UNSTRING);
  }
    break;

  case 1500:

/* Line 1806 of yacc.c  */
#line 10499 "parser.y"
    {
	cb_emit_unstring ((yyvsp[(1) - (6)]), (yyvsp[(2) - (6)]), (yyvsp[(3) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(5) - (6)]));
  }
    break;

  case 1501:

/* Line 1806 of yacc.c  */
#line 10505 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1502:

/* Line 1806 of yacc.c  */
#line 10507 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1503:

/* Line 1806 of yacc.c  */
#line 10511 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1504:

/* Line 1806 of yacc.c  */
#line 10513 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); }
    break;

  case 1505:

/* Line 1806 of yacc.c  */
#line 10518 "parser.y"
    {
	(yyval) = cb_build_unstring_delimited ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1506:

/* Line 1806 of yacc.c  */
#line 10524 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(2) - (2)])); }
    break;

  case 1507:

/* Line 1806 of yacc.c  */
#line 10526 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1508:

/* Line 1806 of yacc.c  */
#line 10531 "parser.y"
    {
	(yyval) = cb_build_unstring_into ((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1509:

/* Line 1806 of yacc.c  */
#line 10537 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1510:

/* Line 1806 of yacc.c  */
#line 10538 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1511:

/* Line 1806 of yacc.c  */
#line 10542 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1512:

/* Line 1806 of yacc.c  */
#line 10543 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1513:

/* Line 1806 of yacc.c  */
#line 10547 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1514:

/* Line 1806 of yacc.c  */
#line 10548 "parser.y"
    { (yyval) = (yyvsp[(3) - (3)]); }
    break;

  case 1515:

/* Line 1806 of yacc.c  */
#line 10553 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), UNSTRING);
  }
    break;

  case 1516:

/* Line 1806 of yacc.c  */
#line 10557 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), UNSTRING);
  }
    break;

  case 1517:

/* Line 1806 of yacc.c  */
#line 10567 "parser.y"
    {
	skip_statements = 0;
	in_debugging = 0;
  }
    break;

  case 1524:

/* Line 1806 of yacc.c  */
#line 10585 "parser.y"
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
    break;

  case 1525:

/* Line 1806 of yacc.c  */
#line 10610 "parser.y"
    {
	use_global_ind = 0;
  }
    break;

  case 1526:

/* Line 1806 of yacc.c  */
#line 10614 "parser.y"
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		use_global_ind = 1;
		current_program->flag_global_use = 1;
	}
  }
    break;

  case 1527:

/* Line 1806 of yacc.c  */
#line 10626 "parser.y"
    {
	cb_tree		l;

	for (l = (yyvsp[(1) - (1)]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
    break;

  case 1528:

/* Line 1806 of yacc.c  */
#line 10636 "parser.y"
    {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
    break;

  case 1529:

/* Line 1806 of yacc.c  */
#line 10641 "parser.y"
    {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
    break;

  case 1530:

/* Line 1806 of yacc.c  */
#line 10646 "parser.y"
    {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
    break;

  case 1531:

/* Line 1806 of yacc.c  */
#line 10651 "parser.y"
    {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
    break;

  case 1532:

/* Line 1806 of yacc.c  */
#line 10659 "parser.y"
    {
	cb_tree		plabel;
	char		name[64];

	cb_verify (cb_use_for_debugging, "USE FOR DEBUGGING");
	
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
    break;

  case 1535:

/* Line 1806 of yacc.c  */
#line 10704 "parser.y"
    {
	cb_tree		l;
	cb_tree		x;
	cb_tree		z;

	if (current_program->flag_debugging) {
		CB_REFERENCE ((yyvsp[(1) - (1)]))->debug_section = current_section;
		CB_REFERENCE ((yyvsp[(1) - (1)]))->flag_debug_code = 1;
		CB_REFERENCE ((yyvsp[(1) - (1)]))->flag_all_debug = 0;

		z = CB_LIST_INIT ((yyvsp[(1) - (1)]));
		current_program->debug_list =
			cb_list_append (current_program->debug_list, z);
		/* Check backward refs to file/data names */
		/* Label refs will be checked later (forward/backward ref) */
		if (CB_WORD_COUNT ((yyvsp[(1) - (1)])) > 0) {
			l = CB_VALUE (CB_WORD_ITEMS ((yyvsp[(1) - (1)])));
			switch (CB_TREE_TAG (l)) {
			case CB_TAG_CD:
				CB_CD (l)->debug_section = current_section;
				CB_CD (l)->flag_field_debug = 1;
				break;
			case CB_TAG_FILE:
				CB_FILE (l)->debug_section = current_section;
				CB_FILE (l)->flag_fl_debug = 1;
				break; 
			case CB_TAG_FIELD:
				x = cb_ref ((yyvsp[(1) - (1)]));
				if (CB_INVALID_TREE (x)) {
					break;
				}
				needs_field_debug = 1;
				CB_FIELD (x)->debug_section = current_section;
				CB_FIELD (x)->flag_field_debug = 1;
				CB_PURPOSE (z) = x;
				break;
			default:
				break;
			}
		}
	}
  }
    break;

  case 1536:

/* Line 1806 of yacc.c  */
#line 10747 "parser.y"
    {
	if (current_program->flag_debugging) {
		if (current_program->all_procedure) {
			cb_error (_("duplicate USE DEBUGGING ON ALL PROCEDURES"));
		} else {
			current_program->all_procedure = current_section;
		}
	}
  }
    break;

  case 1537:

/* Line 1806 of yacc.c  */
#line 10757 "parser.y"
    {
	cb_tree		x;

	if (current_program->flag_debugging) {
		/* Reference must be a data item */
		x = cb_ref ((yyvsp[(3) - (3)]));
		if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
			cb_error (_("invalid target for DEBUGGING ALL"));
		} else {
			needs_field_debug = 1;
			CB_FIELD (x)->debug_section = current_section;
			CB_FIELD (x)->flag_field_debug = 1;
			CB_FIELD (x)->flag_all_debug = 1;
			CB_REFERENCE ((yyvsp[(3) - (3)]))->debug_section = current_section;
			CB_REFERENCE ((yyvsp[(3) - (3)]))->flag_debug_code = 1;
			CB_REFERENCE ((yyvsp[(3) - (3)]))->flag_all_debug = 1;
			CB_CHAIN_PAIR (current_program->debug_list, x, (yyvsp[(3) - (3)]));
		}
	}
  }
    break;

  case 1542:

/* Line 1806 of yacc.c  */
#line 10787 "parser.y"
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "USE AT");
	}
  }
    break;

  case 1543:

/* Line 1806 of yacc.c  */
#line 10796 "parser.y"
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM START"));
	/* emit_entry ("_START", 0, NULL); */
	CB_PENDING ("USE AT PROGRAM START");
  }
    break;

  case 1544:

/* Line 1806 of yacc.c  */
#line 10802 "parser.y"
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM END"));
	/* emit_entry ("_END", 0, NULL); */
	CB_PENDING ("USE AT PROGRAM END");
  }
    break;

  case 1545:

/* Line 1806 of yacc.c  */
#line 10812 "parser.y"
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE BEFORE REPORTING"));
	CB_PENDING ("USE BEFORE REPORTING");
  }
    break;

  case 1546:

/* Line 1806 of yacc.c  */
#line 10821 "parser.y"
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING ("USE AFTER EXCEPTION CONDITION");
  }
    break;

  case 1549:

/* Line 1806 of yacc.c  */
#line 10837 "parser.y"
    {
	begin_statement ("WRITE", TERM_WRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
    break;

  case 1551:

/* Line 1806 of yacc.c  */
#line 10849 "parser.y"
    {
	if (CB_VALID_TREE ((yyvsp[(1) - (6)]))) {
		cb_emit_write ((yyvsp[(1) - (6)]), (yyvsp[(2) - (6)]), (yyvsp[(3) - (6)]), (yyvsp[(5) - (6)]));
	}
	start_debug = save_debug;
  }
    break;

  case 1552:

/* Line 1806 of yacc.c  */
#line 10858 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1553:

/* Line 1806 of yacc.c  */
#line 10859 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 1554:

/* Line 1806 of yacc.c  */
#line 10864 "parser.y"
    {
	(yyval) = cb_int0;
  }
    break;

  case 1555:

/* Line 1806 of yacc.c  */
#line 10868 "parser.y"
    {
	(yyval) = cb_build_write_advancing_lines ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]));
  }
    break;

  case 1556:

/* Line 1806 of yacc.c  */
#line 10872 "parser.y"
    {
	(yyval) = cb_build_write_advancing_mnemonic ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1557:

/* Line 1806 of yacc.c  */
#line 10876 "parser.y"
    {
	(yyval) = cb_build_write_advancing_page ((yyvsp[(1) - (3)]));
  }
    break;

  case 1558:

/* Line 1806 of yacc.c  */
#line 10882 "parser.y"
    { (yyval) = CB_BEFORE; }
    break;

  case 1559:

/* Line 1806 of yacc.c  */
#line 10883 "parser.y"
    { (yyval) = CB_AFTER; }
    break;

  case 1563:

/* Line 1806 of yacc.c  */
#line 10894 "parser.y"
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), WRITE);
  }
    break;

  case 1564:

/* Line 1806 of yacc.c  */
#line 10898 "parser.y"
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), WRITE);
  }
    break;

  case 1567:

/* Line 1806 of yacc.c  */
#line 10912 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
    break;

  case 1568:

/* Line 1806 of yacc.c  */
#line 10922 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1569:

/* Line 1806 of yacc.c  */
#line 10926 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1570:

/* Line 1806 of yacc.c  */
#line 10933 "parser.y"
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1575:

/* Line 1806 of yacc.c  */
#line 10951 "parser.y"
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1580:

/* Line 1806 of yacc.c  */
#line 10967 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
    break;

  case 1581:

/* Line 1806 of yacc.c  */
#line 10977 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1582:

/* Line 1806 of yacc.c  */
#line 10981 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1583:

/* Line 1806 of yacc.c  */
#line 10988 "parser.y"
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1586:

/* Line 1806 of yacc.c  */
#line 11001 "parser.y"
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1589:

/* Line 1806 of yacc.c  */
#line 11013 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT SIZE ERROR before SIZE ERROR"));
	}
  }
    break;

  case 1590:

/* Line 1806 of yacc.c  */
#line 11023 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1591:

/* Line 1806 of yacc.c  */
#line 11027 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1592:

/* Line 1806 of yacc.c  */
#line 11034 "parser.y"
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1595:

/* Line 1806 of yacc.c  */
#line 11047 "parser.y"
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1598:

/* Line 1806 of yacc.c  */
#line 11059 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT OVERFLOW before OVERFLOW"));
	}
  }
    break;

  case 1599:

/* Line 1806 of yacc.c  */
#line 11069 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1600:

/* Line 1806 of yacc.c  */
#line 11073 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1601:

/* Line 1806 of yacc.c  */
#line 11080 "parser.y"
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1604:

/* Line 1806 of yacc.c  */
#line 11093 "parser.y"
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1606:

/* Line 1806 of yacc.c  */
#line 11105 "parser.y"
    {
	cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
  }
    break;

  case 1608:

/* Line 1806 of yacc.c  */
#line 11114 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
	}
  }
    break;

  case 1609:

/* Line 1806 of yacc.c  */
#line 11123 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1610:

/* Line 1806 of yacc.c  */
#line 11127 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1611:

/* Line 1806 of yacc.c  */
#line 11134 "parser.y"
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1614:

/* Line 1806 of yacc.c  */
#line 11147 "parser.y"
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1616:

/* Line 1806 of yacc.c  */
#line 11158 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT AT END-OF-PAGE before AT END-OF-PAGE"));
	}
  }
    break;

  case 1617:

/* Line 1806 of yacc.c  */
#line 11168 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1618:

/* Line 1806 of yacc.c  */
#line 11172 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1619:

/* Line 1806 of yacc.c  */
#line 11179 "parser.y"
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1622:

/* Line 1806 of yacc.c  */
#line 11192 "parser.y"
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1626:

/* Line 1806 of yacc.c  */
#line 11208 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT INVALID KEY before INVALID KEY"));
	}
  }
    break;

  case 1627:

/* Line 1806 of yacc.c  */
#line 11218 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1628:

/* Line 1806 of yacc.c  */
#line 11222 "parser.y"
    {
	(yyval) = cb_int1;
  }
    break;

  case 1629:

/* Line 1806 of yacc.c  */
#line 11229 "parser.y"
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1632:

/* Line 1806 of yacc.c  */
#line 11242 "parser.y"
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[(2) - (2)]);
  }
    break;

  case 1633:

/* Line 1806 of yacc.c  */
#line 11252 "parser.y"
    {
	(yyval) = cb_one;
  }
    break;

  case 1634:

/* Line 1806 of yacc.c  */
#line 11256 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (2)]);
  }
    break;

  case 1635:

/* Line 1806 of yacc.c  */
#line 11266 "parser.y"
    {
	(yyval) = cb_build_cond ((yyvsp[(1) - (1)]));
  }
    break;

  case 1636:

/* Line 1806 of yacc.c  */
#line 11273 "parser.y"
    {
	(yyval) = cb_build_expr ((yyvsp[(1) - (1)]));
  }
    break;

  case 1637:

/* Line 1806 of yacc.c  */
#line 11279 "parser.y"
    {
	current_expr = NULL;
	cb_exp_line = cb_source_line;
  }
    break;

  case 1638:

/* Line 1806 of yacc.c  */
#line 11284 "parser.y"
    {
	(yyval) = cb_list_reverse (current_expr);
  }
    break;

  case 1642:

/* Line 1806 of yacc.c  */
#line 11296 "parser.y"
    { push_expr ('x', (yyvsp[(1) - (1)])); }
    break;

  case 1643:

/* Line 1806 of yacc.c  */
#line 11297 "parser.y"
    { push_expr ('C', (yyvsp[(1) - (1)])); }
    break;

  case 1644:

/* Line 1806 of yacc.c  */
#line 11299 "parser.y"
    { push_expr ('(', NULL); }
    break;

  case 1645:

/* Line 1806 of yacc.c  */
#line 11300 "parser.y"
    { push_expr (')', NULL); }
    break;

  case 1646:

/* Line 1806 of yacc.c  */
#line 11302 "parser.y"
    { push_expr ('+', NULL); }
    break;

  case 1647:

/* Line 1806 of yacc.c  */
#line 11303 "parser.y"
    { push_expr ('-', NULL); }
    break;

  case 1648:

/* Line 1806 of yacc.c  */
#line 11304 "parser.y"
    { push_expr ('*', NULL); }
    break;

  case 1649:

/* Line 1806 of yacc.c  */
#line 11305 "parser.y"
    { push_expr ('/', NULL); }
    break;

  case 1650:

/* Line 1806 of yacc.c  */
#line 11306 "parser.y"
    { push_expr ('^', NULL); }
    break;

  case 1651:

/* Line 1806 of yacc.c  */
#line 11308 "parser.y"
    { push_expr ('=', NULL); }
    break;

  case 1652:

/* Line 1806 of yacc.c  */
#line 11309 "parser.y"
    { push_expr ('>', NULL); }
    break;

  case 1653:

/* Line 1806 of yacc.c  */
#line 11310 "parser.y"
    { push_expr ('<', NULL); }
    break;

  case 1654:

/* Line 1806 of yacc.c  */
#line 11311 "parser.y"
    { push_expr (']', NULL); }
    break;

  case 1655:

/* Line 1806 of yacc.c  */
#line 11312 "parser.y"
    { push_expr ('[', NULL); }
    break;

  case 1656:

/* Line 1806 of yacc.c  */
#line 11313 "parser.y"
    { push_expr ('~', NULL); }
    break;

  case 1657:

/* Line 1806 of yacc.c  */
#line 11315 "parser.y"
    { push_expr ('!', NULL); }
    break;

  case 1658:

/* Line 1806 of yacc.c  */
#line 11316 "parser.y"
    { push_expr ('&', NULL); }
    break;

  case 1659:

/* Line 1806 of yacc.c  */
#line 11317 "parser.y"
    { push_expr ('|', NULL); }
    break;

  case 1660:

/* Line 1806 of yacc.c  */
#line 11319 "parser.y"
    { push_expr ('O', NULL); }
    break;

  case 1661:

/* Line 1806 of yacc.c  */
#line 11320 "parser.y"
    { push_expr ('9', NULL); }
    break;

  case 1662:

/* Line 1806 of yacc.c  */
#line 11321 "parser.y"
    { push_expr ('A', NULL); }
    break;

  case 1663:

/* Line 1806 of yacc.c  */
#line 11322 "parser.y"
    { push_expr ('L', NULL); }
    break;

  case 1664:

/* Line 1806 of yacc.c  */
#line 11323 "parser.y"
    { push_expr ('U', NULL); }
    break;

  case 1665:

/* Line 1806 of yacc.c  */
#line 11326 "parser.y"
    { push_expr ('P', NULL); }
    break;

  case 1666:

/* Line 1806 of yacc.c  */
#line 11327 "parser.y"
    { push_expr ('N', NULL); }
    break;

  case 1675:

/* Line 1806 of yacc.c  */
#line 11357 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1676:

/* Line 1806 of yacc.c  */
#line 11361 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1680:

/* Line 1806 of yacc.c  */
#line 11372 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '+', (yyvsp[(3) - (3)])); }
    break;

  case 1681:

/* Line 1806 of yacc.c  */
#line 11373 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '-', (yyvsp[(3) - (3)])); }
    break;

  case 1682:

/* Line 1806 of yacc.c  */
#line 11374 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1683:

/* Line 1806 of yacc.c  */
#line 11378 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '*', (yyvsp[(3) - (3)])); }
    break;

  case 1684:

/* Line 1806 of yacc.c  */
#line 11379 "parser.y"
    { (yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '/', (yyvsp[(3) - (3)])); }
    break;

  case 1685:

/* Line 1806 of yacc.c  */
#line 11380 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1686:

/* Line 1806 of yacc.c  */
#line 11385 "parser.y"
    {
	(yyval) = cb_build_binary_op ((yyvsp[(1) - (3)]), '^', (yyvsp[(3) - (3)]));
  }
    break;

  case 1687:

/* Line 1806 of yacc.c  */
#line 11388 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1688:

/* Line 1806 of yacc.c  */
#line 11392 "parser.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 1689:

/* Line 1806 of yacc.c  */
#line 11393 "parser.y"
    { (yyval) = cb_build_binary_op (cb_zero, '-', (yyvsp[(2) - (2)])); }
    break;

  case 1690:

/* Line 1806 of yacc.c  */
#line 11394 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1691:

/* Line 1806 of yacc.c  */
#line 11397 "parser.y"
    { (yyval) = (yyvsp[(2) - (3)]); }
    break;

  case 1692:

/* Line 1806 of yacc.c  */
#line 11398 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1693:

/* Line 1806 of yacc.c  */
#line 11409 "parser.y"
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
    break;

  case 1694:

/* Line 1806 of yacc.c  */
#line 11421 "parser.y"
    {
	if (CB_FILE_P (cb_ref ((yyvsp[(3) - (3)])))) {
		(yyval) = CB_FILE (cb_ref ((yyvsp[(3) - (3)])))->linage_ctr;
	} else {
		cb_error_x ((yyvsp[(3) - (3)]), _("'%s' is not a file name"), CB_NAME ((yyvsp[(3) - (3)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1695:

/* Line 1806 of yacc.c  */
#line 11430 "parser.y"
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
    break;

  case 1696:

/* Line 1806 of yacc.c  */
#line 11442 "parser.y"
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[(3) - (3)])))) {
		(yyval) = CB_REPORT (cb_ref ((yyvsp[(3) - (3)])))->line_counter;
	} else {
		cb_error_x ((yyvsp[(3) - (3)]), _("'%s' is not a report name"), CB_NAME ((yyvsp[(3) - (3)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1697:

/* Line 1806 of yacc.c  */
#line 11451 "parser.y"
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
    break;

  case 1698:

/* Line 1806 of yacc.c  */
#line 11463 "parser.y"
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[(3) - (3)])))) {
		(yyval) = CB_REPORT (cb_ref ((yyvsp[(3) - (3)])))->page_counter;
	} else {
		cb_error_x ((yyvsp[(3) - (3)]), _("'%s' is not a report name"), CB_NAME ((yyvsp[(3) - (3)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1699:

/* Line 1806 of yacc.c  */
#line 11477 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1700:

/* Line 1806 of yacc.c  */
#line 11479 "parser.y"
    { (yyval) = cb_list_append ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1701:

/* Line 1806 of yacc.c  */
#line 11484 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[(2) - (2)]), (yyvsp[(1) - (2)]));
  }
    break;

  case 1702:

/* Line 1806 of yacc.c  */
#line 11492 "parser.y"
    { cb_build_identifier ((yyvsp[(1) - (1)]), 0); }
    break;

  case 1703:

/* Line 1806 of yacc.c  */
#line 11499 "parser.y"
    {
	if (!CB_FILE_P (cb_ref ((yyvsp[(1) - (1)])))) {
		(yyval) = (yyvsp[(1) - (1)]);
	} else {
		cb_error_x ((yyvsp[(1) - (1)]), _("%s requires a record name as subject"),
			current_statement->name);
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1704:

/* Line 1806 of yacc.c  */
#line 11509 "parser.y"
    {
	if (CB_FILE_P (cb_ref ((yyvsp[(2) - (2)])))) {
		(yyval) = (yyvsp[(2) - (2)]);
	} else {
		cb_error_x ((yyvsp[(2) - (2)]), _("'%s' is not a file name"), CB_NAME ((yyvsp[(2) - (2)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1705:

/* Line 1806 of yacc.c  */
#line 11523 "parser.y"
    {
	cb_tree x;

	x = cb_ref ((yyvsp[(1) - (1)]));
	if (!CB_FIELD_P (x)) {
		(yyval) = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x ((yyvsp[(1) - (1)]), _("'%s' not indexed"), cb_name ((yyvsp[(1) - (1)])));
		listprint_suppress ();
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		listprint_restore ();
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[(1) - (1)]);
	}
  }
    break;

  case 1706:

/* Line 1806 of yacc.c  */
#line 11545 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1707:

/* Line 1806 of yacc.c  */
#line 11549 "parser.y"
    {
	cb_tree		l;

	if (CB_VALID_TREE ((yyvsp[(2) - (2)]))) {
		for (l = (yyvsp[(1) - (2)]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME ((yyvsp[(2) - (2)])), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ((yyvsp[(2) - (2)]), _("multiple reference to '%s' "),
					    CB_NAME ((yyvsp[(2) - (2)])));
				break;
			}
		}
		if (!l) {
			(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
		}
	}
  }
    break;

  case 1708:

/* Line 1806 of yacc.c  */
#line 11570 "parser.y"
    {
	if (CB_FILE_P (cb_ref ((yyvsp[(1) - (1)])))) {
		(yyval) = (yyvsp[(1) - (1)]);
	} else {
		cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a file name"), CB_NAME ((yyvsp[(1) - (1)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1709:

/* Line 1806 of yacc.c  */
#line 11582 "parser.y"
    {
	if (CB_CD_P (cb_ref ((yyvsp[(1) - (1)])))) {
		(yyval) = (yyvsp[(1) - (1)]);
	} else {
		cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a CD name"), CB_NAME ((yyvsp[(1) - (1)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1710:

/* Line 1806 of yacc.c  */
#line 11623 "parser.y"
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[(1) - (1)])))) {
		(yyval) = (yyvsp[(1) - (1)]);
	} else {
		cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a report name"), CB_NAME ((yyvsp[(1) - (1)])));
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1711:

/* Line 1806 of yacc.c  */
#line 11636 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1712:

/* Line 1806 of yacc.c  */
#line 11638 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1713:

/* Line 1806 of yacc.c  */
#line 11642 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1714:

/* Line 1806 of yacc.c  */
#line 11648 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1715:

/* Line 1806 of yacc.c  */
#line 11650 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1716:

/* Line 1806 of yacc.c  */
#line 11655 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	CB_REFERENCE ((yyval))->offset = CB_TREE (current_section);
	CB_REFERENCE ((yyval))->flag_in_decl = !!in_declaratives;
	CB_REFERENCE ((yyval))->section = current_section;
	CB_REFERENCE ((yyval))->paragraph = current_paragraph;
	CB_ADD_TO_CHAIN ((yyval), current_program->label_list);
  }
    break;

  case 1719:

/* Line 1806 of yacc.c  */
#line 11669 "parser.y"
    {
	CB_REFERENCE ((yyvsp[(1) - (3)]))->chain = (yyvsp[(3) - (3)]);
  }
    break;

  case 1720:

/* Line 1806 of yacc.c  */
#line 11676 "parser.y"
    {
	(yyval) = cb_build_reference ((char *)(CB_LITERAL ((yyvsp[(1) - (1)]))->data));
	(yyval)->source_file = (yyvsp[(1) - (1)])->source_file;
	(yyval)->source_line = (yyvsp[(1) - (1)])->source_line;
  }
    break;

  case 1721:

/* Line 1806 of yacc.c  */
#line 11686 "parser.y"
    { (yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)])); }
    break;

  case 1722:

/* Line 1806 of yacc.c  */
#line 11687 "parser.y"
    { (yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 1723:

/* Line 1806 of yacc.c  */
#line 11692 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
    break;

  case 1724:

/* Line 1806 of yacc.c  */
#line 11700 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
    break;

  case 1725:

/* Line 1806 of yacc.c  */
#line 11708 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1726:

/* Line 1806 of yacc.c  */
#line 11712 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1727:

/* Line 1806 of yacc.c  */
#line 11719 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	CB_REFERENCE((yyval))->flag_optional = 1;
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
    break;

  case 1730:

/* Line 1806 of yacc.c  */
#line 11735 "parser.y"
    {
	if (CB_WORD_COUNT ((yyvsp[(1) - (1)])) > 0) {
		redefinition_error ((yyvsp[(1) - (1)]));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[(1) - (1)]);
	}
  }
    break;

  case 1731:

/* Line 1806 of yacc.c  */
#line 11744 "parser.y"
    {
	  yyclearin;
	  yyerrok;
	  (yyval) = cb_error_node;
  }
    break;

  case 1732:

/* Line 1806 of yacc.c  */
#line 11755 "parser.y"
    {
	if (CB_REFERENCE ((yyvsp[(1) - (1)]))->flag_duped || CB_WORD_COUNT ((yyvsp[(1) - (1)])) > 0) {
		redefinition_error ((yyvsp[(1) - (1)]));
		(yyval) = NULL;
	} else {
		CB_WORD_COUNT ((yyvsp[(1) - (1)]))++;
		(yyval) = (yyvsp[(1) - (1)]);
	}
  }
    break;

  case 1733:

/* Line 1806 of yacc.c  */
#line 11772 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1734:

/* Line 1806 of yacc.c  */
#line 11776 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1737:

/* Line 1806 of yacc.c  */
#line 11785 "parser.y"
    {
	(yyval) = cb_build_address ((yyvsp[(3) - (3)]));
  }
    break;

  case 1738:

/* Line 1806 of yacc.c  */
#line 11791 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1739:

/* Line 1806 of yacc.c  */
#line 11792 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1740:

/* Line 1806 of yacc.c  */
#line 11797 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1741:

/* Line 1806 of yacc.c  */
#line 11801 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1749:

/* Line 1806 of yacc.c  */
#line 11821 "parser.y"
    {
	(yyval) = cb_build_length ((yyvsp[(2) - (2)]));
  }
    break;

  case 1750:

/* Line 1806 of yacc.c  */
#line 11825 "parser.y"
    {
	(yyval) = cb_build_length ((yyvsp[(2) - (2)]));
  }
    break;

  case 1751:

/* Line 1806 of yacc.c  */
#line 11829 "parser.y"
    {
	(yyval) = cb_build_length ((yyvsp[(2) - (2)]));
  }
    break;

  case 1752:

/* Line 1806 of yacc.c  */
#line 11833 "parser.y"
    {
	(yyval) = cb_build_ppointer ((yyvsp[(4) - (4)]));
  }
    break;

  case 1753:

/* Line 1806 of yacc.c  */
#line 11837 "parser.y"
    {
	(yyval) = cb_build_address ((yyvsp[(3) - (3)]));
  }
    break;

  case 1754:

/* Line 1806 of yacc.c  */
#line 11841 "parser.y"
    {
	cb_tree		x;
	cb_tree		switch_id;

	x = cb_ref ((yyvsp[(1) - (1)]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME (x)->category != CB_SWITCH_NAME) {
			cb_error_x ((yyvsp[(1) - (1)]), _("invalid mnemonic identifier"));
			(yyval) = cb_error_node;
		} else {
			switch_id = cb_int (CB_SYSTEM_NAME (x)->token);
			(yyval) = CB_BUILD_FUNCALL_1 ("cob_switch_value", switch_id);
		}
	} else {
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1755:

/* Line 1806 of yacc.c  */
#line 11862 "parser.y"
    {
	(yyval) = CB_LIST_INIT ((yyvsp[(1) - (1)]));
  }
    break;

  case 1756:

/* Line 1806 of yacc.c  */
#line 11866 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  }
    break;

  case 1764:

/* Line 1806 of yacc.c  */
#line 11883 "parser.y"
    {
	(yyval) = cb_build_length ((yyvsp[(2) - (2)]));
  }
    break;

  case 1765:

/* Line 1806 of yacc.c  */
#line 11887 "parser.y"
    {
	(yyval) = cb_build_length ((yyvsp[(2) - (2)]));
  }
    break;

  case 1766:

/* Line 1806 of yacc.c  */
#line 11891 "parser.y"
    {
	(yyval) = cb_build_length ((yyvsp[(2) - (2)]));
  }
    break;

  case 1771:

/* Line 1806 of yacc.c  */
#line 11908 "parser.y"
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[(1) - (1)]));
  }
    break;

  case 1772:

/* Line 1806 of yacc.c  */
#line 11915 "parser.y"
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[(1) - (1)]));
  }
    break;

  case 1778:

/* Line 1806 of yacc.c  */
#line 11933 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1780:

/* Line 1806 of yacc.c  */
#line 11941 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1783:

/* Line 1806 of yacc.c  */
#line 11950 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1786:

/* Line 1806 of yacc.c  */
#line 11959 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1788:

/* Line 1806 of yacc.c  */
#line 11964 "parser.y"
    {
	(yyval) = cb_zero;
  }
    break;

  case 1789:

/* Line 1806 of yacc.c  */
#line 11971 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1791:

/* Line 1806 of yacc.c  */
#line 11979 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1793:

/* Line 1806 of yacc.c  */
#line 11987 "parser.y"
    {
	check_not_88_level ((yyvsp[(1) - (1)]));
  }
    break;

  case 1796:

/* Line 1806 of yacc.c  */
#line 11997 "parser.y"
    { (yyval) = cb_build_identifier ((yyvsp[(1) - (1)]), 0); }
    break;

  case 1797:

/* Line 1806 of yacc.c  */
#line 12001 "parser.y"
    { (yyval) = cb_build_identifier ((yyvsp[(1) - (1)]), 1); }
    break;

  case 1798:

/* Line 1806 of yacc.c  */
#line 12005 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1799:

/* Line 1806 of yacc.c  */
#line 12006 "parser.y"
    { (yyval) = (yyvsp[(1) - (2)]); }
    break;

  case 1800:

/* Line 1806 of yacc.c  */
#line 12011 "parser.y"
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[(1) - (1)]));
  }
    break;

  case 1801:

/* Line 1806 of yacc.c  */
#line 12018 "parser.y"
    {
	if ((yyvsp[(1) - (1)]) != cb_error_node
	    && cb_tree_category ((yyvsp[(1) - (1)])) != CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not numeric"), cb_name ((yyvsp[(1) - (1)])));
	}
  }
    break;

  case 1802:

/* Line 1806 of yacc.c  */
#line 12028 "parser.y"
    {
	int     reference_to_existing_object;

	if (CB_REFERENCE_P ((yyvsp[(1) - (1)])) && (CB_FIELD_P (cb_ref ((yyvsp[(1) - (1)])))
				    || CB_FILE_P (cb_ref ((yyvsp[(1) - (1)]))))) {
		(yyval) = cb_build_identifier ((yyvsp[(1) - (1)]), 0);
	} else {
		reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[(1) - (1)])) && cb_ref ((yyvsp[(1) - (1)])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[(1) - (1)])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a field or file"), cb_name ((yyvsp[(1) - (1)])));
		}
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1803:

/* Line 1806 of yacc.c  */
#line 12047 "parser.y"
    {
	int     reference_to_existing_object;

	if (CB_REFERENCE_P ((yyvsp[(1) - (1)])) && CB_FIELD_P (cb_ref ((yyvsp[(1) - (1)])))) {
		(yyval) = cb_build_identifier ((yyvsp[(1) - (1)]), 0);
	} else {
		reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[(1) - (1)])) && cb_ref ((yyvsp[(1) - (1)])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[(1) - (1)])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[(1) - (1)]), _("'%s' is not a field"), cb_name ((yyvsp[(1) - (1)])));
		}
		(yyval) = cb_error_node;
	}
  }
    break;

  case 1804:

/* Line 1806 of yacc.c  */
#line 12065 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (3)]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (3)]));
	}
  }
    break;

  case 1805:

/* Line 1806 of yacc.c  */
#line 12072 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (2)]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (2)]));
	}
  }
    break;

  case 1806:

/* Line 1806 of yacc.c  */
#line 12079 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (2)]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (2)]));
	}
  }
    break;

  case 1807:

/* Line 1806 of yacc.c  */
#line 12086 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (1)]));
	}
  }
    break;

  case 1808:

/* Line 1806 of yacc.c  */
#line 12096 "parser.y"
    {
	(yyval) = cb_build_identifier ((yyvsp[(1) - (1)]), 0);
  }
    break;

  case 1809:

/* Line 1806 of yacc.c  */
#line 12103 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (3)]);
	if (CB_REFERENCE_P ((yyvsp[(1) - (3)]))) {
		CB_REFERENCE ((yyvsp[(1) - (3)]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (3)]));
	}
  }
    break;

  case 1810:

/* Line 1806 of yacc.c  */
#line 12113 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (2)]);
	if (CB_REFERENCE_P ((yyvsp[(1) - (2)]))) {
		CB_REFERENCE ((yyvsp[(1) - (2)]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (2)]));
	}
  }
    break;

  case 1811:

/* Line 1806 of yacc.c  */
#line 12123 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (2)]);
	if (CB_REFERENCE_P ((yyvsp[(1) - (2)]))) {
		CB_REFERENCE ((yyvsp[(1) - (2)]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (2)]));
	}
  }
    break;

  case 1812:

/* Line 1806 of yacc.c  */
#line 12133 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
	if (CB_REFERENCE_P ((yyvsp[(1) - (1)]))) {
		CB_REFERENCE ((yyvsp[(1) - (1)]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[(1) - (1)]));
	}
  }
    break;

  case 1813:

/* Line 1806 of yacc.c  */
#line 12146 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1814:

/* Line 1806 of yacc.c  */
#line 12150 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (3)]);
	CB_REFERENCE ((yyvsp[(1) - (3)]))->chain = (yyvsp[(3) - (3)]);
  }
    break;

  case 1815:

/* Line 1806 of yacc.c  */
#line 12158 "parser.y"
    {
	(yyval) = (yyvsp[(0) - (3)]);
	CB_REFERENCE ((yyvsp[(0) - (3)]))->subs = cb_list_reverse ((yyvsp[(2) - (3)]));
  }
    break;

  case 1816:

/* Line 1806 of yacc.c  */
#line 12166 "parser.y"
    {
	CB_REFERENCE ((yyvsp[(0) - (4)]))->offset = (yyvsp[(2) - (4)]);
  }
    break;

  case 1817:

/* Line 1806 of yacc.c  */
#line 12170 "parser.y"
    {
	CB_REFERENCE ((yyvsp[(0) - (5)]))->offset = (yyvsp[(2) - (5)]);
	CB_REFERENCE ((yyvsp[(0) - (5)]))->length = (yyvsp[(4) - (5)]);
  }
    break;

  case 1818:

/* Line 1806 of yacc.c  */
#line 12180 "parser.y"
    {
	if (cb_tree_category ((yyvsp[(1) - (1)])) != CB_CATEGORY_NUMERIC
	    || CB_LITERAL ((yyvsp[(1) - (1)]))->sign < 0
	    || CB_LITERAL ((yyvsp[(1) - (1)]))->scale) {
		cb_error (_("non-negative integer value expected"));
		(yyval) = cb_build_numeric_literal(-1, "1", 0);
	} else {
		(yyval) = (yyvsp[(1) - (1)]);
	}
  }
    break;

  case 1819:

/* Line 1806 of yacc.c  */
#line 12194 "parser.y"
    {
	int	n;

	if (cb_tree_category ((yyvsp[(1) - (1)])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else if (CB_LITERAL ((yyvsp[(1) - (1)]))->sign || CB_LITERAL ((yyvsp[(1) - (1)]))->scale) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[(1) - (1)]));
		if (n < 1 || n > 256) {
			cb_error (_("invalid symbolic integer"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[(1) - (1)]);
		}
	}
  }
    break;

  case 1820:

/* Line 1806 of yacc.c  */
#line 12217 "parser.y"
    {
	int	n;

	if (cb_tree_category ((yyvsp[(1) - (1)])) != CB_CATEGORY_NUMERIC
	    || CB_LITERAL ((yyvsp[(1) - (1)]))->sign
	    || CB_LITERAL ((yyvsp[(1) - (1)]))->scale) {
		cb_error (_("unsigned positive integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[(1) - (1)]));
		if (n < 1) {
			cb_error (_("unsigned positive integer value expected"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[(1) - (1)]);
		}
	}
  }
    break;

  case 1821:

/* Line 1806 of yacc.c  */
#line 12239 "parser.y"
    {
	int	n;

	if (cb_tree_category ((yyvsp[(1) - (1)])) == CB_CATEGORY_NUMERIC) {
		if (CB_LITERAL ((yyvsp[(1) - (1)]))->sign || CB_LITERAL ((yyvsp[(1) - (1)]))->scale) {
			cb_error (_("integer value expected"));
		} else {
			n = cb_get_int ((yyvsp[(1) - (1)]));
			if (n < 1 || n > 256) {
				cb_error (_("invalid CLASS value"));
			}
		}
	}
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1822:

/* Line 1806 of yacc.c  */
#line 12254 "parser.y"
    { (yyval) = cb_space; }
    break;

  case 1823:

/* Line 1806 of yacc.c  */
#line 12255 "parser.y"
    { (yyval) = cb_zero; }
    break;

  case 1824:

/* Line 1806 of yacc.c  */
#line 12256 "parser.y"
    { (yyval) = cb_quote; }
    break;

  case 1825:

/* Line 1806 of yacc.c  */
#line 12257 "parser.y"
    { (yyval) = cb_high; }
    break;

  case 1826:

/* Line 1806 of yacc.c  */
#line 12258 "parser.y"
    { (yyval) = cb_low; }
    break;

  case 1827:

/* Line 1806 of yacc.c  */
#line 12259 "parser.y"
    { (yyval) = cb_null; }
    break;

  case 1828:

/* Line 1806 of yacc.c  */
#line 12264 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1829:

/* Line 1806 of yacc.c  */
#line 12268 "parser.y"
    {
	struct cb_literal	*l;

	if (CB_LITERAL_P ((yyvsp[(2) - (2)]))) {
		/* We must not alter the original definition */
		l = cobc_parse_malloc (sizeof(struct cb_literal));
		*l = *(CB_LITERAL((yyvsp[(2) - (2)])));
		l->all = 1;
		(yyval) = CB_TREE (l);
	} else {
		(yyval) = (yyvsp[(2) - (2)]);
	}
  }
    break;

  case 1830:

/* Line 1806 of yacc.c  */
#line 12285 "parser.y"
    {
	(yyval) = (yyvsp[(1) - (1)]);
  }
    break;

  case 1831:

/* Line 1806 of yacc.c  */
#line 12289 "parser.y"
    {
	(yyval) = cb_concat_literals ((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
  }
    break;

  case 1832:

/* Line 1806 of yacc.c  */
#line 12295 "parser.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 1833:

/* Line 1806 of yacc.c  */
#line 12296 "parser.y"
    { (yyval) = cb_space; }
    break;

  case 1834:

/* Line 1806 of yacc.c  */
#line 12297 "parser.y"
    { (yyval) = cb_zero; }
    break;

  case 1835:

/* Line 1806 of yacc.c  */
#line 12298 "parser.y"
    { (yyval) = cb_quote; }
    break;

  case 1836:

/* Line 1806 of yacc.c  */
#line 12299 "parser.y"
    { (yyval) = cb_high; }
    break;

  case 1837:

/* Line 1806 of yacc.c  */
#line 12300 "parser.y"
    { (yyval) = cb_low; }
    break;

  case 1838:

/* Line 1806 of yacc.c  */
#line 12301 "parser.y"
    { (yyval) = cb_null; }
    break;

  case 1839:

/* Line 1806 of yacc.c  */
#line 12308 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (2)]), NULL, (yyvsp[(2) - (2)]), 0);
  }
    break;

  case 1840:

/* Line 1806 of yacc.c  */
#line 12312 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), CB_LIST_INIT ((yyvsp[(3) - (5)])), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1841:

/* Line 1806 of yacc.c  */
#line 12316 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1842:

/* Line 1806 of yacc.c  */
#line 12320 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1843:

/* Line 1806 of yacc.c  */
#line 12324 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), NULL, 0);
  }
    break;

  case 1844:

/* Line 1806 of yacc.c  */
#line 12328 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), NULL, 0);
  }
    break;

  case 1845:

/* Line 1806 of yacc.c  */
#line 12332 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1846:

/* Line 1806 of yacc.c  */
#line 12336 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1847:

/* Line 1806 of yacc.c  */
#line 12340 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1848:

/* Line 1806 of yacc.c  */
#line 12344 "parser.y"
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1849:

/* Line 1806 of yacc.c  */
#line 12348 "parser.y"
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[(1) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(5) - (5)]), 0);
  }
    break;

  case 1850:

/* Line 1806 of yacc.c  */
#line 12352 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), NULL, 0);
  }
    break;

  case 1851:

/* Line 1806 of yacc.c  */
#line 12356 "parser.y"
    {
	(yyval) = cb_build_intrinsic ((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), NULL, 1);
  }
    break;

  case 1861:

/* Line 1806 of yacc.c  */
#line 12381 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1862:

/* Line 1806 of yacc.c  */
#line 12385 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[(2) - (4)]), NULL);
  }
    break;

  case 1863:

/* Line 1806 of yacc.c  */
#line 12389 "parser.y"
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]));
  }
    break;

  case 1864:

/* Line 1806 of yacc.c  */
#line 12396 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1865:

/* Line 1806 of yacc.c  */
#line 12400 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (3)]);
  }
    break;

  case 1866:

/* Line 1806 of yacc.c  */
#line 12404 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1867:

/* Line 1806 of yacc.c  */
#line 12411 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (1)]));
	(yyval) = cb_list_add (x, cb_int0);
  }
    break;

  case 1868:

/* Line 1806 of yacc.c  */
#line 12418 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (3)]));
	(yyval) = cb_list_add (x, cb_int1);
  }
    break;

  case 1869:

/* Line 1806 of yacc.c  */
#line 12425 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (3)]));
	(yyval) = cb_list_add (x, cb_int2);
  }
    break;

  case 1870:

/* Line 1806 of yacc.c  */
#line 12434 "parser.y"
    {
	suppress_data_exceptions = 1;
  }
    break;

  case 1871:

/* Line 1806 of yacc.c  */
#line 12438 "parser.y"
    {		
	suppress_data_exceptions = 0;
	(yyval) = CB_LIST_INIT ((yyvsp[(2) - (2)]));
  }
    break;

  case 1872:

/* Line 1806 of yacc.c  */
#line 12446 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (1)]));
	(yyval) = cb_list_add (x, cb_null);
  }
    break;

  case 1873:

/* Line 1806 of yacc.c  */
#line 12453 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (3)]));
	(yyval) = cb_list_add (x, (yyvsp[(3) - (3)]));
  }
    break;

  case 1874:

/* Line 1806 of yacc.c  */
#line 12463 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (1)]));
	(yyval) = cb_list_add (x, cb_null);
  }
    break;

  case 1875:

/* Line 1806 of yacc.c  */
#line 12470 "parser.y"
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[(1) - (3)]));
	(yyval) = cb_list_add (x, cb_ref ((yyvsp[(3) - (3)])));
  }
    break;

  case 1876:

/* Line 1806 of yacc.c  */
#line 12480 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (1)]), cb_int0);
  }
    break;

  case 1877:

/* Line 1806 of yacc.c  */
#line 12484 "parser.y"
    {
	const int	num_args = cb_list_length ((yyvsp[(1) - (3)]));

	if (num_args == 4) {
		cb_error_x ((yyvsp[(1) - (3)]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[(1) - (3)]), cb_int1);
  }
    break;

  case 1878:

/* Line 1806 of yacc.c  */
#line 12497 "parser.y"
    {
	(yyval) = cb_list_add ((yyvsp[(1) - (1)]), cb_int0);
  }
    break;

  case 1879:

/* Line 1806 of yacc.c  */
#line 12501 "parser.y"
    {
	const int	num_args = cb_list_length ((yyvsp[(1) - (3)]));

	if (num_args == 3) {
		cb_error_x ((yyvsp[(1) - (3)]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[(1) - (3)]), cb_int1);
  }
    break;

  case 1880:

/* Line 1806 of yacc.c  */
#line 12515 "parser.y"
    {
	non_const_word = 1;
  }
    break;

  case 1881:

/* Line 1806 of yacc.c  */
#line 12523 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 1882:

/* Line 1806 of yacc.c  */
#line 12524 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1883:

/* Line 1806 of yacc.c  */
#line 12528 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 1884:

/* Line 1806 of yacc.c  */
#line 12529 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1885:

/* Line 1806 of yacc.c  */
#line 12533 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1886:

/* Line 1806 of yacc.c  */
#line 12534 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1887:

/* Line 1806 of yacc.c  */
#line 12539 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1888:

/* Line 1806 of yacc.c  */
#line 12543 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1889:

/* Line 1806 of yacc.c  */
#line 12550 "parser.y"
    {
	(yyval) = NULL;
  }
    break;

  case 1890:

/* Line 1806 of yacc.c  */
#line 12554 "parser.y"
    {
	(yyval) = (yyvsp[(2) - (2)]);
  }
    break;

  case 1891:

/* Line 1806 of yacc.c  */
#line 12561 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 1892:

/* Line 1806 of yacc.c  */
#line 12562 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1893:

/* Line 1806 of yacc.c  */
#line 12563 "parser.y"
    { (yyval) = cb_int2; }
    break;

  case 1894:

/* Line 1806 of yacc.c  */
#line 12567 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1895:

/* Line 1806 of yacc.c  */
#line 12568 "parser.y"
    { (yyval) = cb_true; }
    break;

  case 1896:

/* Line 1806 of yacc.c  */
#line 12572 "parser.y"
    { (yyval) = cb_int (cb_flag_optional_file); }
    break;

  case 1897:

/* Line 1806 of yacc.c  */
#line 12573 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1898:

/* Line 1806 of yacc.c  */
#line 12574 "parser.y"
    { (yyval) = cb_int0; }
    break;

  case 1899:

/* Line 1806 of yacc.c  */
#line 12579 "parser.y"
    {
	(yyval) = cb_int0;
  }
    break;

  case 1900:

/* Line 1806 of yacc.c  */
#line 12583 "parser.y"
    {
	if ((yyvsp[(2) - (2)])) {
		(yyval) = (yyvsp[(2) - (2)]);
	} else {
		(yyval) = cb_int (COB_STORE_ROUND);
	}
	cobc_cs_check = 0;
  }
    break;

  case 1901:

/* Line 1806 of yacc.c  */
#line 12595 "parser.y"
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
    break;

  case 1902:

/* Line 1806 of yacc.c  */
#line 12600 "parser.y"
    {
	(yyval) = (yyvsp[(3) - (3)]);
	cobc_cs_check = 0;
  }
    break;

  case 1903:

/* Line 1806 of yacc.c  */
#line 12608 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_AWAY_FROM_ZERO);
  }
    break;

  case 1904:

/* Line 1806 of yacc.c  */
#line 12612 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
    break;

  case 1905:

/* Line 1806 of yacc.c  */
#line 12616 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
    break;

  case 1906:

/* Line 1806 of yacc.c  */
#line 12620 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_TOWARD_ZERO);
  }
    break;

  case 1907:

/* Line 1806 of yacc.c  */
#line 12624 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
    break;

  case 1908:

/* Line 1806 of yacc.c  */
#line 12628 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_GREATER);
  }
    break;

  case 1909:

/* Line 1806 of yacc.c  */
#line 12632 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_LESSER);
  }
    break;

  case 1910:

/* Line 1806 of yacc.c  */
#line 12636 "parser.y"
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
    break;

  case 1911:

/* Line 1806 of yacc.c  */
#line 12642 "parser.y"
    { (yyval) = NULL; }
    break;

  case 1912:

/* Line 1806 of yacc.c  */
#line 12643 "parser.y"
    { (yyval) = cb_int1; }
    break;

  case 1913:

/* Line 1806 of yacc.c  */
#line 12650 "parser.y"
    {
	cobc_repeat_last_token = 1;
  }
    break;

  case 1914:

/* Line 1806 of yacc.c  */
#line 12654 "parser.y"
    {
	cobc_repeat_last_token = 1;
  }
    break;

  case 1915:

/* Line 1806 of yacc.c  */
#line 12658 "parser.y"
    {
	cobc_repeat_last_token = 0;
  }
    break;



/* Line 1806 of yacc.c  */
#line 21707 "parser.c"
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

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
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

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

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

  *++yyvsp = yylval;


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

#if !defined(yyoverflow) || YYERROR_VERBOSE
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
  /* Do not reclaim the symbols of the rule which action triggered
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
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 2067 of yacc.c  */
#line 12839 "parser.y"


