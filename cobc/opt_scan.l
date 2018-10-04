/*
   Copyright (C) 2018 Free Software Foundation, Inc.
   Written by James K. Lowden

   This file is part of GnuCOBOL.

   The GnuCOBOL compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.
*/

%option noyywrap warn debug
%{
  #include <ctype.h>

  #include "opt_parse.tab.h"
  #include "options.h"

  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wunused-parameter"

  const char * nonblank( const char text[] );
%}

BLANK ^[[:blank:]]*\n

I			"I"
LINK			"L"
LIB			"l"
D			"D"
FREE	        	"free"
FIXED			"fixed"
STD			"std"
DEBUG			"debug"
G			"g"
O			"O"
O2			"O2"
OS			"Os"
FTRACE			"ftrace"
FTRACEALL		"ftraceall"
FDEBUGGING_LINE		"fdebugging-line"
FSOURCE_LOCATION	"fsource-location"
FIMPLICIT_INIT		"fimplicit-init"
FSTACK_CHECK		"fstack-check"
FNOTRUNC		"fnotrunc"
EXT			"ext"
FMFCOMMENT		"fmfcomment"
ACUCOMMENT		"acucomment"
FSIGN			"fsign"
FFOLD_COPY		"ffold-copy"
SAVE_TEMPS		"save-temps"
TLINES			"tlines"
FTSYMBOLS		"ftsymbols"
FNO_THEADER		"fno-theader"
FNO_TMESSAGES		"fno-tmessages"
FNO_TSOURCE		"fno-tsource"
XREF			"Xref"

ALL			"(no-)?all"
ARCHAIC			"(no-)?archaic"
CALL_PARAMS		"(no-)?call-params"
COLUMN_OVERFLOW		"(no-)?column-overflow"
CONSTANT		"(no-)?constant"
IMPLICIT_DEFINE		"(no-)?implicit-define"
LINKAGE			"(no-)?linkage"
OBSOLETE		"(no-)?obsolete"
PARENTHESES		"(no-)?parentheses"
REDEFINITION		"(no-)?redefinition"
STRICT_TYPING		"(no-)?strict-typing"
TERMINATOR		"(no-)?terminator"
TRUNCATE		"(no-)?truncate"
UNREACHABLE		"(no-)?unreachable"

COMMENT #.*$

EQ [[:blank:]]*=[[:blank:]]*

INTEGER         [[:digit:]]+

VALUE [^[:blank:]\n]+[[:blank:]]*$

COBOL85		"cobol85"
XOPEN		"xopen"
COBOL2002	"cobol2002"
COBOL2014	"cobol2014"
IBM_STRICT	"ibm-strict"
IBM		"ibm"
MVS_STRICT	"mvs-strict"
MVS		"mvs"
MF_STRICT	"mf-strict"
MF		"mf"
BS2000_STRICT	"bs2000-strict"
BS2000		"bs2000"
ACU_STRICT	"acu-strict"
ACU		"acu"
RM_STRICT	"rm-strict"
RM		"rm"

ASCII		"ASCII"
EBCDIC		"EBCDIC"
LOWER		"LOWER"
UPPER		"UPPER"

%%
{BLANK} { return BLANK; }

^[[:blank:]]*{I}		{ yylval = nonblank(yytext); return I; }
^[[:blank:]]*{LINK}		{ yylval = nonblank(yytext); return LINK; }
^[[:blank:]]*{LIB} 		{ yylval = nonblank(yytext); return LIB; }
^[[:blank:]]*{D} 		{ yylval = nonblank(yytext); return D; }
^[[:blank:]]*{FREE} 		{ yylval = nonblank(yytext); return FREE; }
^[[:blank:]]*{FIXED}		{ yylval = nonblank(yytext); return FIXED; }
^[[:blank:]]*{STD} 		{ yylval = nonblank(yytext); return STD; }
^[[:blank:]]*{DEBUG} 		{ yylval = nonblank(yytext); return DEBUG; }
^[[:blank:]]*{G} 		{ yylval = nonblank(yytext); return G; }
^[[:blank:]]*{O} 		{ yylval = nonblank(yytext); return O; }
^[[:blank:]]*{O2} 		{ yylval = nonblank(yytext); return O2; }
^[[:blank:]]*{OS} 		{ yylval = nonblank(yytext); return OS; }
^[[:blank:]]*{FTRACE} 		{ yylval = nonblank(yytext); return FTRACE; }
^[[:blank:]]*{FTRACEALL}	{ yylval = nonblank(yytext); 
                                  return FTRACEALL; }
^[[:blank:]]*{FDEBUGGING_LINE} 	{ yylval = nonblank(yytext);
                                  return FDEBUGGING_LINE; }
^[[:blank:]]*{FSOURCE_LOCATION} { yylval = nonblank(yytext);
                                  return FSOURCE_LOCATION; }
^[[:blank:]]*{FIMPLICIT_INIT} 	{ yylval = nonblank(yytext); 
                                  return FIMPLICIT_INIT; }
^[[:blank:]]*{FSTACK_CHECK} 	{ yylval = nonblank(yytext); 
                                  return FSTACK_CHECK; }
^[[:blank:]]*{FNOTRUNC} 	{ yylval = nonblank(yytext); 
                                  return FNOTRUNC; }
^[[:blank:]]*{EXT} 		{ yylval = nonblank(yytext); return EXT; }
^[[:blank:]]*{FMFCOMMENT} 	{ yylval = nonblank(yytext); 
                                  return FMFCOMMENT; }
^[[:blank:]]*{ACUCOMMENT} 	{ yylval = nonblank(yytext); 
                                  return ACUCOMMENT; }
^[[:blank:]]*{FSIGN} 		{ yylval = nonblank(yytext); return FSIGN; }
^[[:blank:]]*{FFOLD_COPY} 	{ yylval = nonblank(yytext); 
                                  return FFOLD_COPY; }
^[[:blank:]]*{SAVE_TEMPS} 	{ yylval = nonblank(yytext); 
                                  return SAVE_TEMPS; }
^[[:blank:]]*{TLINES} 		{ yylval = nonblank(yytext); return TLINES; }
^[[:blank:]]*{FTSYMBOLS} 	{ yylval = nonblank(yytext); 
                                  return FTSYMBOLS; }
^[[:blank:]]*{FNO_THEADER} 	{ yylval = nonblank(yytext); 
                                  return FNO_THEADER; }
^[[:blank:]]*{FNO_TMESSAGES} 	{ yylval = nonblank(yytext); 
                                  return FNO_TMESSAGES; }
^[[:blank:]]*{FNO_TSOURCE} 	{ yylval = nonblank(yytext); 
                                  return FNO_TSOURCE; }
^[[:blank:]]*{XREF} 		{ yylval = nonblank(yytext); return XREF; }

^[[:blank:]]*W 			{ return WARNING; }

^[[:blank:]]*{COMMENT} 		{ return COMMENT; }

{EQ} 				{ return EQ; }

{INTEGER}[[:blank:]]*$          { yylval = nonblank(yytext); return INTEGER; }

{COBOL85}	{ yylval = nonblank(yytext); return COBOL85; }
{XOPEN}	   	{ yylval = nonblank(yytext); return XOPEN; }
{COBOL2002}     { yylval = nonblank(yytext); return COBOL2002; }
{COBOL2014}     { yylval = nonblank(yytext); return COBOL2014; }
{IBM_STRICT}    { yylval = nonblank(yytext); return IBM_STRICT; }
{IBM}		{ yylval = nonblank(yytext); return IBM; }
{MVS_STRICT}    { yylval = nonblank(yytext); return MVS_STRICT; }
{MVS}     	{ yylval = nonblank(yytext); return MVS; }
{MF_STRICT}     { yylval = nonblank(yytext); return MF_STRICT; }
{MF}      	{ yylval = nonblank(yytext); return MF; }
{BS2000_STRICT} { yylval = nonblank(yytext); return BS2000_STRICT; }
{BS2000}	{ yylval = nonblank(yytext); return BS2000; }
{ACU_STRICT}	{ yylval = nonblank(yytext); return ACU_STRICT; }
{ACU}		{ yylval = nonblank(yytext); return ACU; }
{RM_STRICT}	{ yylval = nonblank(yytext); return RM_STRICT; }
{RM}		{ yylval = nonblank(yytext); return RM; }

{ASCII}		{ yylval = nonblank(yytext); return ASCII; }
{EBCDIC}	{ yylval = nonblank(yytext); return EBCDIC; }
{LOWER}		{ yylval = nonblank(yytext); return LOWER; }
{UPPER}		{ yylval = nonblank(yytext); return UPPER; }

{ALL}[[:blank:]]*$			 { yylval = nonblank(yytext); return ALL; }
{ARCHAIC}		{ yylval = nonblank(yytext); return ARCHAIC; }
{CALL_PARAMS}		{ yylval = nonblank(yytext); return CALL_PARAMS; }
{COLUMN_OVERFLOW}	{ yylval = nonblank(yytext); return COLUMN_OVERFLOW; }
{CONSTANT}		{ yylval = nonblank(yytext); return CONSTANT; }
{IMPLICIT_DEFINE}	{ yylval = nonblank(yytext); return IMPLICIT_DEFINE; }
{LINKAGE}		{ yylval = nonblank(yytext); return LINKAGE; }
{OBSOLETE}		{ yylval = nonblank(yytext); return OBSOLETE; }
{PARENTHESES}		{ yylval = nonblank(yytext); return PARENTHESES; }
{REDEFINITION}		{ yylval = nonblank(yytext); return REDEFINITION; }
{STRICT_TYPING}		{ yylval = nonblank(yytext); return STRICT_TYPING; }
{TERMINATOR}		{ yylval = nonblank(yytext); return TERMINATOR; }
{TRUNCATE}		{ yylval = nonblank(yytext); return TRUNCATE; }
{UNREACHABLE}		{ yylval = nonblank(yytext); return UNREACHABLE; }

{VALUE} 		{ yylval = nonblank(yytext); return VALUE; }

%%
    
const char *
nonblank( const char text[] ) {
    const char *p = text;
    while( isblank(*p) ) {
	p++;
    }
    return p;
}
