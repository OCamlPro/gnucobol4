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

%option noyywrap warn
%{
  #include <ctype.h>
  #include <string.h>

  #include "opt_parse.tab.h"
  #include "options.h"

  static inline char *
  yystr() {
      return strndup(yytext, yyleng);
  }
  
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wunused-parameter"
%}

COMMENT #.*$
EOL [[:blank:]]*({COMMENT}|$)

EQ      [[:blank:]]*=[[:blank:]]*
INTEGER [[:digit:]]+
VALUE   [^[:blank:]\n]+[[:blank:]]*$
xVALUE   [[:alnum:]]+$

%s option eq value eol warn std fsign ffold 
%%

^[[:blank:]]* { BEGIN(option); }

{COMMENT} {}

<option>{
 I                        { yylval = yystr(); BEGIN(value); return I; }
 L                        { yylval = yystr(); BEGIN(value); return LIB; }
 l                        { yylval = yystr(); BEGIN(value); return LINK; }
 D                        { yylval = yystr(); BEGIN(value); return D; }

 W                        { yylval = yystr(); BEGIN(warn); return WARNING; }
 
 free                     { yylval = yystr(); BEGIN(eol);   return FREE; }
 fixed                    { yylval = yystr(); BEGIN(eol);   return FIXED; }

 std/{EQ}                 { yylval = yystr(); BEGIN(std);    return STD; }

 debug                    { yylval = yystr(); BEGIN(eol);   return DEBUG; }
 g                        { yylval = yystr(); BEGIN(eol);   return G; }
 O                        { yylval = yystr(); BEGIN(eol);   return O; }
 O2                       { yylval = yystr(); BEGIN(eol);   return O2; }
 Os                       { yylval = yystr(); BEGIN(eol);   return OS; }
 ftrace                   { yylval = yystr(); BEGIN(eol);   return FTRACE; }
 ftraceall                { yylval = yystr(); BEGIN(eol);   return FTRACEALL; }
 fdebugging-line          { yylval = yystr(); BEGIN(eol);   return FDEBUGGING_LINE; }
 fsource-location         { yylval = yystr(); BEGIN(eol);   return FSOURCE_LOCATION; }
 fimplicit-init           { yylval = yystr(); BEGIN(eol);   return FIMPLICIT_INIT; }
 fstack-check             { yylval = yystr(); BEGIN(eol);   return FSTACK_CHECK; }
 fnotrunc                 { yylval = yystr(); BEGIN(eol);   return FNOTRUNC; }
 
 ext                      { yylval = yystr(); BEGIN(value); return EXT; }
 fmfcomment               { yylval = yystr(); BEGIN(eol);   return FMFCOMMENT; }
 acucomment               { yylval = yystr(); BEGIN(eol);   return ACUCOMMENT; }
 fsign                    { yylval = yystr(); BEGIN(fsign); return FSIGN; }
 ffold-copy               { yylval = yystr(); BEGIN(ffold); return FFOLD_COPY; }
 save-temps               { yylval = yystr(); BEGIN(eq);    return SAVE_TEMPS; }

 tlines                   { yylval = yystr(); BEGIN(eq);    return TLINES; }
 ftsymbols                { yylval = yystr(); BEGIN(eol);   return FTSYMBOLS; }
 fno-theader              { yylval = yystr(); BEGIN(eol);   return FNO_THEADER; }
 fno-tmessages            { yylval = yystr(); BEGIN(eol);   return FNO_TMESSAGES; }
 fno-tsource              { yylval = yystr(); BEGIN(eol);   return FNO_TSOURCE; }
 Xref                     { yylval = yystr(); BEGIN(eol);   return XREF; }
}

<warn>{
 (no-)?all                { yylval = yystr(); BEGIN(eol); return ALL; }
 (no-)?archaic            { yylval = yystr(); BEGIN(eol); return ARCHAIC; }
 (no-)?call-params        { yylval = yystr(); BEGIN(eol); return CALL_PARAMS; }
 (no-)?column-overflow    { yylval = yystr(); BEGIN(eol); return COLUMN_OVERFLOW; }
 (no-)?constant           { yylval = yystr(); BEGIN(eol); return CONSTANT; }
 (no-)?implicit-define    { yylval = yystr(); BEGIN(eol); return IMPLICIT_DEFINE; }
 (no-)?linkage            { yylval = yystr(); BEGIN(eol); return LINKAGE; }
 (no-)?obsolete           { yylval = yystr(); BEGIN(eol); return OBSOLETE; }
 (no-)?parentheses        { yylval = yystr(); BEGIN(eol); return PARENTHESES; }
 (no-)?redefinition       { yylval = yystr(); BEGIN(eol); return REDEFINITION; }
 (no-)?strict-typing      { yylval = yystr(); BEGIN(eol); return STRICT_TYPING; }
 (no-)?terminator         { yylval = yystr(); BEGIN(eol); return TERMINATOR; }
 (no-)?truncate           { yylval = yystr(); BEGIN(eol); return TRUNCATE; }
 (no-)?unreachable        { yylval = yystr(); BEGIN(eol); return UNREACHABLE; }
} 

<std>{
 {EQ}                     { return EQ; }
 cobol85                  { yylval = yystr(); BEGIN(eol); return COBOL85; }
 xopen                    { yylval = yystr(); BEGIN(eol); return XOPEN; }
 cobol2002                { yylval = yystr(); BEGIN(eol); return COBOL2002; }
 cobol2014                { yylval = yystr(); BEGIN(eol); return COBOL2014; }
 ibm-strict               { yylval = yystr(); BEGIN(eol); return IBM_STRICT; }
 ibm                      { yylval = yystr(); BEGIN(eol); return IBM; }
 mvs-strict               { yylval = yystr(); BEGIN(eol); return MVS_STRICT; }
 mvs                      { yylval = yystr(); BEGIN(eol); return MVS; }
 mf-strict                { yylval = yystr(); BEGIN(eol); return MF_STRICT; }
 mf                       { yylval = yystr(); BEGIN(eol); return MF; }
 bs2000-strict            { yylval = yystr(); BEGIN(eol); return BS2000_STRICT; }
 bs2000                   { yylval = yystr(); BEGIN(eol); return BS2000; }
 acu-strict               { yylval = yystr(); BEGIN(eol); return ACU_STRICT; }
 acu                      { yylval = yystr(); BEGIN(eol); return ACU; }
 rm-strict                { yylval = yystr(); BEGIN(eol); return RM_STRICT; }
 rm                       { yylval = yystr(); BEGIN(eol); return RM; }
}
 
<fsign>{
 {EQ}                     { return EQ; }
 ASCII                    { yylval = yystr(); BEGIN(eol); return ASCII; }
 EBCDIC                   { yylval = yystr(); BEGIN(eol); return EBCDIC; }
}

<ffold>{
 {EQ}                     { return EQ; }
 LOWER                    { yylval = yystr(); BEGIN(eol); return LOWER; }
 UPPER                    { yylval = yystr(); BEGIN(eol); return UPPER; }
}
 
<eq>{EQ}/{INTEGER} {
    return EQ;
}
<eq>{INTEGER} {
    yylval = yystr();
    BEGIN(eol);
    return INTEGER;
}
	
<eq>{EQ} {
    BEGIN(value);
    return EQ;
}	

<value>{VALUE}            { yylval = yystr(); BEGIN(eol); return VALUE; }

<eq,eol>{EOL} {
    BEGIN(INITIAL);
}

\n {
    BEGIN(INITIAL);
}

" " {}
%%
