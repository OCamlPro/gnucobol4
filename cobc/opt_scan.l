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
 I                        { yylval = yytext; BEGIN(value); return I; }
 L                        { yylval = yytext; BEGIN(value); return LIB; }
 l                        { yylval = yytext; BEGIN(value); return LINK; }
 D                        { yylval = yytext; BEGIN(value); return D; }

 W                        { yylval = yytext; BEGIN(warn); return WARNING; }
 
 free                     { yylval = yytext; BEGIN(eol);   return FREE; }
 fixed                    { yylval = yytext; BEGIN(eol);   return FIXED; }

 std/{EQ}                 { yylval = yytext; BEGIN(std);    return STD; }

 debug                    { yylval = yytext; BEGIN(eol);   return DEBUG; }
 g                        { yylval = yytext; BEGIN(eol);   return G; }
 O                        { yylval = yytext; BEGIN(eol);   return O; }
 O2                       { yylval = yytext; BEGIN(eol);   return O2; }
 Os                       { yylval = yytext; BEGIN(eol);   return OS; }
 ftrace                   { yylval = yytext; BEGIN(eol);   return FTRACE; }
 ftraceall                { yylval = yytext; BEGIN(eol);   return FTRACEALL; }
 fdebugging-line          { yylval = yytext; BEGIN(eol);   return FDEBUGGING_LINE; }
 fsource-location         { yylval = yytext; BEGIN(eol);   return FSOURCE_LOCATION; }
 fimplicit-init           { yylval = yytext; BEGIN(eol);   return FIMPLICIT_INIT; }
 fstack-check             { yylval = yytext; BEGIN(eol);   return FSTACK_CHECK; }
 fnotrunc                 { yylval = yytext; BEGIN(eol);   return FNOTRUNC; }
 
 ext                      { yylval = yytext; BEGIN(value); return EXT; }
 fmfcomment               { yylval = yytext; BEGIN(eol);   return FMFCOMMENT; }
 acucomment               { yylval = yytext; BEGIN(eol);   return ACUCOMMENT; }
 fsign                    { yylval = yytext; BEGIN(fsign); return FSIGN; }
 ffold-copy               { yylval = yytext; BEGIN(ffold); return FFOLD_COPY; }
 save-temps               { yylval = yytext; BEGIN(eq);    return SAVE_TEMPS; }

 tlines                   { yylval = yytext; BEGIN(eq);    return TLINES; }
 ftsymbols                { yylval = yytext; BEGIN(eol);   return FTSYMBOLS; }
 fno-theader              { yylval = yytext; BEGIN(eol);   return FNO_THEADER; }
 fno-tmessages            { yylval = yytext; BEGIN(eol);   return FNO_TMESSAGES; }
 fno-tsource              { yylval = yytext; BEGIN(eol);   return FNO_TSOURCE; }
 Xref                     { yylval = yytext; BEGIN(eol);   return XREF; }
}

<warn>{
 (no-)?all                { yylval = yytext; BEGIN(eol); return ALL; }
 (no-)?archaic            { yylval = yytext; BEGIN(eol); return ARCHAIC; }
 (no-)?call-params        { yylval = yytext; BEGIN(eol); return CALL_PARAMS; }
 (no-)?column-overflow    { yylval = yytext; BEGIN(eol); return COLUMN_OVERFLOW; }
 (no-)?constant           { yylval = yytext; BEGIN(eol); return CONSTANT; }
 (no-)?implicit-define    { yylval = yytext; BEGIN(eol); return IMPLICIT_DEFINE; }
 (no-)?linkage            { yylval = yytext; BEGIN(eol); return LINKAGE; }
 (no-)?obsolete           { yylval = yytext; BEGIN(eol); return OBSOLETE; }
 (no-)?parentheses        { yylval = yytext; BEGIN(eol); return PARENTHESES; }
 (no-)?redefinition       { yylval = yytext; BEGIN(eol); return REDEFINITION; }
 (no-)?strict-typing      { yylval = yytext; BEGIN(eol); return STRICT_TYPING; }
 (no-)?terminator         { yylval = yytext; BEGIN(eol); return TERMINATOR; }
 (no-)?truncate           { yylval = yytext; BEGIN(eol); return TRUNCATE; }
 (no-)?unreachable        { yylval = yytext; BEGIN(eol); return UNREACHABLE; }
} 

<std>{
 {EQ}                     { yylval = yytext; return EQ; }
 cobol85                  { yylval = yytext; BEGIN(eol); return COBOL85; }
 xopen                    { yylval = yytext; BEGIN(eol); return XOPEN; }
 cobol2002                { yylval = yytext; BEGIN(eol); return COBOL2002; }
 cobol2014                { yylval = yytext; BEGIN(eol); return COBOL2014; }
 ibm-strict               { yylval = yytext; BEGIN(eol); return IBM_STRICT; }
 ibm                      { yylval = yytext; BEGIN(eol); return IBM; }
 mvs-strict               { yylval = yytext; BEGIN(eol); return MVS_STRICT; }
 mvs                      { yylval = yytext; BEGIN(eol); return MVS; }
 mf-strict                { yylval = yytext; BEGIN(eol); return MF_STRICT; }
 mf                       { yylval = yytext; BEGIN(eol); return MF; }
 bs2000-strict            { yylval = yytext; BEGIN(eol); return BS2000_STRICT; }
 bs2000                   { yylval = yytext; BEGIN(eol); return BS2000; }
 acu-strict               { yylval = yytext; BEGIN(eol); return ACU_STRICT; }
 acu                      { yylval = yytext; BEGIN(eol); return ACU; }
 rm-strict                { yylval = yytext; BEGIN(eol); return RM_STRICT; }
 rm                       { yylval = yytext; BEGIN(eol); return RM; }
}
 
<fsign>{
 {EQ}                     { yylval = yytext; return EQ; }
 ASCII                    { yylval = yytext; BEGIN(eol); return ASCII; }
 EBCDIC                   { yylval = yytext; BEGIN(eol); return EBCDIC; }
}

<ffold>{
 {EQ}                     { yylval = yytext; return EQ; }
 LOWER                    { yylval = yytext; BEGIN(eol); return LOWER; }
 UPPER                    { yylval = yytext; BEGIN(eol); return UPPER; }
}
 
<eq>{EQ}/{INTEGER} {
    return EQ;
}
<eq>{INTEGER} {
    yylval = yytext;
    BEGIN(eol);
    return INTEGER;
}
	
<eq>{EQ} {
    BEGIN(value);
    return EQ;
}	

<value>{VALUE}            { yylval = yytext; BEGIN(eol); return VALUE; }

<eq,eol>{EOL} {
    BEGIN(INITIAL);
}

\n {
    BEGIN(INITIAL);
}

" " {}
%%
    
