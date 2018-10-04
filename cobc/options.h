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

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include <stdbool.h>
#include <limits.h>
#include "opt_parse.tab.h"

#define COUNT_OF(x) (sizeof(x)/sizeof(x[0]))

struct conf_opt_t {
  bool configured; // is present in configuration file
  int  ndash;      // 1 or 2 dashes
  enum yytokentype type; // option type token
  char name[32]; 
  char value[PATH_MAX];
};

struct warn_opt_t {
  bool configured;       // is present in configuration file
  bool yn;               // on or off
  enum yytokentype type; // warning type token
  char name[32]; 
};

bool option_set( enum yytokentype type, const char name[] );
bool option_arg_set( enum yytokentype type, const char name[], const char value[] );
bool warning_set( enum yytokentype type,  const char name[] );

#endif
