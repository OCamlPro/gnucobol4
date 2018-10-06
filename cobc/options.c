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

#include "config.h"
#include "defaults.h"

#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <limits.h>
#include <search.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobc.h"

#include "options.h" 

static struct conf_opt_t conf_opts[] = {
  { false, 1, FREE },
  { false, 1, FIXED },

  { false, 1, STD },

  { false, 1, DEBUG },
  { false, 1, G },
  { false, 1, O },
  { false, 1, O2 },
  { false, 1, OS },
  { false, 1, FTRACE },
  { false, 1, FTRACEALL },
  { false, 1, FDEBUGGING_LINE },
  { false, 1, FSOURCE_LOCATION },
  { false, 1, FIMPLICIT_INIT },
  { false, 1, FSTACK_CHECK },
  { false, 1, FNOTRUNC },

  // Miscellaneous options
  { false, 1, EXT },        //  extension (no =)
  { false, 1, FMFCOMMENT },
  { false, 1, ACUCOMMENT },
  { false, 1, FSIGN },      //  = ASCII
  { false, 1, FFOLD_COPY }, //  = UPPER
  { false, 1, SAVE_TEMPS }, //  [= directory]

  //Listing options
  { false, 2, TLINES },     //  = lines
  { false, 1, FTSYMBOLS },
  { false, 1, FNO_THEADER },
  { false, 1, FNO_TMESSAGES },
  { false, 1, FNO_TSOURCE },
  { false, 1, XREF },
}, *conf_opts_end = conf_opts + COUNT_OF(conf_opts);

static struct conf_opt_t *extras,
  *extras_next, *extras_end;

static struct warn_opt_t warn_opts[] = {
  { false, false, ALL },
  { false, false, ARCHAIC },
  { false, false, CALL_PARAMS },
  { false, false, COLUMN_OVERFLOW },
  { false, false, CONSTANT },
  { false, false, IMPLICIT_DEFINE },
  { false, false, LINKAGE },
  { false, false, OBSOLETE },
  { false, false, PARENTHESES },
  { false, false, REDEFINITION },
  { false, false, STRICT_TYPING },
  { false, false, TERMINATOR },
  { false, false, TRUNCATE },
  { false, false, UNREACHABLE },
}, *warn_opts_end = warn_opts + COUNT_OF(warn_opts);
;

static inline bool
accepts_multiple( enum yytokentype type ) {
  switch(type) {
  case I:
  case LINK:
  case LIB:
  case D:
    return true;
  default:
    break;
  }
  return false;
}

// match option by type
static int
conf_opts_cmp( const void *K, const void *E ) {
  const struct conf_opt_t *k = K, *e = E;
  return k->type == e->type? 0 : 1;
}

// match configured/unconfigured option
static int
conf_opts_configured_cmp( const void *K, const void *E ) {
  const struct conf_opt_t *k = K, *e = E;
  return k->configured == e->configured? 0 : 1;
}

static struct conf_opt_t *
option_alloc( enum yytokentype type ) {
  size_t n = extras_end - extras;
  
  if( n == 0 ) {
    warnx("initial %d", type);
    n = 16;
    if( (extras = calloc(n, sizeof(*extras))) == NULL ) {
      err(EXIT_FAILURE, "%s:%d", __func__, __LINE__);
    }
    extras_end = extras + n;
    extras_next = extras;
  }

  if( extras_next == extras_end ) {
    warnx("double %d", type);
    n == 2 * (extras_end - extras);
    if( (extras = realloc(extras, n * sizeof(*extras))) == NULL ) {
      err(EXIT_FAILURE, "%s:%d", __func__, __LINE__);
    }
    extras_end = extras + n;
    extras_next = extras + n/2;
  }
  
  assert(extras <= extras_next && extras_next < extras_end);

  warnx("returning %d", type);

  *extras_next = (struct conf_opt_t) { .type = type, .ndash = 1 };
  struct conf_opt_t *p = extras_next++;
  return p;
}

static struct conf_opt_t *
option_find( enum yytokentype type ) {
  assert(I <= type && type <= XREF);
  struct conf_opt_t *p, key = { .type = type };
  size_t n = COUNT_OF(conf_opts);
  
  return lfind( &key, conf_opts, &n, sizeof(key), conf_opts_cmp);
}

static bool
option_any_set( enum yytokentype type, const char name[], const char value[] ) {
  char *s = index(name, 0x20);
  if( s ) {
    warnx("found space at position %zu in '%s'", s - name, name);
  }

  struct conf_opt_t *p = accepts_multiple(type)?
    option_alloc(type) : option_find(type);

  if( p == NULL ) return false;
    
  if( sizeof(p->name) - 1 < strlen(name) ) {
    warnx("E2BIG: exceeds %zu bytes: '%s'", sizeof(p->name), name);
    return false;
  }

  strcpy(p->name, name);
  free((void*)name);
  
  if( value == NULL ) { // if no value, we're done
    p->value[0] = '\0';
    return p->configured = true;
  }
  
  if( sizeof(p->value) - 1 < strlen(value) ) {
    warnx("E2BIG: exceeds %zu bytes: '%s'", sizeof(p->value), value);
    return false;
  }
  
  strcpy(p->value, value);
  free((void*)value);
  return p->configured = true;
}

bool
option_set( enum yytokentype type, const char name[] ) {
  return option_any_set(type, name, NULL);
}

bool
option_arg_set( enum yytokentype type, const char name[], const char value[] ) {
  return option_any_set(type, name, value);
}

static int
warn_opts_cmp( const void *K, const void *E ) {
  const struct warn_opt_t *k = K, *e = E;
  return k->type == e->type? 0 : 1;
}

// find next configured/unconfigured warning
static int
warn_opts_configured_cmp( const void *K, const void *E ) {
  const struct warn_opt_t *k = K, *e = E;
  return k->configured == e->configured? 0 : 1;
}

bool
warning_set( enum yytokentype type, const char name[] ) {
  struct warn_opt_t *p, key = { .type = type };
  size_t n = COUNT_OF(warn_opts);

  if( (p = lfind( &key, warn_opts, &n, sizeof(key), warn_opts_cmp)) == NULL ) {
    assert(ALL <= type && type <= UNREACHABLE);
    return false;
  }

  assert(strlen(name) < sizeof(p->name) - 1);
  strcpy(p->name, name);
  p->yn = strncmp(name, "no-", 3) == 0;
  free((void*)name);
    
  return p->configured = true;
}

const struct conf_opt_t *
option_next( const struct conf_opt_t *opt ) {
  static const struct conf_opt_t key = { .configured = true };

  if( opt == NULL ) { // first time
    opt = conf_opts;
    size_t n = conf_opts_end - opt;
    return lfind(&key, opt, &n, sizeof(key), conf_opts_configured_cmp);
  }
  assert(opt);

  if( ++opt == conf_opts_end ) return NULL;

  assert(conf_opts <= opt && opt < conf_opts_end);

  size_t n = conf_opts_end - opt;
  return lfind(&key, opt, &n, sizeof(key), conf_opts_configured_cmp);
}

const struct conf_opt_t *
options_extra( size_t *pnextra ) {
  assert(pnextra != NULL );
  
  *pnextra = extras_next - extras;
  return extras;
}

const struct warn_opt_t *
warning_next( const struct warn_opt_t *opt ) {
  opt = opt == NULL ? warn_opts : opt + 1;
  if( opt == warn_opts_end ) return NULL;

  assert(warn_opts <= opt && opt < warn_opts_end);

  struct warn_opt_t key = { .configured = true };
  size_t n = warn_opts_end - opt;
  
  return lfind(&key, opt, &n, sizeof(key), warn_opts_configured_cmp);
}


