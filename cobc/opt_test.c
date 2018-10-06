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

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <mqueue.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <unistd.h>

#include "options.h"

static void
usage( const char name[] ) {
  fprintf( stderr, "%s ", name );
  exit( EXIT_FAILURE );
}

int
main(int argc, char *argv[])
{
  int opt;

  while ((opt = getopt(argc, argv, "h")) != -1) {
    switch (opt) {
    case 'h':
      break;
    default:
      usage(basename(argv[0]));
      abort(); // not reached
    }
  }

  if( argc > 1 ) {
    int fd = open(argv[1], O_RDONLY);
    if( fd == -1 ) {
      err(EXIT_FAILURE, "could not open '%s'", argv[1]);
    }
    if( -1 == dup2(fd, STDIN_FILENO) ) {
      err(EXIT_FAILURE, "could not set standard input");
    }
  }
  
  yyparse();

  
  return EXIT_SUCCESS;
}
