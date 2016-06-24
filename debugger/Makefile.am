#
# Makefile gnucobol/gc-debugger
#
# Copyright (C) 2003,2004,2005,2006,2007 Keisuke Nishida
# Copyright (C) 2008-2012 Roger While
#
# This file is part of GNU Cobol.
#
# The GNU Cobol compiler is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# GNU Cobol is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Cobol.  If not, see <http://www.gnu.org/licenses/>.

lib_LTLIBRARIES = gc-debugger.la
gc_debugger_la_SOURCES = gc-debugger.c gc-debugger.c.h gc-debugger.c.l.h

gc_debugger_la_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir)
gc_debugger_la_LIBADD = $(top_builddir)/libcob/libcob.la
gc_debugger_la_LDFLAGS = $(COB_FIX_LIBTOOL) -version-info 4:0:0 -no-undefined -module
AM_LDFLAGS = $(COB_FIX_LIB)

pkgincludedir = $(includedir)/debugger
pkginclude_HEADERS = gc-debugger.c.h gc-debugger.c.l.h

