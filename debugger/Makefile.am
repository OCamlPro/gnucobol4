#
# Makefile gnucobol/gc-debugger
#
# Copyright (C) 2014-2016 Free Software Foundation
#
# This file is part of GnuCOBOL.
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

animdir = @COB_LIBRARY_PATH@
anim_DATA = gc-debugger.$(COB_MODULE_EXT)

EXTRA_DIST = $(debugger_sources)
CLEANFILES = $(extras_DATA)

debugger_sources = \
	gc-debugger.cob \
	screen.cpy breakpoints.cpy \
	view-variable.cpy \
	logging.cpy \
	helpers.cpy \
	fulltext-search.cpy \
	watchpoints.cpy

gc-debugger.$(COB_MODULE_EXT) : $(debugger_sources)
	. $(top_builddir)/tests/atconfig; . $(top_builddir)/tests/atlocal; \
		cobc -m -Wall -O -o debugger.$(COB_MODULE_EXT) -I $(srcdir) $(srcdir)/gc-debugger.cob
