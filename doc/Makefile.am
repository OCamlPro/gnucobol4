#
# Makefile gnu-cobol-cpp/doc
#
# Copyright (C) 2003-2012, 2015-2017 Free Software Foundation, Inc.
# Written by Keisuke Nishida, Roger While, Simon Sobisch
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

COBCINFOSH = . $(top_builddir)/tests/atconfig && . $(top_builddir)/tests/atlocal && \
	$(top_srcdir)/doc/cobcinfo.sh
info_TEXINFOS = gnucobolcpp.texi
gnucobolcpp_TEXINFOS = cbhelp.tex cbrese.tex cbintr.tex cbsyst.tex cbmnem.tex cbconf.tex

cbhelp.tex: $(top_srcdir)/cobc/cobc.cpp $(top_srcdir)/configure.ac \
	$(top_srcdir)/cobc/flag.def $(top_srcdir)/cobc/warning.def \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@

cbconf.tex: $(top_srcdir)/config/default.conf \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@

cbrese.tex cbintr.tex cbsyst.tex cbmnem.tex: \
	$(top_srcdir)/cobc/cobc.cpp $(top_srcdir)/cobc/reserved.cpp \
	$(top_srcdir)/doc/cobcinfo.sh
	$(COBCINFOSH) $@

EXTRA_DIST = gnucobolcpp.pdf cobcinfo.sh

TEXINFO_TEX = texinfo.texi
AM_MAKEINFOHTMLFLAGS = --no-headers --no-split
CLEANFILES = *.aux  *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr *.vrs
TEXI2DVI = texi2dvi -I $(srcdir)
