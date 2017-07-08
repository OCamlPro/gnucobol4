      *>  Copyright (C) 2017  Free Software Foundation, Inc.
      *>  Written by Brian Tiffin, from information in Regina REXX.
      *>
      *>  This file is part of GnuCOBOL.
      *>
      *>  The GnuCOBOL compiler is free software: you can redistribute
      *>  it and/or modify it under the terms of the GNU General Public
      *>  License as published by the Free Software Foundation, either
      *>  version 3 of the License, or (at your option) any later
      *>  version.
      *>
      *>  GnuCOBOL is distributed in the hope that it will be useful,
      *>  but WITHOUT ANY WARRANTY; without even the implied warranty of
      *>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      *>  GNU General Public License for more details.
      *>
      *>  You should have received a copy of the GNU General Public
      *>  License along with GnuCOBOL.
      *>  If not, see <http://www.gnu.org/licenses/>.

      *> The APIRET return value for Py_RunString
       77 SCRIPT-RETURN-CODE USAGE BINARY-C-LONG  EXTERNAL
                                                AS "SCRIPT_RETURN_CODE".

       01 PYTHON-OK                    CONSTANT AS 0.

      *> Positive values, script not evaluated due to problem
       01 PYAPI-INTERNAL               CONSTANT AS 1.

      *> Special values
       01 PYAPI-FINALIZE               CONSTANT AS 0.
       01 PYAPI-DEBUG                  CONSTANT AS 1.
       01 PYAPI-TRACE                  CONSTANT AS 2.
       01 PYAPI-REPORT                 CONSTANT AS 4.
