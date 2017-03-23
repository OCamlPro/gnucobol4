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

      *> The APIRET return value from RexxStart
       77 SCRIPT-RETURN-CODE USAGE BINARY-C-LONG  EXTERNAL
                                                AS "SCRIPT_RETURN_CODE".

       01 REXX-API-OK                  CONSTANT AS 0.

      *> Positive values, script not evaluated due to problem
       01 REXX-API-DEFINED             CONSTANT AS 10.
       01 REXX-API-NOMEM               CONSTANT AS 20.
       01 REXX-API-NOTREG              CONSTANT AS 30.
       01 REXX-API-MODNOTFND           CONSTANT AS 40.
       01 REXX-API-ENTNOTFND           CONSTANT AS 50.
       01 REXX-API-NOTINIT             CONSTANT AS 60.
       01 REXX-API-BADTYPE             CONSTANT AS 70.
       01 REXX-API-NOEMEM              CONSTANT AS 1002.

      *> Interpreter error status values
       01 REXX-ERR-PROG-UNREADABLE     CONSTANT AS -3.
       01 REXX-ERR-PROG-INTERRUPT      CONSTANT AS -4.
       01 REXX-ERR-STORAGE-EXHAUSTED   CONSTANT AS -5.
       01 REXX-ERR-UNMATCHED-QUOTE     CONSTANT AS -6.
       01 REXX-ERR-WHEN-EXPECTED       CONSTANT AS -7.
       01 REXX-ERR-THEN-UNEXPECTED     CONSTANT AS -8.
       01 REXX-ERR-WHEN-UNEXPECTED     CONSTANT AS -9.
       01 REXX-ERR-UNMATCHED-END       CONSTANT AS -10.
       01 REXX-ERR-FULL-CTRL-STACK     CONSTANT AS -11.
       01 REXX-ERR-TOO-LONG-LINE       CONSTANT AS -12.
       01 REXX-ERR-INVALID-CHAR        CONSTANT AS -13.
       01 REXX-ERR-INCOMPLETE-STRUCT   CONSTANT AS -14.
       01 REXX-ERR-INVALID-HEX-CONST   CONSTANT AS -15.
       01 REXX-ERR-UNEXISTENT-LABEL    CONSTANT AS -16.
       01 REXX-ERR-UNEXPECTED-PROC     CONSTANT AS -17.
       01 REXX-ERR-THEN-EXPECTED       CONSTANT AS -18.
       01 REXX-ERR-STRING-EXPECTED     CONSTANT AS -19.
       01 REXX-ERR-SYMBOL-EXPECTED     CONSTANT AS -20.
       01 REXX-ERR-EXTRA-DATA          CONSTANT AS -21.
       01 REXX-ERR-INVALID-TRACE       CONSTANT AS -24.
       01 REXX-ERR-INV-SUBKEYWORD      CONSTANT AS -25.
       01 REXX-ERR-INVALID-INTEGER     CONSTANT AS -26.
       01 REXX-ERR-INVALID-DO-SYNTAX   CONSTANT AS -27.
       01 REXX-ERR-INVALID-LEAVE       CONSTANT AS -28.
       01 REXX-ERR-ENVIRON-TOO-LONG    CONSTANT AS -29.
       01 REXX-ERR-TOO-LONG-STRING     CONSTANT AS -30.
       01 REXX-ERR-INVALID-START       CONSTANT AS -31.
       01 REXX-ERR-INVALID-STEM        CONSTANT AS -32.
       01 REXX-ERR-INVALID-RESULT      CONSTANT AS -33.
       01 REXX-ERR-UNLOGICAL-VALUE     CONSTANT AS -34.
       01 REXX-ERR-INVALID-EXPRESSION  CONSTANT AS -35.
       01 REXX-ERR-UNMATCHED-PARAN     CONSTANT AS -36.
       01 REXX-ERR-UNEXPECTED-PARAN    CONSTANT AS -37.
       01 REXX-ERR-INVALID-TEMPLATE    CONSTANT AS -38.
       01 REXX-ERR-STACK-OVERFLOW      CONSTANT AS -39.
       01 REXX-ERR-INCORRECT-CALL      CONSTANT AS -40.
       01 REXX-ERR-BAD-ARITHMETIC      CONSTANT AS -41.
       01 REXX-ERR-ARITH-OVERFLOW      CONSTANT AS -42.
       01 REXX-ERR-ROUTINE-NOT-FOUND   CONSTANT AS -43.
       01 REXX-ERR-NO-DATA-RETURNED    CONSTANT AS -44.
       01 REXX-ERR-DATA-NOT-SPEC       CONSTANT AS -45.
       01 REXX-ERR-SYSTEM-FAILURE      CONSTANT AS -48.
       01 REXX-ERR-INTERPRETER-FAILURE CONSTANT AS -49.
       01 REXX-ERR-RESERVED-SYMBOL     CONSTANT AS -50.
       01 REXX-ERR-UNQUOTED-FUNC-STOP  CONSTANT AS -51.
       01 REXX-ERR-INVALID-OPTION      CONSTANT AS -53.
       01 REXX-ERR-INVALID-STEM-OPTION CONSTANT AS -54.
       01 REXX-ERR-CANT-REWIND         CONSTANT AS -60.
       01 REXX-ERR-IMPROPER-SEEK       CONSTANT AS -61.
       01 REXX-ERR-YACC-SYNTAX         CONSTANT AS -64.
       01 REXX-ERR-NON-ANSI-FEATURE    CONSTANT AS -90.
       01 REXX-ERR-STREAM-COMMAND      CONSTANT AS -93.
       01 REXX-ERR-EXTERNAL-QUEUE      CONSTANT AS -94.
       01 REXX-ERR-RESTRICTED          CONSTANT AS -95.
       01 REXX-ERR-UNKNOWN-FS-ERROR    CONSTANT AS -100.
