        identification division.
        program-id. testprog2.

        data division.
        working-storage section.

        01 tab.
           05 tab-row1 occurs 2.
              07 tab-row2 occurs 2.
                 10 tab-data   pic x.
                    88 tab-is-c value 'C'.
                    88 tab-is-any-d value 'd', 'D'.

        procedure division.

        move 'ABCD' to tab
        display "Testprog - 2" end-display
        call "testprog3" end-call

        goback.
