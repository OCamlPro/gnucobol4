      * helpers.cpy
      * small helper functions

      ***************************************************************
        check-for-stop-run section.
       *> FIXME: add callback function for performance issues instead
            if active-line-if >= first-stmt-if
                move active-line-if to tmp-linenumber-bin

       >> IF ENABLE-LOGGING DEFINED
                if cob-anim-logging = 'Y'
                    string "check for stop run. " delimited by size
                        active-line-if delimited by size
                        ", " delimited by size
                        cobol-src-name delimited by size
                        ", " delimited by size
                        GETLINE delimited by size
                        into tmp-log-line
                    end-string
                    perform log-msg
                end-if
       >> END-IF

                call GETLINE using tmp-linenumber-bin
                tmp-source-line-buffer end-call

                move function upper-case(tmp-source-line-buffer)
                    to string-contains-str1
                move "STOP" to string-contains-str2

                perform string-contains

                if string-contains-flag = 1
                    move " RUN" to string-contains-str2

                    perform string-contains

                    if string-contains-flag = 1
                        perform flush-breakpoints
                    end-if
                end-if
            end-if

            continue.

      ***************************************************************
        check-for-perform-call section.
            move active-line-if to tmp-linenumber-bin

       >> IF ENABLE-LOGGING DEFINED
            if cob-anim-logging = 'Y'
                string "check for perform or call statement."
                    delimited by size into tmp-log-line
                end-string
                perform log-msg
            end-if
       >> END-IF

            call GETLINE using tmp-linenumber-bin
            tmp-source-line-buffer end-call

            move function upper-case(tmp-source-line-buffer)
                to string-contains-str1
            move "CALL" to string-contains-str2
            perform string-contains

            if string-contains-flag not = 1
                move "PERFORM" to string-contains-str2
                perform string-contains
            end-if

            continue.

      ***************************************************************
        string-contains section.
            move 0 to string-contains-flag.
            inspect string-contains-str1 tallying string-contains-flag
               for all function trim (string-contains-str2)

            continue.
