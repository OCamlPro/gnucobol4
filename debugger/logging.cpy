      * logging.cpy
      * code parts for internal logging

      ***************************************************************
        log-msg section.
            if cob-anim-logging = 'Y' and
               (cob-logging-module = 'all' or cobol-src-name)

                move spaces to log-line
                move tmp-log-line to log-line
                move spaces to tmp-log-line

                write log-line
            end-if

            move spaces to tmp-log-line

            continue.

      ***************************************************************
        log-interface-block section.

            if cob-anim-logging = 'Y' and
               (cob-logging-module = 'all' or cobol-src-name)

                move "**************************" to tmp-log-line
                perform log-msg

                string "anim-state: " delimited by size
                    anim-state delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "cobol-src-name: " delimited by size
                    cobol-src-name delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "first-stmt-if: " delimited by size
                    first-stmt-if delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "current-line-if: " delimited by size
                    current-line-if delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "active-line-if: " delimited by size
                    active-line-if delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "no-code-lines-if: " delimited by size
                    no-code-lines-if delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "dtf-line: " delimited by size
                    dtf-line delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "dtf-usage: " delimited by size
                    dtf-usage delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "dtf-length: " delimited by size
                    dtf-length delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "dtf-value: " delimited by size
                    dtf-value delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                string "anim-stmt-type: " delimited by size
                    anim-stmt-type delimited by size
                    into tmp-log-line
                end-string
                perform log-msg

                move "**************************" to tmp-log-line
                perform log-msg

            end-if

            continue.
