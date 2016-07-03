      * helpers.cpy
      * kleine Helferlein, die das Leben erleichtern ;)

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

            call static 'string_contains' using string-contains-str1
                string-contains-str2 end-call

            move return-code to string-contains-flag.

      *      move 0 to string-contains-flag.
      *      move function length(function trim (string-contains-str2,
      *          trailing)) to tmp-number-2.
      *      compute tmp-number = function length(function
      *          trim(string-contains-str1, trailing))
      *          - tmp-number-2 end-compute
      *
      *>> IF ENABLE-LOGGING DEFINED
      *      string string-contains-str1 delimited by size
      *             "   " delimited by size
      *             string-contains-str2 delimited by size
      *             "   " delimited by size
      *             tmp-number delimited by size
      *             "   " delimited by size
      *             tmp-number-2 delimited by size
      *             into tmp-log-line
      *      end-string
      *      perform log-msg.
      *>> END-IF
      *
      *      if tmp-number = 0 and string-contains-str1 =
      *          string-contains-str2
      *
      *          move 1 to string-contains-flag
      *      else if tmp-number > 0
      *          perform varying tmp-counter from 1 by 1
      *              until tmp-counter = tmp-number
      *
      *              if string-contains-str1(tmp-counter:tmp-number-2) =
      *                  string-contains-str2
      *
      *                  move 1 to string-contains-flag
      *                  exit perform
      *              end-if
      *
      *          end-perform
      *      end-if

            continue.
