      * watchpoints.cpy
      *

        watchpoint-action section.
            perform extract-option-from-command

            evaluate tmp-unstring-buffer
                when 'del'
                    perform delete-all-watchpoints
                when other
                    perform add-watchpoint
            end-evaluate

            continue.

      ************************************************************
        update-watchpoints section.

            perform varying tmp-number from 1 by 1
                    until   tmp-number > wp-count
                if wp-var-name(tmp-number) not = spaces
                and wp-module(tmp-number) = cobol-src-name
                    move interface-block to wp-interface-block
                    move wp-var-name(tmp-number) to tmp-wp-var-name
                    move low-value to tmp-wp-var-name(50:1)

                    call ANIDATA using "G" wp-interface-block
                        tmp-wp-var-name end-call

                    move wp-value(tmp-number)
                        to last-wp-value(tmp-number)
                    move wp-dtf-value to wp-value(tmp-number)
                end-if
            end-perform

      *>> IF ENABLE-LOGGING DEFINED
      *      perform log-watchpoints
      *>> END-IF

            continue.

      ************************************************************
        check-watchpoints section.
            perform update-watchpoints
            move 0 to watchpoint-changed-flag

            perform varying tmp-number from 1 by 1
                    until   tmp-number > wp-count

                if wp-module(tmp-number) = cobol-src-name
                and wp-value(tmp-number) not = last-wp-value(tmp-number)

                    move spaces to status-line
                    string 'Watchpoint value of '
                        function trim(wp-var-name(tmp-number), trailing)
                        ' changed from '
                        function trim(last-wp-value(tmp-number))
                        ' to '
                        wp-value(tmp-number)
                        into status-line
                    end-string
                    display status-line end-display
                    move 1 to watchpoint-changed-flag

                    exit perform
                end-if
            end-perform


            continue.

      ************************************************************
        add-watchpoint section.

            move tmp-unstring-buffer(1:50) to tmp-wp-var-name
            move spaces to status-line

            if wp-count < wp-max
                move tmp-wp-var-name to wp-var-name(tmp-number)
                move cobol-src-name to wp-module(tmp-number)

                move interface-block to wp-interface-block
                move low-value to tmp-wp-var-name(50:1)

                call ANIDATA using "G" wp-interface-block
                    tmp-wp-var-name end-call

                move wp-dtf-value to wp-value(tmp-number),
                    last-wp-value(tmp-number)

                string 'Added watchpoint ' wp-var-name(tmp-number)
                    into status-line end-string
            else
                move wp-max to tmp-number
                move space to status-line
                string 'No more watchpoints possible. '
                       '(limit: ' tmp-number ')'
                       delimited by size
                       into status-line
                end-string
            end-if

            display status-line end-display
      *      perform log-watchpoints

            continue.

      ************************************************************
        delete-all-watchpoints section.
            move 'Deleted all watchpoints.' to status-line
            display status-line end-display

            initialize watchpoint-lst, wp-count
            continue.

      ************************************************************
      *>> IF ENABLE-LOGGING DEFINED
      *  log-watchpoints section.
      *     perform varying tmp-number from 1 by 1 until tmp-number > 10
      *          string
      *              function trim(wp-var-name(tmp-number))
      *              ';  '
      *              function trim(wp-module(tmp-number))
      *              ' : '
      *              function trim(wp-value(tmp-number))
      *              ' : '
      *              function trim(last-wp-value(tmp-number))
      *              into tmp-log-line
      *          end-string
      *          perform log-msg
      *     end-perform

      *     continue.
      *>> END-IF
