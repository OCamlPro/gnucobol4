      * breakpoints.cpy
      * alle explizit zu Breakpoints gehörenden Codeteile

      ***************************************************************
      *** ----------------- read in breakpoints from list file
        read-in-breakpoints section.
            move spaces to bp-path.
            accept bp-path from environment "COB_BREAKPOINT_FILE"
            end-accept

            if bp-path = spaces
                move "xanim_breakpoint.lst" to bp-path
            end-if

            move 0 to bp-amount.

            open input bp-file.
            if bp-file-status not = 0
                close bp-file
                open output bp-file
                close bp-file
            else
                read bp-file end-read
                perform varying bp-line-counter
                     from 1 by 1 until bp-line-counter = 1000
                     or bp-file-status not = 0

                    move 1 to tmp-line-position
                    unstring bp-line delimited by ';'
                        into tmp-bp-src-name
      *                  into bp-src-name(bp-line-counter)
                        with pointer tmp-line-position
                    end-unstring
                    unstring bp-line delimited by ';'
                        into tmp-bp-src-line
      *                  into bp-src-line(bp-line-counter)
                        with pointer tmp-line-position
                    end-unstring

                    if tmp-bp-src-name not = spaces
                        add 1 to bp-amount end-add
                        move tmp-bp-src-name
                            to bp-src-name(bp-amount)
                        move tmp-bp-src-line
                            to bp-src-line(bp-amount)
                    end-if

                    read bp-file end-read
                end-perform

                if bp-amount = 0
      *          or bp-src-name(1) = spaces
       >> IF ENABLE-LOGGING DEFINED
                    if cob-anim-logging = 'Y'
                        move "Empty bp list" to tmp-log-line
                        perform log-msg
                    end-if
       >> END-IF
                    move 0 to bp-next-line
                    move 0 to bp-amount
                else
                    compute bp-next-line = bp-amount + 1
                    end-compute
      *              move bp-line-counter to bp-amount
                    sort bp-line-struct ascending bp-src-name
                        bp-src-line
                end-if

                close bp-file

      *          display bp-src-name(1) "  " bp-src-line(1)
      *          upon syserr end-display
      *          display bp-src-name(2) "  " bp-src-line(2)
      *          upon syserr end-display
      *          display bp-src-name(3) "  " bp-src-line(3)
      *          upon syserr end-display
      *          display bp-src-name(4) "  " bp-src-line(4)
      *          upon syserr end-display
            end-if

      *>> IF ENABLE-LOGGING DEFINED
      *      perform log-breakpoint-list.
      *>> END-IF

            continue.

      ***************************************************************
        set-unset-breakpoint section.
            move line-cursor-position to bp-check-src-line.
            subtract 1 from line-cursor-position
            giving tmp-linenumber end-subtract
            move linenumber(tmp-linenumber) to bp-add-src-line.

            perform is-breakpoint.

            if is-breakpoint-flag = 0
                perform add-breakpoint
            else
                perform delete-breakpoint
            end-if

            display animator-screen end-display
            perform display-current-breakpoints.
            perform display-active-line-if-visible.

            perform flush-breakpoints.

            continue.

      ***************************************************************
      *** ----------------- add a breakpoint
        add-breakpoint section.
      *      perform is-breakpoint.

            if bp-next-line = 0
                move 1 to bp-next-line
            end-if
            if bp-next-line < 1001
                add 1 to bp-amount end-add

                move cobol-src-name to bp-src-name(bp-next-line)
                move bp-add-src-line to bp-src-line(bp-next-line)
                add 1 to bp-next-line

                sort bp-line-struct ascending bp-src-name bp-src-line
            end-if

            continue.

      ***************************************************************
      *** ----------------- delete a breakpoint
        delete-breakpoint section.
           if bp-delete-src-name = spaces
                move cobol-src-name to bp-delete-src-name
           end-if

           search all bp-line-struct
                when bp-src-name(bp-index) = bp-delete-src-name and
                     bp-src-line(bp-index) = bp-delete-src-line

                    move spaces to bp-wp-temp-record
                    string "L!;" delimited by size
                        function trim (bp-delete-src-name, trailing)
                        delimited by size
                        into bp-wp-temp-record
                    end-string

                    move bp-wp-temp-record to bp-src-name(bp-index)
           end-search

           continue.

      ***************************************************************
      *** ----------------- write breakpoints to list file
        flush-breakpoints section.
            open output bp-file.

      *>> IF ENABLE-LOGGING DEFINED
      *      string "bp-next-line: " delimited by size
      *          bp-next-line delimited by size
      *          into tmp-log-line
      *      end-string
      *      perform log-msg.
      *>> END-IF

            if bp-next-line > 0
                perform varying bp-line-counter from 1 by 1
                until bp-line-counter = bp-next-line
                    move spaces to bp-line
                    if bp-src-name(bp-line-counter)(1:2) = 'L!'
                        exit perform cycle
                    end-if
                    string function trim (bp-src-name(bp-line-counter))
                        delimited by size
                        ';' delimited by size
                        bp-src-line(bp-line-counter) delimited by size
                        into bp-line
                    end-string

                    write bp-line end-write
                end-perform
            end-if

            close bp-file.

            continue.

      ***************************************************************
      *** ----------------- check if breakpoint exists
        is-breakpoint section.

            search all bp-line-struct
                at end
                   move 0 to is-breakpoint-flag

                when bp-src-name(bp-index) = bp-check-src-name and
                     bp-src-line(bp-index) = bp-check-src-line
                   move 1 to is-breakpoint-flag
            end-search
      *      move spaces to bp-check-src-name.

            continue.

      ***************************************************************
        display-current-breakpoints section.
            move cobol-src-name to bp-check-src-name.
      *      move "testprog" to bp-check-src-name.

            perform varying tmp-onscreen-linenumber from 1 by 1
                until tmp-onscreen-linenumber = MAX-ROWS
                or bp-amount = 0

                move linenumber(tmp-onscreen-linenumber)
                    to bp-check-src-line
                perform is-breakpoint

                if is-breakpoint-flag = 1
                    add 1 to tmp-onscreen-linenumber giving
                    tmp-onscreen-linenumber-2 end-add

                    display sourceline(tmp-onscreen-linenumber)(1:74)
                    line tmp-onscreen-linenumber-2 col 7
                    with background-color COB-COLOR-RED end-display
                end-if
            end-perform

            continue.

      ***************************************************************
       >> IF ENABLE-LOGGING DEFINED
        log-breakpoint-list section.
            if cob-anim-logging not = 'Y'
               exit section
            end-if

            move spaces to log-line
            move "**************************" to log-line
            write log-line

            if bp-next-line not = 0
                perform varying tmp-linenumber from 1 by 1
                    until tmp-linenumber = bp-next-line

                    move spaces to log-line
                    string bp-src-name(tmp-linenumber) delimited by size
                        ";" delimited by size
                        bp-src-line(tmp-linenumber) delimited by size
                        into log-line
                    end-string
                    write log-line
                end-perform
            end-if

            move spaces to log-line
            move "*************************" to log-line
            write log-line

            continue.
       >> END-IF
