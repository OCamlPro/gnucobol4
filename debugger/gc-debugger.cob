        identification division.
        program-id. gc-debugger.

        environment division.
        configuration section.
        input-output section.
        file-control.

            select bp-file assign to bp-path
                organization is line sequential
                file status is bp-file-status.
       >> IF ENABLE-LOGGING DEFINED
            select log-file assign to log-file-path
                organization is line sequential
                file status is log-file-status.
       >> END-IF

        data division.

        file section.

            fd  bp-file.
                01 bp-line             pic x(40).

       >> IF ENABLE-LOGGING DEFINED
            fd  log-file.
                01 log-line             pic x(512).
       >> END-IF

        working-storage section.
            copy screenio.

      ***************************************************************
      ** Constants                                                 **
      ***************************************************************
            78 MAX-ROWS             value 19.
            78 MAX-ROW-LENGTH       value 84.

            78 MAX-BREAKPOINTS      value 250.

            77 GETLINE              pic x(128) value "get_aniline".
            77 GETLINECOUNT         pic x(128) value "get_linecount".
            77 ANIDATA              pic x(128) value "anidata".


      ***************************************************************
      ** General flags, management data                            **
      ***************************************************************
            77 global-init-flag             pic 9 value 0.
            77 module-init-flag             pic 9 value 0.

            77 active-line                  pic 9(5) value 0.
            77 active-line-onscreen         pic 99 value 0.

            77 call-depth                   pic 9(6) value 0.
            77 step-over-mode-counter       pic 9(6) value 0.
            77 step-over-return-depth       pic 9(6) value 0.

      *     Saves the run mode:
      *     0... Normal
      *     1... Step-Over
      *     2... Go (till end or breakpoint)
            01 run-mode                     pic 9 value 0.
                88 SINGLE-STEP-MODE         value 0.
                88 STEP-OVER-MODE           value 1.
                88 GO-MODE                  value 2.

            01 step-over-module             pic x(30).
            01 f6-hit                       pic 9.

            77 dummy                        pic x.

       >> IF ENABLE-LOGGING DEFINED
            77 cob-anim-logging             pic x(1).
            77 cob-logging-module           pic x(30).
       >> END-IF

            01 onscreen-mode                pic 9.
                88 DEBUG-SCREEN-MODE        value 1.
                88 VV-SCREEN-MODE           value 2.
                88 SEARCH-SCREEN-MODE       value 3.

            01 continue-search-mode         pic 9.
                88 CONTINUE-SEARCH-ON       value 1.
                88 CONTINUE-SEARCH-OFF      value 0.

      ***************************************************************
      ** Structures (Data, Screen, ... )                           **
      ***************************************************************

            77 lines-set                pic 9(02) value 20.
            01 codelines-data.
                04 codeline occurs 5 to 50 depending on lines-set.
                    05 linenumber       pic 9(5) value zero.
                    05 sourceline       pic x(77).

       >> IF ENABLE-LOGGING DEFINED
            01 log-file-status          pic 99.
            01 log-file-path            pic x(256)
                                        value "gc-debugger.log".
       >> END-IF

            77 VV-NAME                  pic x(10) value "Varname:".
            77 VV-USAGE                 pic x(10) value "Usage:".
            77 VV-SIZE                  pic x(10) value "Length:".
            77 VV-CONTENT               pic x(10) value "Data:".

            77 EMPTYLINE                pic x(2100) value spaces.

            01 SEARCH-SCREEN-HEADLINE   pic x(40)
                value "########################################".
            01 SEARCH-SCREEN-MIDLINE    pic x(40)
                value "#                                      #".
            01 SEARCH-SCREEN-MIDLINE-2  pic x(40)
                value "#  Search term:                        #".
            01 SEARCH-SCREEN-MIDLINE-3  pic x(40)
                value "#  Last Search term (Enter to reuse):  #".
            01 SEARCH-LAST-SEARCH.
                03 filler pic x(3) value "#  ".
                03 last-search-term pic x(30) value spaces.
                03 filler pic x(7) value "      #".


      ***************************************************************
      ** Screen variables (lines, positions, ... )                 **
      ***************************************************************
            01 line-cursor-position     pic 99 value 1.
            01 module-line-count        pic 9(5) value 0.
            01 goto-linenumber          pic 9(5).


      ***************************************************************
      ** Key input processing                                      **
      ***************************************************************
            01 inp-crt-status           pic 9(4).

      ***************************************************************
      ** Buffers and temporary used fields                         **
      ***************************************************************
            01 tmp-command-input-buffer     pic x(70).
            01 tmp-unstring-buffer          pic x(256).
            01 tmp-unstring-ptr             pic 9(3).
            01 tmp-line-position            pic 99.
            01 tmp-source-line-buffer       pic x(512).
            01 tmp-linenumber               pic 9(5).
            01 tmp-linenumber-2             pic 9(5).
            01 tmp-linenumber-3             pic 9(5).
            01 tmp-number                   pic 9(5).
            01 tmp-number-2                 pic 9(5).
            01 tmp-onscreen-linenumber      pic 99.
            01 tmp-onscreen-linenumber-2    pic 99.
            01 tmp-linenumber-bin           usage binary-long.
            01 tmp-counter                  pic 9(5).
            01 tmp-cobol-src-name           pic x(30).
            01 tmp-bp-src-name              pic x(30).
            01 tmp-bp-src-line              pic 9(5).
            01 tmp-wp-var-name              pic x(50).
       >> IF ENABLE-LOGGING DEFINED
            01 tmp-log-line                 pic x(512).
       >> END-IF

      ***************************************************************
      ** Variables for breakpoint management                       **
      ***************************************************************
            77 bp-wp-init-flag                    pic 9.
      * last used line in breakpoint table
            77 bp-next-line                       pic 9(5).

      * line number for delete-breakpoint
            77 bp-delete-src-line                 pic 9(5).
            77 bp-check-src-line redefines bp-delete-src-line pic 9(5).
            77 bp-add-src-line redefines bp-delete-src-line   pic 9(5).

      * src-name for delete, add, check breakpoint
            77 bp-delete-src-name                             pic x(30).
            77 bp-check-src-name redefines bp-delete-src-name pic x(30).
            77 bp-add-src-name redefines bp-delete-src-name   pic x(30).

            77 is-breakpoint-flag                 pic 9 value 0.

      * temporary variable for one record
            77 bp-wp-temp-record                  pic x(40).

            01  bp-file-info.
                03  bp-path                       pic x(256).
                03  bp-file-status                pic 99.
            01  bp-line-counter                   pic 9(4).
            01  bp-amount                         pic 9(4) value 1000.

      * breakpoint table with max. MAX-BREAKPOINTS breakpoints
            01 breakpoint-lst.
                05  bp-line-struct occurs 0 TO MAX-BREAKPOINTS
                              depending on bp-amount,
                              ascending key bp-src-name,
                                            bp-src-line,
                              indexed by bp-index.
                    10  bp-src-name                 pic x(30).
                    10  bp-src-line                 pic 9(5).

      ***************************************************************
      ** Variables for watchpoint management                       **
      ***************************************************************

            77 wp-count                             pic 9(02) value 0.
            78 wp-max                               value 10.
            01 watchpoint-lst.
                05 watchpoint-struct occurs 0  to wp-max
                                     depending on wp-count.
                    10 wp-var-name                  pic x(50).
                    10 wp-module                    pic x(30).
      * wp-value as large as dtf-value in interface-block
                    10 wp-value                     pic x(280).
                    10 last-wp-value                pic x(280).

            01  wp-interface-block.
               03  wp-anim-state                    pic x.
               03  wp-cobol-src-name                pic x(30).
               03  wp-first-stmt-if                 pic 9(6).
      *        current line is the line on which the cursor resides
      *        will be same as active line during animation
      *        but will differ when scrolling through the file
               03  wp-current-line-if               pic 9(6).
      *        active line is the line that will be executed in the file
               03  wp-active-line-if                pic 9(6).
               03  wp-no-code-lines-if              pic 9(6).
               03  wp-dtf-line                      pic 9(6).
               03  wp-dtf-usage                     pic 99.
               03  wp-dtf-length                    pic 999.
               03  wp-dtf-value                     pic x(280).

            01 watchpoint-changed-flag              pic 9.


      ***************************************************************
      ** Variables for string-contains                             **
      ***************************************************************
            01 string-contains-flag             pic 9 value 0.
      * finished by low-value
            01 string-contains-str1             pic x(256).
      * finished by low-value
            01 string-contains-str2             pic x(256).

      ***************************************************************
      ** Variables for fulltext search                             **
      ***************************************************************
            01 search-string                    pic x(30).
            01 last-search-string               pic x(30).
            01 search-result-line               pic 9(5).
            01 last-search-result-line          pic 9(5).

        linkage section.
            01  interface-block.
               03  anim-state                    pic x.
               03  cobol-src-name                pic x(30).
               03  first-stmt-if                 pic 9(6).
      *        current line is the line on which the cursor resides
      *        will be same as active line during animation
      *        but will differ when scrolling through the file
               03  current-line-if               pic 9(6).
      *        active line is the line that will be executed in the file
               03  active-line-if                pic 9(6).
               03  no-code-lines-if              pic 9(6).
               03  dtf-line                      pic 9(6).
               03  dtf-usage                     pic 99.
               03  dtf-length                    pic 999.
               03  dtf-value                     pic x(280).

            01  anim-stmt-type                   pic x.


        copy "screen.cpy".

        procedure division using interface-block anim-stmt-type.
            perform process-interface-block.

            perform forever
                accept tmp-command-input-buffer
                line 23 col 10 end-accept

                perform process-function-key
                perform process-input-buffer
            end-perform

        stop run.

      ***************************************************************
        do-initialization section.
       >> IF ENABLE-LOGGING DEFINED
            if global-init-flag = 0
                accept cob-anim-logging from environment
                'COB_ANIM_LOGGING' end-accept
      *         call "C$TOUPPER" using cob-anim-logging by value 1
      *         end-call
                inspect cob-anim-logging converting 'y1' to 'YY'
                if cob-anim-logging = 'Y'
                    open output log-file
                end-if
                accept cob-logging-module from environment
                'COB_LOGGING_MODULE' end-accept
            end-if
            if cob-anim-logging = 'Y'
                string "Do-Initialization, " delimited by size
                    "global-init-flag: " delimited by size
                    global-init-flag delimited by size
                    ", module-init-flag: " delimited by size
                    module-init-flag delimited by size
                    ", call-depth: " delimited by size
                    call-depth delimited by size
                    into tmp-log-line
                end-string
                perform log-msg
            end-if
       >> END-IF

            if global-init-flag = 0
                set environment 'COB_SCREEN_EXCEPTIONS' to 'Y'
                set environment 'COB_SCREEN_ESC' to 'Y'
            end-if

            if module-init-flag = 0
                perform do-module-init
            end-if

            if global-init-flag = 0
                perform do-screen-init
            end-if

            if SINGLE-STEP-MODE
      *          call "C$SLEEP" using 20 end-call
                display animator-screen end-display
                perform display-current-breakpoints
            end-if
            if bp-wp-init-flag not = 1
                perform read-in-breakpoints
                move 1 to bp-wp-init-flag
                initialize watchpoint-lst, wp-count
            end-if

            move 1 to global-init-flag.

            set DEBUG-SCREEN-MODE to true.

      *      display "end do-init..." upon syserr end-display

            move 1 to return-code.
            if anim-stmt-type = 'X' or  '0'
                goback.

            continue.

      ***************************************************************
        do-screen-init section.
            move "    " to func1.
            move "F1" to func1-2.
            move "Search" to func2.
            move "F4" to func2-2.
            move "Step" to func3.
            move "F5" to func3-2.
            move "Step Over" to func4.
            move "F6" to func4-2.
            move "Go" to func5.
            move "F7" to func5-2.

            move "Command: " to command-input-line.

            continue.

      ***************************************************************

        do-module-init section.

            move spaces to GETLINE.
            move spaces to GETLINECOUNT.
            move spaces to ANIDATA.

            string "get_aniline_" delimited by size
                   cobol-src-name delimited by spaces
                into GETLINE
            end-string

            string "get_linecount_" delimited by size
                   cobol-src-name delimited by spaces
                into GETLINECOUNT
            end-string

            string "anidata_" delimited by size
                   cobol-src-name delimited by spaces
                into ANIDATA
            end-string

            call GETLINECOUNT end-call
            move return-code to module-line-count.

      *      call "C$SLEEP" using 20 end-call
            initialize  codelines-data.

            move spaces to headline.
            string "GnuCOBOL 2.0 Debugger  --  "
                   cobol-src-name
                   delimited by size
                into headline
            end-string

            move headline to headline-vv.

            perform varying tmp-linenumber from 1 by 1
                    until   tmp-linenumber > MAX-ROWS or
                            tmp-linenumber > module-line-count

                    move spaces to tmp-source-line-buffer
                    move tmp-linenumber to tmp-linenumber-bin

                    call GETLINE using tmp-linenumber-bin
                    tmp-source-line-buffer end-call

                    move tmp-linenumber to linenumber(tmp-linenumber)
                    move tmp-source-line-buffer
                        to sourceline(tmp-linenumber)
            end-perform

            move 1 to module-init-flag.

            continue.

      ***************************************************************

        process-function-key section.
            move COB-CRT-STATUS to inp-crt-status.

            evaluate inp-crt-status
                when 1003 perform fulltext-search-continue
                when 1004 perform fulltext-search
                when 1005 perform do-single-step
                when 1006 perform do-step-over
                when 1007 perform do-go
                when 2005 perform quit-debugger
                when 2001 perform page-up
                when 2002 perform page-down
                when 2003 perform line-up
                when 2004 perform line-down
            end-evaluate

            continue.

      ***************************************************************
        process-function-key-vv section.
            move COB-CRT-STATUS to inp-crt-status.

            evaluate inp-crt-status
                when 2005
                     perform quit-debugger
      *         when 0
      *         when 1001
                when other
                     perform goback-from-vv
            end-evaluate

            continue.

      ***************************************************************
        process-input-buffer section.
            call "C$TOUPPER" using tmp-command-input-buffer
            by value 1 end-call

            evaluate tmp-command-input-buffer(1:1)
                when 'B' perform set-unset-breakpoint
                when 'G' perform goto-line
                when 'V' perform view-variable
                when 'W' perform watchpoint-action
            end-evaluate

            move spaces to tmp-command-input-buffer.

            continue.

      ***************************************************************
        process-interface-block section.
       >> IF ENABLE-LOGGING DEFINED
            perform log-interface-block.
       >> END-IF

            perform process-interface-block-cdepth.

            if SINGLE-STEP-MODE
       >> IF ENABLE-LOGGING DEFINED
                if cob-anim-logging = 'Y'
                    move "Normal mode step..." to tmp-log-line
                    perform log-msg
                end-if
       >> END-IF
                if anim-stmt-type = '0'
                        move 0 to module-init-flag
                        perform do-initialization
                else if anim-state = 'I' and module-init-flag = 0
                        perform do-initialization
                        perform display-active-line
                     else if anim-state = 'X'
                              perform check-for-stop-run
                              goback
                          else
                              perform line-cursor-adjustment
                              display animator-screen end-display
                              perform display-active-line
                          end-if
                     end-if
                end-if

                perform check-for-stop-run
            end-if

            if GO-MODE or STEP-OVER-MODE
                if not GO-MODE
                    set GO-MODE to true
       >> IF ENABLE-LOGGING DEFINED
                    move 'Set GO-MODE to true' to tmp-log-line
                    perform log-msg
       >> END-IF
                end-if

                if anim-stmt-type = '0'
                    move 0 to module-init-flag
                    perform do-initialization
                else if anim-state = 'I' and module-init-flag = 0
                        perform do-initialization
                     end-if
                end-if
       >> IF ENABLE-LOGGING DEFINED
                if cob-anim-logging = 'Y'
                    move "Go mode step..." to tmp-log-line
                    perform log-msg
                end-if
       >> END-IF
                move cobol-src-name to bp-check-src-name
                move active-line-if to bp-check-src-line

                *> FIXME: add callback function for performance issues instead
                perform check-for-stop-run

                perform is-breakpoint
                perform check-watchpoints

       >> IF ENABLE-LOGGING DEFINED
                if cob-anim-logging = 'Y'
                    string "Checked for BP... " delimited by size
                        is-breakpoint-flag
                        '; '
                        bp-check-src-name delimited by size
                        ";" delimited by size
                        bp-check-src-line delimited by spaces
                        into tmp-log-line
                    end-string
                    perform log-msg
                end-if
       >> END-IF

                if is-breakpoint-flag = 1 or watchpoint-changed-flag = 1
                    set SINGLE-STEP-MODE to true
      *              display "Breakpoint..." upon syserr end-display
                    display animator-screen end-display
                    perform display-active-line
                    perform display-current-breakpoints
                else
       >> IF ENABLE-LOGGING DEFINED
                    if cob-anim-logging = 'Y'
                        move "No BP, goback..." to tmp-log-line
                        perform log-msg
                    end-if
       >> END-IF
                    goback
                end-if
            end-if

            continue.

      ***************************************************************
        process-interface-block-cdepth section.
            evaluate true
               when anim-state = 'X'
      *           if call-depth > 0
      *               subtract 1 from call-depth end-subtract
      *           end-if
                  move 0 to module-init-flag

               when anim-stmt-type = 'P'
                  if STEP-OVER-MODE
                      set GO-MODE to true
                  end-if

                  add 1 to step-over-mode-counter end-add

       >> IF ENABLE-LOGGING DEFINED
                  if cob-anim-logging = 'Y'
                      move "Perform/Call step..." to tmp-log-line
                      perform log-msg
                  end-if
       >> END-IF
      *              add 1 to call-depth end-add
               when anim-stmt-type = 'R'
                  subtract 1 from step-over-mode-counter end-subtract

                  if step-over-mode-counter = step-over-return-depth
                      and f6-hit = 1

                      set SINGLE-STEP-MODE to true
                      move 0 to f6-hit
                  end-if

                  move spaces to headline
                  string "GnuCOBOL 2.0 Debugger  --  "
                         cobol-src-name
                         delimited by size
                      into headline
                  end-string

                  goback

            end-evaluate

            continue.

      ***************************************************************
        display-active-line section.
            move active-line-if to goto-linenumber.
            move spaces to tmp-command-input-buffer.

            perform goto-line.

            compute active-line-onscreen = active-line-if
                - linenumber(1) + 1 end-compute
            compute tmp-onscreen-linenumber = active-line-onscreen
                + 1 end-compute

            display sourceline(active-line-onscreen)(1:74)
            line tmp-onscreen-linenumber col 7
            with background-color COB-COLOR-GREEN end-display

      *>> IF ENABLE-LOGGING DEFINED
      *      if cob-anim-logging = 'Y'
      *          string "display-active-line: " delimited by size
      *              active-line-onscreen delimited by size
      *              ", " delimited by size
      *              tmp-onscreen-linenumber delimited by size
      *              into tmp-log-line
      *          end-string
      *          perform log-msg
      *      end-if
      *>> END-IF

            continue.

      ***************************************************************
        display-active-line-if-visible section.
            if active-line-if >= linenumber(1)
                and active-line-if <= linenumber(MAX-ROWS)

                compute tmp-onscreen-linenumber = active-line-if
                    - linenumber(1) + 1 end-compute
                compute tmp-onscreen-linenumber-2 =
                    tmp-onscreen-linenumber + 1 end-compute

      *>> IF ENABLE-LOGGING DEFINED
      *          if cob-anim-logging = 'Y'
      *              string "display-active-line-if-visible: "
      *                  delimited by size
      *                  tmp-onscreen-linenumber delimited by size
      *                  ", " delimited by size
      *                  tmp-onscreen-linenumber-2 delimited by size
      *                  into tmp-log-line
      *              end-string
      *              perform log-msg
      *          end-if
      *>> END-IF

                display sourceline(tmp-onscreen-linenumber)(1:74)
                line tmp-onscreen-linenumber-2 col 7
                with background-color COB-COLOR-GREEN end-display

            else if module-line-count <= MAX-ROWS
                    add 1 to active-line-if
                    giving tmp-onscreen-linenumber-2 end-add

                    display sourceline(active-line-if)(1:74)
                    line tmp-onscreen-linenumber-2 col 7
                    with background-color COB-COLOR-GREEN end-display
                end-if
            end-if

            continue.

      ***************************************************************
        refresh-screen section.
            display animator-screen end-display
            perform display-current-breakpoints.
            perform display-active-line-if-visible.

            continue.

      ***************************************************************
        line-cursor-adjustment section.
            display space line line-cursor-position col 6 end-display
            compute line-cursor-position = active-line-if -
                linenumber(1) + 1 end-compute

      *>> IF ENABLE-LOGGING DEFINED
      *      if cob-anim-logging = 'Y'
      *          string "line-cursor-adjustment: " delimited by size
      *              line-cursor-position
      *              into tmp-log-line
      *          end-string
      *          perform log-msg.
      *      end-if
      *>> END-IF

            continue.

      ***************************************************************
        quit-debugger section.
            if SEARCH-SCREEN-MODE
                display animator-screen end-display
                set DEBUG-SCREEN-MODE to true
            else
      *         move "Möchten Sie den Debugger beenden?" to
      *         command-input-line
                move "Do you want to exit the Animator?" to
                command-input-line
                display command-input-line end-display
                display footline end-display

                move spaces to tmp-command-input-buffer
                compute tmp-line-position = function length
                ( function trim (command-input-line) ) + 2 end-compute

                accept tmp-command-input-buffer line 23
                col tmp-line-position end-accept

                if function
                    upper-case(tmp-command-input-buffer(1:1))
                    = 'J' or 'Y'

                    perform flush-breakpoints
      *              Doesn't work in GC 1.1
      *              display spaces upon crt end-display
                    display empty-screen end-display
                    stop run
                end-if

                move "Command: " to command-input-line
                display command-input-line end-display
                display footline end-display
                display footline-2 end-display
            end-if

            continue.

      ***************************************************************
        do-single-step section.
            set SINGLE-STEP-MODE to true.
            goback.

      ***************************************************************
        do-step-over section.
            if anim-stmt-type = 'P'
                set STEP-OVER-MODE to true
                move 1 to f6-hit
                subtract 1 from step-over-mode-counter
                    giving step-over-return-depth
       >> IF ENABLE-LOGGING DEFINED
                if cob-anim-logging = 'Y'
                    string 'saved step-over-return-depth: '
                    step-over-return-depth into tmp-log-line end-string
                    perform log-msg
                end-if
       >> END-IF
                move cobol-src-name to step-over-module
            else
                set SINGLE-STEP-MODE to true
            end-if

            goback.

      ***************************************************************
        do-go section.
            set GO-MODE to true.
            goback.

      ***************************************************************
        goto-line section.
            if module-line-count < MAX-ROWS
                move active-line-if to active-line-onscreen
                display space line line-cursor-position col 6
                end-display
                add 1 to active-line-if giving line-cursor-position
                end-add
            else
                if tmp-command-input-buffer(1:1) = "G"
                    move 3 to tmp-unstring-ptr
                    unstring tmp-command-input-buffer delimited by " "
                        into tmp-unstring-buffer
                        with pointer tmp-unstring-ptr
                    end-unstring

                    move tmp-unstring-buffer to goto-linenumber
                end-if

                compute tmp-linenumber-2 = module-line-count - MAX-ROWS
                end-compute

                divide MAX-ROWS by 2 giving tmp-number
                end-divide
      *          move tmp-number to line-cursor-position

                if goto-linenumber > tmp-linenumber-2
                    compute goto-linenumber = module-line-count -
                    tmp-number end-compute
                else if goto-linenumber < MAX-ROWS
                        add 1 to tmp-number giving goto-linenumber
                        end-add
                     end-if
                end-if

                compute tmp-linenumber-2 = goto-linenumber -
                tmp-number end-compute
                compute tmp-linenumber-3 = goto-linenumber +
                tmp-number + 1 end-compute
                move 1 to tmp-counter

      *          display "mdlc: " module-line-count upon syserr end-display
      *          display "goto: " goto-linenumber upon syserr end-display
      *          display "start: " tmp-linenumber-2 upon syserr end-display
      *          display "end: " tmp-linenumber-3 upon syserr end-display

                perform varying tmp-linenumber
                    from tmp-linenumber-2 by 1
                    until tmp-linenumber = tmp-linenumber-3

                    move spaces to tmp-source-line-buffer
                    move tmp-linenumber to tmp-linenumber-bin

                    call GETLINE using tmp-linenumber-bin
                    tmp-source-line-buffer end-call

                    move tmp-source-line-buffer
                        to sourceline(tmp-counter)
                    move tmp-linenumber to linenumber(tmp-counter)

                    if linenumber(tmp-counter) = goto-linenumber
                        display space line line-cursor-position col 6
                        end-display
                        move tmp-counter to line-cursor-position
       >> IF ENABLE-LOGGING DEFINED
                        string "goto-line, line-cursor-position: "
                            delimited by size
                            line-cursor-position delimited by size
                            into tmp-log-line
                        end-string
                        perform log-msg
       >> END-IF
                    end-if

                    add 1 to tmp-counter end-add
                end-perform

            end-if

            display animator-screen end-display
            perform display-current-breakpoints.

            continue.

      ***************************************************************
        page-up section.
            compute goto-linenumber = linenumber(10) - MAX-ROWS
            end-compute

            move spaces to tmp-command-input-buffer.

            perform goto-line.
            perform display-current-breakpoints.
            perform display-active-line-if-visible.

            continue.

      ***************************************************************
        page-down section.
            compute goto-linenumber = linenumber(10) + MAX-ROWS
            end-compute

            move spaces to tmp-command-input-buffer.

            perform goto-line.
            perform display-current-breakpoints.
            perform display-active-line-if-visible.

            continue.

      ***************************************************************
        line-up section.
            if line-cursor-position > 2
                display ' ' line line-cursor-position col 6
                end-display

                subtract 1 from line-cursor-position end-subtract
                display line-cursor end-display
            else
                if line-cursor-position = 2
                and linenumber(1) > 1
                    perform scroll-one-line-up
                end-if
            end-if

            perform display-current-breakpoints.
            perform display-active-line-if-visible.

            continue.

      ***************************************************************
        line-down section.
            compute tmp-line-position = MAX-ROWS + 1 end-compute
            if line-cursor-position < tmp-line-position
                display ' ' line line-cursor-position col 6
                end-display

                add 1 to line-cursor-position end-add
                display line-cursor end-display
            else
                if line-cursor-position = tmp-line-position
                and linenumber(MAX-ROWS) < module-line-count
                and MAX-ROWS < module-line-count
                    perform scroll-one-line-down
                end-if
            end-if

            perform display-current-breakpoints.
            perform display-active-line-if-visible.

            continue.

      ***************************************************************
        scroll-one-line-down section.

            perform varying tmp-linenumber
                from 2 by 1 until tmp-linenumber > MAX-ROWS

                compute tmp-linenumber-2 = tmp-linenumber - 1
                end-compute

                move linenumber(tmp-linenumber)
                    to linenumber(tmp-linenumber-2)
                move sourceline(tmp-linenumber)
                    to sourceline(tmp-linenumber-2)
            end-perform

            compute tmp-linenumber = linenumber(MAX-ROWS) + 1
            end-compute

            move spaces to tmp-source-line-buffer
            move tmp-linenumber to tmp-linenumber-bin

            call GETLINE using tmp-linenumber-bin
            tmp-source-line-buffer end-call

            move spaces to sourceline(MAX-ROWS).
            move function trim (tmp-source-line-buffer, trailing) to
                sourceline(MAX-ROWS).
            move tmp-linenumber to linenumber(MAX-ROWS).

            display animator-screen end-display

            continue.

      ***************************************************************
        scroll-one-line-up section.
            perform varying tmp-linenumber
                from MAX-ROWS by -1 until tmp-linenumber = 1

                compute tmp-linenumber-2 = tmp-linenumber - 1
                end-compute

      *          display tmp-linenumber upon syserr end-display
      *          display tmp-linenumber-2 upon syserr end-display

                move linenumber(tmp-linenumber-2)
                    to linenumber(tmp-linenumber)
                move sourceline(tmp-linenumber-2)
                    to sourceline(tmp-linenumber)
            end-perform

            compute tmp-linenumber = linenumber(1) - 1
            end-compute

            move spaces to tmp-source-line-buffer
            move tmp-linenumber to tmp-linenumber-bin

            call GETLINE using tmp-linenumber-bin
            tmp-source-line-buffer end-call

            move spaces to sourceline(1).
            move function trim (tmp-source-line-buffer, trailing) to
                sourceline(1).
            move tmp-linenumber to linenumber(1).

            display animator-screen end-display

            continue.

      * Binde ausgelagerte Programmbestandteile ein
        copy "breakpoints.cpy".
        copy "view-variable.cpy".
       >> IF ENABLE-LOGGING DEFINED
        copy "logging.cpy".
       >> END-IF
        copy "helpers.cpy".
        copy "fulltext-search.cpy".
        copy "watchpoints.cpy".
