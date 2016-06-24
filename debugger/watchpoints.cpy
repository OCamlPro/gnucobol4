      * watchpoints.cpy
      * 
      
        watchpoint-action section.
            move 3 to tmp-unstring-ptr.
            move spaces to tmp-unstring-buffer.
            
            unstring tmp-command-input-buffer 
                delimited by spaces 
                into tmp-unstring-buffer
                with pointer tmp-unstring-ptr
            end-unstring
            
            evaluate tmp-unstring-buffer
                when 'del'
                    perform delete-all-watchpoints
                when other
                    perform add-watchpoint
            end-evaluate
            
            continue.

      ************************************************************
        update-watchpoints section.
            
            perform varying tmp-number from 1 by 1 until tmp-number > 10
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
                else 
                    exit perform
                end-if
            end-perform
            
      *      perform log-watchpoints.
            
            continue.
            
      ************************************************************
        check-watchpoints section.
            perform update-watchpoints.
            move 0 to watchpoint-changed-flag.
            
            perform varying tmp-number from 1 by 1 until tmp-number > 10
                if wp-var-name(tmp-number) = spaces 
                    exit perform
                end-if
                
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
            perform get-next-free-watchpoint-index.
            move tmp-unstring-buffer(1:50) to tmp-wp-var-name.
            move spaces to status-line.
            
            if tmp-number > 0 and tmp-number < 11
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
            else if tmp-number > 10 
                    move 'No more watchpoints possible. (limit: 10)'
                        to status-line
                 end-if
            end-if
            
            display status-line end-display
      *      perform log-watchpoints.
            
            continue.
            
      ************************************************************
        delete-all-watchpoints section.
            move 'Deleted all watchpoints.' to status-line.
            display status-line end-display
        
            initialize watchpoint-lst.
            continue.
            
      ************************************************************
        get-next-free-watchpoint-index section.
            
            perform varying tmp-number from 1 by 1 until tmp-number > 10
                if wp-var-name(tmp-number) = spaces 
                    exit perform
                end-if
            end-perform
            
            continue.
            
      ************************************************************
      *  log-watchpoints section.
      *      perform varying tmp-number from 1 by 1 until tmp-number > 10
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
      *      end-perform
        
      *      continue.
