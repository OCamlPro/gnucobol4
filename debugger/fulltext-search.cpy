      * fulltext-search.cpy
      * Volltextsuche im aktuell geladenen Modul
      
      ************************************************************
        fulltext-search-continue section.
            if CONTINUE-SEARCH-ON
                move last-search-string to search-string
                perform search-in-module

                if string-contains-flag = 1
      * Successful search
                    move search-result-line to goto-linenumber
                    move search-result-line 
                        to last-search-result-line
                    perform goto-line
                    perform display-active-line-if-visible
                    perform mark-result-line
                else
      * If we found nothing, just refresh current screen
      *              display animator-screen end-display
      *              perform display-current-breakpoints
      *              perform display-active-line-if-visible
      *              
      *              move spaces to search-string
      *              perform set-unset-continue-search
      
                    move 1 to last-search-result-line
                    perform fulltext-search-continue
                end-if
            end-if
        
            continue.
      
      ************************************************************
        fulltext-search section.
            if CONTINUE-SEARCH-ON
                set CONTINUE-SEARCH-OFF to true
                move "F1" to func1-2 
                move spaces to func1
                initialize last-search-result-line
                
                display footline footline-2 end-display
            end-if
            
            if not SEARCH-SCREEN-MODE
                set SEARCH-SCREEN-MODE to true
                if last-search-string not = spaces
                    move last-search-string to last-search-term
                end-if
                
                display search-screen end-display
                move spaces to search-string
                accept search-string line 14 col 23 end-accept
                
                perform set-unset-continue-search
                if search-string not = spaces 
                    move search-string to last-search-string
                else
                    move last-search-string to search-string
                end-if
                                
                if search-string not = spaces
                    move search-string to last-search-string
                    perform search-in-module

                    if string-contains-flag = 1
      * Successful search
                        move search-result-line to goto-linenumber
                        move search-result-line 
                            to last-search-result-line
                        perform goto-line
                        perform display-active-line-if-visible
                        perform mark-result-line
                    else
      * If we found nothing, just refresh current screen
                        set CONTINUE-SEARCH-OFF to true
                        perform refresh-footline-search-mode
                        perform refresh-screen
                    end-if
                else 
      * If someone is searching with both terms empty, just refresh current screen
                    perform refresh-screen
                end-if
            else
      * If F4 is hit again while search mask is visible
                perform refresh-screen
            end-if

            set DEBUG-SCREEN-MODE to true
            continue.
            
      ************************************************************
        search-in-module section.
            move active-line-if to search-result-line.
            move spaces to tmp-source-line-buffer.
            
            if CONTINUE-SEARCH-ON and last-search-result-line > 0 
                and last-search-result-line < module-line-count
                
                add 1 to last-search-result-line giving tmp-linenumber-2
            else
                move 1 to tmp-linenumber-2
            end-if
            
            perform varying tmp-linenumber from tmp-linenumber-2 
                by 1 until tmp-linenumber > module-line-count
                
                move spaces to tmp-source-line-buffer
                move tmp-linenumber to tmp-linenumber-bin
                
                call GETLINE using tmp-linenumber-bin 
                    tmp-source-line-buffer end-call
                    
                string tmp-source-line-buffer(1:255) delimited by size
                       low-value
                       into string-contains-str1 
                end-string
                string search-string delimited by size
                       low-value
                       into string-contains-str2
                end-string
                
                perform string-contains
                
                if string-contains-flag = 1
                    move tmp-linenumber to search-result-line
                    exit perform
                end-if
            end-perform
            
      *      if CONTINUE-SEARCH-ON 
      *          and search-result-line = active-line-if
      *          
      *          move 1 to last-search-result-line
      *      end-if
            
            continue.
            
      ************************************************************
        mark-result-line section.
            if search-result-line >= linenumber(1) 
                and search-result-line <= linenumber(MAX-ROWS)
                
                compute tmp-onscreen-linenumber = search-result-line 
                    - linenumber(1) + 1 end-compute
                compute tmp-onscreen-linenumber-2 = 
                    tmp-onscreen-linenumber + 1 end-compute
                
                display sourceline(tmp-onscreen-linenumber)(1:74)
                line tmp-onscreen-linenumber-2 col 7 
                with background-color COB-COLOR-BLUE end-display
                
            else if module-line-count <= MAX-ROWS
                    add 1 to search-result-line 
                    giving tmp-onscreen-linenumber-2 end-add
                
                    display sourceline(search-result-line)(1:74)
                    line tmp-onscreen-linenumber-2 col 7 
                    with background-color COB-COLOR-BLUE end-display
                end-if
            end-if
            
            continue.
            
      ************************************************************
        set-unset-continue-search section.
            if search-string not = spaces 
                set CONTINUE-SEARCH-ON to true
                perform refresh-footline-search-mode
            else 
                set CONTINUE-SEARCH-OFF to true
                perform refresh-footline-search-mode
            end-if
        
            continue.
            
      ************************************************************
        refresh-footline-search-mode section.
            if CONTINUE-SEARCH-ON 
                move "F3" to func1-2 
                move "Search On" to func1
                move search-result-line to last-search-result-line
                
                display footline end-display
                display footline-2 end-display
            else 
                move "F1" to func1-2 
                move spaces to func1
                initialize last-search-result-line
                
                display footline end-display
                display footline-2 end-display                
            end-if
        
            continue.
