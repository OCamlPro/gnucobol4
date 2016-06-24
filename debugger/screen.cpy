        screen section.
            01 animator-screen.
                03 headline pic x(60)
                   BACKGROUND-COLOR COB-COLOR-BLACK
                   FOREGROUND-COLOR COB-COLOR-WHITE
                   LINE 1
                   COL 10.
                03 codelines
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                   .
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 2 USING linenumber (01).
                        05 filler pic x(74)
                            COL 7 LINE 2 USING sourceline (01).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 3 USING linenumber (02).
                        05 filler pic x(74)
                            COL 7 LINE 3 USING sourceline (02).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 4 USING linenumber (03).
                        05 filler pic x(74)
                            COL 7 LINE 4 USING sourceline (03).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 5 USING linenumber (04).
                        05 filler pic x(74)
                            COL 7 LINE 5 USING sourceline (04).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 6 USING linenumber (05).
                        05 filler pic x(74)
                            COL 7 LINE 6 USING sourceline (05).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 7 USING linenumber (06).
                        05 filler pic x(74)
                            COL 7 LINE 7 USING sourceline (06).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 8 USING linenumber (07).
                        05 filler pic x(74)
                            COL 7 LINE 8 USING sourceline (07).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 9 USING linenumber (08).
                        05 filler pic x(74)
                            COL 7 LINE 9 USING sourceline (08).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 10 USING linenumber (09).
                        05 filler pic x(74)
                            COL 7 LINE 10 USING sourceline (09).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 11 USING linenumber (10).
                        05 filler pic x(74)
                            COL 7 LINE 11 USING sourceline (10).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 12 USING linenumber (11).
                        05 filler pic x(74)
                            COL 7 LINE 12 USING sourceline (11).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 13 USING linenumber (12).
                        05 filler pic x(74)
                            COL 7 LINE 13 USING sourceline (12).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 14 USING linenumber (13).
                        05 filler pic x(74)
                            COL 7 LINE 14 USING sourceline (13).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 15 USING linenumber (14).
                        05 filler pic x(74)
                            COL 7 LINE 15 USING sourceline (14).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 16 USING linenumber (15).
                        05 filler pic x(74)
                            COL 7 LINE 16 USING sourceline (15).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 17 USING linenumber (16).
                        05 filler pic x(74)
                            COL 7 LINE 17 USING sourceline (16).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 18 USING linenumber (17).
                        05 filler pic x(74)
                            COL 7 LINE 18 USING sourceline (17).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 19 USING linenumber (18).
                        05 filler pic x(74)
                            COL 7 LINE 19 USING sourceline (18).
                    04 filler.
                        05 filler pic 9(5)
                            COL 1 LINE 20 USING linenumber (19).
                        05 filler pic x(74)
                            COL 7 LINE 20 USING sourceline (19).

                03 line-cursor pic x value '>'
                        BACKGROUND-COLOR COB-COLOR-BLACK
                        FOREGROUND-COLOR COB-COLOR-WHITE
                        LINE line-cursor-position
                        COL 6.

                03 status-line pic x(80) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 22
                    COL 1.
                        
                03 command-input-line pic x(50) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 23
                    COL 1.
                            
                03 footline
                    BACKGROUND-COLOR COB-COLOR-BLACK.
                    04 func1 pic x(15)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 25
                        COL 1.
                    04 func2 pic x(15)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 25
                        COL 17.
                    04 func3 pic x(15)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 25
                        COL 33.
                    04 func4 pic x(15)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 25
                        COL 49.
                    04 func5 pic x(15)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 25
                        COL 65.


                03 footline-2
                    BACKGROUND-COLOR COB-COLOR-BLACK.
                    04 func1-2 pic x(2)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 24
                        COL 1.
                    04 func2-2 pic x(2)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 24
                        COL 17.
                    04 func3-2 pic x(2)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 24
                        COL 33.
                    04 func4-2 pic x(2)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 24
                        COL 49.
                    04 func5-2 pic x(2)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 24
                        COL 65.

            01 view-variable-screen.
                03 headline-vv pic x(80)
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 1
                    COL 1.
                03 name-vv.
                    04 name-vv-head pic x(10)
                        BACKGROUND-COLOR COB-COLOR-BLACK
                        FOREGROUND-COLOR COB-COLOR-WHITE
                        LINE 3
                        COL 1
                        USING VV-NAME.
                    04 name-vv-content pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 3
                        COL 12.
                03 usage-vv.
                    04 usage-vv-head pic x(10)
                        BACKGROUND-COLOR COB-COLOR-BLACK
                        FOREGROUND-COLOR COB-COLOR-WHITE
                        LINE 4
                        COL 1
                        USING VV-USAGE.
                    04 usage-vv-content pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 4
                        COL 12.                
                03 size-vv.
                    04 size-vv-head pic x(10)
                        BACKGROUND-COLOR COB-COLOR-BLACK
                        FOREGROUND-COLOR COB-COLOR-WHITE
                        LINE 5
                        COL 1
                        USING VV-SIZE.
                    04 size-vv-content pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 5
                        COL 12.                
                03 content-vv.
                    04 content-vv-head pic x(10)
                        BACKGROUND-COLOR COB-COLOR-BLACK
                        FOREGROUND-COLOR COB-COLOR-WHITE
                        LINE 6
                        COL 1
                        USING VV-CONTENT.
                    04 content-vv-l1 pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 6
                        COL 12.
                    04 content-vv-l2 pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 7
                        COL 12.
                    04 content-vv-l3 pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 8
                        COL 12.
                    04 content-vv-l4 pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 9
                        COL 12.
                    04 content-vv-l5 pic x(60)
                        BACKGROUND-COLOR COB-COLOR-WHITE
                        FOREGROUND-COLOR COB-COLOR-BLACK
                        LINE 10
                        COL 12.
                       
            01 empty-screen.
                03 filler pic x(2100) LINE 1 COL 1 using EMPTYLINE.
                
            01 search-screen.
                03 filler pic x(40)
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 11
                    COL 20
                    using SEARCH-SCREEN-HEADLINE.
                03 filler pic x(40) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 12
                    COL 20
                    using SEARCH-SCREEN-MIDLINE.
                03 filler pic x(40)
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 13
                    COL 20
                    using SEARCH-SCREEN-MIDLINE-2.
                03 filler pic x(40) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 14
                    COL 20
                    using SEARCH-SCREEN-MIDLINE.
                03 filler pic x(40) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 15
                    COL 20
                    using SEARCH-SCREEN-MIDLINE-3.
                03 filler pic x(40) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 16
                    COL 20
                    using SEARCH-LAST-SEARCH.
                03 filler pic x(40) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 17
                    COL 20
                    using SEARCH-SCREEN-MIDLINE.
                03 filler pic x(40) 
                    BACKGROUND-COLOR COB-COLOR-BLACK
                    FOREGROUND-COLOR COB-COLOR-WHITE
                    LINE 18
                    COL 20
                    using SEARCH-SCREEN-HEADLINE.
