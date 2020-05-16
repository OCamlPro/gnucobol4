01  TBL-Path-File-Table.
    05  TBL-dir-logs            PIC 9(01) VALUE 1.
    05  TBL-dir-reports         PIC 9(01) VALUE 2.
    05  TBL-dir-files           PIC 9(01) VALUE 3.
    05  TBL-Nbr-Elements        PIC 9(02) VALUE 3.
    05  TBL-Values.
*> 1
        10                      PIC X(08) VALUE SPACE.
        10                      PIC X(40) VALUE  "./logs/".
*> 2
        10                      PIC X(08) VALUE SPACE.
        10                      PIC X(40) VALUE  "./reports/".
*> 3
        10                      PIC X(08) VALUE SPACE.
        10                      PIC X(40) VALUE  "./files/".
    05  TBL-Redefine
        REDEFINES TBL-Values.
        10  TBL-Element
            OCCURS 3 TIMES
            INDEXED BY TBL-Index.
            15  TBL-Presence    PIC X(08).
            15  TBL-Path-Name   PIC X(40).

01  WS-Path-Name                PIC X(40) VALUE SPACE.
