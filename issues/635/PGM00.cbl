*>      $ SET SOURCEFORMAT "FREE"
*>------------------------------------------------
IDENTIFICATION DIVISION.
PROGRAM-ID.  PGM00.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*> Checks for the existence of the proper directory structure
*> and creates any missing directories.
*>*> If a new file is found in Downloads,
*>*> it is placed in ./files.
*> It runs automatically with every invocation of MainScreen.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.

REPOSITORY.
    FUNCTION ALL INTRINSIC.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
COPY "./copybooks/Log-File-Select.cpy".

DATA DIVISION.
FILE SECTION.
COPY "./copybooks/Log-File-FD.cpy".

*>------------------------------------------------
WORKING-STORAGE SECTION.
*>------------------------------------------------
01  WS-Pgm-ID                   PIC X(10) VALUE "PGM00".

01  WS-STATUS                   PIC X(02) VALUE SPACE.
01  WS-MSG                      PIC X(80) VALUE SPACE.

COPY "./copybooks/Log-File-WS.cpy".

COPY "./copybooks/Dir-Files-Tbl.cpy".

COPY "./copybooks/File-Info.cpy".

COPY "./copybooks/WS-DateTime.cpy".

*>------------------------------------------------
LINKAGE SECTION.
*>------------------------------------------------
COPY "./copybooks/linkage.cpy".

*>------------------------------------------------
PROCEDURE DIVISION
        USING BY REFERENCE WS-Option
                           WS-Message-Line-1
                           WS-Message-Line-2
                           WS-Message-Line-3
                           WS-Message-Line-4
                           WS-Message-Line-5
                           WS-Message-Line-5
                           WS-Message-Line-6
                           WS-Message-Line-7
                           WS-Return
                           WS-Return-Msg.
*>------------------------------------------------
000-Main.

    MOVE "PGM00 BEGIN"
      TO WS-Message-Line-1.

    PERFORM 110-Check-Logs-Dir.

    PERFORM 120-Check-Reports-Dir.

    PERFORM 130-Check-Files-Dir.

    MOVE "PGM00 SUCCESSFUL EOJ xx"
      TO WS-Message-Line-5
         Msg OF WS-Log-Line

    WRITE Log-File-Printline
      FROM WS-Log-Line
      AFTER ADVANCING 1
    END-WRITE.

    CLOSE Log-File.

    GOBACK.

*>------------------------------------------------
110-Check-Logs-Dir.
*>------------------------------------------------
*> Check if ./logs/ exists
*> if not; create it
*>------------------------------------------------
*> See associated ./copybooks/Log-File-PD-Init.cpy
    ACCEPT CDT-Date FROM DATE YYYYMMDD END-ACCEPT.
    ACCEPT CDT-Time FROM TIME          END-ACCEPT.
    MOVE CDT-Year    TO Date-YYYY OF WS-Log-Line.
    MOVE CDT-Month   TO Date-MM   OF WS-Log-Line.
    MOVE CDT-Day     TO Date-DD   OF WS-Log-Line.
    MOVE CDT-Hour    TO Time-HH   OF WS-Log-Line.
    MOVE CDT-Minutes TO Time-MM   OF WS-Log-Line.
    MOVE CDT-Seconds TO Time-SS   OF WS-Log-Line.
    MOVE CDT-Hundredths-Of-Secs
                     TO Time-HS   OF WS-Log-Line.
    MOVE WS-Pgm-ID   TO Pgm-ID    OF WS-Log-Line.
    MOVE "Begin job" TO Msg       OF WS-Log-Line.

    SET TBL-Index TO TBL-dir-logs.
    MOVE TBL-Path-Name(TBL-Index)
      TO WS-Path-Name.
    PERFORM Check-File-Exist.
    IF  TBL-Presence(TBL-Index) = "PRESENT"
        MOVE "./log directory exists"
          TO Msg OF WS-Log-Line
    ELSE
        CALL "C$MAKEDIR"
          USING WS-Path-Name
        END-CALL
        MOVE "./log directory created"
          TO Msg OF WS-Log-Line
    END-IF.

    MOVE WS-Log-Line(1:60)
      TO WS-Message-Line-2.

*> all subsequent opens by other programs are "EXTEND"
    OPEN OUTPUT Log-File.

    WRITE Log-File-Printline
      FROM WS-Log-Line
      AFTER ADVANCING 1
    END-WRITE.

    MOVE "log file OPENed"
      TO Msg OF WS-Log-Line.
    WRITE Log-File-Printline
      FROM WS-Log-Line
      AFTER ADVANCING 1
    END-WRITE.

*>------------------------------------------------
120-Check-Reports-Dir.
*>------------------------------------------------
*> Check if ./reports/ exists
*> if not; create it
*>------------------------------------------------
    SET TBL-Index TO TBL-dir-reports.
    MOVE TBL-Path-Name(TBL-Index)
      TO WS-Path-Name.
    PERFORM Check-File-Exist.
    IF  TBL-Presence(TBL-Index) = "PRESENT"
        MOVE "./reports directory exists"
          TO Msg OF WS-Log-Line
    ELSE
        CALL "C$MAKEDIR"
          USING WS-Path-Name
        END-CALL
        MOVE "./reports directory created"
          TO Msg OF WS-Log-Line
    END-IF.

    MOVE WS-Log-Line(1:60)
      TO WS-Message-Line-3.

    WRITE Log-File-Printline
      FROM WS-Log-Line
      AFTER ADVANCING 1
    END-WRITE.

*>------------------------------------------------
130-Check-Files-Dir.
*>------------------------------------------------
*> Check if ./files/ exists
*> if not; create it.
*> create IDX files.
*>------------------------------------------------
   SET TBL-Index TO TBL-dir-files.
    MOVE TBL-Path-Name(TBL-Index)
      TO WS-Path-Name.
    PERFORM Check-File-Exist.
    IF  TBL-Presence(TBL-Index) = "PRESENT"
        MOVE "./files directory exists"
          TO Msg OF WS-Log-Line
    ELSE
        CALL "C$MAKEDIR"
          USING WS-Path-Name
        END-CALL
        MOVE "./files directory created"
          TO Msg OF WS-Log-Line
    END-IF.

    MOVE WS-Log-Line(1:60)
      TO WS-Message-Line-4.

    WRITE Log-File-Printline
      FROM WS-Log-Line
      AFTER ADVANCING 1
    END-WRITE.

*>------------------------------------------------
Check-File-Exist.
*>------------------------------------------------
    CALL "CBL_CHECK_FILE_EXIST"
        USING WS-Path-Name
              File-Info        *> from File-Info.cpy
    END-CALL.

    IF  Return-Code = ZERO
        MOVE "PRESENT"
          TO TBL-Presence (TBL-Index)
    ELSE
        MOVE "ABSENT"
          TO TBL-Presence (TBL-Index)
    END-IF.

*>------------------------------------------------
999-Status-Handler.
*>------------------------------------------------
COPY "./copybooks/FileStat-Msgs.cpy".

END PROGRAM PGM00.
