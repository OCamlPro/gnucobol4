*>COPY "./copybooks/Log-File-WS.cpy".
01  WS-StatusLOG                PIC X(02).

01  WS-Log-Line.
    05  Date-MM                 PIC X(02) VALUE SPACE.
    05                          PIC X(01) VALUE "/".
    05  Date-DD                 PIC X(02) VALUE SPACE.
    05                          PIC X(01) VALUE "/".
    05  Date-YYYY               PIC X(04) VALUE SPACE.
    05                          PIC X(01) VALUE SPACE.
    05  Time-HH                 PIC X(02) VALUE SPACE.
    05                          PIC X(01) VALUE ":".
    05  Time-MM                 PIC X(02) VALUE SPACE.
    05                          PIC X(01) VALUE ".".
    05  Time-SS                 PIC X(02) VALUE SPACE.
    05                          PIC X(01) VALUE ".".
    05  Time-HS                 PIC X(02) VALUE SPACE.
    05                          PIC X(01) VALUE SPACE.
    05  Pgm-ID                  PIC X(10) VALUE SPACE.
    05                          PIC X(01) VALUE SPACE.
    05  Msg                     PIC X(66) VALUE SPACE.
