*> File-Info.cpy
*> CALL "C$FILEINFO" USING filepath file-info

01  File-Info.
    05  File-Size-In-Bytes    PIC 9(18) COMP.
*> Modification date
    05  Mod-DD                PIC 9(02) COMP.
    05  Mod-MO                PIC 9(02) COMP.
    05  Mod-YYYY              PIC 9(04) COMP.
*> Modification time
    05  Mod-HH                PIC 9(02) COMP.
    05  Mod-MM                PIC 9(02) COMP.
    05  MOD-SS                PIC 9(02) COMP.
    05  Mod-00                PIC 9(02) COMP.  *> always 00
