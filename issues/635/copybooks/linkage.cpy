*> Common storage areas to be used in all CALLs
*> Place in Working-Storage Section in CALLing program
*> Place in Linkage Section in CALLed program
01  WS-Option                   PIC 9(01).
01  WS-Message-Line-1           PIC X(60).
01  WS-Message-Line-2           PIC X(60).
01  WS-Message-Line-3           PIC X(60).
01  WS-Message-Line-4           PIC X(60).
01  WS-Message-Line-5           PIC X(60).
01  WS-Message-Line-6           PIC X(60).
01  WS-Message-Line-7           PIC X(60).
01  WS-Message-Line-8           PIC X(60).
01  WS-Return                   PIC 9(10).
01  WS-Return-Msg               PIC X(80).
