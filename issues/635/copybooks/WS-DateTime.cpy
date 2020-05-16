*>Returns the current date and time as the following 21-character structure:
01  CURRENT-DATE-AND-TIME.
    05  CDT-Date.
        10  CDT-Year                        PIC 9(4).
        10  CDT-Month                       PIC 9(2). *> 01-12
        10  CDT-Day                         PIC 9(2). *> 01-31
    05  CDT-Time.
        10  CDT-Hour                        PIC 9(2). *> 00-23
        10  CDT-Minutes                     PIC 9(2). *> 00-59
        10  CDT-Seconds                     PIC 9(2). *> 00-59
        10  CDT-Hundredths-Of-Secs          PIC 9(2). *> 00-99
        10  CDT-GMT-Diff-Hours              PIC S9(2)
            SIGN LEADING SEPARATE.
        10 CDT-GMT-Diff-Minutes             PIC 9(2). *> 00 or 30
