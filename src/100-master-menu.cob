       IDENTIFICATION DIVISION.
       PROGRAM-ID. 100-MASTER-MENU.
       AUTHOR JOHN CHIRPICH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MENU-SEL2X PIC XX.
       01 MENU-SUB2 REDEFINES MENU-SEL2X PIC 99.
       01 MENU-SUB1 PIC 99.
       01 USER-SEL PIC XXXX.
       01 KEY-STATUS PIC 9999. 
       01 WS-MSG PIC X(80).
       01 WS-RETURN pic 9.
       SCREEN SECTION.
       01 CLEAR-SCREEN.
           05 BLANK SCREEN.
       01 SS-MASTER-MENU.
           05 LINE 1 COL 35 VALUE IS "MASTER MENU".
           05 LINE 3 COL 1 VALUE IS "ENTER SELECTION:".
           05 PIC XXXX USING USER-SEL.
           05 LINE 5 COL 10 VALUE IS "1. CHECK IN MENU".
           05 LINE 6 COL 10 VALUE IS "2. ADD CAMP/ERS MENU".
           05 LINE 7 COL 10 VALUE IS "3. REPORTS MENU".
           05 LINE 8 COL 10 VALUE IS "4. FILE MAINTNENCE".
           05 LINE 9 COL 10 VALUE IS "5. AUTO CAMPSITE".
           05 LINE 23 COL 1 VALUE IS "F3 - BACK   F9 - PROCESS".
       01 SS-100-ADD-MENU.
           05 LINE 1 COL 35 VALUE IS "ADD CAMP/ERS MENU".
           05 LINE 3 COL 1 VALUE IS "ENTER SEELECTION:".
           05 PIC XX USING MENU-SEL2X.
           05 LINE 5 COL 10 VALUE IS "1. ADD CAMPSITES".
           05 LINE 6 COL 10 VALUE IS "2. ADD CAMPERS".
       01 SS-MESSAGE.
           05 LINE 24 COL 1 PIC X(80) USING WS-MSG
           FOREGROUND-COLOR IS 2.
       PROCEDURE DIVISION.
        100-MASTER-MENU.
           DISPLAY SS-MESSAGE
           DISPLAY SS-MASTER-MENU
           ACCEPT SS-MASTER-MENU
           IF KEY-STATUS = 1009 OR KEY-STATUS = 0000
                   PERFORM 101-PARSE-SEL
                   PERFORM 102-BOUNCER   
           END-IF
           PERFORM 100-MASTER-MENU.
        101-PARSE-SEL.
           UNSTRING USER-SEL DELIMITED BY '.' 
              INTO MENU-SUB1 MENU-SUB2
      *       IF MENU-SUB1 IS NOT NUMERIC 
      *          OR MENU-SUB2 IS NOT NUMERIC
                 if MENU-SUB1 > 5
                       MOVE "INVALID MENU" TO WS-MSG
      *                 move menu-sub1 to ws-msg
                        PERFORM 100-MASTER-MENU
              END-IF.

        102-BOUNCER.
           EVALUATE MENU-SUB1
              WHEN 2
                      PERFORM 122-ADD-MENU.     
       122-ADD-MENU. 
           DISPLAY CLEAR-SCREEN    
           EVALUATE MENU-SUB2
              WHEN ZEROES
                      DISPLAY SS-MESSAGE
                      MOVE SPACES TO WS-MSG
                      DISPLAY SS-100-ADD-MENU
                      ACCEPT SS-100-ADD-MENU
                      PERFORM 122-ADD-MENU
      *       WHEN 1
      *               CALL '200-ADD-CAMP-MAIN' USING WS-RETURN
              WHEN 2
                      CALL '300-ADD-CAMPER' USING WS-RETURN
              WHEN OTHER
                   MOVE ZEROES TO MENU-SUB2
                   MOVE "INVALID SELECTION" TO WS-MSG
                   PERFORM 122-ADD-MENU.
              
