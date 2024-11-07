       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. 200-ADD-CAMPSITE.                                            
       AUTHOR. JOHN CHIRPICH.                                                   
       DATE-WRITTEN. OCT 31, 2024.                                              
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SPECIAL-NAMES.                                                           
           CRT STATUS IS KEY-STATUS.                                            
       SOURCE-COMPUTER. X86.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT FC-CAMPS ASSIGN TO './db/camps.dat'                           
           ORGANIZATION IS INDEXED                                              
           ACCESS MODE IS DYNAMIC                                               
           RECORD KEY IS FS-CAMP-SUBCAMP.                                       
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD FC-CAMPS.                                                             
       01 FS-CAMP.                                                              
           88 EOF VALUE HIGH-VALUE.                                             
           02 FS-CAMP-MAIN-CAMP PIC X(2).                                       
           02 FS-CAMP-SUBCAMP PIC X(15).                                        
           02 FS-CAMP-TOTAL-CAPACITY PIC 999.                                   
           02 FS-CAMP-CAPACITY-LEFT PIC 999.                                    
       WORKING-STORAGE SECTION.                                                 
       01 KEY-STATUS PIC 9999.                                                  
       01 WS-CAMP.                                                              
               02 WS-CAMP-MAIN-CAMP PIC X(2).                                   
               02 WS-CAMP-SUBCAMP PIC X(15).                                    
               02 WS-CAMP-TOTAL-CAPACITY PIC 999.                               
       01 WS-MSG PIC X(80).                                                     
                                                                                
       SCREEN SECTION.                                                          
       01 CLEAR-SCREEN.                                                         
               05 BLANK SCREEN.                                                 
       01 SS-CAMP-ENTRY AUTO.                                                   
           05 LINE 1 COL 35 VALUE IS "ADD CAMPSITE".                            
           05 LINE 12 COL 2 VALUE IS "MAIN CAMP:".                              
           05 PIC XX USING WS-CAMP-MAIN-CAMP.                                   
           05 LINE 12 COL 20                                                    
           VALUE IS " LS - LONESTAR, SM - SAWMILL, PA - PIERCING ARROW".        
           05 LINE 13 COL 2 VALUE IS "SUB CAMP:".                               
           05 PIC X(15) USING WS-CAMP-SUBCAMP.                                  
           05 LINE 14 COL 2 VALUE IS "CAPACITY:".                               
           05 PIC 999 USING WS-CAMP-TOTAL-CAPACITY.                             
       01 SS-MESSAGE.                                                           
           05 LINE 24 COL 1 PIC X(80) USING WS-MSG                              
           FOREGROUND-COLOR IS 2.                                               
       PROCEDURE DIVISION.                                                      
       200-ADD-CAMP-MAIN.    
           DISPLAY CLEAR-SCREEN
           DISPLAY SS-MESSAGE
           MOVE ZEROS TO KEY-STATUS
           MOVE SPACES TO WS-MSG
           DISPLAY SS-CAMP-ENTRY
           ACCEPT SS-CAMP-ENTRY
           IF KEY-STATUS = 1009
                   PERFORM 210-ADD-CAMPSITE        
           END-IF                           
           IF KEY-STATUS = 0000 
                   PERFORM 200-ADD-CAMP-MAIN
           END-IF
           EVALUATE KEY-STATUS
                   WHEN 1003
                           PERFORM 216-EXIT-WRAPPER
                   WHEN 0000
                           PERFORM 200-ADD-CAMP-MAIN
                   WHEN 1009
                           PERFORM 210-ADD-CAMPSITE
           END-EVALUATE.
      *    PERFORM 200-ADD-CAMP-MAIN.
       201-CLEAR-DATA.
           MOVE SPACES TO WS-CAMP-MAIN-CAMP
           MOVE SPACES TO WS-CAMP-SUBCAMP
           MOVE ZEROES TO WS-CAMP-TOTAL-CAPACITY.
       210-ADD-CAMPSITE.
           IF WS-CAMP-MAIN-CAMP IS NOT ZEROES AND
                   WS-CAMP-SUBCAMP IS NOT ZEROES AND
                   WS-CAMP-TOTAL-CAPACITY IS NOT ZEROES
           OPEN I-O FC-CAMPS
           MOVE WS-CAMP TO FS-CAMP
           WRITE FS-CAMP
           INVALID KEY MOVE "CAMP ALREADY EXISTS" TO WS-MSG
           NOT INVALID KEY MOVE "CAMP ADDED" TO WS-MSG
           END-WRITE
           CLOSE FC-CAMPS
           PERFORM 201-CLEAR-DATA
           END-IF
           PERFORM 200-ADD-CAMP-MAIN.
          
       216-EXIT-WRAPPER.
           IF KEY-STATUS = 1003
                   EVALUATE WS-CAMP-TOTAL-CAPACITY  
                           WHEN ZEROES
                                   EXIT PROGRAM
                           WHEN NOT ZEROES  
                                   PERFORM 201-CLEAR-DATA
                   END-EVALUATE
           END-IF.

