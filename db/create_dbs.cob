       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. CREATE-DBS.
       AUTHOR. JOHN CHIRPICH.                                                   
       DATE-WRITTEN. 10-17-2024.                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SPECIAL-NAMES.                                                           
       SOURCE-COMPUTER. X86.                                                    
       INPUT-OUTPUT SECTION.                                                    
           FILE-CONTROL.                                                        
           SELECT FC-CAMPERS ASSIGN TO 'campers.dat'                       
           ORGANIZATION IS INDEXED                                              
           ACCESS MODE IS DYNAMIC                                               
           RECORD KEY IS FD-CAMPER-TROOP.                                       
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD FC-CAMPERS.                                                           
       01 FD-CAMPER.                                                            
          88 EOF VALUE HIGH-VALUE.                                              
           02 FD-CAMPER-TROOP.                                                  
                03 FD-CAMPER-AREA PIC 9.                                        
                03 FD-CAMPER-UNIT PIC 999.                                      
           02 FD-SIGNED-UP.                                                     
                03 FD-ADULTS PIC 99.                                           
                03 FD-YOUTH PIC 99.                                            
           02 FD-ACTUAL.                                                        
                03 FD-ACTAUL-ADULTS PIC 99.                                    
                03 FD-ACTUAL-YOUTH PIC 99. 
       PROCEDURE DIVISION.
       CREATE-DBS.
          OPEN OUTPUT FC-CAMPERS
          CLOSE FC-CAMPERS
          DISPLAY "CREATED CAMPERS DB"

          STOP RUN.
