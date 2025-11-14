       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVE3.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  KUNDEID     PIC 9(10) VALUE ZEROS.
       01  FORNAVN     PIC X(20) VALUE SPACES.
       01  EFTERNAVN   PIC X(20) VALUE SPACES.
       01  NAVN        PIC X(41) VALUE SPACES.
       01  INDEX1      PIC 99 VALUE 1.
       01  INDEX2      PIC 99 VALUE 1.
       01  CURRENTCHAR PIC X VALUE SPACE.
       01  PREVIOUSCHAR PIC X VALUE SPACE.
       01  CLEANNAME   PIC X(41) VALUE SPACES.
       01  KONTONUMMER PIC X(30) VALUE SPACES.
       01  BALANCE     PIC S9(7)V99 VALUE ZEROS.
       01  VALUTAKODE  PIC X(3) VALUE SPACES. 
       
       

       PROCEDURE DIVISION.
           MOVE 1234567890 TO KUNDEID.
           MOVE "LARS              " TO FORNAVN.
           MOVE "MADSEN" TO EFTERNAVN.
           MOVE "DKK0000000001111111" TO KONTONUMMER.
           MOVE 9.50 TO BALANCE.
           MOVE "EUR" TO VALUTAKODE.
           
           STRING
           FORNAVN      DELIMITED BY SPACE
           " "          DELIMITED BY SIZE
           EFTERNAVN    DELIMITED BY SPACE
           INTO NAVN
           END-STRING.


           PERFORM VARYING INDEX1 FROM 1 BY 1 UNTIL INDEX1 > LENGTH OF 
           NAVN
               MOVE NAVN(INDEX1:1) TO CURRENTCHAR
               IF CURRENTCHAR NOT = " "
               MOVE CURRENTCHAR TO CLEANNAME(INDEX2:1)
               ADD 1 TO INDEX2
                 ELSE
               IF PREVIOUSCHAR NOT = SPACE AND INDEX2 > 1
            MOVE SPACE TO CLEANNAME(INDEX2:1)
            ADD 1 TO INDEX2
           END-IF
           END-IF
           MOVE CURRENTCHAR TO PREVIOUSCHAR
               END-PERFORM.
               MOVE CLEANNAME TO NAVN.


           DISPLAY "-------------------------------".
           DISPLAY "Kunde-ID: " KUNDEID.
           DISPLAY "Fornavn og Efternavn: " NAVN.
           DISPLAY "Kontonummer: " KONTONUMMER.
           DISPLAY "Balance og valutakode: " BALANCE " " VALUTAKODE.
           DISPLAY "-------------------------------".
           DISPLAY "CURRENTCHAR: " CURRENTCHAR.
           DISPLAY "PREVIOUSCHAR: " PREVIOUSCHAR.
           STOP RUN.
