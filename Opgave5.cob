>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVE5.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  KUNDEOPL.     
           COPY "KUNDER".
       01  INDEX1      PIC 99 VALUE 1.
       01  INDEX2      PIC 99 VALUE 1.
       01  CURRENTCHAR PIC X VALUE SPACE.
       01  PREVIOUSCHAR PIC X VALUE SPACE.
       01  CLEANNAME   PIC X(41) VALUE SPACES.
           
       
       

       PROCEDURE DIVISION.
           MOVE 1234567890            TO KUNDEID OF KUNDEOPL
           MOVE "LARS"                TO FORNAVN OF KUNDEOPL
           MOVE "MADSEN"              TO EFTERNAVN OF KUNDEOPL

           *> Byg NAVN pænt (trim giver ingen overskydende spaces)
           STRING
               FUNCTION TRIM(FORNAVN OF KUNDEOPL)   DELIMITED BY SIZE
               " "                                  DELIMITED BY SIZE
               FUNCTION TRIM(EFTERNAVN OF KUNDEOPL) DELIMITED BY SIZE
           INTO NAVN OF KUNDEOPL
           END-STRING

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

         
           MOVE "DKK0000000001111111" TO KONTONUMMER OF KONTOOPL
           MOVE 9.50                  TO BALANCE     OF KONTOOPL
           MOVE "EUR"                 TO VALUTAKODE  OF KONTOOPL

           MOVE "Hovedgade"           TO VEJNAVN     OF ADRESSE
           MOVE "12A"                 TO HUSNR       OF ADRESSE
           MOVE "2"                   TO ETAGE       OF ADRESSE
           MOVE "TV"                  TO SIDE        OF ADRESSE
           MOVE "København"           TO BYNAVN      OF ADRESSE
           MOVE 2100                  TO POSTNR      OF ADRESSE   
           MOVE "DK"                  TO LANDKODE    OF ADRESSE

           MOVE "12345678"            TO TELEFON     OF KONTAKTOPL
           MOVE "lars.madsen@example.com"
                                       TO EMAIL      OF KONTAKTOPL
           
           
           DISPLAY "-------------------------------".
           DISPLAY "Kunde-ID: "                KUNDEID OF KUNDEOPL
           DISPLAY "Navn: "                    NAVN OF KUNDEOPL
           DISPLAY "Adresse: "                 VEJNAVN OF ADRESSE " " HUSNR OF ADRESSE
           DISPLAY "           "               POSTNR OF ADRESSE " " BYNAVN OF ADRESSE " (" LANDKODE OF ADRESSE ")"
           DISPLAY "Konto: "                   KONTONUMMER OF KONTOOPL
           DISPLAY "Balance/valuta: "          BALANCE OF KONTOOPL " " VALUTAKODE OF KONTOOPL
           DISPLAY "Kontakt: "                 TELEFON OF KONTAKTOPL " / " EMAIL OF KONTAKTOPL


           DISPLAY "-------------------------------".
    
           STOP RUN.
