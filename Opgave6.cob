>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. OPGAVE6.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Kundeoplysninger ASSIGN TO "Kundeoplysninger.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD  Kundeoplysninger.
01  KUNDEOPL.
    COPY "KUNDER".  

WORKING-STORAGE SECTION.
01  EOF-FLAG          PIC X     VALUE "N".
01  INDEX1            PIC 99    VALUE 1.
01  INDEX2            PIC 99    VALUE 1.
01  CURRENTCHAR       PIC X     VALUE SPACE.
01  PREVIOUSCHAR      PIC X     VALUE SPACE.
01  CLEANNAME         PIC X(41) VALUE SPACES.

PROCEDURE DIVISION.
    OPEN INPUT Kundeoplysninger
    PERFORM UNTIL EOF-FLAG = "Y"
        READ Kundeoplysninger
            AT END
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                *> Trim and build full name
                STRING
                    FUNCTION TRIM(FORNAVN OF KUNDEOPL)   DELIMITED BY SIZE
                    " "                                  DELIMITED BY SIZE
                    FUNCTION TRIM(EFTERNAVN OF KUNDEOPL) DELIMITED BY SIZE
                INTO NAVN OF KUNDEOPL
                END-STRING

                DISPLAY "-------------------------------"
                DISPLAY "Kunde-ID: "         KUNDEID OF KUNDEOPL
                DISPLAY "Navn: "             NAVN OF KUNDEOPL
                DISPLAY "Adresse: "          VEJNAVN OF ADRESSE " " HUSNR OF ADRESSE
                DISPLAY "           "        POSTNR OF ADRESSE " " BYNAVN OF ADRESSE
                                            " (" LANDKODE OF ADRESSE ")"
                DISPLAY "Konto: "            KONTONUMMER OF KONTOOPL
                DISPLAY "Balance/valuta: "   BALANCE OF KONTOOPL " " VALUTAKODE OF KONTOOPL
                DISPLAY "Kontakt: "          TELEFON OF KONTAKTOPL " / " EMAIL OF KONTAKTOPL
                DISPLAY "-------------------------------"
        END-READ
    END-PERFORM

    CLOSE Kundeoplysninger
    STOP RUN.
END PROGRAM OPGAVE6.
