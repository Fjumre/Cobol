>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. OPGAVE8.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Kundefil
        ASSIGN TO "Kundeoplysninger.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT Kontofil
        ASSIGN TO "KontoOpl.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT UdFil
        ASSIGN TO "KundeoplysningerOut.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.

FILE SECTION.

FD Kundefil.
01 RAW-KUNDE PIC X(269).
01 KUNDEOPL REDEFINES RAW-KUNDE.
   COPY "KUNDER.cpy".

FD Kontofil.
01 RAW-KONTO PIC X(52).
01 KONTO-REC REDEFINES RAW-KONTO.
   COPY "KONTOOPL.cpy".

FD UdFil.
01 UDLINJE.
   02 OUT-TEXT PIC X(200).

WORKING-STORAGE SECTION.
01 EOF-KUNDE   PIC X VALUE "N".
01 EOF-KONTO   PIC X VALUE "N".

PROCEDURE DIVISION.
    OPEN INPUT  Kundefil
         INPUT  Kontofil
         OUTPUT UdFil

    *> ==============================================
    *> PROCESS ALL CUSTOMERS
    *> ==============================================
    PERFORM UNTIL EOF-KUNDE = "Y"

        READ Kundefil
            AT END MOVE "Y" TO EOF-KUNDE
            NOT AT END

                *> -------------------------
                *> OUTPUT CUSTOMER HEADER
                *> -------------------------
                PERFORM WRITE-CUSTOMER-BLOCK

                *> -------------------------
                *> PROCESS MATCHING ACCOUNTS
                *> -------------------------
                MOVE "N" TO EOF-KONTO
                CLOSE Kontofil
                OPEN INPUT Kontofil

                PERFORM UNTIL EOF-KONTO = "Y"
                    READ Kontofil
                        AT END
                            MOVE "Y" TO EOF-KONTO
                        NOT AT END
                            IF KUNDE-ID OF KONTO-REC =
                            KUNDEID  OF KUNDEOPL
                                PERFORM WRITE-ACCOUNT-BLOCK
                            END-IF
                    END-READ
                END-PERFORM


                *> Blank line after each customer
                MOVE SPACES TO OUT-TEXT
                WRITE UDLINJE

        END-READ
    END-PERFORM

    CLOSE Kundefil Kontofil UdFil
    STOP RUN.

*> =====================================================
*>  WRITE CUSTOMER DATA BLOCK
*> =====================================================
WRITE-CUSTOMER-BLOCK.
    MOVE SPACES TO OUT-TEXT
    STRING "Kunde-ID: " FUNCTION TRIM(KUNDEID OF KUNDEOPL)
        INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE

    MOVE SPACES TO OUT-TEXT
    STRING "Navn: " FUNCTION TRIM(FORNAVN OF KUNDEOPL)
           " " FUNCTION TRIM(EFTERNAVN OF KUNDEOPL)
        INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE

    MOVE SPACES TO OUT-TEXT
    STRING "Adresse: "
           FUNCTION TRIM(VEJNAVN OF ADRESSE OF KUNDEOPL)
           " " FUNCTION TRIM(HUSNR OF ADRESSE OF KUNDEOPL)
        INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE

    MOVE SPACES TO OUT-TEXT
    STRING FUNCTION TRIM(BYNAVN OF ADRESSE OF KUNDEOPL)
           " " FUNCTION TRIM(POSTNR OF ADRESSE OF KUNDEOPL)
           " (" FUNCTION TRIM(LANDKODE OF ADRESSE OF KUNDEOPL) ")"
        INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE

    MOVE SPACES TO OUT-TEXT
    STRING "Telefon: " FUNCTION TRIM(TELEFON OF KONTAKTOPL OF KUNDEOPL)
        INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE

    MOVE SPACES TO OUT-TEXT
    STRING "Email: " FUNCTION TRIM(EMAIL OF KONTAKTOPL OF KUNDEOPL)
        INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE
    .

*> =====================================================
*>  WRITE ACCOUNT BLOCK
*> =====================================================
WRITE-ACCOUNT-BLOCK.
    MOVE SPACES TO OUT-TEXT
    STRING
        "Konto: "
        FUNCTION TRIM(KONTO-ID   OF KONTO-REC) DELIMITED BY SIZE
        "  "                                   DELIMITED BY SIZE
        FUNCTION TRIM(KONTO-TYPE OF KONTO-REC) DELIMITED BY SIZE
        "  "                                   DELIMITED BY SIZE
        FUNCTION TRIM(BALANCE    OF KONTO-REC) DELIMITED BY SIZE
        " "                                    DELIMITED BY SIZE
        FUNCTION TRIM(VALUTA-KD  OF KONTO-REC) DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE UDLINJE
    .

