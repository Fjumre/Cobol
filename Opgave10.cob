>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. OPGAVE10.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Bankfil
        ASSIGN TO "Banker.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT Transfil
        ASSIGN TO "Transaktioner.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT UdFil
        ASSIGN TO "Kontoudskrifter.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.

*> -------- BANKER --------
FD Bankfil.
01 RAW-BANK        PIC X(130).      *> 4+30+50+15+30 = 129
01 BANK-REC REDEFINES RAW-BANK.
   COPY "BANKER.cpy".

*> -------- TRANSAKTIONER --------
FD Transfil.
01 RAW-TRANS       PIC X(215).      *> 15+30+50+11+14+6+15+4+20+20+26 = 211
01 TRANS-REC REDEFINES RAW-TRANS.
   COPY "TRANSAKTIONER.cpy".

*> -------- OUTPUT --------
FD UdFil.
01 OUT-REC.
   02 OUT-TEXT     PIC X(200).

WORKING-STORAGE SECTION.

01 EOF-BANK        PIC X VALUE "N".
01 EOF-TRANS       PIC X VALUE "N".

*> Array til banker
01 ANTAL-BANKER    PIC 9(3) VALUE 0.
01 IDX-BANK        PIC 9(3) VALUE 0.


01 BANK-TABEL.
   02 BANK-POST OCCURS 100 TIMES.
      03 T-REG-NR        PIC X(4).
      03 T-BANKNAVN      PIC X(30).
      03 T-BANKADRESSE   PIC X(51).
      03 T-TELEFON       PIC X(15).
      03 T-EMAIL         PIC X(30).

*> Til opslag under behandling
01 AKT-BANKNAVN      PIC X(30).
01 AKT-BANKADRESSE   PIC X(51).
01 AKT-TELEFON       PIC X(15).
01 AKT-EMAIL         PIC X(30).
01 WS-REG-KEY        PIC X(6).

*> Kontogruppering
01 SIDSTE-KONTO-ID   PIC X(14) VALUE SPACES.

*> Startsaldo pr. konto (DKK)
01 START-SALDO-DKK   PIC S9(13)V99 VALUE 50000.00.

*> Beløb (numerisk) og i DKK
01 BELØB-NUM        PIC S9(11)V99.
01 BELØB-DKK-NUM    PIC S9(13)V99.

*> Totals i DKK per konto
01 TOTAL-IN-NUM     PIC S9(13)V99 VALUE 0.
01 TOTAL-UD-NUM     PIC S9(13)V99 VALUE 0.
01 SALDO-NUM        PIC S9(13)V99 VALUE 0.

*> Editerede beløb til udskrift
01 BELØB-ORG-EDIT   PIC ZZ,ZZZ,ZZ9.99.
01 BELØB-DKK-EDIT   PIC ZZ,ZZZ,ZZ9.99.
01 TOTAL-IN-EDIT    PIC ZZ,ZZZ,ZZ9.99.
01 TOTAL-UD-EDIT    PIC ZZ,ZZZ,ZZ9.99.
01 SALDO-EDIT       PIC ZZ,ZZZ,ZZ9.99.

PROCEDURE DIVISION.
    *> 1) Læs alle banker ind i array
    OPEN INPUT Bankfil

    MOVE "N" TO EOF-BANK
    PERFORM UNTIL EOF-BANK = "Y"
        READ Bankfil
            AT END
                MOVE "Y" TO EOF-BANK
            NOT AT END
                ADD 1 TO ANTAL-BANKER
                IF ANTAL-BANKER <= 100
                    MOVE REG-NR       OF BANK-REC TO T-REG-NR      (ANTAL-BANKER)
                    MOVE BANKNAVN     OF BANK-REC TO T-BANKNAVN    (ANTAL-BANKER)
                    MOVE BANKADRESSE  OF BANK-REC TO T-BANKADRESSE (ANTAL-BANKER)
                    MOVE TELEFON      OF BANK-REC TO T-TELEFON     (ANTAL-BANKER)
                    MOVE EMAIL        OF BANK-REC TO T-EMAIL       (ANTAL-BANKER)
                END-IF
        END-READ
    END-PERFORM

    CLOSE Bankfil

    *> 2) Behandl transaktioner og skriv kontoudskrifter
    OPEN INPUT  Transfil
         OUTPUT UdFil

    MOVE "N"    TO EOF-TRANS
    MOVE SPACES TO SIDSTE-KONTO-ID

       PERFORM UNTIL EOF-TRANS = "Y"
        READ Transfil
            AT END
                MOVE "Y" TO EOF-TRANS
            NOT AT END
                *> Spring helt over tomme linjer (ingen konto, ingen navn)
                IF KONTO-ID = SPACES AND NAVN = SPACES
                    CONTINUE
                ELSE
                    *> Ny konto?
                    IF KONTO-ID NOT = SIDSTE-KONTO-ID

                        *> Hvis det IKKE er første konto: skriv totals for forrige konto
                        IF SIDSTE-KONTO-ID NOT = SPACES
                            PERFORM SKRIV-TOTAL-LINJER

                            *> Blank linje mellem kontoudskrifter
                            MOVE SPACES TO OUT-TEXT
                            WRITE OUT-REC
                            WRITE OUT-REC
                        END-IF

                        MOVE KONTO-ID TO SIDSTE-KONTO-ID

                        *> Reset totals for ny konto
                        MOVE 0              TO TOTAL-IN-NUM
                                             TOTAL-UD-NUM
                        MOVE START-SALDO-DKK TO SALDO-NUM

                        *> Skriv konto-header + bankinfo
                        PERFORM SKRIV-KONTO-HEADER
                    END-IF

                    *> Behandl én transaktionslinje
                    PERFORM BEHANDL-TRANS-LINJE
                END-IF
        END-READ
    END-PERFORM


    *> Efter løkken: skriv totals for sidste konto (hvis der var nogen)
    IF SIDSTE-KONTO-ID NOT = SPACES
        PERFORM SKRIV-TOTAL-LINJER
    END-IF

    CLOSE Transfil UdFil
    STOP RUN.

*>-----------------------------------------------------------------
*>  SKRIV-KONTO-HEADER – én gang pr. konto
*>-----------------------------------------------------------------
SKRIV-KONTO-HEADER.
    *> Forbered REG-NR nøgle (trim alle spaces)
    MOVE FUNCTION TRIM(REG-NR OF TRANS-REC) TO WS-REG-KEY

    *> Find bank på REG-NR
    PERFORM FIND-BANK

    MOVE SPACES TO OUT-TEXT
    STRING "--------------------------------------------------------"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "Kunde: "  DELIMITED BY SIZE
           NAVN       DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "Adresse: " DELIMITED BY SIZE
           ADRESSE     DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "Kontoudskrift for kontonr.: "
           DELIMITED BY SIZE
           KONTO-ID DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "     Registreringsnummer: "
           DELIMITED BY SIZE
           REG-NR OF TRANS-REC DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "     Bank: " DELIMITED BY SIZE
           AKT-BANKNAVN DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "     Bankadresse: " DELIMITED BY SIZE
           AKT-BANKADRESSE     DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "     Telefon: " DELIMITED BY SIZE
           AKT-TELEFON       DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING "     E-mail: " DELIMITED BY SIZE
           AKT-EMAIL        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    *> Overskrift for transaktioner
    MOVE SPACES TO OUT-TEXT
    STRING
        "Dato/Tid                  "
        "Transaktionstype     "
        "Beløb (DKK)   "
        "Beløb (valuta)   "
        "Valuta   "
        "Butik"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC
    .


*>-----------------------------------------------------------------
*>  BEHANDL-TRANS-LINJE – én linje fra Transaktioner.txt
*>-----------------------------------------------------------------
BEHANDL-TRANS-LINJE.
    *> 1) Konverter til DKK
    PERFORM CONVERT-TO-DKK

    *> 2) Opdatér totals i DKK
    IF BELØB-DKK-NUM > 0
        ADD BELØB-DKK-NUM TO TOTAL-IN-NUM
    ELSE
        ADD BELØB-DKK-NUM TO TOTAL-UD-NUM
    END-IF

    ADD BELØB-DKK-NUM TO SALDO-NUM

    *> 3) Formater beløb til tekst
    MOVE FUNCTION NUMVAL(BELØB-TEXT) TO BELØB-NUM
    MOVE BELØB-NUM     TO BELØB-ORG-EDIT
    MOVE BELØB-DKK-NUM TO BELØB-DKK-EDIT

    MOVE SPACES TO OUT-TEXT
    STRING
        TIDSPUNKT           DELIMITED BY SIZE
        "  "                DELIMITED BY SIZE
        TRANSAKTIONSTYPE    DELIMITED BY SIZE
        "  "                DELIMITED BY SIZE
        BELØB-DKK-EDIT      DELIMITED BY SIZE
        "  "                DELIMITED BY SIZE
        BELØB-ORG-EDIT      DELIMITED BY SIZE
        "  "                DELIMITED BY SIZE
        VALUTA              DELIMITED BY SIZE
        "  "                DELIMITED BY SIZE
        BUTIK               DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC
    .

*>-----------------------------------------------------------------
*>  SKRIV-TOTAL-LINJER – totals for én konto
*>-----------------------------------------------------------------
SKRIV-TOTAL-LINJER.
    MOVE TOTAL-IN-NUM TO TOTAL-IN-EDIT
    MOVE TOTAL-UD-NUM TO TOTAL-UD-EDIT
    MOVE SALDO-NUM    TO SALDO-EDIT

    MOVE SPACES TO OUT-TEXT
    STRING
        "Totalt indbetalt (DKK):  "
        TOTAL-IN-EDIT DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING
        "Totalt udbetalt (DKK):   "
        TOTAL-UD-EDIT DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING
        "Saldo (DKK):             "
        SALDO-EDIT DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING
        "Med venlig hilsen"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING
        AKT-BANKNAVN DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC
    .

*>-----------------------------------------------------------------
*>  FIND-BANK – opslag på REG-NR i BANK-TABEL
*>-----------------------------------------------------------------
FIND-BANK.
    MOVE SPACES TO AKT-BANKNAVN
                   AKT-BANKADRESSE
                   AKT-TELEFON
                   AKT-EMAIL

    MOVE 1 TO IDX-BANK

    PERFORM UNTIL IDX-BANK > ANTAL-BANKER
        IF FUNCTION TRIM(T-REG-NR(IDX-BANK)) =
           FUNCTION TRIM(WS-REG-KEY)
            MOVE T-BANKNAVN    (IDX-BANK) TO AKT-BANKNAVN
            MOVE T-BANKADRESSE (IDX-BANK) TO AKT-BANKADRESSE
            MOVE T-TELEFON     (IDX-BANK) TO AKT-TELEFON
            MOVE T-EMAIL       (IDX-BANK) TO AKT-EMAIL
            EXIT PERFORM
        END-IF
        ADD 1 TO IDX-BANK
    END-PERFORM
    .



*>-----------------------------------------------------------------
*>  CONVERT-TO-DKK – konverter BELØB-TEXT til DKK
*>-----------------------------------------------------------------
CONVERT-TO-DKK.
    *> BELØB-TEXT er fx '        -1234.56'
    MOVE FUNCTION NUMVAL(BELØB-TEXT) TO BELØB-NUM

    EVALUATE FUNCTION TRIM(VALUTA)
        WHEN "USD"
            COMPUTE BELØB-DKK-NUM = BELØB-NUM * 6.8
        WHEN "EUR"
            COMPUTE BELØB-DKK-NUM = BELØB-NUM * 7.5
        WHEN OTHER
            *> DKK eller andet => ingen konvertering
            MOVE BELØB-NUM TO BELØB-DKK-NUM
    END-EVALUATE
    .
END PROGRAM OPGAVE10.
