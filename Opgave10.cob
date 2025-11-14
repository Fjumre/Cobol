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
        ASSIGN TO "TransaktionerOut.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.

*> -------- BANKER --------
FD Bankfil.
01 RAW-BANK        PIC X(129).      *> 4+30+50+15+30 = 129
01 BANK-REC REDEFINES RAW-BANK.
   COPY "BANKER.cpy".

*> -------- TRANSAKTIONER --------
FD Transfil.
01 RAW-TRANS       PIC X(199).      *> 15+30+50+10+15+4+11+3+15+20+26 = 199
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
   02 BANK-POST OCCURS 50 TIMES.
      03 T-REG-NR        PIC X(4).
      03 T-BANKNAVN      PIC X(30).
      03 T-BANKADRESSE   PIC X(50).
      03 T-TELEFON       PIC X(15).
      03 T-EMAIL         PIC X(30).

*> Til opslag under behandling
01 AKT-BANKNAVN      PIC X(30).
01 AKT-BANKADRESSE   PIC X(50).

*> Kontogruppering
01 SIDSTE-KONTO-ID   PIC X(15) VALUE SPACES.

*> Lidt til formatering af beløb
01 BELØB-EDIT       PIC ZZ.ZZZ.ZZ9,99.

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
                MOVE REG-NR       TO T-REG-NR      (ANTAL-BANKER)
                MOVE BANKNAVN     TO T-BANKNAVN    (ANTAL-BANKER)
                MOVE BANKADRESSE  TO T-BANKADRESSE (ANTAL-BANKER)
                MOVE TELEFON      TO T-TELEFON     (ANTAL-BANKER)
                MOVE EMAIL        TO T-EMAIL       (ANTAL-BANKER)
        END-READ
    END-PERFORM

    CLOSE Bankfil

    *> 2) Behandl transaktioner og skriv rapport
    OPEN INPUT  Transfil
         OUTPUT UdFil

    MOVE "N" TO EOF-TRANS
    MOVE SPACES TO SIDSTE-KONTO-ID

    PERFORM UNTIL EOF-TRANS = "Y"
        READ Transfil
            AT END
                MOVE "Y" TO EOF-TRANS
            NOT AT END

                *> Ny konto? Så skriv konto-header og bankinfo
                IF KONTO-ID NOT = SIDSTE-KONTO-ID
                    MOVE KONTO-ID TO SIDSTE-KONTO-ID

                    *> Blank linje mellem konti
                    MOVE SPACES TO OUT-TEXT
                    WRITE OUT-REC

                    *> Konto-header
                    MOVE SPACES TO OUT-TEXT
                    STRING
                        "Konto: "       DELIMITED BY SIZE
                        KONTO-ID        DELIMITED BY SIZE
                    INTO OUT-TEXT
                    END-STRING
                    WRITE OUT-REC

                    MOVE SPACES TO OUT-TEXT
                    STRING
                        "Kunde: "       DELIMITED BY SIZE
                        NAVN           DELIMITED BY SIZE
                        "  (CPR: "     DELIMITED BY SIZE
                        CPR            DELIMITED BY SIZE
                        ")"            DELIMITED BY SIZE
                    INTO OUT-TEXT
                    END-STRING
                    WRITE OUT-REC

                    *> Find bank vha REG-NR
                    PERFORM FIND-BANK

                    MOVE SPACES TO OUT-TEXT
                    STRING
                        "Bank: "          DELIMITED BY SIZE
                        AKT-BANKNAVN      DELIMITED BY SIZE
                    INTO OUT-TEXT
                    END-STRING
                    WRITE OUT-REC

                    MOVE SPACES TO OUT-TEXT
                    STRING
                        "Adresse: "       DELIMITED BY SIZE
                        AKT-BANKADRESSE   DELIMITED BY SIZE
                    INTO OUT-TEXT
                    END-STRING
                    WRITE OUT-REC

                    *> Overskrift til transaktioner
                    MOVE SPACES TO OUT-TEXT
                    STRING
                        "  Dato/Tid                Type            Beløb        Valuta   Butik"
                        DELIMITED BY SIZE
                    INTO OUT-TEXT
                    END-STRING
                    WRITE OUT-REC
                END-IF

                *> 3) Skriv selve transaktionen for denne konto
                MOVE BELOEB TO BELOEB-EDIT

                MOVE SPACES TO OUT-TEXT
                STRING
                    "  "                      DELIMITED BY SIZE
                    TIDSPUNKT                 DELIMITED BY SIZE
                    "  "                      DELIMITED BY SIZE
                    TRANSAKTIONSTYPE          DELIMITED BY SIZE
                    "  "                      DELIMITED BY SIZE
                    BELOEB-EDIT               DELIMITED BY SIZE
                    "  "                      DELIMITED BY SIZE
                    VALUTA                    DELIMITED BY SIZE
                    "  "                      DELIMITED BY SIZE
                    BUTIK                     DELIMITED BY SIZE
                INTO OUT-TEXT
                END-STRING
                WRITE OUT-REC

        END-READ
    END-PERFORM

    CLOSE Transfil UdFil
    STOP RUN.


*> =====================================================
*> FIND-BANK – opslag på REG-NR i BANK-TABEL
*> =====================================================
FIND-BANK.
    MOVE SPACES TO AKT-BANKNAVN AKT-BANKADRESSE
    MOVE 1 TO IDX-BANK

    PERFORM UNTIL IDX-BANK > ANTAL-BANKER
        IF T-REG-NR(IDX-BANK) = REG-NR
            MOVE T-BANKNAVN    (IDX-BANK) TO AKT-BANKNAVN
            MOVE T-BANKADRESSE (IDX-BANK) TO AKT-BANKADRESSE
            EXIT PERFORM
        END-IF
        ADD 1 TO IDX-BANK
    END-PERFORM
    .
END PROGRAM OPGAVE10.
