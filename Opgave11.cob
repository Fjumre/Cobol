>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. OPGAVE11.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Transfil
        ASSIGN TO "Transaktioner.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT UdFil
        ASSIGN TO "Statistik.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.

FD Transfil.
01 RAW-TRANS         PIC X(215).
01 TRANS-REC REDEFINES RAW-TRANS.
   COPY "TRANSAKTIONER.cpy".

FD UdFil.
01 OUT-REC.
   02 OUT-TEXT       PIC X(200).

WORKING-STORAGE SECTION.

01 EOF-TRANS         PIC X VALUE "N".

*> Startsaldo i DKK (pr. kunde)
01 START-SALDO-DKK   PIC S9(13)V99 VALUE 50000.00.

*> -------------------------------------------------
*>  Kundetabel (CPR = kunde-id)
*> -------------------------------------------------
01 ANTAL-KUNDER      PIC 9(5) VALUE 0.
01 IDX-KUNDE         PIC 9(5) VALUE 0.
01 LOOP-KUNDE        PIC 9(5) VALUE 0.

01 KUNDE-TABEL.
   02 KUNDE-POST OCCURS 15000 TIMES.
      03 K-CPR      PIC X(15).
      03 K-NAVN     PIC X(30).
      03 K-SALDO    PIC S9(13)V99.

*> Top 3 kunders index
01 BEST1-IDX         PIC 9(5) VALUE 0.
01 BEST2-IDX         PIC 9(5) VALUE 0.
01 BEST3-IDX         PIC 9(5) VALUE 0.

*> -------------------------------------------------
*>  Månedsstatistik
*> -------------------------------------------------
01 MND-IN.
   02 MND-IN-BELØB   OCCURS 12 PIC S9(15)V99 VALUE 0.
01 MND-UD.
   02 MND-UD-BELØB   OCCURS 12 PIC S9(15)V99 VALUE 0.

01 MND-STR          PIC X(2).
01 MND-INDEX        PIC 99.

*> Mest anvendte transaktionstype pr. måned
01 MND-TYPE-IND   OCCURS 12 PIC 9(9)     VALUE 0.
01 MND-TYPE-UDB   OCCURS 12 PIC 9(9)     VALUE 0.
01 MND-TYPE-OVF   OCCURS 12 PIC 9(9)     VALUE 0.

01 WS-TYPE-STR    PIC X(20).
01 TYPE-MAX       PIC 9(9).
01 TYPE-NAVN      PIC X(20).


*> Månednavne til udskrift
01 MND-NAVNE.
   02 MND1          PIC X(9) VALUE "Januar".
   02 MND2          PIC X(9) VALUE "Februar".
   02 MND3          PIC X(9) VALUE "Marts".
   02 MND4          PIC X(9) VALUE "April".
   02 MND5          PIC X(9) VALUE "Maj".
   02 MND6          PIC X(9) VALUE "Juni".
   02 MND7          PIC X(9) VALUE "Juli".
   02 MND8          PIC X(9) VALUE "August".
   02 MND9          PIC X(9) VALUE "September".
   02 MND10         PIC X(9) VALUE "Oktober".
   02 MND11         PIC X(9) VALUE "November".
   02 MND12         PIC X(9) VALUE "December".
01 WS-MND-NAVN      PIC X(9).

*> -------------------------------------------------
*>  Butik-statistik
*> -------------------------------------------------
01 ANTAL-BUTIKKER   PIC 9(4) VALUE 0.
01 IDX-BUTIK        PIC 9(4) VALUE 0.
01 LOOP-BUTIK       PIC 9(4) VALUE 0.
01 WS-BUTIK-NAVN   PIC X(35).
01 WS-DIGIT-COUNT PIC 9(4) VALUE 0.

01 BUTIK-TABEL.
   02 BUTIK-POST OCCURS 500 TIMES.
      03 B-NAVN    PIC X(35).
      03 B-ANTAL   PIC 9(9).
      03 B-OMS-NUM PIC S9(15)V99.

*> Top 5 butikker (index ind i BUTIK-TABEL)
01 TOP-BUTIK-IDX     OCCURS 5 PIC 9(4) VALUE 0.
01 RANK-BUTIK        PIC 9 VALUE 0.
01 BEST-BUTIK-IDX    PIC 9(4) VALUE 0.
01 BEST-OMS          PIC S9(15)V99 VALUE 0.
01 ALREADY-USED      PIC X VALUE "N".
01 LOOP-RANK         PIC 9 VALUE 0.
01 LOOP-J            PIC 9 VALUE 0.

*> -------------------------------------------------
*>  Beløb / formatering
*> -------------------------------------------------
01 BELØB-NUM         PIC S9(13)V99.
01 BELØB-DKK-NUM     PIC S9(15)V99.

01 SALDO-EDIT        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
01 NUM-EDIT1         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
01 NUM-EDIT2         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
01 NUM-ANTAL-EDIT    PIC Z,ZZZ,ZZ9. 

*> Valuta-omsætning pr. måned (i DKK)
01 MND-USD-DKK    OCCURS 12 PIC S9(15)V99 VALUE 0.
01 MND-EUR-DKK    OCCURS 12 PIC S9(15)V99 VALUE 0.
01 MND-DKK-DKK    OCCURS 12 PIC S9(15)V99 VALUE 0.

01 TEMP-DKK       PIC S9(15)V99.
01 WS-VALUTA-TRIM  PIC X(6).
01 WS-VALUTA-CODE  PIC X(3).

*> Ekstra formattering til valuta-tabel
01 NUM-EDIT3      PIC ZZ,ZZZ,ZZZ,ZZ9.99.

*> =================================================
PROCEDURE DIVISION.
    OPEN INPUT  Transfil
         OUTPUT UdFil

    MOVE "N" TO EOF-TRANS

    PERFORM UNTIL EOF-TRANS = "Y"
        READ Transfil
            AT END
                MOVE "Y" TO EOF-TRANS
            NOT AT END
                PERFORM BEHANDL-TRANS
        END-READ
    END-PERFORM

    PERFORM BEREGN-TOP-3-KUNDER
    PERFORM BEREGN-TOP-5-BUTIKKER

    PERFORM SKRIV-TOP-3-KUNDER
    PERFORM SKRIV-MND-STATISTIK
    PERFORM SKRIV-MND-TYPE-STAT
    PERFORM SKRIV-BUTIK-STATISTIK
    PERFORM SKRIV-TOP-5-BUTIKKER
    PERFORM SKRIV-MND-VALUTA-TABEL

    CLOSE Transfil UdFil
    STOP RUN.

*>--------------------------------------------------
*>  BEHANDL-TRANS – én linje fra Transaktioner.txt
*>--------------------------------------------------
BEHANDL-TRANS.
    *> 1) Kunde (CPR) – find eller opret med startsaldo
    PERFORM FIND-ELLER-OPRET-KUNDE

    *> 2) Konverter beløb til DKK
    PERFORM CONVERT-TO-DKK

    *> 3) Opdater kundens saldo
    ADD BELØB-DKK-NUM TO K-SALDO(IDX-KUNDE)

    *> 4) Månedsstatistik
    PERFORM OPDATER-MND-STAT

    *> 5) Butik-statistik
    PERFORM OPDATER-BUTIK-STAT
    .

*>--------------------------------------------------
*>  FIND-ELLER-OPRET-KUNDE – via CPR
*>--------------------------------------------------
FIND-ELLER-OPRET-KUNDE.
    MOVE 1 TO IDX-KUNDE
    PERFORM UNTIL IDX-KUNDE > ANTAL-KUNDER
               OR K-CPR(IDX-KUNDE) = CPR
        ADD 1 TO IDX-KUNDE
    END-PERFORM

    IF IDX-KUNDE > ANTAL-KUNDER
        *> Ny kunde – men pas på max 15000
        IF ANTAL-KUNDER < 15000
            ADD 1 TO ANTAL-KUNDER
            MOVE ANTAL-KUNDER   TO IDX-KUNDE
            MOVE CPR            TO K-CPR  (IDX-KUNDE)
            MOVE NAVN           TO K-NAVN (IDX-KUNDE)
            MOVE START-SALDO-DKK TO K-SALDO(IDX-KUNDE)
        ELSE
            *> Vi har ikke plads til flere – brug sidste plads som "overflow"
            MOVE 15000 TO IDX-KUNDE
            *> (valgfrit) DISPLAY en advarsel:
            *> DISPLAY "ADVARSEL: For mange kunder, resterende samles i indeks 15000".
        END-IF
    END-IF
    .


*>--------------------------------------------------
*>  OPDATER-MND-STAT – ind/udbetaling pr. måned
*>--------------------------------------------------
OPDATER-MND-STAT.
    *> TIDSPUNKT: YYYY-MM-DD-...
    MOVE TIDSPUNKT(6:2) TO MND-STR

    EVALUATE MND-STR
        WHEN "01" MOVE 1  TO MND-INDEX
        WHEN "02" MOVE 2  TO MND-INDEX
        WHEN "03" MOVE 3  TO MND-INDEX
        WHEN "04" MOVE 4  TO MND-INDEX
        WHEN "05" MOVE 5  TO MND-INDEX
        WHEN "06" MOVE 6  TO MND-INDEX
        WHEN "07" MOVE 7  TO MND-INDEX
        WHEN "08" MOVE 8  TO MND-INDEX
        WHEN "09" MOVE 9  TO MND-INDEX
        WHEN "10" MOVE 10 TO MND-INDEX
        WHEN "11" MOVE 11 TO MND-INDEX
        WHEN "12" MOVE 12 TO MND-INDEX
        WHEN OTHER MOVE 1 TO MND-INDEX
    END-EVALUATE

    *> 1) Indbetaling / udbetaling pr. måned (som før)
    IF BELØB-DKK-NUM > 0
        ADD BELØB-DKK-NUM TO MND-IN-BELØB(MND-INDEX)
    ELSE
        ADD BELØB-DKK-NUM TO MND-UD-BELØB(MND-INDEX)
    END-IF

    *> 2) Mest anvendte transaktionstype pr. måned
    MOVE FUNCTION TRIM(TRANSAKTIONSTYPE) TO WS-TYPE-STR

    EVALUATE WS-TYPE-STR
        WHEN "Indbetaling"
            ADD 1 TO MND-TYPE-IND(MND-INDEX)
        WHEN "Udbetaling"
            ADD 1 TO MND-TYPE-UDB(MND-INDEX)
        WHEN "Overførsel"
            ADD 1 TO MND-TYPE-OVF(MND-INDEX)
        WHEN OTHER
            *> Hvis noget andet dukker op, regn det som Overførsel
            ADD 1 TO MND-TYPE-OVF(MND-INDEX)
    END-EVALUATE

    *> 3) Valuta-omsætning pr. måned (USD/EUR/DKK) – i DKK
    MOVE BELØB-DKK-NUM TO TEMP-DKK
    IF TEMP-DKK < 0
        COMPUTE TEMP-DKK = -TEMP-DKK
    END-IF

    EVALUATE FUNCTION TRIM(VALUTA)
        WHEN "USD"
            ADD TEMP-DKK TO MND-USD-DKK(MND-INDEX)
        WHEN "EUR"
            ADD TEMP-DKK TO MND-EUR-DKK(MND-INDEX)
        WHEN OTHER
            ADD TEMP-DKK TO MND-DKK-DKK(MND-INDEX)
    END-EVALUATE
    .


*>--------------------------------------------------
*>  OPDATER-BUTIK-STAT – tæller og omsætning
*>--------------------------------------------------
OPDATER-BUTIK-STAT.
    PERFORM FIND-ELLER-OPRET-BUTIK

    ADD 1 TO B-ANTAL(IDX-BUTIK)

    *> omsætning = absolut værdi af beløb i DKK
    IF BELØB-DKK-NUM < 0
        COMPUTE BELØB-DKK-NUM = -BELØB-DKK-NUM
    END-IF
    ADD BELØB-DKK-NUM TO B-OMS-NUM(IDX-BUTIK)
    .

*>--------------------------------------------------
*>  FIND-ELLER-OPRET-BUTIK – via BUTIK-navn
*>--------------------------------------------------
FIND-ELLER-OPRET-BUTIK.
    MOVE 1 TO IDX-BUTIK
    PERFORM UNTIL IDX-BUTIK > ANTAL-BUTIKKER
               OR B-NAVN(IDX-BUTIK) = BUTIK
        ADD 1 TO IDX-BUTIK
    END-PERFORM

    IF IDX-BUTIK > ANTAL-BUTIKKER
        *> Ny butik – men max 500
        IF ANTAL-BUTIKKER < 500
            ADD 1 TO ANTAL-BUTIKKER
            MOVE ANTAL-BUTIKKER TO IDX-BUTIK
            MOVE BUTIK          TO B-NAVN(IDX-BUTIK)
            MOVE 0              TO B-ANTAL(IDX-BUTIK)
            MOVE 0              TO B-OMS-NUM(IDX-BUTIK)
        ELSE
            *> Overflow-butik – saml resten i indeks 500
            MOVE 500 TO IDX-BUTIK
            *> (valgfrit) DISPLAY "ADVARSEL: For mange butikker, resterende samles i indeks 500".
        END-IF
    END-IF
    .


*>--------------------------------------------------
*>  CONVERT-TO-DKK – beløb + valuta -> DKK
*>--------------------------------------------------
CONVERT-TO-DKK.
    MOVE FUNCTION NUMVAL(BELØB-TEXT) TO BELØB-NUM

    MOVE FUNCTION TRIM(VALUTA) TO WS-VALUTA-TRIM

    *> Tag de sidste 3 karakterer (hvis der er så mange)
    IF FUNCTION LENGTH(WS-VALUTA-TRIM) >= 3
        MOVE WS-VALUTA-TRIM(
             FUNCTION LENGTH(WS-VALUTA-TRIM) - 2:3
             )
          TO WS-VALUTA-CODE
    ELSE
        MOVE WS-VALUTA-TRIM TO WS-VALUTA-CODE
    END-IF

    EVALUATE WS-VALUTA-CODE
        WHEN "USD"
            COMPUTE BELØB-DKK-NUM = BELØB-NUM * 6.8
        WHEN "EUR"
            COMPUTE BELØB-DKK-NUM = BELØB-NUM * 7.5
        WHEN OTHER
            MOVE BELØB-NUM TO BELØB-DKK-NUM
    END-EVALUATE
    .

*>--------------------------------------------------
*>  BEREGN-TOP-3-KUNDER – baseret på K-SALDO
*>--------------------------------------------------
BEREGN-TOP-3-KUNDER.
    MOVE 0 TO BEST1-IDX BEST2-IDX BEST3-IDX

    MOVE 1 TO LOOP-KUNDE
    PERFORM UNTIL LOOP-KUNDE > ANTAL-KUNDER

        *> Indsæt i 1. plads?
        IF BEST1-IDX = 0
           OR K-SALDO(LOOP-KUNDE) > K-SALDO(BEST1-IDX)
            MOVE BEST2-IDX  TO BEST3-IDX
            MOVE BEST1-IDX  TO BEST2-IDX
            MOVE LOOP-KUNDE TO BEST1-IDX

        *> Ellers i 2. plads?
        ELSE
            IF BEST2-IDX = 0
               OR K-SALDO(LOOP-KUNDE) > K-SALDO(BEST2-IDX)
                MOVE BEST2-IDX  TO BEST3-IDX
                MOVE LOOP-KUNDE TO BEST2-IDX

        *> Ellers i 3. plads?
            ELSE
                IF BEST3-IDX = 0
                   OR K-SALDO(LOOP-KUNDE) > K-SALDO(BEST3-IDX)
                    MOVE LOOP-KUNDE TO BEST3-IDX
                END-IF
            END-IF
        END-IF

        ADD 1 TO LOOP-KUNDE
    END-PERFORM
    .

*>--------------------------------------------------
*>  BEREGN-TOP-5-BUTIKKER – på B-OMS-NUM (omsætning)
*>--------------------------------------------------
BEREGN-TOP-5-BUTIKKER.
    MOVE 0 TO TOP-BUTIK-IDX(1)
    MOVE 0 TO TOP-BUTIK-IDX(2)
    MOVE 0 TO TOP-BUTIK-IDX(3)
    MOVE 0 TO TOP-BUTIK-IDX(4)
    MOVE 0 TO TOP-BUTIK-IDX(5)

    MOVE 1 TO RANK-BUTIK
    PERFORM UNTIL RANK-BUTIK > 5 OR RANK-BUTIK > ANTAL-BUTIKKER
        MOVE 0             TO BEST-BUTIK-IDX
        MOVE 0             TO BEST-OMS

        MOVE 1 TO LOOP-BUTIK
        PERFORM UNTIL LOOP-BUTIK > ANTAL-BUTIKKER
            MOVE "N" TO ALREADY-USED
            MOVE 1   TO LOOP-J
            PERFORM UNTIL LOOP-J > 5
                IF TOP-BUTIK-IDX(LOOP-J) = LOOP-BUTIK
                    MOVE "Y" TO ALREADY-USED
                END-IF
                ADD 1 TO LOOP-J
            END-PERFORM

            *> Trim navnet én gang til WS-BUTIK-NAVN
            MOVE FUNCTION TRIM(B-NAVN(LOOP-BUTIK)) TO WS-BUTIK-NAVN

            IF ALREADY-USED = "N"
               AND B-OMS-NUM(LOOP-BUTIK) > BEST-OMS
               AND WS-BUTIK-NAVN NOT = SPACES
               AND WS-BUTIK-NAVN(1:1) NOT = "2"
               AND WS-BUTIK-NAVN(1:1) NOT = "0"
               AND WS-BUTIK-NAVN(1:1) NOT = "-"
            THEN
                MOVE B-OMS-NUM(LOOP-BUTIK) TO BEST-OMS
                MOVE LOOP-BUTIK            TO BEST-BUTIK-IDX
            END-IF

            ADD 1 TO LOOP-BUTIK
        END-PERFORM

        IF BEST-BUTIK-IDX > 0
            MOVE BEST-BUTIK-IDX TO TOP-BUTIK-IDX(RANK-BUTIK)
        END-IF

        ADD 1 TO RANK-BUTIK
    END-PERFORM
    .

*>--------------------------------------------------
*>  SKRIV-TOP-3-KUNDER
*>--------------------------------------------------
SKRIV-TOP-3-KUNDER.
    MOVE SPACES TO OUT-TEXT
    STRING "Top 3 kunder med højeste saldo:"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    PERFORM SKRIV-EN-TOP-KUNDE-1
    PERFORM SKRIV-EN-TOP-KUNDE-2
    PERFORM SKRIV-EN-TOP-KUNDE-3

    MOVE SPACES TO OUT-TEXT
    WRITE OUT-REC
    .

SKRIV-EN-TOP-KUNDE-1.
    IF BEST1-IDX = 0
        EXIT PARAGRAPH
    END-IF
    MOVE K-SALDO(BEST1-IDX) TO SALDO-EDIT
    MOVE SPACES TO OUT-TEXT
    STRING
        " Kunde-ID: "  DELIMITED BY SIZE
        K-CPR(BEST1-IDX) DELIMITED BY SIZE
        ", Navn: "     DELIMITED BY SIZE
        K-NAVN(BEST1-IDX) DELIMITED BY SIZE
        ", Saldo: "    DELIMITED BY SIZE
        SALDO-EDIT     DELIMITED BY SIZE
        " DKK"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC
    .

SKRIV-EN-TOP-KUNDE-2.
    IF BEST2-IDX = 0
        EXIT PARAGRAPH
    END-IF
    MOVE K-SALDO(BEST2-IDX) TO SALDO-EDIT
    MOVE SPACES TO OUT-TEXT
    STRING
        " Kunde-ID: "  DELIMITED BY SIZE
        K-CPR(BEST2-IDX) DELIMITED BY SIZE
        ", Navn: "     DELIMITED BY SIZE
        K-NAVN(BEST2-IDX) DELIMITED BY SIZE
        ", Saldo: "    DELIMITED BY SIZE
        SALDO-EDIT     DELIMITED BY SIZE
        " DKK"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC
    .

SKRIV-EN-TOP-KUNDE-3.
    IF BEST3-IDX = 0
        EXIT PARAGRAPH
    END-IF
    MOVE K-SALDO(BEST3-IDX) TO SALDO-EDIT
    MOVE SPACES TO OUT-TEXT
    STRING
        " Kunde-ID: "  DELIMITED BY SIZE
        K-CPR(BEST3-IDX) DELIMITED BY SIZE
        ", Navn: "     DELIMITED BY SIZE
        K-NAVN(BEST3-IDX) DELIMITED BY SIZE
        ", Saldo: "    DELIMITED BY SIZE
        SALDO-EDIT     DELIMITED BY SIZE
        " DKK"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC
    .

*>--------------------------------------------------
*>  SKRIV-MND-STATISTIK
*>--------------------------------------------------
SKRIV-MND-STATISTIK.
    MOVE SPACES TO OUT-TEXT
    STRING
        "Måned       Indbetalinger (DKK)   Udbetalinger (DKK)"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE 1 TO MND-INDEX
    PERFORM UNTIL MND-INDEX > 12
        EVALUATE MND-INDEX
            WHEN 1  MOVE MND1  TO WS-MND-NAVN
            WHEN 2  MOVE MND2  TO WS-MND-NAVN
            WHEN 3  MOVE MND3  TO WS-MND-NAVN
            WHEN 4  MOVE MND4  TO WS-MND-NAVN
            WHEN 5  MOVE MND5  TO WS-MND-NAVN
            WHEN 6  MOVE MND6  TO WS-MND-NAVN
            WHEN 7  MOVE MND7  TO WS-MND-NAVN
            WHEN 8  MOVE MND8  TO WS-MND-NAVN
            WHEN 9  MOVE MND9  TO WS-MND-NAVN
            WHEN 10 MOVE MND10 TO WS-MND-NAVN
            WHEN 11 MOVE MND11 TO WS-MND-NAVN
            WHEN 12 MOVE MND12 TO WS-MND-NAVN
        END-EVALUATE

        MOVE MND-IN-BELØB(MND-INDEX) TO NUM-EDIT1
        MOVE MND-UD-BELØB(MND-INDEX) TO NUM-EDIT2

        MOVE SPACES TO OUT-TEXT
        STRING
            WS-MND-NAVN DELIMITED BY SIZE
            " "         DELIMITED BY SIZE
            NUM-EDIT1   DELIMITED BY SIZE
            "   "       DELIMITED BY SIZE
            NUM-EDIT2   DELIMITED BY SIZE
        INTO OUT-TEXT
        END-STRING
        WRITE OUT-REC

        ADD 1 TO MND-INDEX
    END-PERFORM

    MOVE SPACES TO OUT-TEXT
    WRITE OUT-REC
    .

*>--------------------------------------------------
*>  SKRIV-MND-TYPE-STAT – mest anvendte type pr. måned
*>--------------------------------------------------
SKRIV-MND-TYPE-STAT.
    MOVE SPACES TO OUT-TEXT
    STRING
        "Mest anvendte transaktionstype pr. måned:"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING
        "Måned       Type"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE 1 TO MND-INDEX
    PERFORM UNTIL MND-INDEX > 12

        *> Vælg månednavn
        EVALUATE MND-INDEX
            WHEN 1  MOVE MND1  TO WS-MND-NAVN
            WHEN 2  MOVE MND2  TO WS-MND-NAVN
            WHEN 3  MOVE MND3  TO WS-MND-NAVN
            WHEN 4  MOVE MND4  TO WS-MND-NAVN
            WHEN 5  MOVE MND5  TO WS-MND-NAVN
            WHEN 6  MOVE MND6  TO WS-MND-NAVN
            WHEN 7  MOVE MND7  TO WS-MND-NAVN
            WHEN 8  MOVE MND8  TO WS-MND-NAVN
            WHEN 9  MOVE MND9  TO WS-MND-NAVN
            WHEN 10 MOVE MND10 TO WS-MND-NAVN
            WHEN 11 MOVE MND11 TO WS-MND-NAVN
            WHEN 12 MOVE MND12 TO WS-MND-NAVN
        END-EVALUATE

        *> Find hvilken type der er størst
        MOVE 0          TO TYPE-MAX
        MOVE SPACES     TO TYPE-NAVN

        IF MND-TYPE-IND(MND-INDEX) > TYPE-MAX
            MOVE MND-TYPE-IND(MND-INDEX) TO TYPE-MAX
            MOVE "Indbetaling"           TO TYPE-NAVN
        END-IF

        IF MND-TYPE-UDB(MND-INDEX) > TYPE-MAX
            MOVE MND-TYPE-UDB(MND-INDEX) TO TYPE-MAX
            MOVE "Udbetaling"            TO TYPE-NAVN
        END-IF

        IF MND-TYPE-OVF(MND-INDEX) > TYPE-MAX
            MOVE MND-TYPE-OVF(MND-INDEX) TO TYPE-MAX
            MOVE "Overførsel"            TO TYPE-NAVN
        END-IF

        MOVE SPACES TO OUT-TEXT
        STRING
            WS-MND-NAVN DELIMITED BY SIZE
            "   "       DELIMITED BY SIZE
            TYPE-NAVN   DELIMITED BY SIZE
        INTO OUT-TEXT
        END-STRING
        WRITE OUT-REC

        ADD 1 TO MND-INDEX
    END-PERFORM

    MOVE SPACES TO OUT-TEXT
    WRITE OUT-REC
    .

*>--------------------------------------------------
*>  SKRIV-MND-VALUTA-TABEL – USD/EUR/DKK pr. måned
*>--------------------------------------------------
SKRIV-MND-VALUTA-TABEL.
    MOVE SPACES TO OUT-TEXT
    STRING
        "Transaktioner pr. måned pr. valuta (i DKK):"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE SPACES TO OUT-TEXT
    STRING
        "Måned      USD (DKK)        EUR (DKK)        DKK (DKK)"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE 1 TO MND-INDEX
    PERFORM UNTIL MND-INDEX > 12
        EVALUATE MND-INDEX
            WHEN 1  MOVE MND1  TO WS-MND-NAVN
            WHEN 2  MOVE MND2  TO WS-MND-NAVN
            WHEN 3  MOVE MND3  TO WS-MND-NAVN
            WHEN 4  MOVE MND4  TO WS-MND-NAVN
            WHEN 5  MOVE MND5  TO WS-MND-NAVN
            WHEN 6  MOVE MND6  TO WS-MND-NAVN
            WHEN 7  MOVE MND7  TO WS-MND-NAVN
            WHEN 8  MOVE MND8  TO WS-MND-NAVN
            WHEN 9  MOVE MND9  TO WS-MND-NAVN
            WHEN 10 MOVE MND10 TO WS-MND-NAVN
            WHEN 11 MOVE MND11 TO WS-MND-NAVN
            WHEN 12 MOVE MND12 TO WS-MND-NAVN
        END-EVALUATE

        MOVE MND-USD-DKK(MND-INDEX) TO NUM-EDIT1
        MOVE MND-EUR-DKK(MND-INDEX) TO NUM-EDIT2
        MOVE MND-DKK-DKK(MND-INDEX) TO NUM-EDIT3

        MOVE SPACES TO OUT-TEXT
        STRING
            WS-MND-NAVN DELIMITED BY SIZE
            "   "       DELIMITED BY SIZE
            NUM-EDIT1   DELIMITED BY SIZE
            "   "       DELIMITED BY SIZE
            NUM-EDIT2   DELIMITED BY SIZE
            "   "       DELIMITED BY SIZE
            NUM-EDIT3   DELIMITED BY SIZE
        INTO OUT-TEXT
        END-STRING
        WRITE OUT-REC

        ADD 1 TO MND-INDEX
    END-PERFORM

    MOVE SPACES TO OUT-TEXT
    WRITE OUT-REC
    .

*>--------------------------------------------------
*>  SKRIV-BUTIK-STATISTIK – antal pr. butik
*>--------------------------------------------------
SKRIV-BUTIK-STATISTIK.
    MOVE SPACES TO OUT-TEXT
    STRING
        "Butik                Antal transaktioner"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE 1 TO LOOP-BUTIK
    PERFORM UNTIL LOOP-BUTIK > ANTAL-BUTIKKER

        *> Trim
        MOVE FUNCTION TRIM(B-NAVN(LOOP-BUTIK)) TO WS-BUTIK-NAVN

        *> Tæl cifre
        MOVE 0 TO WS-DIGIT-COUNT
        INSPECT WS-BUTIK-NAVN
           TALLYING WS-DIGIT-COUNT
             FOR ALL "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"

        IF WS-BUTIK-NAVN NOT = SPACES
           AND B-ANTAL(LOOP-BUTIK) > 0
           AND WS-DIGIT-COUNT = 0
           AND WS-BUTIK-NAVN(1:1) IS ALPHABETIC
        THEN
            MOVE B-ANTAL(LOOP-BUTIK) TO NUM-ANTAL-EDIT

            MOVE SPACES TO OUT-TEXT
            STRING
                WS-BUTIK-NAVN    DELIMITED BY SIZE
                "   "            DELIMITED BY SIZE
                NUM-ANTAL-EDIT   DELIMITED BY SIZE
            INTO OUT-TEXT
            END-STRING
            WRITE OUT-REC
        END-IF

        ADD 1 TO LOOP-BUTIK
    END-PERFORM

    MOVE SPACES TO OUT-TEXT
    WRITE OUT-REC
    .



*>--------------------------------------------------
*>  SKRIV-TOP-5-BUTIKKER – på omsætning
*>--------------------------------------------------
SKRIV-TOP-5-BUTIKKER.
    MOVE SPACES TO OUT-TEXT
    STRING
        "Top 5 butikker med højeste omsætning (DKK):"
        DELIMITED BY SIZE
    INTO OUT-TEXT
    END-STRING
    WRITE OUT-REC

    MOVE 1 TO LOOP-RANK
    PERFORM UNTIL LOOP-RANK > 5
        IF TOP-BUTIK-IDX(LOOP-RANK) > 0
            MOVE B-OMS-NUM(TOP-BUTIK-IDX(LOOP-RANK)) TO NUM-EDIT1

            MOVE SPACES TO OUT-TEXT
            STRING
                " "                         DELIMITED BY SIZE
                B-NAVN(TOP-BUTIK-IDX(LOOP-RANK))
                                            DELIMITED BY SIZE
                " : "                       DELIMITED BY SIZE
                NUM-EDIT1                   DELIMITED BY SIZE
                " DKK"                      DELIMITED BY SIZE
            INTO OUT-TEXT
            END-STRING
            WRITE OUT-REC
        END-IF
        ADD 1 TO LOOP-RANK
    END-PERFORM

    MOVE SPACES TO OUT-TEXT
    WRITE OUT-REC
    .
END PROGRAM OPGAVE11.
