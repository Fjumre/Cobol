>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. LOAD-BANKER.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Bankfil ASSIGN TO "Banker.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD Bankfil.
01 RAW-BANK     PIC X(130).

01 BANK-REC REDEFINES RAW-BANK.
   02 REG-NR        PIC X(4).
   02 BANKNAVN      PIC X(30).
   02 BANKADRESSE   PIC X(51).
   02 TELEFON       PIC X(15).
   02 EMAIL         PIC X(30).

WORKING-STORAGE SECTION.
01 EOF-BANK     PIC X VALUE "N".


*> Command buffer to call sqlite3
01 CMD-LINE     PIC X(400).

*> Trimmed fields
01 T-REG-NR       PIC X(10).
01 T-BANKNAVN     PIC X(40).
01 T-BANKADR      PIC X(80).
01 T-TELEFON      PIC X(30).
01 T-EMAIL        PIC X(60).

PROCEDURE DIVISION.

    OPEN INPUT Bankfil

    PERFORM UNTIL EOF-BANK = "Y"
        READ Bankfil
            AT END MOVE "Y" TO EOF-BANK
            NOT AT END PERFORM BEHANDL-BANK-REC
        END-READ
    END-PERFORM

    CLOSE Bankfil
    STOP RUN.

BEHANDL-BANK-REC.

    INSPECT RAW-BANK REPLACING ALL X"00" BY SPACES.
    INSPECT T-BANKNAVN REPLACING ALL X"22" BY X"27".
    INSPECT T-BANKADR  REPLACING ALL X"22" BY X"27".
    INSPECT T-EMAIL    REPLACING ALL X"22" BY X"27".

    MOVE FUNCTION TRIM(REG-NR)      TO T-REG-NR
    MOVE FUNCTION TRIM(BANKNAVN)    TO T-BANKNAVN
    MOVE FUNCTION TRIM(BANKADRESSE) TO T-BANKADR
    MOVE FUNCTION TRIM(TELEFON)     TO T-TELEFON
    MOVE FUNCTION TRIM(EMAIL)       TO T-EMAIL

    IF T-REG-NR = SPACES
        EXIT PARAGRAPH
    END-IF

            MOVE SPACES TO CMD-LINE
      

    STRING
        "sqlite3 bank.db " 
        X"22"   *> opening "
        "INSERT INTO banker (reg_nr, banknavn, bankadresse, telefon, email) "
        "VALUES ('" T-REG-NR "', '" T-BANKNAVN "', '" T-BANKADR "', '" 
                   T-TELEFON "', '" T-EMAIL "')" 
        " ON CONFLICT(reg_nr) DO UPDATE SET "
        "banknavn = excluded.banknavn, "
        "bankadresse = excluded.bankadresse, "
        "telefon = excluded.telefon, "
        "email = excluded.email;"
        X"22"   *> closing "
        DELIMITED BY SIZE
        INTO CMD-LINE
    END-STRING.


    DISPLAY "CMD: [" CMD-LINE "]".
    CALL "SYSTEM" USING CMD-LINE.

    EXIT PARAGRAPH.

