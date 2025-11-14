>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. OPGAVE7.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Kundeoplysninger
           ASSIGN TO "Kundeoplysninger.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
    SELECT KundeoplysningerOut
           ASSIGN TO "KundeoplysningerOut.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD  Kundeoplysninger.
01  KUNDEOPL.
    COPY "KUNDER".

FD  KundeoplysningerOut.
01  KUNDE-ADR.
    02 NAVN-ADR PIC X(200).

WORKING-STORAGE SECTION.
01  EOF-FLAG  PIC X VALUE "N".

PROCEDURE DIVISION.
    OPEN INPUT  Kundeoplysninger
         OUTPUT KundeoplysningerOut.

    PERFORM UNTIL EOF-FLAG = "Y"
        READ Kundeoplysninger
            AT END
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                *> 1) Kunde-ID
                MOVE SPACES TO NAVN-ADR
                STRING "Kunde-ID: "                       DELIMITED BY SIZE
                       FUNCTION TRIM(KUNDEID OF KUNDEOPL) DELIMITED BY SIZE
                  INTO NAVN-ADR
                END-STRING
                WRITE KUNDE-ADR

                *> 2) Navn (fornavn + efternavn)
                MOVE SPACES TO NAVN-ADR
                STRING "Navn: "                                  DELIMITED BY SIZE
                       FUNCTION TRIM(FORNAVN  OF KUNDEOPL)       DELIMITED BY SIZE
                       " "                                       DELIMITED BY SIZE
                       FUNCTION TRIM(EFTERNAVN OF KUNDEOPL)      DELIMITED BY SIZE
                  INTO NAVN-ADR
                END-STRING
                WRITE KUNDE-ADR

                *> 3) Adresse: vej + nr + [etage] [side]
                MOVE SPACES TO NAVN-ADR
                STRING "Adresse: "                                DELIMITED BY SIZE
                       FUNCTION TRIM(VEJNAVN OF ADRESSE OF KUNDEOPL) DELIMITED BY SIZE
                       " "                                        DELIMITED BY SIZE
                       FUNCTION TRIM(HUSNR   OF ADRESSE OF KUNDEOPL) DELIMITED BY SIZE
                  INTO NAVN-ADR
                END-STRING
                IF FUNCTION LENGTH(FUNCTION TRIM(ETAGE OF ADRESSE OF KUNDEOPL)) > 0
                    STRING NAVN-ADR                              DELIMITED BY SIZE
                           " "                                   DELIMITED BY SIZE
                           FUNCTION TRIM(ETAGE OF ADRESSE OF KUNDEOPL) DELIMITED BY SIZE
                      INTO NAVN-ADR
                    END-STRING
                END-IF
                IF FUNCTION LENGTH(FUNCTION TRIM(SIDE OF ADRESSE OF KUNDEOPL)) > 0
                    STRING NAVN-ADR                              DELIMITED BY SIZE
                           " "                                   DELIMITED BY SIZE
                           FUNCTION TRIM(SIDE OF ADRESSE OF KUNDEOPL)  DELIMITED BY SIZE
                      INTO NAVN-ADR
                    END-STRING
                END-IF
                WRITE KUNDE-ADR

                *> 4) By + postnr + (land)
                MOVE SPACES TO NAVN-ADR
                STRING FUNCTION TRIM(BYNAVN  OF ADRESSE OF KUNDEOPL) DELIMITED BY SIZE
                       " "                                           DELIMITED BY SIZE
                       FUNCTION TRIM(POSTNR  OF ADRESSE OF KUNDEOPL) DELIMITED BY SIZE
                       " ("                                          DELIMITED BY SIZE
                       FUNCTION TRIM(LANDKODE OF ADRESSE OF KUNDEOPL) DELIMITED BY SIZE
                       ")"                                           DELIMITED BY SIZE
                  INTO NAVN-ADR
                END-STRING
                WRITE KUNDE-ADR

                *> 5) Telefon
                MOVE SPACES TO NAVN-ADR
                STRING "Telefon: "                                        DELIMITED BY SIZE
                       FUNCTION TRIM(TELEFON OF KONTAKTOPL OF KUNDEOPL)   DELIMITED BY SIZE
                  INTO NAVN-ADR
                END-STRING
                WRITE KUNDE-ADR

                *> 5) Email
                MOVE SPACES TO NAVN-ADR
                STRING "Email: "                                          DELIMITED BY SIZE
                       FUNCTION TRIM(EMAIL   OF KONTAKTOPL OF KUNDEOPL)   DELIMITED BY SIZE
                  INTO NAVN-ADR
                END-STRING
                WRITE KUNDE-ADR

                *> 6) Blank linje mellem kunder
                MOVE SPACES TO NAVN-ADR
                WRITE KUNDE-ADR
        END-READ
    END-PERFORM

    CLOSE Kundeoplysninger
          KundeoplysningerOut.
    STOP RUN.
END PROGRAM OPGAVE7.
