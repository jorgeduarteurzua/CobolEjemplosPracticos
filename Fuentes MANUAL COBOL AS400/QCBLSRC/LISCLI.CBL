       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. LISCLI.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLIENTE    ASSIGN        TO DATABASE-CLIENTES
                             ORGANIZATION  IS INDEXED
                             ACCESS MODE   IS DYNAMIC
                             RECORD KEY    IS EXTERNALLY-DESCRIBED-KEY
                             FILE STATUS   IS FS-CLIENTE.


           SELECT PANTALLA   ASSIGN TO WORKSTATION-LIST01-SI
                             ORGANIZATION IS TRANSACTION
                             ACCESS MODE  IS DYNAMIC
                             RELATIVE KEY IS NREL
                             CONTROL-AREA IS AREA-CONTROL
                             FILE STATUS  IS FS-PAN.

       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*

       FD  CLIENTE   LABEL   RECORD STANDARD.
       01  R-CLIENTE.
           COPY  DDS-ALL-FORMAT  OF CLIENTES.

       FD PANTALLA  LABEL RECORD IS OMITTED.
       01 R-PANTALLA             PIC X(1920).

       WORKING-STORAGE SECTION.
      *------------------------*
       77 I-ON                PIC 1 VALUE B"1".
       77 I-OFF               PIC 1 VALUE B"0".
       77 I                   PIC 9(02).

       01 AREA-CONTROL.
             06 MDTO                PIC X(02).
                88 F3               VALUE "03".

       01 VARIABLES-DE-STATUS.
          05 FS-CLIENTE     PIC XX VALUE "00".
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC  9(03).

       01 C01-OO.
      *----------
           COPY DDS-C01-O        OF LIST01.

       01 S01-OO.
      *----------
           COPY DDS-S01-O        OF LIST01.

       01 C01-IND.
      *------------
          COPY DDS-C01-INDIC     OF LIST01.

       01 DEL-INPUT.
           05 DEL-RUT     PIC 9(10).
       01 DEL-OUTPUT.
           05 DEL-STATUS  PIC X(02).
      *       '00' = OK
      *    <> '00' = NO OK

       PROCEDURE DIVISION.
      *-------------------
           OPEN I-O PANTALLA
           OPEN INPUT CLIENTE

           MOVE I-OFF     TO IN03 OF C01-I-INDIC

           MOVE ZEROES    TO RUTPOS OF C01-OO

           PERFORM CARGAR-S01
           PERFORM PROCESAR-PANTALLA UNTIL
                   IN03 OF C01-I-INDIC = I-ON

           CLOSE    PANTALLA CLIENTE

           GOBACK.

      *
       PROCESAR-PANTALLA.
      *------------------
           WRITE R-PANTALLA              FORMAT "R01"
           WRITE R-PANTALLA  FROM C01-OO FORMAT "C01"
                             INDICATORS ARE C01-O-INDIC END-WRITE
           READ  PANTALLA    INTO C01-OO FORMAT "C01"
                             INDICATORS ARE C01-I-INDIC END-READ

           IF IN06 OF C01-I-INDIC = I-ON
              CALL "MANCLICBL"
              CANCEL "MANCLICBL"
           ELSE
              PERFORM RECORRER-S01 UNTIL FS-PAN NOT = "00"
           END-IF
           PERFORM CARGAR-S01

           .

       LIMPIAR-S01.
      *-------------
           MOVE ZEROES          TO NREL
           MOVE I-OFF           TO IN30 OF C01-O-INDIC
                                   IN31 OF C01-O-INDIC
           MOVE I-ON            TO IN32 OF C01-O-INDIC
           WRITE R-PANTALLA  FROM C01-OO FORMAT "C01"
                             INDICATORS ARE C01-O-INDIC END-WRITE.


       CARGAR-S01.
      *-------------
           PERFORM LIMPIAR-S01
           MOVE RUTPOS OF C01-OO   TO RUT OF CLIENTE
           START  CLIENTE KEY IS NOT < EXTERNALLY-DESCRIBED-KEY
                  END-START
           IF FS-CLIENTE = "00"
              PERFORM LEER-NEXT-CLIENTE
              PERFORM RECORRE-CLIENTE
                      UNTIL FS-CLIENTE NOT = "00"
                      OR NREL > 100
           END-IF
           IF NREL > 0
              MOVE I-ON         TO IN30 OF C01-O-INDIC
                                   IN31 OF C01-O-INDIC
              MOVE I-OFF        TO IN32 OF C01-O-INDIC
           ELSE
              MOVE I-OFF        TO IN30 OF C01-O-INDIC
              MOVE I-ON         TO IN31 OF C01-O-INDIC
              MOVE I-OFF        TO IN32 OF C01-O-INDIC
           END-IF
           .
      *
       RECORRE-CLIENTE.
      *----------------
           IF FS-CLIENTE = "00"
                 ADD 1   TO NREL
                 MOVE SPACES          TO  OPCS01 OF S01-OO
                 MOVE RUT OF CLIENTE  TO  RUTS01 OF S01-OO
                 MOVE DV  OF CLIENTE  TO  DVS01  OF S01-OO
                 MOVE SPACES          TO  NOMS01 OF S01-OO
                 STRING APEPAT OF CLIENTE   DELIMITED BY " "
                        " "                 DELIMITED SIZE
                        APEMAT OF CLIENTE   DELIMITED BY " "
                        " "                 DELIMITED SIZE
                        NOMBRE OF CLIENTE   DELIMITED BY " "
                                       INTO NOMS01 OF S01-OO
                 WRITE SUBFILE R-PANTALLA  FROM S01-OO FORMAT "S01"
                       END-WRITE
           END-IF
           PERFORM LEER-NEXT-CLIENTE
           .

       LEER-NEXT-CLIENTE.
      *------------------
            READ CLIENTE NEXT RECORD END-READ
            .

       RECORRER-S01.
      *--------------
           READ SUBFILE PANTALLA  NEXT MODIFIED INTO S01-OO
                        FORMAT "S01"  END-READ
           IF FS-PAN  = "00"
              IF OPCS01 OF S01-OO = "B" OR "b"
                 MOVE  RUTS01 OF S01-OO  TO DEL-RUT
                 MOVE  SPACES            TO DEL-STATUS
                 CALL "BORRARCLI" USING DEL-INPUT
                                        DEL-OUTPUT
              END-IF
              IF OPCS01 OF S01-OO = "C" OR "c"
                 CALL "CONCLI"    USING RUTS01 OF S01-OO
                 CANCEL "CONCLI"
              END-IF
           END-IF
           .
