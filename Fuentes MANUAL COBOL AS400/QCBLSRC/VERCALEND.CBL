       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. VERCALEND.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CALENDA    ASSIGN        TO DATABASE-CALEND
                             ORGANIZATION  IS INDEXED
                             ACCESS MODE   IS DYNAMIC
                             RECORD KEY    IS EXTERNALLY-DESCRIBED-KEY
                             FILE STATUS   IS FS-CALENDA.


           SELECT PANTALLA   ASSIGN TO WORKSTATION-CALENDARIO-SI
                             ORGANIZATION IS TRANSACTION
                             ACCESS MODE  IS DYNAMIC
                             RELATIVE KEY IS NREL
                             CONTROL-AREA IS AREA-CONTROL
                             FILE STATUS  IS FS-PAN.

       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD  CALENDA   LABEL   RECORD STANDARD.
       01  R-CALEND.
           COPY  DDS-ALL-FORMAT  OF CALEND.

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
          05 FS-CALENDA     PIC XX VALUE "00".
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC  9(03).

       01 VARIABLES-DE-TRABAJO.
          05 WS-FECHA.
             10 WS-ANO       PIC 9(04).
             10 WS-MES       PIC 9(02).
             10 WS-DIA       PIC 9(02).

          05 WS-LIN-CAL       PIC 9(1).

          05 ARR-DIA OCCURS 07 TIMES.
              10 FILLER       PIC X.
              10 ARR-DIA-S    PIC 99.
              10 FILLER       PIC X.

       01 W01-OO.
      *----------
           COPY DDS-W01-O        OF CALENDARIO.

       01 W01-II.
      *----------
           COPY DDS-W01-I        OF CALENDARIO.


       01 W01-IND.
      *------------
          COPY DDS-W01-INDIC     OF CALENDARIO.

       01 INP-HLP.
          05 HLP-TABLA         PIC X(10).
       01 OUT-HLP.
          05 HLP-CODIGO        PIC X(10).
          05 HLP-DESC-CORTA    PIC X(20).
          05 HLP-DESC-LARGA    PIC X(50).
          05 HLP-LARGO-COD     PIC 9(03).

       LINKAGE SECTION.
      *----------------
       01 INP-CALENDARIO.
          05 CALENDARIO-MES    PIC 9(02).
          05 CALENDARIO-ANO    PIC 9(04).
       01 OUT-CALENDARIO.
          05 CALENDARIO-ANO-O  PIC 9(04).
          05 CALENDARIO-MES-O  PIC 9(02).
          05 CALENDARIO-DIA-O  PIC 9(02).

       PROCEDURE DIVISION USING INP-CALENDARIO
                                OUT-CALENDARIO.
      *----------------------------------------
           OPEN I-O PANTALLA
           OPEN INPUT CALENDA

           MOVE CALENDARIO-ANO   TO CALENDARIO-ANO-O
           MOVE CALENDARIO-MES   TO CALENDARIO-MES-O
           MOVE ZEROES           TO CALENDARIO-DIA-O
                                    POSCURSOR OF W01-O
                                    BTNPREV   OF W01-O
                                    BTNNEXT   OF W01-O
           MOVE I-OFF     TO IN03 OF W01-I-INDIC
           MOVE CALENDARIO-MES   TO MES    OF W01-OO
           MOVE CALENDARIO-ANO   TO ANO    OF W01-OO

           PERFORM RECORRER-MES
           PERFORM PROCESAR-PANTALLA UNTIL
                   IN03 OF W01-I-INDIC = I-ON
               OR  CALENDARIO-DIA-O NOT = 0

           CLOSE    PANTALLA CALENDA

           GOBACK.

      *
       PROCESAR-PANTALLA.
      *------------------
           MOVE CORR W01-O   TO W01-I
           WRITE R-PANTALLA  FROM W01-OO FORMAT "W01"
                             INDICATORS ARE W01-O-INDIC END-WRITE
           READ  PANTALLA    INTO W01-II FORMAT "W01"
                             INDICATORS ARE W01-I-INDIC END-READ

           IF IN04 OF W01-I-INDIC = I-ON THEN
              IF CAMCURSOR OF W01-II = "MES" THEN
                 PERFORM MOSTRAR-AYUDA
                 PERFORM RECORRER-MES
              END-IF
           ELSE
             IF BTNPREV OF W01-II = 1 THEN
                SUBTRACT 1    FROM MES OF W01-OO
                IF MES OF W01-OO < 1 THEN
                   MOVE 12       TO MES OF W01-OO
                   SUBTRACT 1  FROM ANO OF W01-OO
                END-IF
                PERFORM RECORRER-MES
             ELSE
                IF BTNNEXT OF W01-II = 1 THEN
                   ADD 1         TO MES OF W01-OO
                   IF MES OF W01-OO > 12 THEN
                      MOVE 1     TO MES OF W01-OO
                      ADD  1     TO ANO OF W01-OO
                   END-IF
                   PERFORM RECORRER-MES
                 ELSE
                   IF IN03 OF W01-I-INDIC = I-OFF
                      PERFORM EVALUA-BOTON
                      IF CALENDARIO-DIA-O = 0
                         MOVE MES OF W01-II TO
                                         MES OF W01-OO  CALENDARIO-MES-O
                         MOVE ANO OF W01-II TO
                                         ANO OF W01-OO  CALENDARIO-ANO-O
                         PERFORM RECORRER-MES
                      END-IF
                   END-IF
                 END-IF
             END-IF
           END-IF

           .
      *
       MOSTRAR-AYUDA.
      *--------------
           MOVE "MESES"            TO HLP-TABLA
           MOVE SPACES             TO HLP-CODIGO
                                      HLP-DESC-CORTA
                                      HLP-DESC-LARGA
           MOVE ZEROES             TO HLP-LARGO-COD
           CALL "VERHLPSRCH" USING INP-HLP
                                   OUT-HLP
           IF HLP-CODIGO NOT = SPACES
              MOVE HLP-CODIGO(1:HLP-LARGO-COD) TO MES OF W01-OO
                                                  MES OF W01-II
           END-IF
           .

       LIMPIAR-TEXTOS.
      *----------------
           MOVE I-OFF    TO  IN50 OF W01-O-INDIC
                             IN51 OF W01-O-INDIC
                             IN52 OF W01-O-INDIC
                             IN53 OF W01-O-INDIC
                             IN54 OF W01-O-INDIC
                             IN55 OF W01-O-INDIC
                             IN56 OF W01-O-INDIC

                             IN60 OF W01-O-INDIC
                             IN61 OF W01-O-INDIC
                             IN62 OF W01-O-INDIC
                             IN63 OF W01-O-INDIC
                             IN64 OF W01-O-INDIC
                             IN65 OF W01-O-INDIC
                             IN66 OF W01-O-INDIC

                             IN70 OF W01-O-INDIC
                             IN71 OF W01-O-INDIC
                             IN72 OF W01-O-INDIC
                             IN73 OF W01-O-INDIC
                             IN74 OF W01-O-INDIC
                             IN75 OF W01-O-INDIC
                             IN76 OF W01-O-INDIC

                             IN80 OF W01-O-INDIC
                             IN81 OF W01-O-INDIC
                             IN82 OF W01-O-INDIC
                             IN83 OF W01-O-INDIC
                             IN84 OF W01-O-INDIC
                             IN85 OF W01-O-INDIC
                             IN86 OF W01-O-INDIC

                             IN90 OF W01-O-INDIC
                             IN91 OF W01-O-INDIC
                             IN92 OF W01-O-INDIC
                             IN93 OF W01-O-INDIC
                             IN94 OF W01-O-INDIC
                             IN95 OF W01-O-INDIC
                             IN96 OF W01-O-INDIC

           MOVE ZEROES                 TO BTNL1C1 OF W01-OO
                                          BTNL1C2 OF W01-OO
                                          BTNL1C3 OF W01-OO
                                          BTNL1C4 OF W01-OO
                                          BTNL1C5 OF W01-OO
                                          BTNL1C6 OF W01-OO
                                          BTNL1C7 OF W01-OO
                                          BTNL2C1 OF W01-OO
                                          BTNL2C2 OF W01-OO
                                          BTNL2C3 OF W01-OO
                                          BTNL2C4 OF W01-OO
                                          BTNL2C5 OF W01-OO
                                          BTNL2C6 OF W01-OO
                                          BTNL2C7 OF W01-OO
                                          BTNL3C1 OF W01-OO
                                          BTNL3C2 OF W01-OO
                                          BTNL3C3 OF W01-OO
                                          BTNL3C4 OF W01-OO
                                          BTNL3C5 OF W01-OO
                                          BTNL3C6 OF W01-OO
                                          BTNL3C7 OF W01-OO
                                          BTNL4C1 OF W01-OO
                                          BTNL4C2 OF W01-OO
                                          BTNL4C3 OF W01-OO
                                          BTNL4C4 OF W01-OO
                                          BTNL4C5 OF W01-OO
                                          BTNL4C6 OF W01-OO
                                          BTNL4C7 OF W01-OO
                                          BTNL5C1 OF W01-OO
                                          BTNL5C2 OF W01-OO
                                          BTNL5C3 OF W01-OO
                                          BTNL5C4 OF W01-OO
                                          BTNL5C5 OF W01-OO
                                          BTNL5C6 OF W01-OO
                                          BTNL5C7 OF W01-OO

           MOVE SPACES               TO  TBTNL1C1 OF W01-OO
                                         TBTNL1C2 OF W01-OO
                                         TBTNL1C3 OF W01-OO
                                         TBTNL1C4 OF W01-OO
                                         TBTNL1C5 OF W01-OO
                                         TBTNL1C6 OF W01-OO
                                         TBTNL1C7 OF W01-OO
                                         TBTNL2C1 OF W01-OO
                                         TBTNL2C2 OF W01-OO
                                         TBTNL2C3 OF W01-OO
                                         TBTNL2C4 OF W01-OO
                                         TBTNL2C5 OF W01-OO
                                         TBTNL2C6 OF W01-OO
                                         TBTNL2C7 OF W01-OO
                                         TBTNL3C1 OF W01-OO
                                         TBTNL3C2 OF W01-OO
                                         TBTNL3C3 OF W01-OO
                                         TBTNL3C4 OF W01-OO
                                         TBTNL3C5 OF W01-OO
                                         TBTNL3C6 OF W01-OO
                                         TBTNL3C7 OF W01-OO
                                         TBTNL4C1 OF W01-OO
                                         TBTNL4C2 OF W01-OO
                                         TBTNL4C3 OF W01-OO
                                         TBTNL4C4 OF W01-OO
                                         TBTNL4C5 OF W01-OO
                                         TBTNL4C6 OF W01-OO
                                         TBTNL4C7 OF W01-OO
                                         TBTNL5C1 OF W01-OO
                                         TBTNL5C2 OF W01-OO
                                         TBTNL5C3 OF W01-OO
                                         TBTNL5C4 OF W01-OO
                                         TBTNL5C5 OF W01-OO
                                         TBTNL5C6 OF W01-OO
                                         TBTNL5C7 OF W01-OO

           MOVE " 00 "                 TO ARR-DIA(1)
                                          ARR-DIA(2)
                                          ARR-DIA(3)
                                          ARR-DIA(4)
                                          ARR-DIA(5)
                                          ARR-DIA(6)
                                          ARR-DIA(7)
           .
       RECORRER-MES.
      *-------------
           PERFORM LIMPIAR-TEXTOS
           IF ANO OF W01-OO NOT = 0 AND
              MES OF W01-OO NOT = 0
              MOVE 1                 TO WS-LIN-CAL

              MOVE ANO   OF W01-OO   TO WS-ANO
              MOVE MES   OF W01-OO   TO WS-MES
              MOVE ZEROES            TO WS-DIA
              MOVE WS-FECHA          TO FECHA  OF CALENDA
              START CALENDA KEY IS NOT < EXTERNALLY-DESCRIBED-KEY
                    END-START
              IF FS-CALENDA = "00"
                 PERFORM LEER-NEXT-CALENDA
                 PERFORM RECORRER-CALENDA UNTIL FS-CALENDA NOT = "00"
                         OR WS-ANO NOT = ANO OF W01-OO
                         OR WS-MES NOT = MES OF W01-OO
                 PERFORM MOVER-TEXTOS
              END-IF
           END-IF
           .
      *
       RECORRER-CALENDA.
      *-----------------
           MOVE FECHA OF CALENDA  TO WS-FECHA

           EVALUATE DIASEMANA OF CALENDA
             WHEN 1 MOVE WS-DIA   TO ARR-DIA-S(7)
             WHEN 2 MOVE WS-DIA   TO ARR-DIA-S(1)
             WHEN 3 MOVE WS-DIA   TO ARR-DIA-S(2)
             WHEN 4 MOVE WS-DIA   TO ARR-DIA-S(3)
             WHEN 5 MOVE WS-DIA   TO ARR-DIA-S(4)
             WHEN 6 MOVE WS-DIA   TO ARR-DIA-S(5)
             WHEN 7 MOVE WS-DIA   TO ARR-DIA-S(6)
           END-EVALUATE
           IF DIASEMANA OF CALENDA = 1
              PERFORM MOVER-TEXTOS
              ADD 1   TO WS-LIN-CAL
              MOVE " 00 "                 TO ARR-DIA(1)
                                             ARR-DIA(2)
                                             ARR-DIA(3)
                                             ARR-DIA(4)
                                             ARR-DIA(5)
                                             ARR-DIA(6)
                                             ARR-DIA(7)
           END-IF


           PERFORM LEER-NEXT-CALENDA
           IF FS-CALENDA = "00"
              MOVE FECHA OF CALENDA  TO WS-FECHA
           END-IF
           .

       LEER-NEXT-CALENDA.
      *------------------
           READ CALENDA NEXT RECORD END-READ.

      *
       MOVER-TEXTOS.
      *-------------
            EVALUATE WS-LIN-CAL
              WHEN 1
                   IF ARR-DIA-S(1)   NOT = 0 THEN
                      MOVE  ARR-DIA (1) TO TBTNL1C1    OF W01-OO
                      MOVE I-ON         TO IN50 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(2)   NOT = 0 THEN
                      MOVE  ARR-DIA (2) TO TBTNL1C2    OF W01-OO
                      MOVE I-ON         TO IN51 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(3)   NOT = 0 THEN
                      MOVE  ARR-DIA (3) TO TBTNL1C3    OF W01-OO
                      MOVE I-ON         TO IN52 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(4)   NOT = 0 THEN
                      MOVE  ARR-DIA (4) TO TBTNL1C4    OF W01-OO
                      MOVE I-ON         TO IN53 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(5)   NOT = 0 THEN
                      MOVE  ARR-DIA (5) TO TBTNL1C5    OF W01-OO
                      MOVE I-ON         TO IN54 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(6)   NOT = 0 THEN
                      MOVE  ARR-DIA (6) TO TBTNL1C6    OF W01-OO
                      MOVE I-ON         TO IN55 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(7)   NOT = 0 THEN
                      MOVE  ARR-DIA (7) TO TBTNL1C7    OF W01-OO
                      MOVE I-ON         TO IN56 OF W01-O-INDIC
                   END-IF
              WHEN 2
                   IF ARR-DIA-S(1)   NOT = 0 THEN
                      MOVE  ARR-DIA (1) TO TBTNL2C1    OF W01-OO
                      MOVE I-ON         TO IN60 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(2)   NOT = 0 THEN
                      MOVE  ARR-DIA (2) TO TBTNL2C2    OF W01-OO
                      MOVE I-ON         TO IN61 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(3)   NOT = 0 THEN
                      MOVE  ARR-DIA (3) TO TBTNL2C3    OF W01-OO
                      MOVE I-ON         TO IN62 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(4)   NOT = 0 THEN
                      MOVE  ARR-DIA (4) TO TBTNL2C4    OF W01-OO
                      MOVE I-ON         TO IN63 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(5)   NOT = 0 THEN
                      MOVE  ARR-DIA (5) TO TBTNL2C5    OF W01-OO
                      MOVE I-ON         TO IN64 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(6)   NOT = 0 THEN
                      MOVE  ARR-DIA (6) TO TBTNL2C6    OF W01-OO
                      MOVE I-ON         TO IN65 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(7)   NOT = 0 THEN
                      MOVE  ARR-DIA (7) TO TBTNL2C7    OF W01-OO
                      MOVE I-ON         TO IN66 OF W01-O-INDIC
                   END-IF
              WHEN 3
                   IF ARR-DIA-S(1)   NOT = 0 THEN
                      MOVE  ARR-DIA (1) TO TBTNL3C1    OF W01-OO
                      MOVE I-ON         TO IN70 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(2)   NOT = 0 THEN
                      MOVE  ARR-DIA (2) TO TBTNL3C2    OF W01-OO
                      MOVE I-ON         TO IN71 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(3)   NOT = 0 THEN
                      MOVE  ARR-DIA (3) TO TBTNL3C3    OF W01-OO
                      MOVE I-ON         TO IN72 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(4)   NOT = 0 THEN
                      MOVE  ARR-DIA (4) TO TBTNL3C4    OF W01-OO
                      MOVE I-ON         TO IN73 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(5)   NOT = 0 THEN
                      MOVE  ARR-DIA (5) TO TBTNL3C5    OF W01-OO
                      MOVE I-ON         TO IN74 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(6)   NOT = 0 THEN
                      MOVE  ARR-DIA (6) TO TBTNL3C6    OF W01-OO
                      MOVE I-ON         TO IN75 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(7)   NOT = 0 THEN
                      MOVE  ARR-DIA (7) TO TBTNL3C7    OF W01-OO
                      MOVE I-ON         TO IN76 OF W01-O-INDIC
                   END-IF
              WHEN 4
                   IF ARR-DIA-S(1)   NOT = 0 THEN
                      MOVE  ARR-DIA (1) TO TBTNL4C1    OF W01-OO
                      MOVE I-ON         TO IN80 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(2)   NOT = 0 THEN
                      MOVE  ARR-DIA (2) TO TBTNL4C2    OF W01-OO
                      MOVE I-ON         TO IN81 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(3)   NOT = 0 THEN
                      MOVE  ARR-DIA (3) TO TBTNL4C3    OF W01-OO
                      MOVE I-ON         TO IN82 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(4)   NOT = 0 THEN
                      MOVE  ARR-DIA (4) TO TBTNL4C4    OF W01-OO
                      MOVE I-ON         TO IN83 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(5)   NOT = 0 THEN
                      MOVE  ARR-DIA (5) TO TBTNL4C5    OF W01-OO
                      MOVE I-ON         TO IN84 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(6)   NOT = 0 THEN
                      MOVE  ARR-DIA (6) TO TBTNL4C6    OF W01-OO
                      MOVE I-ON         TO IN85 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(7)   NOT = 0 THEN
                      MOVE  ARR-DIA (7) TO TBTNL4C7    OF W01-OO
                      MOVE I-ON         TO IN86 OF W01-O-INDIC
                   END-IF
              WHEN 5
                   IF ARR-DIA-S(1)   NOT = 0 THEN
                      MOVE  ARR-DIA (1) TO TBTNL5C1    OF W01-OO
                      MOVE I-ON         TO IN90 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(2)   NOT = 0 THEN
                      MOVE  ARR-DIA (2) TO TBTNL5C2    OF W01-OO
                      MOVE I-ON         TO IN91 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(3)   NOT = 0 THEN
                      MOVE  ARR-DIA (3) TO TBTNL5C3    OF W01-OO
                      MOVE I-ON         TO IN92 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(4)   NOT = 0 THEN
                      MOVE  ARR-DIA (4) TO TBTNL5C4    OF W01-OO
                      MOVE I-ON         TO IN93 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(5)   NOT = 0 THEN
                      MOVE  ARR-DIA (5) TO TBTNL5C5    OF W01-OO
                      MOVE I-ON         TO IN94 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(6)   NOT = 0 THEN
                      MOVE  ARR-DIA (6) TO TBTNL5C6    OF W01-OO
                      MOVE I-ON         TO IN95 OF W01-O-INDIC
                   END-IF
                   IF ARR-DIA-S(7)   NOT = 0 THEN
                      MOVE  ARR-DIA (7) TO TBTNL5C7    OF W01-OO
                      MOVE I-ON         TO IN96 OF W01-O-INDIC
                   END-IF
            END-EVALUATE
            .

       EVALUA-BOTON.
      *--------------
            IF BTNL1C1 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C1 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL1C2 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C2 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL1C3 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C3 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL1C4 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C4 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL1C5 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C5 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL1C6 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C6 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL1C7 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C7 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF

            IF BTNL2C1 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL2C1 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL2C2 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL2C2 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL2C3 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL2C3 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL2C4 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL2C4 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL2C5 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL1C5 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL2C6 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL2C6 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL2C7 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL2C7 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF

            IF BTNL3C1 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C1 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL3C2 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C2 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL3C3 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C3 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL3C4 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C4 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL3C5 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C5 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL3C6 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C6 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL3C7 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL3C7 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF

            IF BTNL4C1 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C1 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL4C2 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C2 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL4C3 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C3 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL4C4 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C4 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL4C5 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C5 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL4C6 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C6 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL4C7 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL4C7 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF

            IF BTNL5C1 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C1 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL5C2 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C2 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL5C3 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C3 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL5C4 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C4 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL5C5 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C5 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL5C6 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C6 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            IF BTNL5C7 OF W01-II NOT = ZEROES THEN
               MOVE TBTNL5C7 OF W01-OO(2:2)  TO CALENDARIO-DIA-O
            END-IF
            .
