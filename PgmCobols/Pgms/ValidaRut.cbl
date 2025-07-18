      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 07-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ValidaRut.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 WS-RUT     PIC 9(10).
           05 WS-DV      PIC X.
           05 ENTERO     PIC 9(2).
           05 RESTO      PIC 9(2).
           05 RESTA-11   PIC 9(3).
           05 I-MULT     PIC 9.
           05 SUMA-MULT  PIC 9(5).

       LINKAGE SECTION.
       01 PARAMETRO-ENTRADA.
           05 LK-RUT    PIC 9(10).
           05 LK-DV     PIC X.
       01 PARAMETRO-SALIDA.
           05 LK-CODRET PIC 9.
      *       0 = OK
      *       1 = NO OK
       PROCEDURE DIVISION USING PARAMETRO-ENTRADA
                                PARAMETRO-SALIDA.
       MAIN-PROCEDURE.

           MOVE LK-RUT      TO WS-RUT
           MOVE 2           TO I-MULT
           MOVE 0           TO SUMA-MULT
           PERFORM UNTIL WS-RUT = 0
               DIVIDE WS-RUT BY 10 GIVING WS-RUT
                      REMAINDER RESTO
               IF I-MULT > 7
                  MOVE 2   TO I-MULT
               END-IF
               COMPUTE SUMA-MULT = SUMA-MULT + (RESTO * I-MULT)
               ADD 1       TO I-MULT
           END-PERFORM

           DIVIDE SUMA-MULT BY 11 GIVING ENTERO
                                 REMAINDER RESTO
           COMPUTE RESTA-11 = 11 - RESTO
           EVALUATE RESTA-11
             WHEN 0   MOVE "0"   TO WS-DV
             WHEN 1   MOVE "1"   TO WS-DV
             WHEN 2   MOVE "2"   TO WS-DV
             WHEN 3   MOVE "3"   TO WS-DV
             WHEN 4   MOVE "4"   TO WS-DV
             WHEN 5   MOVE "5"   TO WS-DV
             WHEN 6   MOVE "6"   TO WS-DV
             WHEN 7   MOVE "7"   TO WS-DV
             WHEN 8   MOVE "8"   TO WS-DV
             WHEN 9   MOVE "9"   TO WS-DV
             WHEN 10  MOVE "K"   TO WS-DV
             WHEN 11  MOVE "0"   TO WS-DV
           END-EVALUATE

           IF LK-DV NOT = WS-DV THEN
               MOVE 1     TO LK-CODRET
           ELSE
               MOVE 0     TO LK-CODRET
           END-IF

           GOBACK.


       END PROGRAM ValidaRut.
