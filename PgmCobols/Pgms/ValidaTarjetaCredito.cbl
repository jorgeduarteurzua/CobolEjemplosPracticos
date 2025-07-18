      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 14-07-2025
      * Purpose: Enseñanza - Utiliza Algoritmo de Luhn
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ValidaTarjetaCredito.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 WS-SUMA    PIC S9(7).
           05 WS-MULT    PIC S9(2).
           05 I          PIC 9(02).
           05 ENTERO     PIC 9(3).
           05 RESTO      PIC 9(3).
           05 WS-DIGITO  PIC 9.


       LINKAGE SECTION.
       01 PAR-INPUT.
           05 LK-TARJETA    PIC X(16).
       01 PAR-OUTPUT.
           05 LK-CODRET      PIC X.
      *       S = Tarjeta Válida
      *       N = Tarjeta Inválida
       PROCEDURE DIVISION USING PAR-INPUT
                                PAR-OUTPUT.
       MAIN-PROCEDURE.

           MOVE ZEROES         TO WS-SUMA
           MOVE "N"            TO LK-CODRET
           MOVE "4830310043224451" TO LK-TARJETA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16
              IF LK-TARJETA(I:1) IS NUMERIC THEN
                  MOVE LK-TARJETA(I:1) TO WS-DIGITO
                  DIVIDE I BY 2 GIVING ENTERO
                                      REMAINDER RESTO
                  IF RESTO = 0 THEN
                     ADD WS-DIGITO  TO WS-SUMA
                  ELSE
                     COMPUTE WS-MULT = WS-DIGITO * 2
                     IF WS-MULT > 9 THEN
                         SUBTRACT 9 FROM WS-MULT
                     END-IF
                     ADD WS-MULT   TO WS-SUMA
                  END-IF
              ELSE
      *           SI EL DIGITO NO ES UN NUMERO DEVOLVEMOS INMEIDATAMENTE
      *           QUE LA TARJETA NO ES VALIDA
                  ADD 26 TO I
              END-IF
           END-PERFORM

           IF I <= 17 THEN
               DIVIDE WS-SUMA BY 10 GIVING ENTERO
                                 REMAINDER RESTO
               IF RESTO = 0 THEN
                  MOVE "S"    TO LK-CODRET
               END-IF
           END-IF

           GOBACK.
       END PROGRAM ValidaTarjetaCredito.
