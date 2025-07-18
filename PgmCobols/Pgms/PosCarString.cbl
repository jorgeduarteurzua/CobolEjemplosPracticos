      ******************************************************************
      * Author: JORGE DUARTE URZUA.
      * Date: 16-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PosCarString.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 I                    PIC 9(05).
       77 SALIR                PIC X.

       01 INPUT-LARGOSTR10000.
           05 STRING-CALCULAR   PIC X(10000).
       01 OUTPUT-LARGOSTR10000.
           05 LK-LARGO-STR     PIC 9(05).

       LINKAGE SECTION.
       01 INP-POSCARSTR.
           05 POSCARSTR-STRING-BUSCAR    PIC X(10000).
           05 POSCARSTR-CARACTER-BUSCAR  PIC X.
       01 OUT-POSCARSTR.
           05 POSCARSTR-POSICION         PIC 9(05).

       PROCEDURE DIVISION USING INP-POSCARSTR
                                OUT-POSCARSTR.
       MAIN-PROCEDURE.

           MOVE POSCARSTR-STRING-BUSCAR TO STRING-CALCULAR
           MOVE ZEROES                  TO LK-LARGO-STR
           CALL "LargoString10000" USING INPUT-LARGOSTR10000
                                         OUTPUT-LARGOSTR10000
           MOVE 0   TO I
           MOVE "N" TO SALIR
           PERFORM  UNTIL I > LK-LARGO-STR OR SALIR = 'S'
                ADD 1    TO I
                IF POSCARSTR-STRING-BUSCAR(I:1) =
                   POSCARSTR-CARACTER-BUSCAR THEN
                   MOVE "S"    TO SALIR
                END-IF

           END-PERFORM

           IF SALIR = "S" THEN
              MOVE I    TO POSCARSTR-POSICION
           ELSE
              MOVE 0    TO POSCARSTR-POSICION
           END-IF


           GOBACK.
       END PROGRAM PosCarString.
