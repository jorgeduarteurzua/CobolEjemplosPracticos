      ******************************************************************
      * Author: JORGE DUARTE URZUA.
      * Date: 25-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ContarStr.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 I                 PIC 9(05).
           05 SALIR             PIC X.
           05 WS-LARGO-STR-REV  PIC 9(05).
           05 WS-LARGO-STR-BUS  PIC 9(05).

       01 INPUT-LARGOSTR10000.
           05 STRING-CALCULAR   PIC X(10000).
       01 OUTPUT-LARGOSTR10000.
           05 LK-LARGO-STR     PIC 9(05).

       LINKAGE SECTION.
       01 INP-CONTARSTR.
           05 CONTARSTR-STRING-REVISAR    PIC X(10000).
           05 CONTARSTR-STRING-BUSCAR     PIC X(20).
       01 OUT-CONTARSTR.
           05 TOTAL-STR         PIC 9(05).

       PROCEDURE DIVISION USING INP-CONTARSTR
                                OUT-CONTARSTR.
       MAIN-PROCEDURE.

           MOVE ZEROES           TO TOTAL-STR

           IF CONTARSTR-STRING-BUSCAR = SPACES
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LARGO-STR-REV
                    IF CONTARSTR-STRING-REVISAR(I:1) = " "
                        ADD 1            TO TOTAL-STR
                    END-IF
               END-PERFORM
           ELSE
               MOVE CONTARSTR-STRING-REVISAR TO STRING-CALCULAR
               MOVE ZEROES                   TO LK-LARGO-STR
               CALL "LargoString10000" USING INPUT-LARGOSTR10000
                                             OUTPUT-LARGOSTR10000
               MOVE LK-LARGO-STR             TO WS-LARGO-STR-REV

               MOVE CONTARSTR-STRING-BUSCAR TO STRING-CALCULAR
               MOVE ZEROES                   TO LK-LARGO-STR
               CALL "LargoString10000" USING INPUT-LARGOSTR10000
                                             OUTPUT-LARGOSTR10000
               MOVE LK-LARGO-STR             TO WS-LARGO-STR-BUS

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LARGO-STR-REV
                    IF CONTARSTR-STRING-REVISAR(I:WS-LARGO-STR-BUS) =
                        CONTARSTR-STRING-BUSCAR(1:WS-LARGO-STR-BUS)
                        ADD 1            TO TOTAL-STR
                    END-IF
               END-PERFORM
           END-IF

           GOBACK.
       END PROGRAM ContarStr.
