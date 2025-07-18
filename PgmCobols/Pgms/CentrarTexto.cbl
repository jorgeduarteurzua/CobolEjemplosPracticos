      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 16-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CentrarTexto.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 POS-STR          PIC 9(3).

       01 PAR-INPUT-LARGOSTR.
           05 STRING-CALCULAR   PIC X(500).
       01 PAR-OUTPUT-LARGOSTR.
           05 LK-LARGO-STR     PIC 9(03).

       LINKAGE SECTION.
       01 INP-CENTRAR-TXT.
           05 TEXTO-CENTRAR PIC X(100).
       01 OUT-CENTRAR-TXT.
           05 TXT-CODRET     PIC X.
      *       S = TEXTO TEXTO-CENTRADO
      *       N = NO SE PUDO CENTRAR TEXTO
           05 TEXTO-CENTRADO PIC X(100).

       PROCEDURE DIVISION USING INP-CENTRAR-TXT
                                OUT-CENTRAR-TXT.
       MAIN-PROCEDURE.

            MOVE TEXTO-CENTRAR   TO STRING-CALCULAR
            MOVE 0               TO LK-LARGO-STR
            CALL "LargoString"  USING PAR-INPUT-LARGOSTR
                                      PAR-OUTPUT-LARGOSTR

            IF LK-LARGO-STR < 1 THEN
                MOVE "N"           TO TXT-CODRET
                MOVE TEXTO-CENTRAR TO TEXTO-CENTRADO
            ELSE
                COMPUTE POS-STR = (100 - LK-LARGO-STR ) / 2
                MOVE SPACES         TO TEXTO-CENTRADO
                MOVE TEXTO-CENTRAR(1:LK-LARGO-STR)  TO
                                       TEXTO-CENTRADO(POS-STR:)
                MOVE "S"             TO TXT-CODRET
            END-IF

            GOBACK.
       END PROGRAM CentrarTexto.
