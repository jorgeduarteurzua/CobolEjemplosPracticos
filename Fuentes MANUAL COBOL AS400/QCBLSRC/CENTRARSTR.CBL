       IDENTIFICATION DIVISION.
       PROGRAM-ID. CENTRARSTR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 POS-STR   PIC 9(03).

       01 PAR-INPUT.
           05 STRING-CALULAR   PIC X(500).
       01 PAR-OUTPUT.
           05 LK-LARGO-STR     PIC 9(03).

       LINKAGE SECTION.
       01 INP-CENTRAR-TXT.
           05 TEXTO-CENTRAR       PIC X(100).
           05 TEXTO-LARGO-CENTRAR PIC 9(3).
       01 OUT-CENTRAR-TXT.
           05 TXT-CODRET     PIC X.
      *       S = TEXTO TEXTO-CENTRADO
      *       N = NO SE PUDO CENTRAR TEXTO
           05 TEXTO-CENTRADO PIC X(100).



       PROCEDURE DIVISION USING INP-CENTRAR-TXT
                                OUT-CENTRAR-TXT.

           MOVE TEXTO-CENTRAR  TO TEXTO-CENTRADO
           IF TEXTO-LARGO-CENTRAR > 100 THEN
              MOVE "N"             TO TXT-CODRET
           ELSE
              MOVE TEXTO-CENTRAR   TO STRING-CALULAR
              MOVE ZEROES          TO LK-LARGO-STR
              CALL "LARGOSTR" USING PAR-INPUT
                                    PAR-OUTPUT
              IF LK-LARGO-STR > TEXTO-LARGO-CENTRAR THEN
                 MOVE "N"          TO TXT-CODRET
              ELSE
                COMPUTE POS-STR = (TEXTO-LARGO-CENTRAR
                                - LK-LARGO-STR ) / 2
                MOVE SPACES         TO TEXTO-CENTRADO
                MOVE TEXTO-CENTRAR(1:LK-LARGO-STR)  TO
                                       TEXTO-CENTRADO(POS-STR:)
                MOVE "S"             TO TXT-CODRET
              END-IF
           END-IF
           GOBACK
           .
