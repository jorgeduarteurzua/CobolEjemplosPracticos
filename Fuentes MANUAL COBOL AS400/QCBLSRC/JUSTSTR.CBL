       IDENTIFICATION DIVISION.
       PROGRAM-ID. CENTRARSTR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 WS-SALIR        PIC X.
           05 POS-STR         PIC 9(05).
           05 POS-STR-J       PIC 9(05).
           05 POS-STR-F       PIC 9(05).
           05 I               PIC 9(05).
           05 CONT-PALABRAS   PIC 9(05).
           05 DIF-SPACES      PIC 9(05).
           05 WS-LARGO        PIC 9(05).
           05 WS-STR-TRABAJAR PIC X(500).
           05 WS-STR-MOVER    PIC X(500).

       01 PAR-INPUT.
           05 STRING-CALULAR   PIC X(500).
       01 PAR-OUTPUT.
           05 LK-LARGO-STR     PIC 9(03).

       LINKAGE SECTION.
       01 INP-JUSTIFICAR-TXT.
           05 TEXTO-JUSTIFICAR    PIC X(5000).
           05 TEXTO-LARGO-JUST    PIC 9(4).
       01 OUT-JUSTIFICAR-TXT.
           05 TEXTO-JUSTIFICADO   PIC X(10000).



       PROCEDURE DIVISION USING INP-JUSTIFICAR-TXT
                                OUT-JUSTIFICAR-TXT.
           MOVE SPACES         TO TEXTO-JUSTIFICADO
           MOVE 1              TO POS-STR
           IF TEXTO-LARGO-JUST > 5000 THEN
              MOVE TEXTO-JUSTIFICAR TO TEXTO-JUSTIFICADO
           ELSE
              PERFORM RECORRER-STRING-INICIAL
           END-IF
           GOBACK.
      *
       RECORRER-STRING-INICIAL.
      *-------------------------
           MOVE 1               TO POS-STR POS-STR-J
           MOVE ZEROES          TO POS-STR-F
           MOVE "N"             TO WS-SALIR
           PERFORM UNTIL POS-STR > 5000 OR WS-SALIR = "S"
               IF TEXTO-JUSTIFICAR(POS-STR:TEXTO-LARGO-JUST) = SPACES
                  MOVE "S"      TO WS-SALIR
               ELSE
                  MOVE TEXTO-JUSTIFICAR(POS-STR:TEXTO-LARGO-JUST)
                                       TO STRING-CALULAR
                                          WS-STR-TRABAJAR
                  MOVE ZEROES          TO LK-LARGO-STR
                  CALL "LARGOSTR" USING PAR-INPUT
                                        PAR-OUTPUT


                  IF TEXTO-JUSTIFICAR((POS-STR + TEXTO-LARGO-JUST):1)
                     NOT = SPACES                                  OR
                    TEXTO-JUSTIFICAR((POS-STR + TEXTO-LARGO-JUST - 1):1)
                     NOT = SPACES                                  OR
                    TEXTO-JUSTIFICAR((POS-STR + TEXTO-LARGO-JUST - 2):1)
                     NOT = SPACES                                  THEN

                     COMPUTE POS-STR-J = POS-STR + TEXTO-LARGO-JUST - 1

                     PERFORM UNTIL TEXTO-JUSTIFICAR(POS-STR-J:1)
                                 = SPACES
                        SUBTRACT 1 FROM POS-STR-J
                     END-PERFORM
                     COMPUTE WS-LARGO = POS-STR-J - POS-STR

                     MOVE ZEROES      TO CONT-PALABRAS
                     PERFORM VARYING I FROM POS-STR BY 1
                             UNTIL I > POS-STR-J
                         IF TEXTO-JUSTIFICAR(I:1) = " "
                            ADD 1   TO CONT-PALABRAS
                         END-IF
                     END-PERFORM
                     COMPUTE DIF-SPACES = TEXTO-LARGO-JUST
                                        - WS-LARGO

                     PERFORM VARYING I FROM POS-STR BY 1
                             UNTIL I > POS-STR-J
                         IF TEXTO-JUSTIFICAR(I:1) NOT = SPACES
                            ADD  1   TO POS-STR-F
                            MOVE TEXTO-JUSTIFICAR(I:1)    TO
                                 TEXTO-JUSTIFICADO(POS-STR-F:1)
                         ELSE
                          IF DIF-SPACES NOT = 0 THEN
                            ADD  1   TO POS-STR-F
                            MOVE "  " TO TEXTO-JUSTIFICADO(POS-STR-F:2)
                            ADD  1    TO POS-STR-F
                            SUBTRACT 1 FROM DIF-SPACES
                          ELSE
                            ADD  1   TO POS-STR-F
                            MOVE " "  TO TEXTO-JUSTIFICADO(POS-STR-F:1)
                          END-IF
                         END-IF
                     END-PERFORM
                     MOVE POS-STR-J   TO POS-STR
                     SUBTRACT 1     FROM POS-STR-F
                     ADD  1           TO POS-STR
                  ELSE
                     ADD  1   TO POS-STR-F
                     MOVE TEXTO-JUSTIFICAR(POS-STR:1)  TO
                          TEXTO-JUSTIFICADO(POS-STR-F:1)
                     ADD  1           TO POS-STR
                  END-IF
               END-IF
           END-PERFORM
           .
