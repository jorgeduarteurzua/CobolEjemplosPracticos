      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 08-07-2025
      * Purpose: Eseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ConvertirMayusculas.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 I-STR     PIC 9(3).
           05 I-MAY     PIC 9(3).
           05 LARGO-STR PIC 9(03).

           05 WS-MIN PIC X(27) VALUE "abcdefghijklmnñopqrstuvwxyz".
           05 WS-MAY PIC X(27) VALUE "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ".

       LINKAGE SECTION.
       01 PAR-INPUT.
           05 STRING-CONVERTIR PIC X(500).
       01 PAR-OUTPUT.
           05 STRING-CONVERTIDO PIC X(500).

       PROCEDURE DIVISION USING PAR-INPUT
                                PAR-OUTPUT.

       MAIN-PROCEDURE.

            CALL "LargoString" using PAR-INPUT LARGO-STR

            PERFORM VARYING I-STR FROM 1 BY 1 UNTIL I-STR > LARGO-STR
                MOVE STRING-CONVERTIR (I-STR:1) TO
                     STRING-CONVERTIDO(I-STR:1)
                IF STRING-CONVERTIDO(I-STR:1) NOT = SPACES
                   PERFORM VARYING I-MAY FROM 1 BY 1 UNTIL I-MAY > 27
                      IF STRING-CONVERTIDO(I-STR:1) =
                         WS-MIN(I-MAY:1) THEN
                         MOVE WS-MAY(I-MAY:1) TO
                              STRING-CONVERTIDO(I-STR:1)
                         ADD 28  TO I-MAY
                       END-IF
                   END-PERFORM
                END-IF

            END-PERFORM

            GOBACK.

       END PROGRAM ConvertirMayusculas.
