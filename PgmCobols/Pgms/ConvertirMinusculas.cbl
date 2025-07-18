      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 08-07-2025
      * Purpose: Eseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ConvertirMinusculas.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 I-STR     PIC 9(3).
           05 I-MIN     PIC 9(3).
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
                   PERFORM VARYING I-MIN FROM 1 BY 1 UNTIL I-MIN > 27
                      IF STRING-CONVERTIDO(I-STR:1) =
                         WS-MAY(I-MIN:1) THEN
                         MOVE WS-MIN(I-MIN:1) TO
                              STRING-CONVERTIDO(I-STR:1)
                         ADD 28  TO I-MIN
                       END-IF
                   END-PERFORM
                END-IF

            END-PERFORM

            GOBACK.

       END PROGRAM ConvertirMinusculas.
