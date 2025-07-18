      ******************************************************************
      * Author: Jorge Duarte
      * Date: 14-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SnakeCase.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 PAR-INPUT-MIN.
           05 MIN-STRING-CONVERTIR PIC X(500).
       01 PAR-OUTPUT-MIN.
           05 MIN-STRING-CONVERTIDO PIC X(500).

       LINKAGE SECTION.
       01 PAR-INP-SNAKE.
           05 SNAKE-TEXTO-CONVERTIR PIC X(500).
       01 PAR-OUT-SNAKE.
           05 SNAKE-TEXTO-CONVERTIDO PIC X(500).

       PROCEDURE DIVISION USING PAR-INP-SNAKE
                                PAR-OUT-SNAKE.
       MAIN-PROCEDURE.

            MOVE SNAKE-TEXTO-CONVERTIR TO MIN-STRING-CONVERTIR
            MOVE SPACES                TO MIN-STRING-CONVERTIDO
            CALL "ConvertirMinusculas" USING PAR-INPUT-MIN
                                             PAR-OUTPUT-MIN

            MOVE MIN-STRING-CONVERTIDO TO SNAKE-TEXTO-CONVERTIDO
            INSPECT SNAKE-TEXTO-CONVERTIDO
                   REPLACING ALL " " BY "_"

            GOBACK.
       END PROGRAM SnakeCase.
