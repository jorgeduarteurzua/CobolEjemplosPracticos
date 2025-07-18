      ******************************************************************
      * Author: Jorge Duarte
      * Date: 14-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProbarTrimySnake.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 WS-TEXTO PIC X(500).

       01 PAR-INPUT-TRIM.
           05 TEXTO-INICIAL PIC X(500).
       01 PAR-OUTPUT-TRIM.
           05 NEW-TEXTO     PIC X(500).
           05 LARGO-NEW-TEXTO PIC 9(03).

       01 PAR-INP-SNAKE.
           05 SNAKE-TEXTO-CONVERTIR PIC X(500).
       01 PAR-OUT-SNAKE.
           05 SNAKE-TEXTO-CONVERTIDO PIC X(500).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE SPACES   TO WS-TEXTO
            STRING "     Este es un nuevo "    DELIMITED SIZE
                   "TEXTO que usaremos para "  DELIMITED SIZE
                   "probar TRIM y SNAKE CASE." DELIMITED SIZE
                                          INTO WS-TEXTO

            MOVE WS-TEXTO                   TO TEXTO-INICIAL
                                               SNAKE-TEXTO-CONVERTIR
            MOVE ZEROES                     TO LARGO-NEW-TEXTO

            DISPLAY "TEXTO USAR [" WS-TEXTO "]"

            CALL "Trim" USING PAR-INPUT-TRIM
                              PAR-OUTPUT-TRIM

            CALL "SnakeCase" USING PAR-INP-SNAKE
                                   PAR-OUT-SNAKE

            DISPLAY "TRIM"
            DISPLAY "Nuevo Texto : " NEW-TEXTO(1:LARGO-NEW-TEXTO)
                    " --- Largo Nuevo Texto : " LARGO-NEW-TEXTO

            DISPLAY "SNAKECASE"
            DISPLAY "Nuevo Texto : " SNAKE-TEXTO-CONVERTIDO


            STOP RUN.
       END PROGRAM ProbarTrimySnake.
