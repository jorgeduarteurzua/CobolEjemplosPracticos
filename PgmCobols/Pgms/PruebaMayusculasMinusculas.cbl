      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 07-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PruebaMayusculasMinusculas.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 TEXTO-TRANSFORMAR PIC X(40) VALUE "Prueba Convertir".

       01 PAR-INPUT-MAY.
           05 MAY-STRING-CONVERTIR PIC X(500).
       01 PAR-OUTPUT-MAY.
           05 MAY-STRING-CONVERTIDO PIC X(500).

       01 PAR-INPUT-MIN.
           05 MIN-STRING-CONVERTIR PIC X(500).
       01 PAR-OUTPUT-MIN.
           05 MIN-STRING-CONVERTIDO PIC X(500).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE TEXTO-TRANSFORMAR TO MAY-STRING-CONVERTIR
                                     MIN-STRING-CONVERTIR

           CALL "ConvertirMayusculas" USING PAR-INPUT-MAY
                                            PAR-OUTPUT-MAY

           CALL "ConvertirMinusculas" USING PAR-INPUT-MIN
                                            PAR-OUTPUT-MIN


           DISPLAY "TEXTO A CONVERTIR : " TEXTO-TRANSFORMAR
           DISPLAY "TEXTO EN MAYUSCULAS : " MAY-STRING-CONVERTIDO
           DISPLAY "TEXTO EN MINUSCULAS : " MIN-STRING-CONVERTIDO

            STOP RUN.
       END PROGRAM PruebaMayusculasMinusculas.
