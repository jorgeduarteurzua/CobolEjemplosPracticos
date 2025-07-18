      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 08-07-2025
      * Purpose: Eseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LargoString10000.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 LARGO-STR PIC 9(05).

       LINKAGE SECTION.
       01 INPUT-LARGOSTR10000.
           05 STRING-CALCULAR   PIC X(10000).
       01 OUTPUT-LARGOSTR10000.
           05 LK-LARGO-STR     PIC 9(05).

       PROCEDURE DIVISION USING INPUT-LARGOSTR10000
                                OUTPUT-LARGOSTR10000.

       MAIN-PROCEDURE.

           PERFORM VARYING LARGO-STR FROM 10000 BY -1 UNTIL
                   LARGO-STR = 0 OR
                   STRING-CALCULAR(LARGO-STR:1) NOT = SPACES
           END-PERFORM

           MOVE LARGO-STR   TO LK-LARGO-STR

           GOBACK
           .

       END PROGRAM LargoString10000.
