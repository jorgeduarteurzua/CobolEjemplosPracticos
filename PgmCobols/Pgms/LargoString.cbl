      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 08-07-2025
      * Purpose: Eseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LargoString.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 LARGO-STR PIC 9(03).

       LINKAGE SECTION.
       01 INPUT-LARGOSTR500.
           05 STRING-CALCULAR500   PIC X(500).
       01 OUTPUT-LARGOSTR500.
           05 LK-LARGO-STR500      PIC 9(03).

       PROCEDURE DIVISION USING INPUT-LARGOSTR500
                                OUTPUT-LARGOSTR500.

       MAIN-PROCEDURE.

           PERFORM VARYING LARGO-STR FROM 500 BY -1 UNTIL
                   LARGO-STR = 0 OR
                   STRING-CALCULAR500(LARGO-STR:1) NOT = SPACES
           END-PERFORM

           MOVE LARGO-STR   TO LK-LARGO-STR500

           GOBACK
           .

       END PROGRAM LargoString.
