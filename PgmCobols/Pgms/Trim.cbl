      ******************************************************************
      * Author: Jorge Duarte
      * Date: 14-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Trim.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 STR-INICIO   PIC 9(3).

       01 PAR-INPUT-LARGO-STRING.
           05 STRING-CALULAR   PIC X(500).
       01 PAR-OUTPUT-LARGO-STRING.
           05 LK-LARGO-STR     PIC 9(03).

       LINKAGE SECTION.
       01 PAR-INPUT-TRIM.
           05 TEXTO-INICIAL PIC X(500).
       01 PAR-OUTPUT-TRIM.
           05 NEW-TEXTO     PIC X(500).
           05 LARGO-NEW-TEXTO PIC 9(03).

       PROCEDURE DIVISION USING PAR-INPUT-TRIM
                                PAR-OUTPUT-TRIM.

       MAIN-PROCEDURE.

           PERFORM VARYING
           STR-INICIO FROM 1 BY 1 UNTIL STR-INICIO > 500
                   OR TEXTO-INICIAL(STR-INICIO:1) <> " "
           END-PERFORM

           MOVE TEXTO-INICIAL(STR-INICIO:) TO NEW-TEXTO
                                              STRING-CALULAR
           MOVE ZEROES                     TO LK-LARGO-STR
           CALL "LargoString" USING PAR-INPUT-LARGO-STRING
                                    PAR-OUTPUT-LARGO-STRING

           MOVE LK-LARGO-STR               TO LARGO-NEW-TEXTO


           GOBACK.
       END PROGRAM Trim.
