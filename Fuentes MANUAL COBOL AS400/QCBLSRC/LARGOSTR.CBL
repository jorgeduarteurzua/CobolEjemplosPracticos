       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGOSTRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 LARGO-STR PIC 9(03).

       LINKAGE SECTION.
       01 PAR-INPUT.
           05 STRING-CALULAR   PIC X(500).
       01 PAR-OUTPUT.
           05 LK-LARGO-STR     PIC 9(03).

       PROCEDURE DIVISION USING PAR-INPUT
                                PAR-OUTPUT.

           MOVE 500            TO LARGO-STR
           PERFORM UNTIL
                   LARGO-STR = 0 OR
                   STRING-CALULAR(LARGO-STR:1) NOT = SPACES

                   SUBTRACT 1 FROM LARGO-STR

           END-PERFORM

           MOVE LARGO-STR   TO LK-LARGO-STR

           GOBACK
           .






