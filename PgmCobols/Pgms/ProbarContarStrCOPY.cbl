      ******************************************************************
      * Author: Jorge Duarte
      * Date: 14-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProbarContarStrCOPY.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 WS-TEXTO PIC X(500).

       01 INP-CONTARSTR.
           05 CONTARSTR-STRING-REVISAR    PIC X(10000).
           05 CONTARSTR-STRING-BUSCAR     PIC X(20).
       01 OUT-CONTARSTR.
           05 TOTAL-STR         PIC 9(05).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE SPACES   TO WS-TEXTO
            STRING "Este es un nuevo "         DELIMITED SIZE
                   "TEXTO que usaremos para "  DELIMITED SIZE
                   "probar CONTARSTR con COPY" DELIMITED SIZE
                                          INTO WS-TEXTO

            MOVE WS-TEXTO     TO CONTARSTR-STRING-REVISAR
            MOVE "para"       TO CONTARSTR-STRING-BUSCAR
            DISPLAY "TEXTO USAR   [" WS-TEXTO "]"
            DISPLAY "TEXTO BUSCAR [" CONTARSTR-STRING-BUSCAR "]"
            CALL "ContarStrCOPY" USING INP-CONTARSTR
                                       OUT-CONTARSTR


            DISPLAY "Veces encontrado [" TOTAL-STR "]"

            STOP RUN.
       END PROGRAM ProbarContarStrCOPY.
