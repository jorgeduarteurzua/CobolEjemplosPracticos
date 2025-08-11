      ******************************************************************
      * Author: JORGE DUARTE URZUA.
      * Date: 25-07-2025
      * Purpose: ENSEÑANZA USO DE COPY
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ContarStrCOPY.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

           COPY "C:\PgmCobols\FD\CONTARSTR_VARIABLES.cpy".

       LINKAGE SECTION.
       01 INP-CONTARSTR.
           05 CONTARSTR-STRING-REVISAR    PIC X(10000).
           05 CONTARSTR-STRING-BUSCAR     PIC X(20).
       01 OUT-CONTARSTR.
           05 TOTAL-STR         PIC 9(05).

       PROCEDURE DIVISION USING INP-CONTARSTR
                                OUT-CONTARSTR.
       MAIN-PROCEDURE.

           COPY "C:\PgmCobols\FD\CONTARSTR_CODIGO.cbl".

       END PROGRAM ContarStrCOPY.
