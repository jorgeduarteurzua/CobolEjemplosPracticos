       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. GRABARCLI.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLIENTE    ASSIGN        TO DATABASE-CLIENTES
                             ORGANIZATION  IS INDEXED
                             ACCESS MODE   IS DYNAMIC
                             RECORD KEY    IS EXTERNALLY-DESCRIBED-KEY
                             FILE STATUS   IS FS-CLIENTE.


       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD  CLIENTE   LABEL   RECORD STANDARD.
       01  R-CLIENTE.
           COPY  DDS-ALL-FORMAT  OF CLIENTES.


       WORKING-STORAGE SECTION.
      *------------------------*

       01 VARIABLES-DE-STATUS.
          05 FS-CLIENTE     PIC XX VALUE "00".

       LINKAGE SECTION.
       01 INP-REGCLI.
           COPY  DDS-ALL-FORMAT  OF CLIENTES.
       01 OUT-REGCLI.
           05 ST-REGCLI   PIC X(02).
      *       '00' = OK
      *    <> '00' = NO OK

       PROCEDURE DIVISION USING INP-REGCLI OUT-REGCLI.
      *-----------------------------------------------
           OPEN I-O CLIENTE

           MOVE CORR CLIREG OF INP-REGCLI     TO
                     CLIREG OF R-CLIENTE

           WRITE R-CLIENTE END-WRITE

           MOVE FS-CLIENTE   TO ST-REGCLI

           CLOSE    CLIENTE

           GOBACK.

