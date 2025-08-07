       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. BORRARCLI.
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
           05 LK-RUT      PIC 9(10).
       01 OUT-REGCLI.
           05 ST-REGCLI   PIC X(02).
      *       '00' = OK
      *    <> '00' = NO OK

       PROCEDURE DIVISION USING INP-REGCLI OUT-REGCLI.
      *-----------------------------------------------
           OPEN I-O CLIENTE

           MOVE LK-RUT     TO RUT OF R-CLIENTE
           READ CLIENTE END-READ
           IF FS-CLIENTE = "00"
              DELETE CLIENTE END-DELETE
           END-IF
           MOVE FS-CLIENTE   TO ST-REGCLI

           CLOSE    CLIENTE

           GOBACK.

