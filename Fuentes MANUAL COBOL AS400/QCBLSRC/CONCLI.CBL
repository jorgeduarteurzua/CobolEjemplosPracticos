       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. CONCLI.
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

           SELECT PANTALLA   ASSIGN TO WORKSTATION-CONS01-SI
                             ORGANIZATION IS TRANSACTION
                             ACCESS MODE  IS DYNAMIC
                             RELATIVE KEY IS NREL
                             FILE STATUS  IS FS-PAN.


       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD  CLIENTE   LABEL   RECORD STANDARD.
       01  R-CLIENTE.
           COPY  DDS-ALL-FORMAT  OF CLIENTES.

       FD PANTALLA  LABEL RECORD IS OMITTED.
       01 R-PANTALLA             PIC X(1920).

       WORKING-STORAGE SECTION.
      *------------------------*
       77 I-ON                PIC 1 VALUE B"1".
       77 I-OFF               PIC 1 VALUE B"0".
       77 I-MSG               PIC 9(2) VALUE ZEROES.

       01 AREA-CONTROL.
             06 MDTO                PIC X(02).
                88 F3               VALUE "03".

       01 VARIABLES-DE-STATUS.
          05 FS-CLIENTE     PIC XX VALUE "00".
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC 9(03).

       01 R01-II.
      *----------
           COPY DDS-R01-I        OF MANT01.

       01 R01-INDIC.
      *--------------
           COPY DDS-R01-INDIC    OF MANT01.


       LINKAGE SECTION.
       01 INP-CONCLI.
           05 LK-RUT      PIC 9(10).

       PROCEDURE DIVISION USING INP-CONCLI.
      *--------------------------------------
           OPEN I-O   PANTALLA
           OPEN INPUT CLIENTE

           MOVE LK-RUT      TO RUT OF CLIENTE
           READ CLIENTE END-READ
           IF FS-CLIENTE = "00"
              MOVE CORR CLIREG OF CLIENTE TO
                        R01-I
              MOVE I-OFF       TO IN03 OF R01-I-INDIC
              PERFORM PROCESAR-PANTALLA UNTIL
                      IN03 OF R01-I-INDIC = I-ON

           END-IF
           CLOSE     PANTALLA CLIENTE

           GOBACK.

      *
       PROCESAR-PANTALLA.
      *------------------
           WRITE R-PANTALLA  FROM R01-II  FORMAT "R01"
                             INDICATORS ARE R01-O-INDIC  END-WRITE
           READ  PANTALLA    INTO R01-II  FORMAT "R01"
                             INDICATORS ARE R01-I-INDIC  END-READ

           .


