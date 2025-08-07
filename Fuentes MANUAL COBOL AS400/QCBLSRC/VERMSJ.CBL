       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. VERMSJ.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT PANTALLA   ASSIGN TO WORKSTATION-VENMSG-SI
                             ORGANIZATION IS TRANSACTION
                             ACCESS MODE  IS DYNAMIC
                             RELATIVE KEY IS NREL
                             CONTROL-AREA IS AREA-CONTROL
                             FILE STATUS  IS FS-PAN.

       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*

       FD PANTALLA  LABEL RECORD IS OMITTED.
       01 R-PANTALLA             PIC X(1920).

       WORKING-STORAGE SECTION.
      *------------------------*
       77 I-ON                PIC 1 VALUE B"1".
       77 I-OFF               PIC 1 VALUE B"0".
       77 I                   PIC 9(02).

       01 AREA-CONTROL.
             06 MDTO                PIC X(02).
                88 F3               VALUE "03".

       01 VARIABLES-DE-STATUS.
          05 FS-CLIENTE     PIC XX VALUE "00".
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC  9(03).

       01 WC01-OO
      *----------
           COPY DDS-WC01-O       OF VENMSG.

       01 WS01-OO
      *----------
           COPY DDS-WS01-O       OF VENMSG.

       01 WC01-IND.
      *------------
          COPY DDS-WC01-INDIC    OF VENMSG.

       LINKAGE SECTION.
       01 INP-MSG.
          05 TIT-MSG           PIC X(30).
          05 ARR-MSG OCCURS 50 TIMES.
             10 MSG            PIC X(50).

       PROCEDURE DIVISION USING INP-MSG.
      *---------------------------------
           OPEN I-O PANTALLA
           MOVE I-OFF     TO IN03 OF WC01-I-INDIC

           PERFORM LIMPIAR-WS01
           PERFORM CARGAR-WS01
           MOVE TIT-MSG  TO WDWTIT OF WC01-OO
           PERFORM PROCESAR-PANTALLA UNTIL
                   IN03 OF WC01-I-INDIC = I-ON

           CLOSE    PANTALLA

           GOBACK.

      *
       PROCESAR-PANTALLA.
      *------------------
           MOVE TIT-MSG        TO WDWTIT   OF WC01-OO
           WRITE R-PANTALLA  FROM WC01-OO FORMAT "WC01"
                             INDICATORS ARE WC01-O-INDIC END-WRITE
           READ  PANTALLA    INTO WC01-OO FORMAT "WC01"
                             INDICATORS ARE WC01-I-INDIC END-READ

           .

       LIMPIAR-WS01.
      *-------------
           MOVE ZEROES          TO NREL
           MOVE I-OFF           TO IN30 OF WC01-O-INDIC
                                   IN31 OF WC01-O-INDIC
           MOVE I-ON            TO IN32 OF WC01-O-INDIC
           WRITE R-PANTALLA  FROM WC01-OO FORMAT "WC01"
                             INDICATORS ARE WC01-O-INDIC END-WRITE.


       CARGAR-WS01.
      *-------------
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
              IF MSG(I) NOT = SPACES
                 ADD 1   TO NREL
                 MOVE MSG(I)         TO MENSAJE OF WS01-OO
                 WRITE SUBFILE R-PANTALLA  FROM WS01-OO FORMAT "WS01"
                       END-WRITE
              END-IF
           END-PERFORM
           IF NREL > 0
              MOVE I-ON         TO IN30 OF WC01-O-INDIC
                                   IN31 OF WC01-O-INDIC
              MOVE I-OFF        TO IN32 OF WC01-O-INDIC
           ELSE
              MOVE I-OFF        TO IN30 OF WC01-O-INDIC
              MOVE I-ON         TO IN31 OF WC01-O-INDIC
              MOVE I-OFF        TO IN32 OF WC01-O-INDIC
           END-IF
           .

