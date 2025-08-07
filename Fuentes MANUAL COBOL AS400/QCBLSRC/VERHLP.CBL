       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. VERHLP.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TBLCOD     ASSIGN        TO DATABASE-TBLCOD
                             ORGANIZATION  IS INDEXED
                             ACCESS MODE   IS DYNAMIC
                             RECORD KEY    IS EXTERNALLY-DESCRIBED-KEY
                             FILE STATUS   IS FS-TBLCOD.


           SELECT PANTALLA   ASSIGN TO WORKSTATION-VENHLP-SI
                             ORGANIZATION IS TRANSACTION
                             ACCESS MODE  IS DYNAMIC
                             RELATIVE KEY IS NREL
                             CONTROL-AREA IS AREA-CONTROL
                             FILE STATUS  IS FS-PAN.

       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD  TBLCOD    LABEL   RECORD STANDARD.
       01  R-TBLCOD.
           COPY  DDS-ALL-FORMAT  OF TBLCOD.


       FD PANTALLA  LABEL RECORD IS OMITTED.
       01 R-PANTALLA             PIC X(1920).

       WORKING-STORAGE SECTION.
      *------------------------*
       77 I-ON                PIC 1 VALUE B"1".
       77 I-OFF               PIC 1 VALUE B"0".

       01 AREA-CONTROL.
             06 MDTO                PIC X(02).
                88 F3               VALUE "03".

       01 VARIABLES-DE-STATUS.
          05 FS-TBLCOD      PIC XX VALUE "00".
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC  9(03).

       01 WC01-OO
      *----------
           COPY DDS-WC01-O       OF VENHLP.

       01 WC01-II
      *----------
           COPY DDS-WC01-I       OF VENHLP.

       01 WS01-OO
      *----------
           COPY DDS-WS01-O       OF VENHLP.

       01 WC01-IND.
      *------------
          COPY DDS-WC01-INDIC    OF VENHLP.

       LINKAGE SECTION.
       01 INP-HLP.
          05 HLP-TABLA         PIC X(10).
       01 OUT-HLP.
          05 HLP-CODIGO        PIC X(10).
          05 HLP-DESC-CORTA    PIC X(20).
          05 HLP-DESC-LARGA    PIC X(50).
          05 HLP-LARGO-COD     PIC 9(03).

       PROCEDURE DIVISION USING INP-HLP
                                OUT-HLP.
      *---------------------------------
           OPEN I-O PANTALLA
           OPEN INPUT TBLCOD

           MOVE I-OFF     TO IN03 OF WC01-I-INDIC

           PERFORM LIMPIAR-WS01
           PERFORM CARGAR-WS01
           PERFORM PROCESAR-PANTALLA UNTIL
                   IN03 OF WC01-I-INDIC = I-ON

           CLOSE    PANTALLA  TBLCOD

           GOBACK.

      *
       PROCESAR-PANTALLA.
      *------------------
           MOVE  HLP-TABLA   TO TITULO OF WC01-OO
           WRITE R-PANTALLA  FROM WC01-OO FORMAT "WC01"
                             INDICATORS ARE WC01-O-INDIC END-WRITE
           READ  PANTALLA    INTO WC01-II FORMAT "WC01"
                             INDICATORS ARE WC01-I-INDIC END-READ

           IF BTNSEL OF WC01-II = 1 THEN
               READ SUBFILE PANTALLA NEXT MODIFIED
                                     INTO WS01-OO FORMAT "WS01"
                    END-READ
               IF FS-PAN = "00"
                  MOVE CODIGO   OF WS01-OO TO  HLP-CODIGO
                  MOVE DESCORTA OF WS01-OO TO  HLP-DESC-CORTA
                  MOVE DESLARGA OF WS01-OO TO  HLP-DESC-LARGA
                  MOVE LARGOFLD OF WS01-OO TO  HLP-LARGO-COD
                  MOVE I-ON      TO IN03 OF WC01-I-INDIC
               END-IF
           END-IF
           .

       LIMPIAR-WS01.
      *-------------
           MOVE ZEROES          TO NREL
                                   BTNSEL OF WC01-II
                                   BTNSEL OF WC01-OO
           MOVE I-OFF           TO IN30 OF WC01-O-INDIC
                                   IN31 OF WC01-O-INDIC
           MOVE I-ON            TO IN32 OF WC01-O-INDIC
           WRITE R-PANTALLA  FROM WC01-OO FORMAT "WC01"
                             INDICATORS ARE WC01-O-INDIC END-WRITE.


       CARGAR-WS01.
      *-------------
           MOVE 0                  TO NREL
           MOVE HLP-TABLA          TO NOMTBL OF TBLCOD
           MOVE SPACES             TO FLDCOD OF TBLCOD
           START TBLCOD KEY IS NOT < EXTERNALLY-DESCRIBED-KEY
                 END-START
           IF FS-TBLCOD = "00"
              PERFORM LEER-NEXT-TBLCOD
              PERFORM UNTIL FS-TBLCOD  NOT = "00"         OR
                      NOMTBL OF TBLCOD NOT = HLP-TABLA

                 ADD 1   TO NREL
                 MOVE FLDCOD         TO CODIGO   OF WS01-OO
                 MOVE FLDDESC        TO DESCORTA OF WS01-OO
                                        W01DESC  OF WS01-OO
                 MOVE FLDDESL        TO DESLARGA OF WS01-OO
                 MOVE FLDLEN         TO LARGOFLD OF WS01-OO
                 WRITE SUBFILE R-PANTALLA  FROM WS01-OO FORMAT "WS01"
                       END-WRITE

                 PERFORM LEER-NEXT-TBLCOD
              END-PERFORM
           END-IF
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
      *
       LEER-NEXT-TBLCOD.
      *------------------
           READ TBLCOD NEXT RECORD END-READ.

