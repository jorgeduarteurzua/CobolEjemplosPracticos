       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. VERHLPSRCH.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT PANTALLA   ASSIGN TO WORKSTATION-VENHLPSRCH-SI
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

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01 AREA-CONTROL.
             06 MDTO                PIC X(02).
                88 F3               VALUE "03".

       01 VARIABLES-DE-STATUS.
          05 FS-TBLCOD      PIC XX VALUE "00".
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC  9(03).

       01 VARIABLES-DE-TRABAJO.
          05 SEN-SQL        PIC  X(500).

       01 WC01-OO
      *
           COPY DDS-WC01-O       OF VENHLPSRCH.

       01 WC01-II
      *
           COPY DDS-WC01-I       OF VENHLPSRCH.

       01 WS01-OO
      *
           COPY DDS-WS01-O       OF VENHLPSRCH.

       01 WC01-IND.
      *------------
          COPY DDS-WC01-INDIC    OF VENHLPSRCH.

       01 R-TBLCOD.
           COPY  DDS-ALL-FORMAT  OF TBLCOD.

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

           MOVE I-OFF     TO IN03 OF WC01-I-INDIC

           MOVE SPACES          TO BUSQUEDA OF WC01-II
                                   BUSQUEDA OF WC01-OO
           PERFORM LIMPIAR-WS01
           PERFORM CARGAR-WS01
           PERFORM PROCESAR-PANTALLA UNTIL
                   IN03 OF WC01-I-INDIC = I-ON

           CLOSE    PANTALLA

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
           ELSE
               PERFORM LIMPIAR-WS01
               PERFORM CARGAR-WS01
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
           MOVE SPACES             TO SEN-SQL
           STRING 'SELECT * FROM '   DELIMITED SIZE
                  'TBLCOD '          DELIMITED SIZE
                  'WHERE '           DELIMITED SIZE
                  ' NOMTBL = "'      DELIMITED SIZE
                  HLP-TABLA          DELIMITED SIZE
                  '" '               DELIMITED SIZE
                                 INTO SEN-SQL
           IF BUSQUEDA OF WC01-II NOT = SPACES
              STRING  SEN-SQL(1:60)  DELIMITED BY SIZE
                     ' AND FLDDESC LIKE "%' DELIMITED SIZE
                     BUSQUEDA OF WC01-II DELIMITED BY ' '
                     '%" '               DELIMITED SIZE
                                    INTO SEN-SQL
           END-IF
           STRING  SEN-SQL(1:200) DELIMITED BY SIZE
                    ' ORDER BY FLDCOD    ' DELIMITED SIZE
                                      INTO SEN-SQL


           EXEC SQL
              DECLARE SELREGISTRO STATEMENT
           END-EXEC

           EXEC SQL
              PREPARE SELREGISTRO FROM :SEN-SQL
           END-EXEC
           EXEC SQL
              DECLARE C1 CURSOR FOR SELREGISTRO
           END-EXEC
           EXEC SQL OPEN C1 END-EXEC

           PERFORM LEER-FETCH-C1
           PERFORM UNTIL SQLCODE NOT = 0

              ADD 1   TO NREL
              MOVE FLDCOD         TO CODIGO   OF WS01-OO
              MOVE FLDDESC        TO DESCORTA OF WS01-OO
                                     W01DESC  OF WS01-OO
              MOVE FLDDESL        TO DESLARGA OF WS01-OO
              MOVE FLDLEN         TO LARGOFLD OF WS01-OO
              WRITE SUBFILE R-PANTALLA  FROM WS01-OO FORMAT 'WS01'
                    END-WRITE

              PERFORM LEER-FETCH-C1
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
           EXEC SQL CLOSE C1 END-EXEC
           .
      *
       LEER-FETCH-C1.
      *--------------
           EXEC SQL FETCH C1 INTO: RTBLCOD END-EXEC.

