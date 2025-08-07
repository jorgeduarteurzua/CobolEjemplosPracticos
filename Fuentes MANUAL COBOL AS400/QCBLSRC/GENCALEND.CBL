       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. GENCALEND.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CALENDA    ASSIGN        TO DATABASE-CALEND
                             ORGANIZATION  IS INDEXED
                             ACCESS MODE   IS DYNAMIC
                             RECORD KEY    IS EXTERNALLY-DESCRIBED-KEY
                             FILE STATUS   IS FS-CALENDA.


       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD  CALENDA   LABEL   RECORD STANDARD.
       01  R-CALEND.
           COPY  DDS-ALL-FORMAT  OF CALEND.


       WORKING-STORAGE SECTION.
      *------------------------*

       01 VARIABLES-DE-STATUS.
          05 FS-CALENDA     PIC XX VALUE "00".

       01 VARIABLES-DE-TRABAJO.
          05  FECHA-INI             PIC 9(08) VALUE 19250101.
          05  FECHA-TOPE            PIC 9(08) VALUE 21010101.
          05  DIA-INI               PIC 9     VALUE 5.
          05  NUMERO-DIA            PIC 9(06).

       01 PARAMETROS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 SUM-RES-DIAS   PIC S9(03).
           05 SUM-RES-SIGNO  PIC X.
       01 PARAMETRO-SALIDA.
           05 FECHA-VALIDA         PIC X(01).
           05 NUEVA-FECHA-YYYYMMDD PIC X(08).

       PROCEDURE DIVISION.
      *-------------------
           OPEN OUTPUT CALENDA

           DISPLAY "FECHA-INI  : " FECHA-INI
           MOVE ZEROES   TO NUMERO-DIA
           PERFORM UNTIL FECHA-INI = FECHA-TOPE  OR
                         FS-CALENDA    NOT = "00"
               MOVE FECHA-INI   TO FECHA
               MOVE DIA-INI     TO DIASEMANA
               IF DIA-INI = 1 THEN
                  MOVE "S"      TO ISFERIADO
               ELSE
                  MOVE "N"      TO ISFERIADO
               END-IF
               ADD 1            TO NUMERO-DIA
               MOVE NUMERO-DIA  TO NUMDIA
               WRITE R-CALEND     END-WRITE

               IF FS-CALENDA    = "00"
                  MOVE FECHA-INI        TO FECHA-YYYYMMDD
                  MOVE 1                TO SUM-RES-DIAS
                  MOVE "+"              TO SUM-RES-SIGNO
                  CALL "SUMDIAFEC"      USING PARAMETROS-ENTRADA
                                              PARAMETRO-SALIDA
                  MOVE NUEVA-FECHA-YYYYMMDD TO FECHA-INI
                  ADD 1            TO DIA-INI
                  IF DIA-INI > 7 THEN
                     MOVE 1        TO DIA-INI
                  END-IF
               END-IF

           END-PERFORM

           DISPLAY "ULTIMA FECHA " FECHA-INI

           CLOSE    CALENDA

           GOBACK.

