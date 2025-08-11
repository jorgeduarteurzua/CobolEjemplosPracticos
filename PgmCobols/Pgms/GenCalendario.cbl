      ******************************************************************
      * Author: JORGE DUARTE URZUA
      * Date: 17-07-2025
      * Purpose: ENSEÑANZA, SE DEJA TODOS LOS DOMINGOS COMO FERIADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GenCalendario.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT calendario
               ASSIGN TO
           "C:\PgmCobols\Data\calendario.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS calendario-fecha
               FILE STATUS IS FS-CALENDARIO.

           SELECT calendario-D
               ASSIGN TO
           "C:\PgmCobols\Data\calendario.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS calendario-numdia-d
               FILE STATUS IS FS-CALENDARIO-D.


       DATA DIVISION.
       FILE SECTION.
       FD  calendario.
           copy "C:\PgmCobols\FD\FDCalendario.cpy".

       FD  calendario-D.
       01  r-calendario-d.
           05  calendario-fecha-d   PIC 9(08).
           05  r-calendario-fecha-d redefines calendario-fecha-d.
               10 fecha-ano-d       PIC 9(04).
               10 fecha-mes-d       PIC 9(02).
               10 fecha-dia-d       PIC 9(02).
           05  calendario-dia-sem-d PIC 9.
      *        1 = Domingo
      *        2 = Lunes
      *        3 = Martes
      *        4 = Miercoles
      *        5 = Jueves
      *        6 = Viernes
      *        7 = Sabado
           05  calendario-feriado-d PIC X.
      *        N = No
      *        S = Si
           05  calendario-numdia-d  PIC 9(06).


       WORKING-STORAGE SECTION.
       01  FS-CALENDARIO         PIC XX.
       01  FS-CALENDARIO-D       PIC XX.
       01  FECHA-INI             PIC 9(08) VALUE 19250101.
       01  FECHA-TOPE            PIC 9(08) VALUE 21010101.
       01  DIA-INI               PIC 9     VALUE 5.
       01  NUMERO-DIA            PIC 9(06).

       01 PARAMETROS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 SUM-RES-DIAS   PIC S9(03).
           05 SUM-RES-SIGNO  PIC X.
       01 PARAMETRO-SALIDA.
           05 FECHA-VALIDA         PIC X(01).
           05 NUEVA-FECHA-YYYYMMDD PIC X(08).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "FECHA-INI  : " FECHA-INI
            OPEN OUTPUT calendario calendario-D
            MOVE ZEROES   TO NUMERO-DIA
            PERFORM UNTIL FECHA-INI = FECHA-TOPE  OR
                          FS-CALENDARIO NOT = "00"
                MOVE FECHA-INI   TO calendario-fecha
                MOVE DIA-INI     TO calendario-dia-sem
                IF DIA-INI = 1 THEN
                   MOVE "S"      TO calendario-feriado
                ELSE
                   MOVE "N"      TO calendario-feriado
                END-IF
                ADD 1            TO NUMERO-DIA
                MOVE NUMERO-DIA  TO calendario-numdia
                WRITE r-calendario END-WRITE

                MOVE r-calendario TO r-calendario-d
                WRITE r-calendario-d END-WRITE

                IF FS-CALENDARIO = "00"
                   MOVE FECHA-INI        TO FECHA-YYYYMMDD
                   MOVE 1                TO SUM-RES-DIAS
                   MOVE "+"              TO SUM-RES-SIGNO
                   CALL "SumarDiasFecha" USING PARAMETROS-ENTRADA
                                               PARAMETRO-SALIDA
                   MOVE NUEVA-FECHA-YYYYMMDD TO FECHA-INI
                   ADD 1            TO DIA-INI
                   IF DIA-INI > 7 THEN
                      MOVE 1        TO DIA-INI
                   END-IF
                END-IF

            END-PERFORM
            DISPLAY "ULTIMA FECHA " FECHA-INI

            CLOSE calendario calendario-D
            STOP RUN.
       END PROGRAM GenCalendario.
