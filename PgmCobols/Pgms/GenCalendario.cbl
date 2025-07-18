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
       DATA DIVISION.
       FILE SECTION.
       FD  calendario.
           copy "C:\PgmCobols\FD\FDCalendario.cpy".

       WORKING-STORAGE SECTION.
       01  FS-CALENDARIO         PIC XX.
       01  FECHA-INI             PIC 9(08) VALUE 19250101.
       01  FECHA-TOPE            PIC 9(08) VALUE 21010101.
       01  DIA-INI               PIC 9     VALUE 5.

       01 PARAMETROS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 SUM-RES-DIAS   PIC S9(03).
       01 PARAMETRO-SALIDA.
           05 FECHA-VALIDA         PIC X(01).
           05 NUEVA-FECHA-YYYYMMDD PIC X(08).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "FECHA-INI  : " FECHA-INI
            OPEN OUTPUT calendario

            PERFORM UNTIL FECHA-INI = FECHA-TOPE  OR
                          FS-CALENDARIO NOT = "00"
                MOVE FECHA-INI   TO calendario-fecha
                MOVE DIA-INI     TO calendario-dia-sem
                IF DIA-INI = 1 THEN
                   MOVE "S"      TO calendario-feriado
                ELSE
                   MOVE "N"      TO calendario-feriado
                END-IF
                WRITE r-calendario END-WRITE
                IF FS-CALENDARIO = "00"
                   MOVE FECHA-INI        TO FECHA-YYYYMMDD
                   MOVE 1                TO SUM-RES-DIAS
                   CALL "SumarDiasFecha" USING PARAMETROS-ENTRADA
                                               PARAMETRO-SALIDA
                   MOVE NUEVA-FECHA-YYYYMMDD TO FECHA-INI
                   ADD 1            TO DIA-INI
                   IF DIA-INI > 7 THEN
                      MOVE 1        TO DIA-INI
                   END-IF
                END-IF
                DISPLAY "FECHA-INI (GEN) : " FECHA-INI
            END-PERFORM
            DISPLAY "ULTIMA FECHA " FECHA-INI

            CLOSE calendario
            STOP RUN.
       END PROGRAM GenCalendario.
