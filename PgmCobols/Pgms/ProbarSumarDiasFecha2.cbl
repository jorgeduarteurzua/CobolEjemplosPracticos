      ******************************************************************
      * Author: JORGE DUARTE URZUA
      * Date: 21-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProbarSumarDiasFecha2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 FECHA-INICIO PIC X(08) VALUE "20250714".

       01 SUMARDIAS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 SUM-RES-DIAS   PIC 9(03).
           05 SUM-RES-SIGNO  PIC X.
       01 SUMARDIAS-SALIDA.
           05 FECHA-VALIDA         PIC X(01).
           05 NUEVA-FECHA-YYYYMMDD PIC X(08).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE FECHA-INICIO     TO FECHA-YYYYMMDD

           MOVE ZEROES           TO SUM-RES-DIAS
           ADD 5                 TO SUM-RES-DIAS
           MOVE "+"              TO SUM-RES-SIGNO
           MOVE SPACES           TO FECHA-VALIDA
                                    NUEVA-FECHA-YYYYMMDD

           CALL "SumarDiasFecha2" USING SUMARDIAS-ENTRADA
                                       SUMARDIAS-SALIDA

      *    AL SUMAR 5 DIAS NUEVA FECHA SERIA 20250719
           DISPLAY "FECHA INICIO   : " FECHA-INICIO
           DISPLAY "NUEVA FECHA    : " NUEVA-FECHA-YYYYMMDD

           MOVE ZEROES           TO SUM-RES-DIAS
           MOVE 20               TO SUM-RES-DIAS
           MOVE "-"              TO SUM-RES-SIGNO
           MOVE SPACES           TO FECHA-VALIDA
                                    NUEVA-FECHA-YYYYMMDD
           CALL "SumarDiasFecha2" USING SUMARDIAS-ENTRADA
                                       SUMARDIAS-SALIDA
      *    AL RESTAR 20 DIAS NUEVA FECHA SERIA 20250624
           DISPLAY "FECHA INICIO   : " FECHA-INICIO
           DISPLAY "NUEVA FECHA    : " NUEVA-FECHA-YYYYMMDD

           MOVE "19250131"        TO FECHA-YYYYMMDD

           MOVE ZEROES           TO SUM-RES-DIAS
           MOVE 1                 TO SUM-RES-DIAS
           MOVE "+"              TO SUM-RES-SIGNO
           MOVE SPACES           TO FECHA-VALIDA
                                    NUEVA-FECHA-YYYYMMDD

           CALL "SumarDiasFecha2" USING SUMARDIAS-ENTRADA
                                       SUMARDIAS-SALIDA

      *    AL SUMAR 5 DIAS NUEVA FECHA SERIA 19250201
           DISPLAY "FECHA INICIO   : " FECHA-YYYYMMDD
           DISPLAY "NUEVA FECHA    : " NUEVA-FECHA-YYYYMMDD


            STOP RUN.
       END PROGRAM ProbarSumarDiasFecha2.
