      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProbarSumarDiasFecha.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 FECHA-INICIO PIC X(08) VALUE "20250714".

       01 SUMARDIAS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 SUM-RES-DIAS   PIC S9(03).
       01 SUMARDIAS-SALIDA.
           05 FECHA-VALIDA         PIC X(01).
           05 NUEVA-FECHA-YYYYMMDD PIC X(08).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE FECHA-INICIO     TO FECHA-YYYYMMDD

           MOVE ZEROES           TO SUM-RES-DIAS
           ADD 5                 TO SUM-RES-DIAS
           MOVE SPACES           TO FECHA-VALIDA
                                    NUEVA-FECHA-YYYYMMDD

           CALL "SumarDiasFecha" USING SUMARDIAS-ENTRADA
                                       SUMARDIAS-SALIDA

      *    AL SUMAR 5 DIAS NUEVA FECHA SERIA 20250719
           DISPLAY "FECHA INICIO   : " FECHA-INICIO
           DISPLAY "NUEVA FECHA    : " NUEVA-FECHA-YYYYMMDD

           MOVE ZEROES           TO SUM-RES-DIAS
           SUBTRACT 20         FROM SUM-RES-DIAS
           MOVE SPACES           TO FECHA-VALIDA
                                    NUEVA-FECHA-YYYYMMDD
           CALL "SumarDiasFecha" USING SUMARDIAS-ENTRADA
                                       SUMARDIAS-SALIDA
      *    AL RESTAR 20 DIAS NUEVA FECHA SERIA 20250624
           DISPLAY "FECHA INICIO   : " FECHA-INICIO
           DISPLAY "NUEVA FECHA    : " NUEVA-FECHA-YYYYMMDD

           MOVE "19250131"        TO FECHA-YYYYMMDD

           MOVE ZEROES           TO SUM-RES-DIAS
           ADD 1                 TO SUM-RES-DIAS
           MOVE SPACES           TO FECHA-VALIDA
                                    NUEVA-FECHA-YYYYMMDD

           CALL "SumarDiasFecha" USING SUMARDIAS-ENTRADA
                                       SUMARDIAS-SALIDA

      *    AL SUMAR 5 DIAS NUEVA FECHA SERIA 20250719
           DISPLAY "FECHA INICIO   : " FECHA-YYYYMMDD
           DISPLAY "NUEVA FECHA    : " NUEVA-FECHA-YYYYMMDD


            STOP RUN.
       END PROGRAM ProbarSumarDiasFecha.
