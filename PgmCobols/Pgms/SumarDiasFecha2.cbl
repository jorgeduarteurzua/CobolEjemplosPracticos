      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 18-07-2025
      * Purpose: ENSEÑANZA, para poder usar esta Rutina, se requiere
      *          haber generado el Calendario (GenCalendario.cbl)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SumarDiasFecha2.

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
       01 VARIABLES-DE-STATUS.
           05  FS-CALENDARIO       PIC XX.
           05  FS-CALENDARIO-D     PIC XX.
       01 VARIABLES-DE-TRABAJO.
           05  DIA-FECHA1          PIC 9(06).

       01 PARAMETROS-ENTRADA-VALFEC.
           05 VALFEC-FECHA-YYYYMMDD PIC X(08).
           05 VALFEC-FECHA-FORMATO  PIC X.

      *       1 : YYYYMMDD
      *       2 : DDMMYYYY

       01 PARAMETRO-SALIDA-VALFEC.
           05 VALFEC-FECHA-VALIDA   PIC X.
      *       S : FECHA ES VALIDATE
      *       N : FECHA ES INVALIDA

       LINKAGE SECTION.
       01 PARAMETROS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 SUM-RES-DIAS   PIC 9(03).
           05 SUM-RES-SIGNO  PIC X.
       01 PARAMETRO-SALIDA.
           05 FECHA-VALIDA         PIC X(01).
           05 NUEVA-FECHA-YYYYMMDD PIC X(08).

       PROCEDURE DIVISION USING PARAMETROS-ENTRADA
                                PARAMETRO-SALIDA.

       MAIN-PROCEDURE.

           MOVE "00000000"   TO NUEVA-FECHA-YYYYMMDD
           MOVE "N"          TO FECHA-VALIDA

           MOVE FECHA-YYYYMMDD TO VALFEC-FECHA-YYYYMMDD
           MOVE "1"            TO VALFEC-FECHA-FORMATO
           MOVE " "            TO VALFEC-FECHA-VALIDA
           CALL "ValidarFecha" USING PARAMETROS-ENTRADA-VALFEC
                                     PARAMETRO-SALIDA-VALFEC
           IF VALFEC-FECHA-VALIDA = "S" THEN
              IF SUM-RES-SIGNO NOT = "+" AND "-" THEN
               MOVE "N"        TO FECHA-VALIDA
               MOVE "00000000" TO NUEVA-FECHA-YYYYMMDD
            ELSE
               PERFORM PROCESAR-OPERACION
            END-IF
           END-IF
           MOVE VALFEC-FECHA-VALIDA  TO FECHA-VALIDA

           GOBACK
            .

       PROCESAR-OPERACION.
            OPEN INPUT calendario calendario-D

            MOVE ZEROES            TO DIA-FECHA1


            MOVE FECHA-YYYYMMDD   TO calendario-fecha
            READ calendario END-READ
            IF FS-CALENDARIO = "00" THEN
                MOVE calendario-numdia TO DIA-FECHA1
            END-IF

            IF SUM-RES-SIGNO = "+" THEN
               ADD SUM-RES-DIAS   TO DIA-FECHA1
            ELSE
                SUBTRACT SUM-RES-DIAS FROM DIA-FECHA1
            END-IF

            MOVE DIA-FECHA1   TO calendario-numdia-d
            READ calendario-d
                 INVALID KEY
                  MOVE "0000000"           TO NUEVA-FECHA-YYYYMMDD
                 NOT INVALID KEY
                   MOVE calendario-fecha-d TO NUEVA-FECHA-YYYYMMDD

            END-READ

           CLOSE calendario calendario-D
           .


       END PROGRAM SumarDiasFecha2.
