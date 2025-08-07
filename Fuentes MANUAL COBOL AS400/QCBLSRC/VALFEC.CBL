      ******************************************************************
      * AUTHOR: JORGE DUARTE
      * DATE: 07-07-2025
      * PURPOSE: ENSE#ANZA
      * TECTONICS: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALFEC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 ANO            PIC 9(04).
           05 MES            PIC 9(02).
           05 DIA            PIC 9(02).

           05 ENTERO         PIC 9(04).
           05 ENTERO-2       PIC 9(04).
           05 ENTERO-3       PIC 9(04).

           05 RESTO          PIC 9(04).
           05 RESTO-2        PIC 9(04).
           05 RESTO-3        PIC 9(04).

           05 TABLA-DIAS-MESES PIC X(24) VALUE
                             "312831303130313130313032".
           05 ARR-DIAS REDEFINES TABLA-DIAS-MESES
                       OCCURS 12 TIMES.
              10 DIA-MES     PIC 9(02).

       LINKAGE SECTION.
       01 PARAMETROS-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 FECHA-FORMATO  PIC X.
      *       1 : YYYYMMDD
      *       2 : DDMMYYYY

       01 PARAMETRO-SALIDA.
           05 FECHA-VALIDA   PIC X.
      *       S : FECHA ES VALIDATE
      *       N : FECHA ES INVALIDA

       PROCEDURE DIVISION USING PARAMETROS-ENTRADA
                                PARAMETRO-SALIDA.
           MOVE "S"            TO FECHA-VALIDA
           EVALUATE FECHA-FORMATO
             WHEN "1"
                   MOVE FECHA-YYYYMMDD(1:4) TO ANO
                   MOVE FECHA-YYYYMMDD(5:2) TO MES
                   MOVE FECHA-YYYYMMDD(7:2) TO DIA
             WHEN "2"
                   MOVE FECHA-YYYYMMDD(5:4) TO ANO
                   MOVE FECHA-YYYYMMDD(3:2) TO MES
                   MOVE FECHA-YYYYMMDD(1:2) TO DIA
             WHEN OTHER
                   MOVE "N"         TO FECHA-VALIDA
           END-EVALUATE
           IF FECHA-VALIDA NOT = "N" THEN
               PERFORM VALIDAR-FECHA
           END-IF


           GOBACK.

       VALIDAR-FECHA.

           IF MES NOT = 2 THEN
              IF MES < 1 OR > 12 THEN
                 MOVE "N"   TO FECHA-VALIDA
              ELSE
                  IF ANO < 1 THEN
                     MOVE "N"   TO FECHA-VALIDA
                  ELSE
                      IF DIA < 1 OR > DIA-MES(MES) THEN
                         MOVE "N" TO FECHA-VALIDA
                      END-IF
                  END-IF
              END-IF
           ELSE
              PERFORM BISIESTO

      *    VERIFICAMOS SI EL MES DEL A#O ES BISIESTO
              IF (RESTO = 0 AND RESTO-2 NOT = 0) OR (RESTO-3 = 0) THEN
                 MOVE 29    TO DIA-MES(MES)
              ELSE
                 MOVE 28    TO DIA-MES(MES)
              END-IF
              IF DIA < 1 OR > DIA-MES(MES) THEN
                 MOVE "N" TO FECHA-VALIDA
              END-IF
           END-IF
           .
       BISIESTO.
           DIVIDE ANO BY 4 GIVING ENTERO
                       REMAINDER RESTO
           DIVIDE ANO BY 100 GIVING ENTERO-2
                       REMAINDER RESTO-2
           DIVIDE ANO BY 400 GIVING ENTERO-3
                       REMAINDER RESTO-3
           .
