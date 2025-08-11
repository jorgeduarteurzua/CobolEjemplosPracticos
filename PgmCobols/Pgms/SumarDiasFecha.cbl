      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 07-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SumarDiasFecha.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.

           05 ANO-2          PIC 9(04).
           05 MES-2          PIC 9(02).
           05 DIA-2          PIC 9(02).

           05 ENTERO         PIC 9(04).
           05 ENTERO-2       PIC 9(04).
           05 ENTERO-3       PIC 9(04).

           05 RESTO          PIC 9(04).
           05 RESTO-2        PIC 9(04).
           05 RESTO-3        PIC 9(04).

           05 I              PIC S9(03).

           05 TABLA-DIAS-MESES PIC X(24) VALUE
                             "312831303130313130313031".
           05 ARR-DIAS REDEFINES TABLA-DIAS-MESES
                       OCCURS 12 TIMES.
              10 DIA-MES     PIC 9(02).

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

           MOVE FECHA-YYYYMMDD(1:4) TO ANO-2
           MOVE FECHA-YYYYMMDD(5:2) TO MES-2
           MOVE FECHA-YYYYMMDD(7:2) TO DIA-2

           MOVE FECHA-YYYYMMDD TO VALFEC-FECHA-YYYYMMDD
           MOVE "1"            TO VALFEC-FECHA-FORMATO
           MOVE " "            TO VALFEC-FECHA-VALIDA
           CALL "ValidarFecha" USING PARAMETROS-ENTRADA-VALFEC
                                     PARAMETRO-SALIDA-VALFEC
           IF VALFEC-FECHA-VALIDA = "S" THEN
              IF SUM-RES-SIGNO = "+" THEN
                 PERFORM SUMAR-DIAS
              ELSE
                 IF SUM-RES-SIGNO = "-" THEN
                    PERFORM RESTAR-DIAS
                 END-IF
              END-IF

           END-IF

           MOVE ANO-2    TO NUEVA-FECHA-YYYYMMDD(1:4)
           MOVE MES-2    TO NUEVA-FECHA-YYYYMMDD(5:2)
           MOVE DIA-2    TO NUEVA-FECHA-YYYYMMDD(7:2)

           GOBACK.

       SUMAR-DIAS.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SUM-RES-DIAS
               ADD 1        TO DIA-2
               IF MES-2 NOT = 2 THEN
                   IF DIA-2 > DIA-MES(MES-2) THEN
                       MOVE 1 TO DIA-2
                       ADD 1 TO MES-2
                       IF MES-2 > 12 THEN
                           MOVE 1   TO MES-2
                           ADD  1   TO ANO-2
                       END-IF
                   END-IF
                ELSE
                  PERFORM BISIESTO

      *    VERIFICAMOS SI EL MES DEL AÑO ES BISIESTO
                  IF (RESTO = 0 AND RESTO-2 <> 0) OR (RESTO-3 = 0) THEN
                     MOVE 29    TO DIA-MES(MES-2)
                  ELSE
                     MOVE 28    TO DIA-MES(MES-2)
                  END-IF
                  IF DIA-2 > DIA-MES(MES-2) THEN
                       ADD 1 TO MES-2
                       MOVE 1  TO DIA-2
                       IF MES-2 > 12 THEN
                           MOVE 1   TO MES-2
                           ADD  1   TO ANO-2
                       END-IF
                       MOVE 1       TO DIA-2
                  END-IF
                END-IF

           END-PERFORM

           .
       RESTAR-DIAS.

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > SUM-RES-DIAS
               SUBTRACT 1      FROM DIA-2
               IF DIA-2 < 1 THEN
                   SUBTRACT 1 FROM MES-2
                   IF MES-2 < 1 THEN
                       SUBTRACT 1 FROM ANO-2
                       MOVE 12      TO MES-2
                       MOVE DIA-MES(MES-2) TO DIA-2
                   ELSE
                       IF MES-2 NOT = 2 THEN
                           MOVE DIA-MES(MES-2) TO DIA-2
                       ELSE
                           PERFORM BISIESTO

      *    VERIFICAMOS SI EL MES DEL AÑO ES BISIESTO
                          IF (RESTO = 0 AND RESTO-2 <> 0) OR
                             (RESTO-3 = 0) THEN
                             MOVE 29    TO DIA-2
                          ELSE
                             MOVE 28    TO DIA-2
                          END-IF

                       END-IF
                   END-IF
               END-IF

           END-PERFORM
           .
       BISIESTO.
           DIVIDE ANO-2 BY 4 GIVING ENTERO
                       REMAINDER RESTO
           DIVIDE ANO-2 BY 100 GIVING ENTERO-2
                       REMAINDER RESTO-2
           DIVIDE ANO-2 BY 400 GIVING ENTERO-3
                       REMAINDER RESTO-3
           .

       END PROGRAM SumarDiasFecha.
