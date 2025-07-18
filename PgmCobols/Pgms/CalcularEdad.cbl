      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 16-06-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalcularEdad.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 VARIABLES-DE-TRABAJO.
           05 ENTERO               PIC 9(04).
           05 ENTERO2              PIC 9(04).
           05 ENTERO3              PIC 9(04).
           05 RESTO                PIC 9(04).
           05 RESTO2               PIC 9(04).
           05 RESTO3               PIC 9(04).
           05  FECHA1              PIC 9(8).
           05  R-FECHA1 REDEFINES FECHA1.
               10  ANO1                PIC 9(4).
               10  MES1                PIC 9(2).
               10  DIA1                PIC 9(2).
           05  FECHA2              PIC 9(8).
           05 R-FECHA2 REDEFINES FECHA2.
               10  ANO2                PIC 9(4).
               10  MES2                PIC 9(2).
               10  DIA2                PIC 9(2).
           05 WS-EDAD-ANOS     PIC S9(04).
           05 WS-EDAD-MESES    PIC S9(04).
           05 WS-EDAD-DIAS     PIC S9(02).
           05 CANTIDAD-DIA-MES PIC 9(02).


       LINKAGE SECTION.
       01 INP-CALCULA-EDAD.
           05 FECHA1-YYYYMMDD PIC X(08).
      *       |
      *       +--> Fecha Inicio a Calcular por Ejemplo Fecha de Nacimiento
           05 FECHA2-YYYYMMDD PIC X(08).
      *       |
      *       +--> Fecha contra la cual calcular, generalmente la fecha Actual

       01 OUT-CALCULA-EDAD.
           05 OUT-CODRET-EDAD PIC 9(02).
      *       0 = Fecha2 es Mayor o Igual a Fecha1
      *       1 = No se puede calcular Edad
           05 OUT-EDAD-ANOS   PIC 9(04).
           05 OUT-EDAD-MESES  PIC 9(04).
           05 OUT-EDAD-DIAS   PIC 9(02).

       PROCEDURE DIVISION USING INP-CALCULA-EDAD
                               OUT-CALCULA-EDAD.

       MAIN-PROCEDURE.

           MOVE 0                TO OUT-CODRET-EDAD
           MOVE FECHA1-YYYYMMDD  TO FECHA1
           MOVE FECHA2-YYYYMMDD  TO FECHA2
           IF FECHA2 >= FECHA1 THEN
               PERFORM CALCULAR-EDAD
           ELSE
               MOVE 1   TO OUT-CODRET-EDAD
           END-IF

           GOBACK.

       CALCULAR-EDAD.
      *--------------
           MOVE    ZEROES           TO WS-EDAD-ANOS
                                       WS-EDAD-MESES
                                       WS-EDAD-DIAS

           COMPUTE WS-EDAD-ANOS  = ANO2 - ANO1
           COMPUTE WS-EDAD-MESES = MES2 - MES1
           COMPUTE WS-EDAD-DIAS  = DIA2 -  DIA1

           IF WS-EDAD-DIAS < 0 THEN
              SUBTRACT 1 FROM WS-EDAD-MESES
              PERFORM DIAS-MES
              ADD CANTIDAD-DIA-MES TO WS-EDAD-DIAS
           END-IF

           IF WS-EDAD-MESES < 0 THEN
              SUBTRACT 1    FROM WS-EDAD-ANOS
              ADD 12          TO WS-EDAD-MESES
           END-IF

           MOVE WS-EDAD-ANOS   TO OUT-EDAD-ANOS
           MOVE WS-EDAD-MESES  TO OUT-EDAD-MESES
           MOVE WS-EDAD-DIAS   TO OUT-EDAD-DIAS

           .
       DIAS-MES.

           MOVE   31   TO CANTIDAD-DIA-MES
           IF MES1 = 2 THEN
               DIVIDE   ANO1  BY 4   GIVING ENTERO   REMAINDER RESTO
               DIVIDE   ANO1  BY 100 GIVING ENTERO2  REMAINDER RESTO2
               DIVIDE   ANO1  BY 400 GIVING ENTERO3  REMAINDER RESTO3
               IF RESTO3 = 0 OR (RESTO = 0 AND RESTO2 NOT = 0) THEN
                   MOVE 29      TO CANTIDAD-DIA-MES
               ELSE
                   MOVE 28      TO CANTIDAD-DIA-MES
               END-IF
           ELSE
               IF MES1 = 4 OR 6 OR 9 OR 11 THEN
                   MOVE 30      TO CANTIDAD-DIA-MES
               END-IF
           END-IF
           .
       END PROGRAM CalcularEdad.
