      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 18-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DiffDiasFecha.
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
           05 WS-FECHA1.
               10 ANOMES1.
                  15 ANO1        PIC 9(04).
                  15 MES1        PIC 9(02).
               10 DIA1        PIC 9(02).
           05 WS-FECHA2.
               10 ANOMES2.
                  15 ANO2        PIC 9(04).
                  15 MES2        PIC 9(02).
               10 DIA2        PIC 9(02).
           05 WS-DIFF          PIC 9(07).
           05 DIAS-X-MES PIC X(24) VALUE "312831303130313130313031".
           05 R-MES REDEFINES DIAS-X-MES OCCURS 12 TIMES.
              10 DIAS-MES     PIC 9(02).

       LINKAGE SECTION.
       01 INP-DIFFDIASFECHA.
           05 FECHA1-YYYYMMDD PIC 9(08).
           05 FECHA2-YYYYMMDD PIC 9(08).
       01 OUT-DIFFDIASFECHA.
           05 DIFF-DIAS       PIC 9(07).

       PROCEDURE DIVISION USING INP-DIFFDIASFECHA
                                OUT-DIFFDIASFECHA.
       MAIN-PROCEDURE.

            MOVE FECHA1-YYYYMMDD   TO WS-FECHA1
            MOVE FECHA2-YYYYMMDD   TO WS-FECHA2

            IF FECHA2-YYYYMMDD < FECHA1-YYYYMMDD THEN
                MOVE 0             TO WS-DIFF
            ELSE
                IF FECHA1-YYYYMMDD = FECHA2-YYYYMMDD THEN
                   MOVE 0          TO WS-DIFF
                ELSE
                   PERFORM CALCULAR-DIAS
                END-IF
            END-IF

            MOVE WS-DIFF  TO DIFF-DIAS

            GOBACK.

       CALCULAR-DIAS.
           MOVE 0          TO WS-DIFF
           IF ANO1 = ANO2 AND
              MES1 = MES2
              COMPUTE WS-DIFF = DIA2 - DIA1
           ELSE

      *    PARTIMOS SUMANDO LA DIFERENCIA ENTRE LA FECHA INICIAL
           COMPUTE WS-DIFF = WS-DIFF + (DIAS-MES(MES1) - DIA1)
      *    AGREGAMOS LOS DIAS DE LA FECHA FINAL
           COMPUTE WS-DIFF = WS-DIFF + DIA2

             ADD 1        TO MES1
             IF MES1 > 12 THEN
                 MOVE 1  TO MES1
                 ADD  1  TO ANO1
             END-IF

             SUBTRACT 1 FROM MES2
             IF MES2 < 1 THEN
                MOVE 12    TO MES2
                SUBTRACT 1 FROM ANO2
             END-IF

             PERFORM UNTIL ANOMES1 > ANOMES2

               IF MES1 = 2 THEN
                  DIVIDE ANO1  BY 4   GIVING ENTERO   REMAINDER RESTO
                  DIVIDE ANO1  BY 100 GIVING ENTERO2  REMAINDER RESTO2
                  DIVIDE ANO1  BY 400 GIVING ENTERO3  REMAINDER RESTO3
                  IF RESTO3 = 0 OR (RESTO = 0 AND RESTO2 NOT = 0) THEN
                     ADD 29      TO WS-DIFF
                  ELSE
                     ADD 28      TO WS-DIFF
               ELSE
                  ADD DIAS-MES(MES1)   TO WS-DIFF
               END-IF

               ADD 1    TO MES1
               IF MES1 > 12 THEN
                   MOVE 1  TO MES1
                   ADD  1  TO ANO1
               END-IF

             END-PERFORM
           END-IF
           .

       END PROGRAM DiffDiasFecha.
