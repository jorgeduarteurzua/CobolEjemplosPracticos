      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 18-07-2025
      * Purpose: ENSEÑANZA, para poder usar esta Rutina, se requiere
      *          haber generado el Calendario (GenCalendario.cbl)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DiffDiasFecha2.

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
       01  FS-CALENDARIO       PIC XX.
       01  DIA-FECHA1          PIC 9(06).
       01  DIA-FECHA2          PIC 9(06).


       LINKAGE SECTION.
       01 INP-DIFFDIASFECHA.
           05 FECHA1-YYYYMMDD PIC 9(08) VALUE 20200501.
           05 FECHA2-YYYYMMDD PIC 9(08) VALUE 20250721.
       01 OUT-DIFFDIASFECHA.
           05 DIFF-DIAS       PIC 9(07).

       PROCEDURE DIVISION USING INP-DIFFDIASFECHA
                                OUT-DIFFDIASFECHA.

       MAIN-PROCEDURE.

            OPEN INPUT calendario

            MOVE ZEROES            TO DIA-FECHA1
                                      DIA-FECHA2

            MOVE FECHA1-YYYYMMDD   TO calendario-fecha
            READ calendario END-READ
            IF FS-CALENDARIO = "00" THEN
                MOVE calendario-numdia TO DIA-FECHA1
            END-IF

            MOVE FECHA2-YYYYMMDD   TO calendario-fecha
            READ calendario END-READ
            IF FS-CALENDARIO = "00" THEN
                MOVE calendario-numdia TO DIA-FECHA2
            END-IF

           IF DIA-FECHA1 NOT = ZEROES AND
              DIA-FECHA2 NOT = ZEROES
              IF DIA-FECHA2 > DIA-FECHA1 THEN
                 COMPUTE DIFF-DIAS = DIA-FECHA2 - DIA-FECHA1
              ELSE
                  COMPUTE DIFF-DIAS = DIA-FECHA1 - DIA-FECHA2
              END-IF
           ELSE
               MOVE ZEROES   TO DIFF-DIAS
           END-IF

           CLOSE calendario

           GOBACK.


       END PROGRAM DiffDiasFecha2.
