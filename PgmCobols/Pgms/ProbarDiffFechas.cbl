      ******************************************************************
      * Author: JORGE DUARTE URZUA
      * Date: 18-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProbarDiffFechas.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 INP-DIFFDIASFECHA.
           05 FECHA1-YYYYMMDD PIC 9(08).
           05 FECHA2-YYYYMMDD PIC 9(08).
       01 OUT-DIFFDIASFECHA.
           05 DIFF-DIAS       PIC 9(07).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      * 1- Caso 1 Mostramos la Diferencia entre 2 fechas iguales
           MOVE 20250718     TO FECHA1-YYYYMMDD
           MOVE 20250718     TO FECHA2-YYYYMMDD
           MOVE ZEROES       TO DIFF-DIAS
           CALL "DiffDiasFecha" USING INP-DIFFDIASFECHA
                                      OUT-DIFFDIASFECHA

           DISPLAY "CASO PRUEBA 1"
           DISPLAY "FECHA1-YYYYMMDD ..: " FECHA1-YYYYMMDD
           DISPLAY "FECHA2-YYYYMMDD ..: " FECHA2-YYYYMMDD
           DISPLAY "DIFERENCIA DIAS ..: " DIFF-DIAS
           DISPLAY "-------------------------------------------"
           DISPLAY " "

      * 2- Caso 2 Fecha Hasta < Fecha Desde
           MOVE 20250718     TO FECHA1-YYYYMMDD
           MOVE 20250710     TO FECHA2-YYYYMMDD
           MOVE ZEROES       TO DIFF-DIAS
           CALL "DiffDiasFecha" USING INP-DIFFDIASFECHA
                                      OUT-DIFFDIASFECHA

           DISPLAY "CASO PRUEBA 2"
           DISPLAY "FECHA1-YYYYMMDD ..: " FECHA1-YYYYMMDD
           DISPLAY "FECHA2-YYYYMMDD ..: " FECHA2-YYYYMMDD
           DISPLAY "DIFERENCIA DIAS ..: " DIFF-DIAS
           DISPLAY "-------------------------------------------"
           DISPLAY " "

      * 3- Caso 3 Fecha en el mismo mes
           MOVE 20250718     TO FECHA1-YYYYMMDD
           MOVE 20250731     TO FECHA2-YYYYMMDD
           MOVE ZEROES       TO DIFF-DIAS
           CALL "DiffDiasFecha" USING INP-DIFFDIASFECHA
                                      OUT-DIFFDIASFECHA

           DISPLAY "CASO PRUEBA 3"
           DISPLAY "FECHA1-YYYYMMDD ..: " FECHA1-YYYYMMDD
           DISPLAY "FECHA2-YYYYMMDD ..: " FECHA2-YYYYMMDD
           DISPLAY "DIFERENCIA DIAS ..: " DIFF-DIAS
           DISPLAY "-------------------------------------------"
           DISPLAY " "

      * 4- Caso 4
           MOVE 20240201     TO FECHA1-YYYYMMDD
           MOVE 20251015     TO FECHA2-YYYYMMDD
           MOVE ZEROES       TO DIFF-DIAS
           CALL "DiffDiasFecha" USING INP-DIFFDIASFECHA
                                      OUT-DIFFDIASFECHA

           DISPLAY "CASO PRUEBA 4"
           DISPLAY "FECHA1-YYYYMMDD ..: " FECHA1-YYYYMMDD
           DISPLAY "FECHA2-YYYYMMDD ..: " FECHA2-YYYYMMDD
           DISPLAY "DIFERENCIA DIAS ..: " DIFF-DIAS
           DISPLAY "-------------------------------------------"
           DISPLAY " "

            STOP RUN.
       END PROGRAM ProbarDiffFechas.
