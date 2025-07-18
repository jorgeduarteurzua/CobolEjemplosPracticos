      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Generar Tabla de Multiplicar del 1 al 10
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SumarNNumeros.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-TRABAJO.
           05 TotalSuma  PIC 9(07) VALUE 0.
           05 Rec-Numeros PIC 9(03).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *     SUMAEREMOS DEL 1 AL 100
            PERFORM VARYING Rec-Numeros FROM 1 BY 1
                    UNTIL Rec-Numeros > 100
                ADD Rec-Numeros TO TotalSuma
            END-PERFORM
            DISPLAY "El Total Sumado de los 100 primeros Numeros : "
                    TotalSuma
            STOP RUN.
       END PROGRAM SumarNNumeros.
