      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Generar Tabla de Multiplicar del 1 al 10
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FactorialNumeroN.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-TRABAJO.
           05 TotalFactorial  PIC 9(07) VALUE 1.
           05 Fac-Numeros PIC 9(03).
           05 Mult            PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *     SUMAEREMOS DEL 1 AL 100
            PERFORM VARYING Fac-Numeros FROM 4 BY -1
                    UNTIL Fac-Numeros < 1
                COMPUTE TotalFactorial = TotalFactorial *
                                         Fac-Numeros
            END-PERFORM
            DISPLAY "El Total FACTORIAL : "
                    TotalFactorial
            STOP RUN.
       END PROGRAM FactorialNumeroN.
