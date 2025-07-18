      ******************************************************************
      * Author: Jorge Duarte
      * Date: 01-07-2025
      * Purpose: Generar Tabla de Multiplicar del 1 al 10
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgmTablaMultiplicar.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-TRABAJO.
           05 NUM-TABLA   PIC 9(02).
           05 REC-TABLA   PIC 9(02).
           05 RES-TABLA   PIC 9(03).
           05 STR-TABLA   PIC X(20).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM VARYING NUM-TABLA FROM 1 BY 1
                    UNTIL NUM-TABLA > 10
                DISPLAY "TABLA : " NUM-TABLA
                DISPLAY "======================"
                PERFORM VARYING REC-TABLA FROM 1 BY 1
                        UNTIL REC-TABLA > 10
                    COMPUTE RES-TABLA = NUM-TABLA * REC-TABLA
                    DISPLAY NUM-TABLA " * " REC-TABLA " = " RES-TABLA
                END-PERFORM
                DISPLAY " "
            END-PERFORM
            STOP RUN.
       END PROGRAM pgmTablaMultiplicar.
