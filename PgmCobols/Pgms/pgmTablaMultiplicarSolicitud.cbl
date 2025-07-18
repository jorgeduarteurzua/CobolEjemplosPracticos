      ******************************************************************
      * Author: Jorge Duarte
      * Date: 01-07-2025
      * Purpose: Generar Tabla de Multiplicar del 1 al 10
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgmTablaMultiplicarSolicitud.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 OPCION          PIC 9.
       01 VARIABLES-TRABAJO.
           05 NUM-TABLA   PIC 9(02).
           05 REC-TABLA   PIC 9(02).
           05 RES-TABLA   PIC 9(03).
           05 STR-TABLA   PIC X(20).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 0 TO OPCION
           PERFORM UNTIL OPCION = 2
              DISPLAY "MENU DE TABLA "
              DISPLAY "--------------"
              DISPLAY "1.- Ingrese Tabla"
              DISPLAY "2.- Salir"
              DISPLAY " "
              ACCEPT OPCION
              IF OPCION = 1 THEN
                DISPLAY "INGRESE TABLA : "
                ACCEPT NUM-TABLA
                DISPLAY "======================"
                PERFORM VARYING REC-TABLA FROM 1 BY 1
                        UNTIL REC-TABLA > 10
                    COMPUTE RES-TABLA = NUM-TABLA * REC-TABLA
                    DISPLAY NUM-TABLA " * " REC-TABLA " = " RES-TABLA
                END-PERFORM
                DISPLAY " "
              END-IF
           END-PERFORM
           STOP RUN.
       END PROGRAM pgmTablaMultiplicarSolicitud.
