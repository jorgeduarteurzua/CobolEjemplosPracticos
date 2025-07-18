      ******************************************************************
      * Author: Jorge Duarte
      * Date: 17-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MostrarCalendario.

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
       01  fin-de-archivo      PIC X VALUE 'N'.
       01  opcion              PIC X(1).
       01  continuar           PIC X VALUE 'Y'.
       01  usuario-clave       PIC X(10).
       01  done                PIC X VALUE 'N'.
       01  FS-CALENDARIO       PIC XX.
       01  SALIR               PIC X.

       01 VARIABLES-DE-TRABAJO.
           05 BLANCO           PIC X.
           05 PEDIR-MES        PIC 9(02).
           05 PEDIR-ANO        PIC 9(04).
           05 WS-FECHA         PIC X(08).
           05 WS-DIA           PIC X(10).
           05 FECHA-POS        PIC 9(08).
           05 R-FECHA-POS REDEFINES FECHA-POS.
              10 F-POS-DIA     PIC 9(02).
              10 F-POS-MES     PIC 9(02).
              10 F-POS-ANO     PIC 9(04).
           05 FECHA-AMD.
              10 F-POS-ANO     PIC 9(04).
              10 F-POS-MES     PIC 9(02).
              10 F-POS-DIA     PIC 9(02).
           05 WS-NOMBRE        PIC X(50).
           05 POS-ARR          PIC 9(3).
           05 WS-LINEA         PIC 9(3).
           05 WS-LIN           PIC 9(2).
           05 WS-OPC           PIC X.
           05 POS-I-DIAS       PIC 9.
           05 ARR-DIA OCCURS 07 TIMES.
              10 ARR-DIA-S    PIC ZZ.
           05 ARR-MES OCCURS 05 TIMES.
              10 ARR-DET    PIC X(38).

           05 WS-PEDIR-X      PIC X(06).
           05 WS-PEDIR REDEFINES WS-PEDIR-X PIC 9(06).

           05 WS-FEC-ARC.
              10 WS-FEC-YYYYMM PIC 9(06).
              10 WS-FEC-DD     PIC 9(02).




       SCREEN SECTION.
       01  PANTALLA-ENTRADA.
         03  BLANK SCREEN.
         03  LINE 1 COL 40  VALUE "Calendario".
         03  LINE 3 COL 5   VALUE "Mes : ".
         03  LINE 3 COL 10 PIC 9(02) USING PEDIR-MES.
         03  LINE 3 COL 15   VALUE "Año : ".
         03  LINE 3 COL 24 PIC 9(04) USING PEDIR-ANO.

       01 PANTALLA-VISUALIZACION.
         03  LINE 5 COL 10 VALUE "+====+====+====+====+====+====+----+".
         03  LINE 6 COL 10 VALUE "| LN | MA | MI | JU | VI | SA | DO |".
         03  LINE 7 COL 10 VALUE "+====+====+====+====+====+====+====+".

         03  LINE 08 COL 10  PIC X(80) FROM ARR-DET(01).
         03  LINE 09 COL 10  PIC X(80) FROM ARR-DET(02).
         03  LINE 10 COL 10  PIC X(80) FROM ARR-DET(03).
         03  LINE 11 COL 10  PIC X(80) FROM ARR-DET(04).
         03  LINE 12 COL 10  PIC X(80) FROM ARR-DET(05).
         03 LINE 13 COL 10 VALUE "+====+====+====+====+====+====+====+".

       01 PANTALLA-SALIR.
         03 LINE 24 COL 5 VALUE "SALIR (S/N) : ".
         03 LINE 24 COL 19 PIC X USING SALIR.


       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT calendario
           MOVE 0      TO FECHA-POS
           MOVE "N"    TO SALIR
           PERFORM UNTIL SALIR = "S" OR "s"
               DISPLAY PANTALLA-ENTRADA
               ACCEPT  PANTALLA-ENTRADA

               PERFORM mostrar-fechas
               PERFORM PEDIR-SALIR

           END-PERFORM
           CLOSE calendario
           STOP RUN.

       mostrar-fechas.

           PERFORM cargar-fechas

           DISPLAY PANTALLA-ENTRADA
           ACCEPT  PANTALLA-VISUALIZACION

            .

       cargar-fechas.
           PERFORM START-READING

           PERFORM VARYING POS-ARR FROM 1 BY 1 UNTIL POS-ARR > 5
              MOVE SPACES   TO ARR-DET    (POS-ARR)
           END-PERFORM

           MOVE ZEROES   TO ARR-DIA-S(1)
                            ARR-DIA-S(2)
                            ARR-DIA-S(3)
                            ARR-DIA-S(4)
                            ARR-DIA-S(5)
                            ARR-DIA-S(6)
                            ARR-DIA-S(7)

           MOVE 1  TO POS-ARR

           PERFORM UNTIL fin-de-archivo = 'Y' OR POS-ARR > 5 OR
                   FS-CALENDARIO NOT = "00"

               READ calendario
                   AT END
                       MOVE 'Y' TO  fin-de-archivo
                   NOT AT END
                   MOVE calendario-fecha TO WS-FEC-ARC

                   IF WS-FEC-YYYYMM NOT = WS-PEDIR THEN
                      MOVE "Y" TO fin-de-archivo
                   ELSE
                       EVALUATE calendario-dia-sem
                         WHEN 1 MOVE 7      TO POS-I-DIAS
                         WHEN 2 MOVE 1      TO POS-I-DIAS
                         WHEN 3 MOVE 2      TO POS-I-DIAS
                         WHEN 4 MOVE 3      TO POS-I-DIAS
                         WHEN 5 MOVE 4      TO POS-I-DIAS
                         WHEN 6 MOVE 5      TO POS-I-DIAS
                         WHEN 7 MOVE 6      TO POS-I-DIAS
                       END-EVALUATE

                       MOVE WS-FEC-DD TO ARR-DIA-S(POS-I-DIAS)
                       IF POS-I-DIAS = 7 THEN

                          STRING  "| "           DELIMITED SIZE
                                 ARR-DIA-S(1)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(2)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(3)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(4)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(5)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(6)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(7)    DELIMITED SIZE
                                 " |"            DELIMITED SIZE
                                          INTO ARR-DET    (POS-ARR)

                          MOVE ZEROES   TO ARR-DIA-S(1)
                                           ARR-DIA-S(2)
                                           ARR-DIA-S(3)
                                           ARR-DIA-S(4)
                                           ARR-DIA-S(5)
                                           ARR-DIA-S(6)
                                           ARR-DIA-S(7)

                          ADD  1               TO POS-ARR
                        END-IF
                    END-IF
               END-READ

           END-PERFORM

      *    AGREGAMOS LO ULTIMO
           IF POS-ARR < 6 THEN
              STRING  "| "                       DELIMITED SIZE
                                 ARR-DIA-S(1)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(2)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(3)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(4)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(5)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(6)    DELIMITED SIZE
                                 " | "           DELIMITED SIZE
                                 ARR-DIA-S(7)    DELIMITED SIZE
                                 " |"            DELIMITED SIZE
                                          INTO ARR-DET    (POS-ARR)
           END-IF
           .

       START-READING.
           MOVE 'N'             TO fin-de-archivo

           MOVE PEDIR-MES       TO F-POS-MES OF R-FECHA-POS
                                   WS-PEDIR-X(5:2)
           MOVE PEDIR-ANO       TO F-POS-ANO OF R-FECHA-POS
                                   WS-PEDIR-X(1:4)
           MOVE 01              TO F-POS-DIA OF R-FECHA-POS



           MOVE CORR R-FECHA-POS TO FECHA-AMD

           MOVE FECHA-AMD        TO calendario-fecha

           START calendario KEY IS NOT < calendario-fecha
               INVALID KEY
                   DISPLAY "Error al iniciar lectura de registros."
                   MOVE "Y"    TO fin-de-archivo

           END-START.

       PEDIR-SALIR.
           MOVE " "            TO SALIR
           PERFORM UNTIL SALIR = "S" OR "s" or "N" or "n"
              DISPLAY PANTALLA-SALIR
              ACCEPT  PANTALLA-SALIR

           END-PERFORM
           .

       END PROGRAM MostrarCalendario.
