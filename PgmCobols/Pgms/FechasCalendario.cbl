      ******************************************************************
      * Author: Jorge Duarte
      * Date: 17-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FechasCalendario.

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
           05 ARR-DETALLE OCCURS 15 TIMES.
              10 ARR-LIN       PIC 9(3).
              10 ARR-FEC       PIC X(10).
              10 ARR-DIASEM    PIC X(10).
              10 ARR-FERIADO   PIC X.
              10 ARR-DET       PIC X(50).

       SCREEN SECTION.
       01  PANTALLA-ENTRADA.
         03  BLANK SCREEN.
         03  LINE 1 COL 30  VALUE "Fechas Calendario".
         03  LINE 3 COL 5   VALUE "Posicionar Fecha : ".
         03  LINE 3 COL 24 PIC 9(08) USING FECHA-POS.

       01 PANTALLA-VISUALIZACION.
         03  LINE 5 COL 01 VALUE "====================================".
         03  LINE 5 COL 37 VALUE "====================================".
         03  LINE 5 COL 73 VALUE "=====".
         03  LINE 6 COL 01 VALUE "LIN FECHA       DIA SEMANA  FERIADO".
         03  LINE 7 COL 01 VALUE "====================================".
         03  LINE 7 COL 37 VALUE "====================================".
         03  LINE 7 COL 73 VALUE "=====".

         03  LINE 08 COL 1  PIC X(80) FROM ARR-DET(01).
         03  LINE 09 COL 1  PIC X(80) FROM ARR-DET(02).
         03  LINE 10 COL 1  PIC X(80) FROM ARR-DET(03).
         03  LINE 11 COL 1  PIC X(80) FROM ARR-DET(04).
         03  LINE 12 COL 1  PIC X(80) FROM ARR-DET(05).
         03  LINE 13 COL 1  PIC X(80) FROM ARR-DET(06).
         03  LINE 14 COL 1  PIC X(80) FROM ARR-DET(07).
         03  LINE 15 COL 1  PIC X(80) FROM ARR-DET(08).
         03  LINE 16 COL 1  PIC X(80) FROM ARR-DET(09).
         03  LINE 17 COL 1  PIC X(80) FROM ARR-DET(10).
         03  LINE 18 COL 1  PIC X(80) FROM ARR-DET(11).
         03  LINE 19 COL 1  PIC X(80) FROM ARR-DET(12).
         03  LINE 20 COL 1  PIC X(80) FROM ARR-DET(13).
         03  LINE 21 COL 1  PIC X(80) FROM ARR-DET(14).
         03  LINE 22 COL 1  PIC X(80) FROM ARR-DET(15).

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

           PERFORM VARYING POS-ARR FROM 1 BY 1 UNTIL POS-ARR > 15
              MOVE ZEROES   TO ARR-LIN    (POS-ARR)
              MOVE SPACES   TO ARR-FEC    (POS-ARR)
                               ARR-DIASEM (POS-ARR)
                               ARR-FERIADO(POS-ARR)
                               ARR-DET    (POS-ARR)
           END-PERFORM

           MOVE 1  TO POS-ARR

           PERFORM UNTIL fin-de-archivo = 'Y' OR POS-ARR > 15 OR
                   FS-CALENDARIO NOT = "00"

               READ calendario
                   AT END
                       MOVE 'Y' TO  fin-de-archivo
                   NOT AT END

                       MOVE POS-ARR             TO ARR-LIN    (POS-ARR)
                       MOVE calendario-fecha    TO ARR-FEC    (POS-ARR)
                                                   WS-FECHA

                       EVALUATE calendario-dia-sem
                         WHEN 1 MOVE "DOMINGO"    TO WS-DIA
                         WHEN 2 MOVE "LUNES"      TO WS-DIA
                         WHEN 3 MOVE "MARTES"     TO WS-DIA
                         WHEN 4 MOVE "MIERCOLES"  TO WS-DIA
                         WHEN 5 MOVE "JUEVES"     TO WS-DIA
                         WHEN 6 MOVE "VIERNES"    TO WS-DIA
                         WHEN 7 MOVE "SABADO"     TO WS-DIA
                       END-EVALUATE

                       STRING  POS-ARR    DELIMITED SIZE
                               " "        DELIMITED SIZE
                               WS-FECHA(7:2) DELIMITED SIZE
                               "-"           DELIMITED SIZE
                               WS-FECHA(5:2) DELIMITED SIZE
                               "-"           DELIMITED SIZE
                               WS-FECHA(1:4) DELIMITED SIZE
                               "  "          DELIMITED SIZE
                               WS-DIA        DELIMITED SIZE
                               "     "       DELIMITED SIZE
                          calendario-feriado DELIMITED SIZE
                                          INTO ARR-DET    (POS-ARR)


                       ADD  1               TO POS-ARR
               END-READ

           END-PERFORM

           .

       START-READING.
           MOVE 'N'             TO fin-de-archivo

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

       END PROGRAM FechasCalendario.
