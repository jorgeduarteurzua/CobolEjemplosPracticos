      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ListadoClientes.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT datos-usuarios
               ASSIGN TO
           "C:\PgmCobols\Data\clientes.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS usuario-rut
               FILE STATUS IS FS-USUARIOS.

       DATA DIVISION.
       FILE SECTION.
       FD  datos-usuarios.
           copy "C:\PgmCobols\FD\FDCliente.cbl".

       WORKING-STORAGE SECTION.
       01  fin-de-archivo      PIC X VALUE 'N'.
       01  opcion              PIC X(1).
       01  continuar           PIC X VALUE 'Y'.
       01  usuario-clave       PIC X(10).
       01  done                PIC X VALUE 'N'.
       01  FS-USUARIOS         PIC XX.
       01  SALIR               PIC X.

       01 VARIABLES-DE-TRABAJO.
           05 BLANCO           PIC X.
           05 RUT-POS          PIC 9(10).
           05 WS-NOMBRE        PIC X(50).
           05 POS-ARR          PIC 9(3).
           05 WS-LINEA         PIC 9(3).
           05 WS-LIN           PIC 9(2).
           05 WS-OPC           PIC X.
           05 ARR-DETALLE OCCURS 15 TIMES.
              10 ARR-LIN       PIC 9(3).
              10 ARR-RUT       PIC 9(10).
              10 ARR-GUI       PIC X.
              10 ARR-DV        PIC X.
              10 ARR-NOM       PIC X(50).

       01 PAR-OUT-BORRAR.
           05 LK-BORRADO    PIC X.
      *       S = Se Borro Cliente
      *       N = No Borrado
           05 LK-STATUS     PIC X(02).

       SCREEN SECTION.
       01  PANTALLA-ENTRADA.
         03  BLANK SCREEN.
         03  LINE 1 COL 30  VALUE "Listado Clientes".
         03  LINE 3 COL 5   VALUE "Posicionar Rut : ".
         03  LINE 3 COL 24 PIC 9(10) USING RUT-POS.

       01 PANTALLA-VISUALIZACION.
         03  LINE 5 COL 01 VALUE "====================================".
         03  LINE 5 COL 37 VALUE "====================================".
         03  LINE 5 COL 73 VALUE "=====".
         03  LINE 6 COL 01 VALUE "LIN RUT           NOMBRE".
         03  LINE 7 COL 01 VALUE "====================================".
         03  LINE 7 COL 37 VALUE "====================================".
         03  LINE 7 COL 73 VALUE "=====".

         03  LINE 8 COL 1  PIC ZZZ FROM ARR-LIN(1).
         03  LINE 8 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(1).
         03  LINE 8 COL 15 PIC X FROM ARR-GUI(1).
         03  LINE 8 COL 16 PIC X  FROM ARR-DV(1).
         03  LINE 8 COL 19 PIC X(50) FROM ARR-NOM(1).

         03  LINE 9 COL 1  PIC ZZZ FROM ARR-LIN(2).
         03  LINE 9 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(2).
         03  LINE 9 COL 15 PIC X FROM ARR-GUI(2).
         03  LINE 9 COL 16 PIC X  FROM ARR-DV(2).
         03  LINE 9 COL 19 PIC X(50) FROM ARR-NOM(2).

         03  LINE 10 COL 1  PIC ZZZ FROM ARR-LIN(3).
         03  LINE 10 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(3).
         03  LINE 10 COL 15 PIC X FROM ARR-GUI(3).
         03  LINE 10 COL 16 PIC X  FROM ARR-DV(3).
         03  LINE 10 COL 19 PIC X(50) FROM ARR-NOM(3).


         03  LINE 11 COL 1  PIC ZZZ FROM ARR-LIN(4).
         03  LINE 11 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(4).
         03  LINE 11 COL 15 PIC X FROM ARR-GUI(4).
         03  LINE 11 COL 16 PIC X  FROM ARR-DV(4).
         03  LINE 11 COL 19 PIC X(50) FROM ARR-NOM(4).

         03  LINE 12 COL 1  PIC ZZZ FROM ARR-LIN(5).
         03  LINE 12 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(5).
         03  LINE 12 COL 15 PIC X FROM ARR-GUI(5).
         03  LINE 12 COL 16 PIC X  FROM ARR-DV(5).
         03  LINE 12 COL 19 PIC X(50) FROM ARR-NOM(5).

         03  LINE 13 COL 1  PIC ZZZ FROM ARR-LIN(6).
         03  LINE 13 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(6).
         03  LINE 13 COL 15 PIC X FROM ARR-GUI(6).
         03  LINE 13 COL 16 PIC X  FROM ARR-DV(6).
         03  LINE 13 COL 19 PIC X(50) FROM ARR-NOM(6).

         03  LINE 14 COL 1  PIC ZZZ FROM ARR-LIN(7).
         03  LINE 14 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(7).
         03  LINE 14 COL 15 PIC X FROM ARR-GUI(7).
         03  LINE 14 COL 16 PIC X  FROM ARR-DV(7).
         03  LINE 14 COL 19 PIC X(50) FROM ARR-NOM(7).

         03  LINE 15 COL 1  PIC ZZZ FROM ARR-LIN(8).
         03  LINE 15 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(8).
         03  LINE 15 COL 15 PIC X FROM ARR-GUI(8).
         03  LINE 15 COL 16 PIC X  FROM ARR-DV(8).
         03  LINE 15 COL 19 PIC X(50) FROM ARR-NOM(8).

         03  LINE 16 COL 1  PIC ZZZ FROM ARR-LIN(9).
         03  LINE 16 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(9).
         03  LINE 16 COL 15 PIC X FROM ARR-GUI(9).
         03  LINE 16 COL 16 PIC X  FROM ARR-DV(9).
         03  LINE 16 COL 19 PIC X(50) FROM ARR-NOM(9).

         03  LINE 17 COL 1  PIC ZZZ FROM ARR-LIN(10).
         03  LINE 17 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(10).
         03  LINE 17 COL 15 PIC X FROM ARR-GUI(10).
         03  LINE 17 COL 16 PIC X  FROM ARR-DV(10).
         03  LINE 17 COL 19 PIC X(50) FROM ARR-NOM(10).

         03  LINE 18 COL 1  PIC ZZZ FROM ARR-LIN(11).
         03  LINE 18 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(11).
         03  LINE 18 COL 15 PIC X FROM ARR-GUI(11).
         03  LINE 18 COL 16 PIC X  FROM ARR-DV(11).
         03  LINE 18 COL 19 PIC X(50) FROM ARR-NOM(11).

         03  LINE 19 COL 1  PIC ZZZ FROM ARR-LIN(12).
         03  LINE 19 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(12).
         03  LINE 19 COL 15 PIC X FROM ARR-GUI(12).
         03  LINE 19 COL 16 PIC X  FROM ARR-DV(12).
         03  LINE 19 COL 19 PIC X(50) FROM ARR-NOM(12).

         03  LINE 20 COL 1  PIC ZZZ FROM ARR-LIN(13).
         03  LINE 20 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(13).
         03  LINE 20 COL 15 PIC X FROM ARR-GUI(13).
         03  LINE 20 COL 16 PIC X  FROM ARR-DV(13).
         03  LINE 20 COL 19 PIC X(50) FROM ARR-NOM(13).

         03  LINE 21 COL 1  PIC ZZZ FROM ARR-LIN(14).
         03  LINE 21 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(14).
         03  LINE 21 COL 15 PIC X FROM ARR-GUI(14).
         03  LINE 21 COL 16 PIC X  FROM ARR-DV(14).
         03  LINE 21 COL 19 PIC X(50) FROM ARR-NOM(14).

         03  LINE 22 COL 1  PIC ZZZ FROM ARR-LIN(15).
         03  LINE 22 COL 5  PIC ZZZZZZZZZZ FROM ARR-RUT(15).
         03  LINE 22 COL 15 PIC X FROM ARR-GUI(15).
         03  LINE 22 COL 16 PIC X  FROM ARR-DV(15).
         03  LINE 22 COL 19 PIC X(50) FROM ARR-NOM(15).

       01 PANTALLA-SALIR.
         03 LINE 24 COL 5 VALUE "SALIR (S/N) : ".
         03 LINE 24 COL 19 PIC X USING SALIR.
         03 LINE 24 COL 21 VALUE "LINEA :".
         03 LINE 24 COL 28 PIC 9(02) USING WS-LIN.
         03 LINE 24 COL 31 VALUE "Opcion C=Consultar; B=Borrar :".
         03 LINE 24 COL 64 PIC X USING WS-OPC.

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT datos-usuarios
           MOVE 0      TO RUT-POS
           MOVE "N"    TO SALIR
           PERFORM UNTIL SALIR = "S" OR "s"
               DISPLAY PANTALLA-ENTRADA
               ACCEPT  PANTALLA-ENTRADA

               PERFORM mostrar-todos-los-usuarios
               PERFORM PEDIR-SALIR
           END-PERFORM
           CLOSE datos-usuarios
           STOP RUN.

       mostrar-todos-los-usuarios.
           PERFORM cargar-usuarios

           DISPLAY PANTALLA-ENTRADA
           ACCEPT  PANTALLA-VISUALIZACION

            .

       cargar-usuarios.
           PERFORM START-READING

           PERFORM VARYING POS-ARR FROM 1 BY 1 UNTIL POS-ARR > 15
              MOVE ZEROES   TO ARR-LIN(POS-ARR)
                               ARR-RUT(POS-ARR)
              MOVE SPACES   TO ARR-DV (POS-ARR)
                               ARR-GUI(POS-ARR)
                               ARR-NOM(POS-ARR)
           END-PERFORM

           MOVE 1  TO POS-ARR

           PERFORM UNTIL fin-de-archivo = 'Y' OR POS-ARR > 15
               READ datos-usuarios
                   AT END
                       MOVE 'Y' TO  fin-de-archivo
                   NOT AT END

                       MOVE POS-ARR         TO ARR-LIN    (POS-ARR)
                       MOVE usuario-rut     TO ARR-RUT    (POS-ARR)
                       MOVE "-"             TO ARR-GUI    (POS-ARR)
                       MOVE usuario-dv      TO ARR-DV     (POS-ARR)
                       STRING usuario-nombre DELIMITED BY " "
                            " "            DELIMITED BY SIZE
                            usuario-apepat DELIMITED BY " "
                            " "            DELIMITED BY SIZE
                            usuario-apemat DELIMITED BY " "
                                          INTO ARR-NOM    (POS-ARR)

                       ADD  1               TO POS-ARR
               END-READ
           END-PERFORM

           .

       START-READING.
           MOVE 'N'        TO fin-de-archivo
           MOVE RUT-POS    TO usuario-rut
           START datos-usuarios KEY IS NOT < usuario-rut
               INVALID KEY
                   DISPLAY "Error al iniciar lectura de registros."
                   MOVE "Y"    TO fin-de-archivo
           END-START.

       PEDIR-SALIR.
           MOVE " "            TO SALIR
           PERFORM UNTIL SALIR = "S" OR "s" or "N" or "n"
              DISPLAY PANTALLA-SALIR
              ACCEPT  PANTALLA-SALIR
              IF WS-LIN NOT = 0 AND WS-LIN < 16 THEN
                  IF WS-OPC = "C" OR "c" THEN
                     PERFORM LLAMA-CONSULTA
                     MOVE ZEROES  TO WS-LIN
                     MOVE SPACES  TO WS-OPC
                     MOVE "N" TO SALIR
                  END-IF
                  IF WS-OPC = "B" OR "b" THEN
                     PERFORM ELIMINA-CLIENTE
                     MOVE ZEROES  TO WS-LIN
                     MOVE SPACES  TO WS-OPC
                     MOVE "N" TO SALIR
                  END-IF
              END-IF
           END-PERFORM
           .

       LLAMA-CONSULTA.
           IF ARR-RUT(WS-LIN) NOT = 0 THEN
               CALL "ConsultaCliente" USING ARR-RUT(WS-LIN)
           END-IF
           .

        ELIMINA-CLIENTE.
           IF ARR-RUT(WS-LIN) NOT = 0 THEN
               CALL "BorrarCliente" USING ARR-RUT(WS-LIN)
                                         PAR-OUT-BORRAR
           END-IF
           .

       END PROGRAM ListadoClientes.
