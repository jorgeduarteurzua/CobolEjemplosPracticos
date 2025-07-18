      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Mantenedor.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       01  fin-de-archivo      PIC X VALUE 'N'.
       01  opcion              PIC X(1).
       01  continuar           PIC X VALUE 'Y'.
       01  usuario-clave       PIC X(10).
       01  done                PIC X VALUE 'N'.
       01  FS-USUARIOS         PIC XX.

       01 VARIABLE-DE-TRABAJO.
           05 FECHA-SYS        PIC 9(08).
           05 CUENTA-1         PIC 9(03).
           05 CUENTA-2         PIC 9(03).
           05 CUENTA-3         PIC 9(03).
           05 WS-ANTES-DE-ARROBA   PIC X(100).
           05 WS-DESPUES-DE-ARROBA PIC X(100).
           05 WS-GRABAR        PIC X.
           05 WS-SALIR         PIC X.

       01 PAR-OUT-GRABAR.
           05  OUT-CODRET-GRABAR   PIC 9(04).
           05  OUT-DESRET-GRABAR   PIC X(50).

       01 VARIABLES-PEDIR-PANTALLA.

           05 RUT        PIC 9(10).
           05 DV         PIC X.
           05 NOMBRE     PIC X(30).
                05 APEPAT     PIC X(30).
                05 APEMAT     PIC X(30).
                05 FECNAC     PIC 9(08).
           05 R-FECNAC REDEFINES FECNAC.
              10 DIA-NAC PIC 9(02).
              10 MES-NAC PIC 9(02).
              10 ANO-NAC PIC 9(04).
                05 GENERO     PIC X.
           05 DIRECCION  PIC X(50).
                05 CIUDAD     PIC X(50).
                05 TELEFONO1  PIC X(15).
                05 TELEFONO2  PIC X(15).
                05 EMAIL      PIC X(100).
                05 FECCRE     PIC 9(08).
           05 SALIR      PIC X.
           05 MSG-ERROR  PIC X(70).

       01 PARAMETROS-VALIDA-FECHA.
           05 PAR-INP-FECHA.
               10 FECHA-X     PIC X(08).
               10 FORMATO-X   PIC X VALUE "2".
           05 PAR-OUT-FECHA.
               10 FECHA-VALIDA PIC X.

       01 PARAMETROS-LARGO-STRING.
           05 STRING-CALCULAR   PIC X(500).
           05 STRING-LARGO      PIC 9(03).

       01 VARIABLES-WS-PANTALLA.

           05 WS-RUT        PIC 9(10).
           05 WS-DV         PIC X.
           05 WS-NOMBRE     PIC X(30).
           05 WS-APEPAT     PIC X(30).
           05 WS-APEMAT     PIC X(30).
           05 WS-FECNAC     PIC 9(08).
           05 WS-R-FECNAC REDEFINES WS-FECNAC.
              10 WS-DIA-NAC PIC 9(02).
              10 WS-MES-NAC PIC 9(02).
              10 WS-ANO-NAC PIC 9(04).
           05 WS-GENERO     PIC X.
           05 WS-DIRECCION  PIC X(50).
           05 WS-CIUDAD     PIC X(50).
           05 WS-TELEFONO1  PIC X(15).
           05 WS-TELEFONO2  PIC X(15).
           05 WS-EMAIL      PIC X(100).
           05 WS-FECCRE     PIC 9(08).



       01 PAR-INP.
           10 INP-RUT    PIC 9(10).
           10 INP-DV     PIC X.
       01 PAR-OUT.
           10 OUT-CODRET PIC 9.

       SCREEN SECTION.
       01  PANTALLA-ENTRADA.
         03  BLANK SCREEN.
         03  LINE 1 COL 30  VALUE "Mantenedor Clientes".
      *  03  LINE 1 COL 70 PIC X(10) USING FECHA-SYS DISPLAY.
         03  LINE 5 COL 5  VALUE "RUT...........: ".
         03  LINE 5 COL 22 PIC ZZZZZZZZZ9 USING RUT .
         03  LINE 5 COL 33  VALUE "-".
         03  LINE 5 COL 35 PIC X(1) USING DV.
         03  LINE 6 COL 5  VALUE "NOMBRE........: ".
         03  LINE 6 COL 22 PIC X(30) USING NOMBRE.
         03  LINE 7 COL 5  VALUE "APE. PATERNO..: ".
         03  LINE 7 COL 22 PIC X(30) USING APEPAT.
         03  LINE 8 COL 5  VALUE "APE. MATERNO..: ".
         03  LINE 8 COL 22 PIC X(30) USING APEMAT.
         03  LINE 9 COL 5  VALUE "FEC NACIMIENTO: ".
         03  LINE 9 COL 22 PIC 9(02) USING DIA-NAC.
         03  LINE 9 COL 24 VALUE "/".
         03  LINE 9 COL 25 PIC 9(02) USING MES-NAC.
         03  LINE 9 COL 27 VALUE "/".
         03  LINE 9 COL 28 PIC 9(04) USING ANO-NAC.
         03  LINE 10 COL 5 VALUE "GENERO........: ".
         03  LINE 10 COL 22 PIC X USING GENERO.
         03  LINE 10 COL 25 VALUE "H=HOMBRE; M:MUJER".
         03  LINE 11 COL 5 VALUE "DIRECCION.....: ".
         03  LINE 11 COL 22 PIC X(50) USING DIRECCION.
         03  LINE 12 COL 5 VALUE "CIUDAD........: ".
         03  LINE 12 COL 22 PIC X(50) USING CIUDAD.
         03  LINE 13 COL 5 VALUE "TELEFONOS.....: ".
         03  LINE 13 COL 22 PIC X(15) USING TELEFONO1.
         03  LINE 13 COL 38 PIC X(15) USING TELEFONO2.
         03  LINE 14 COL 5 VALUE "EMAIL.........: ".
         03  LINE 14 COL 22 PIC X(60) USING EMAIL.
         03  LINE 22 COL 1 VALUE "----------------------------------".
         03  LINE 22 COL 35 VALUE "----------------------------------".
         03  LINE 22 COL 69 VALUE "-------------".
         03  LINE 23 COL 5 VALUE "Intro-Validar".
         03  LINE 23 COL 30 VALUE "SALIR (S/N) : ".
         03  LINE 23 COL 44 PIC X USING SALIR.
      *  03  LINE 24 COL 1 PIC X(70) USING MSG-ERROR .


       PROCEDURE DIVISION.

           ACCEPT FECHA-SYS FROM DATE
           DISPLAY FECHA-SYS AT LINE 1 COLUMN 70
           MOVE "N"   TO SALIR
           PERFORM UNTIL SALIR = 'S' OR 's'
             DISPLAY PANTALLA-ENTRADA
             ACCEPT PANTALLA-ENTRADA
             MOVE SPACES   TO MSG-ERROR
             IF SALIR = "N" THEN
                MOVE VARIABLES-PEDIR-PANTALLA  TO
                     VARIABLES-WS-PANTALLA

                PERFORM VALIDAR-DATOS
                IF MSG-ERROR = SPACES THEN
                   PERFORM PEDIR-GRABAR
                END-IF

             END-IF
           END-PERFORM
           STOP RUN.

       VALIDAR-DATOS.
           MOVE SPACES   TO MSG-ERROR
           PERFORM VALIDA-RUT
           PERFORM VALIDA-NOMBRE-APELLIDOS
           PERFORM VALIDA-FECHA-NACIMIENTO
           PERFORM VALIDA-GENERO
           PERFORM VALIDA-DIRECCION
           PERFORM VALIDA-CIUDAD
           PERFORM VALIDA-TELEFONOS
           PERFORM VALIDA-EMAIL

           IF MSG-ERROR NOT = SPACES THEN
              CALL "VentanaMsj" USING MSG-ERROR
           END-IF
           .
       VALIDA-RUT.
           IF WS-RUT = 0 OR WS-DV = SPACES THEN
               MOVE "RUT INCORRECTO" TO MSG-ERROR
           ELSE
               MOVE WS-RUT        TO INP-RUT
               MOVE WS-DV         TO INP-DV
               MOVE 0          TO OUT-CODRET
               CALL "ValidaRut" USING PAR-INP PAR-OUT
               IF OUT-CODRET = 1 THEN
                  MOVE "RUT INCORRECTO" TO MSG-ERROR
               END-IF
           END-IF
           .
       VALIDA-NOMBRE-APELLIDOS.
           IF MSG-ERROR = SPACES
               IF WS-NOMBRE = SPACES OR
                  WS-APEPAT = SPACES OR
                  WS-APEMAT = SPACES THEN
                  MOVE "NOMBRE O APELLIDOS INCORRECTOS"
                                        TO MSG-ERROR
               END-IF
           END-IF
           .

       VALIDA-FECHA-NACIMIENTO.
           IF MSG-ERROR = SPACES
              MOVE WS-FECNAC     TO FECHA-X
              MOVE " "           TO FECHA-VALIDA
              CALL "ValidarFecha" using PAR-INP-FECHA PAR-OUT-FECHA

              IF FECHA-VALIDA = "N" THEN
                  MOVE "FECHA NACIMIENTO INCORRECTA"
                                        TO MSG-ERROR
              END-IF
           END-IF
           .

       VALIDA-GENERO.
           IF MSG-ERROR = SPACES
              IF WS-GENERO = SPACES OR
                 (WS-GENERO NOT = "H" AND WS-GENERO NOT = "M") THEN

                  MOVE "Género debe ser H ó M"
                                        TO MSG-ERROR
              END-IF
           END-IF
           .

       VALIDA-DIRECCION.
           IF MSG-ERROR = SPACES

              MOVE WS-DIRECCION   TO STRING-CALCULAR
              PERFORM CALCULAR-LARGO-STRING
              IF STRING-LARGO < 5 THEN
                 MOVE "DEBE INGRESAR UNA DIRECCION MINIMO 5 CARACTERES"
                                        TO MSG-ERROR
              END-IF
           END-IF
           .

       VALIDA-CIUDAD.
           IF MSG-ERROR = SPACES
               MOVE WS-CIUDAD     TO STRING-CALCULAR
               PERFORM CALCULAR-LARGO-STRING
              IF STRING-LARGO < 5 THEN
                 MOVE "DEBE INGRESAR UNA CIUDAD MINIMO 5 CARACTERES"
                                        TO MSG-ERROR
              END-IF
           END-IF
           .

       VALIDA-TELEFONOS.
           IF MSG-ERROR = SPACES
              IF WS-TELEFONO1 = SPACES AND
                 WS-TELEFONO2 = SPACES
                 MOVE "DEBE INGRESAR AL MENOS 1 TELEFONO"
                                        TO MSG-ERROR
              END-IF
           END-IF
           .

       VALIDA-EMAIL.
           IF MSG-ERROR = SPACES
              IF WS-EMAIL = SPACES
                 MOVE "DEBE INGRESAR EMAIL (1)"
                                        TO MSG-ERROR
              ELSE
                  INSPECT WS-EMAIL TALLYING CUENTA-1 FOR ALL "@"

                  IF CUENTA-1 = 1 THEN
                      UNSTRING WS-EMAIL DELIMITED BY "@"
                      INTO     WS-ANTES-DE-ARROBA,
                               WS-DESPUES-DE-ARROBA
                      IF WS-ANTES-DE-ARROBA(1:3) NOT = SPACES THEN
                         INSPECT WS-DESPUES-DE-ARROBA TALLYING
                         CUENTA-3 FOR ALL "."
                         IF CUENTA-3 = 0 THEN
                            MOVE
                       "DEBE INGRESAR EMAIL CON FORMATO CORRECTO (2)"
                                            TO MSG-ERROR
                         ELSE
                             IF WS-DESPUES-DE-ARROBA(1:3) = SPACES THEN
                                MOVE
                          "DEBE INGRESAR EMAIL CON FORMATO CORRECTO (3)"
                                TO MSG-ERROR
                             END-IF
                         END-IF
                      ELSE
                    MOVE "DEBE INGRESAR EMAIL CON FORMATO CORRECTO (4)"
                                            TO MSG-ERROR
                      END-IF

                  ELSE
                     MOVE "DEBE INGRESAR EMAIL CON FORMATO CORRECTO (5)"
                                            TO MSG-ERROR
                  END-IF

              END-IF
           END-IF
           .

       CALCULAR-LARGO-STRING.

           MOVE 0             TO STRING-LARGO
           CALL "LargoString" USING STRING-CALCULAR STRING-LARGO
           .

       PEDIR-GRABAR.

           MOVE " "   TO WS-GRABAR
           PERFORM UNTIL WS-GRABAR = "S" OR "N"
              DISPLAY "Grabar Cliente S/N" AT LINE 23 COLUMN 50
              ACCEPT WS-GRABAR             AT LINE 23 COLUMN 70

              IF WS-GRABAR = "S" OR "s" THEN
                  MOVE 0               TO OUT-CODRET-GRABAR
                  MOVE SPACES          TO OUT-DESRET-GRABAR
                  CALL "GrabarCliente" USING VARIABLES-WS-PANTALLA
                                             PAR-OUT-GRABAR

                  MOVE OUT-DESRET-GRABAR TO MSG-ERROR
                  CALL "VentanaErr" USING MSG-ERROR

                  IF OUT-CODRET-GRABAR = 0 THEN
                     MOVE SPACES            TO MSG-ERROR
                     MOVE "S"               TO SALIR
                  END-IF
              END-IF
           END-PERFORM
           .

       END PROGRAM Mantenedor.
