      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ConsultaCliente.

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
           copy "C:\PgmCobols\FD\FDCliente.cpy".

       WORKING-STORAGE SECTION.
       01  fin-de-archivo      PIC X VALUE 'N'.
       01  opcion              PIC X(1).
       01  continuar           PIC X VALUE 'Y'.
       01  usuario-clave       PIC X(10).
       01  done                PIC X VALUE 'N'.
       01  FS-USUARIOS         PIC XX.

       01 VARIABLE-DE-TRABAJO.
           05 SALIR      PIC X.

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

       LINKAGE SECTION.
       01 PAR-INPUT.
           05 RUT-CLIENTE   PIC 9(10).

       SCREEN SECTION.
       01  PANTALLA-ENTRADA.
         03  BLANK SCREEN.
         03  LINE 1 COL 30  VALUE "Consulta Cliente".
         03  LINE 5 COL 5  VALUE "RUT...........: ".
         03  LINE 5 COL 22 PIC ZZZZZZZZZ9 FROM RUT .
         03  LINE 5 COL 33  VALUE "-".
         03  LINE 5 COL 35 PIC X(1) FROM DV.
         03  LINE 6 COL 5  VALUE "NOMBRE........: ".
         03  LINE 6 COL 22 PIC X(30) FROM NOMBRE.
         03  LINE 7 COL 5  VALUE "APE. PATERNO..: ".
         03  LINE 7 COL 22 PIC X(30) FROM APEPAT.
         03  LINE 8 COL 5  VALUE "APE. MATERNO..: ".
         03  LINE 8 COL 22 PIC X(30) FROM APEMAT.
         03  LINE 9 COL 5  VALUE "FEC NACIMIENTO: ".
         03  LINE 9 COL 22 PIC 9(02) FROM DIA-NAC.
         03  LINE 9 COL 24 VALUE "/".
         03  LINE 9 COL 25 PIC 9(02) FROM MES-NAC.
         03  LINE 9 COL 27 VALUE "/".
         03  LINE 9 COL 28 PIC 9(04) FROM ANO-NAC.
         03  LINE 10 COL 5 VALUE "GENERO........: ".
         03  LINE 10 COL 22 PIC X FROM GENERO.
         03  LINE 10 COL 25 VALUE "H=HOMBRE; M:MUJER".
         03  LINE 11 COL 5 VALUE "DIRECCION.....: ".
         03  LINE 11 COL 22 PIC X(50) FROM DIRECCION.
         03  LINE 12 COL 5 VALUE "CIUDAD........: ".
         03  LINE 12 COL 22 PIC X(50) FROM CIUDAD.
         03  LINE 13 COL 5 VALUE "TELEFONOS.....: ".
         03  LINE 13 COL 22 PIC X(15) FROM TELEFONO1.
         03  LINE 13 COL 38 PIC X(15) FROM TELEFONO2.
         03  LINE 14 COL 5 VALUE "EMAIL.........: ".
         03  LINE 14 COL 22 PIC X(60) FROM EMAIL.
         03  LINE 22 COL 1 VALUE "----------------------------------".
         03  LINE 22 COL 35 VALUE "----------------------------------".
         03  LINE 22 COL 69 VALUE "-------------".
         03  LINE 23 COL 5 VALUE "SALIR (S/N) : ".
         03  LINE 23 COL 19 PIC X USING SALIR.

       PROCEDURE DIVISION USING PAR-INPUT.

           OPEN INPUT datos-usuarios
           MOVE RUT-CLIENTE   TO usuario-rut
           READ datos-usuarios END-READ
           IF FS-USUARIOS = "00" THEN
               MOVE usuario-registro TO VARIABLES-PEDIR-PANTALLA
               MOVE "N"   TO SALIR
               PERFORM UNTIL SALIR = 'S' OR 's'

                 DISPLAY PANTALLA-ENTRADA
                 ACCEPT PANTALLA-ENTRADA

               END-PERFORM
           END-IF
           CLOSE datos-usuarios
           GOBACK.

       END PROGRAM ConsultaCliente.
