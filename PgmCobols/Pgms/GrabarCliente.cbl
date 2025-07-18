      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GrabarCliente.

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
       01  usuario-clave       PIC X(10).
       01  FS-USUARIOS         PIC XX.


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
           05 WS-SALIR      PIC X.

       LINKAGE SECTION.
       01 PAR-ENTRADA.
           05  lk-rut        PIC 9(10).
           05  lk-dv         PIC X.
           05  lk-nombre     PIC X(30).
           05  lk-apepat     PIC X(30).
           05  lk-apemat     PIC X(30).
           05  lk-fecnac     PIC 9(08).
           05  lk-genero     PIC X.
           05  lk-direccion  PIC X(50).
           05  lk-ciudad     PIC X(50).
           05  lk-telefono1  PIC X(15).
           05  lk-telefono2  PIC X(15).
           05  lk-email      PIC X(100).
           05  lk-feccre     PIC 9(08).
       01 PAR-SALIDA.
           05  OUT-CODRET   PIC 9(04).
           05  OUT-DESRET   PIC X(50).


       PROCEDURE DIVISION USING PAR-ENTRADA
                                PAR-SALIDA.

           OPEN I-O datos-usuarios

           MOVE PAR-ENTRADA   TO usuario-registro

           READ datos-usuarios END-READ
           IF FS-USUARIOS = "00"
              MOVE 1    TO OUT-CODRET
              MOVE "USUARIO YA EXISTE" TO OUT-DESRET
           ELSE
               WRITE usuario-registro END-WRITE
               IF FS-USUARIOS = "00"
                  MOVE 0                    TO OUT-CODRET
                  MOVE "REGISTRO INSERTADO" TO OUT-DESRET
               ELSE
                  MOVE 2      TO OUT-CODRET
                  MOVE SPACES TO OUT-DESRET
                  STRING "ERROR AL INSERTAR (" DELIMITED SIZE
                         FS-USUARIOS           DELIMITED SIZE
                         ")"                   DELIMITED SIZE
                                           INTO OUT-DESRET

               END-IF
           END-IF
           CLOSE datos-usuarios

           GOBACK
           .
       END PROGRAM GrabarCliente.
