      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BorrarCliente.

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

       LINKAGE SECTION.
       01 PAR-INPUT.
           05 RUT-CLIENTE   PIC 9(10).
       01 PAR-OUTPUT.
           05 LK-BORRADO    PIC X.
      *       S = Se Borro Cliente
      *       N = No Borrado
           05 LK-STATUS     PIC X(02).


       PROCEDURE DIVISION USING PAR-INPUT
                                PAR-OUTPUT.

           OPEN I-O datos-usuarios
           MOVE RUT-CLIENTE   TO usuario-rut
           MOVE "N"      TO LK-BORRADO

           READ datos-usuarios END-READ
           IF FS-USUARIOS = "00" THEN
              DELETE datos-usuarios END-DELETE
              IF FS-USUARIOS = "00" THEN
                  MOVE "S"      TO LK-BORRADO
              END-IF
           END-IF

           MOVE FS-USUARIOS  TO LK-STATUS

           CLOSE datos-usuarios
           GOBACK.

       END PROGRAM BorrarCliente.
