       IDENTIFICATION DIVISION.
       PROGRAM-ID. LeerArchivo.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT archivo-entrada ASSIGN TO
           "C:\PgmCobols\Data\cargar.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  archivo-entrada.
       01  registro-entrada      PIC X(255).

       WORKING-STORAGE SECTION.
       01  RUT                   PIC 9(10).
       01  DV                    PIC X(1).
       01  NOMBRE                PIC X(30).
       01  APEPAT                PIC X(30).
       01  APEMAT                PIC X(30).
       01  TELEFONO              PIC X(15).
       01  EMAIL                 PIC X(100).
       01  GENERO                PIC X(01).
       01  FECNAC                PIC X(10).
       01  NACIONALIDAD          PIC X(30).
       01  DIRECCION             PIC X(100).
       01  CIUDAD                PIC X(50).

       01  fin-lectura           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT archivo-entrada
           PERFORM UNTIL fin-lectura = 'S'
               READ archivo-entrada INTO registro-entrada
                   AT END
                       MOVE 'S' TO fin-lectura
                   NOT AT END
                       PERFORM separar-campos
               END-READ
           END-PERFORM
           CLOSE archivo-entrada
           DISPLAY "Fin de la lectura."
           STOP RUN.

       SEPARAR-CAMPOS.
           MOVE registro-entrada TO RUT
           UNSTRING registro-entrada DELIMITED BY ";"
               INTO RUT, DV, NOMBRE, APEPAT, APEMAT,
                    TELEFONO, EMAIL, GENERO, FECNAC,
                    NACIONALIDAD, DIRECCION, CIUDAD

           END-UNSTRING
           DISPLAY "RUT: " RUT
           DISPLAY "DV: " DV
           DISPLAY "Nombre: " NOMBRE
           DISPLAY "Apellido Paterno: " APEPAT
           DISPLAY "Apellido Materno: " APEMAT
           DISPLAY "Telefono: " TELEFONO
           DISPLAY "Email: " EMAIL
           DISPLAY "Genero: " GENERO
           DISPLAY "Fecha de Nacimiento: " FECNAC
           DISPLAY "Nacionalidad: " NACIONALIDAD
           DISPLAY "Direccion: " DIRECCION
           DISPLAY "Ciudad: " CIUDAD
           DISPLAY "-------------------------".
