       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. MANCLICBL.
       AUTHOR. JORGE DUARTE U.
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PANTALLA   ASSIGN TO WORKSTATION-MANT01-SI
                             ORGANIZATION IS TRANSACTION
                             ACCESS MODE  IS DYNAMIC
                             RELATIVE KEY IS NREL
                             FILE STATUS  IS FS-PAN.

       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD PANTALLA  LABEL RECORD IS OMITTED.
       01 R-PANTALLA             PIC X(1920).

       WORKING-STORAGE SECTION.
      *------------------------*
       77 I-ON                PIC 1 VALUE B"1".
       77 I-OFF               PIC 1 VALUE B"0".
       77 I-MSG               PIC 9(2) VALUE ZEROES.

       01 AREA-CONTROL.
             06 MDTO                PIC X(02).
                88 F3               VALUE "03".
                88 F7               VALUE "07".

       01 VARIABLES-DE-TRABAJO.
           05 WS-EMAIL      PIC X(100).
           05 CUENTA-1         PIC 9(03).
           05 CUENTA-2         PIC 9(03).
           05 CUENTA-3         PIC 9(03).
           05 WS-ANTES-DE-ARROBA   PIC X(100).
           05 WS-DESPUES-DE-ARROBA PIC X(100).

       01 VARIABLES-DE-STATUS.
          05 FS-PAN         PIC XX VALUE "00".

       01 VARIABLES-RELATIVAS.
          05 NREL           PIC 9(03).

       01 R01-II.
      *----------
           COPY DDS-R01-I        OF MANT01.

       01 R01-INDIC.
      *--------------
           COPY DDS-R01-INDIC    OF MANT01.

       01 INP-MSG.
          05 TIT-MSG           PIC X(30).
          05 ARR-MSG OCCURS 50 TIMES.
             10 MSG            PIC X(50).

       01 VALRUT-ENTRADA.
           05 VAL-RUT   PIC 9(10).
           05 VAL-DV    PIC X.
       01 VALRUT-SALIDA.
           05 VAL-CODRET PIC 9.
      *       0 = OK
      *       1 = NO OK

       01 VALFEC-ENTRADA.
           05 FECHA-YYYYMMDD PIC X(08).
           05 FECHA-FORMATO  PIC X.
      *       1 : YYYYMMDD
      *       2 : DDMMYYYY

       01 VALFEC-SALIDA.
           05 FECHA-VALIDA   PIC X.
      *       S : FECHA ES VALIDATE
      *       N : FECHA ES INVALIDA

       01 PARAMETROS-LARGO-STRING.
           05 STRING-CALCULAR   PIC X(500).
           05 STRING-LARGO      PIC 9(03).

       01 INP-REGCLI.
           COPY  DDS-ALL-FORMAT  OF CLIENTES.
       01 OUT-REGCLI.
           05 ST-REGCLI   PIC X(02).
      *       '00' = OK
      *    <> '00' = NO OK

       PROCEDURE DIVISION.
      *--------------------
           OPEN I-O  PANTALLA
           MOVE I-OFF       TO IN03 OF R01-I-INDIC
           INITIALIZE R01-I REPLACING ALPHANUMERIC DATA BY SPACES
                                           NUMERIC DATA BY ZEROES
           PERFORM PROCESAR-PANTALLA UNTIL
                   IN03 OF R01-I-INDIC = I-ON

           CLOSE     PANTALLA

           GOBACK.

      *
       PROCESAR-PANTALLA.
      *------------------
           WRITE R-PANTALLA  FROM R01-II  FORMAT "R01"
                             INDICATORS ARE R01-O-INDIC  END-WRITE
           READ  PANTALLA    INTO R01-II  FORMAT "R01"
                             INDICATORS ARE R01-I-INDIC  END-READ

           MOVE ZEROES         TO I-MSG
           IF IN03 OF R01-I-INDIC = I-OFF AND
              IN07 OF R01-I-INDIC = I-OFF
              PERFORM VALIDA-CAMPOS
              MOVE I-OFF     TO IN03 OF R01-I-INDIC
           ELSE
              IF IN07 OF R01-I-INDIC = I-ON
                 PERFORM GRABA-REGISTRO
                 MOVE I-OFF    TO IN50 OF R01-O-INDIC
              END-IF
           END-IF
           IF I-MSG > 0 THEN
              MOVE "ERRORES"  TO TIT-MSG
              CALL "VERMSJ" USING INP-MSG
           END-IF
           .

       VALIDA-CAMPOS.
      *--------------
           PERFORM LIMPIA-ARR-MSG
           MOVE ZEROES         TO I-MSG

           PERFORM VALIDA-RUT
           PERFORM VALIDA-NOMBRE-APELLIDOS
           PERFORM VALIDA-FECHA-NACIMIENTO
           PERFORM VALIDA-GENERO
           PERFORM VALIDA-DIRECCION
           PERFORM VALIDA-CIUDAD
           PERFORM VALIDA-TELEFONOS
           PERFORM VALIDA-EMAIL
           IF I-MSG = 0 THEN
              MOVE I-ON   TO IN50 OF R01-O-INDIC
           END-IF

           .

       LIMPIA-ARR-MSG.

           PERFORM VARYING I-MSG FROM 1 BY 1 UNTIL I-MSG > 50
              MOVE SPACES TO MSG(I-MSG)
           END-PERFORM
           .

       VALIDA-RUT.
      *------------
           IF RUT OF R01-II = ZEROES        OR
              DV  OF R01-II = SPACES       THEN
               ADD  1                TO I-MSG
               MOVE "RUT INCORRECTO" TO MSG(I-MSG)
           ELSE
               MOVE RUT OF R01-II TO VAL-RUT
               MOVE DV  OF R01-II TO VAL-DV
               MOVE 0          TO VAL-CODRET
               CALL "VALRUT"    USING VALRUT-ENTRADA
                                      VALRUT-SALIDA
               IF VAL-CODRET = 1 THEN
                  ADD   1               TO I-MSG
                  MOVE "RUT INCORRECTO" TO MSG(I-MSG)
               END-IF
           END-IF
           .


       VALIDA-NOMBRE-APELLIDOS.
      *------------------------
           IF NOMBRE OF R01-II = SPACES  OR
              APEPAT OF R01-II = SPACES  OR
              APEMAT OF R01-II = SPACES
              ADD  1                     TO I-MSG
              MOVE "NOMBRE O APELLIDOS INCORRECTOS"
                                        TO MSG(I-MSG)
           END-IF
           .

       VALIDA-FECHA-NACIMIENTO.
      *------------------------
           MOVE FECNAC OF R01-II TO FECHA-YYYYMMDD
           MOVE "2"              TO FECHA-FORMATO

           MOVE " "           TO FECHA-VALIDA
           CALL "VALFEC"       USING VALFEC-ENTRADA
                                     VALFEC-SALIDA

           IF FECHA-VALIDA = "N" THEN
              ADD  1                     TO I-MSG
              MOVE "FECHA NACIMIENTO INCORRECTA"
                                     TO MSG(I-MSG)
           END-IF
           .

       VALIDA-GENERO.
      *--------------
           IF GENERO OF R01-II = SPACES  OR
              (GENERO OF R01-II NOT = "H" AND "M" )         THEN

              ADD  1                    TO I-MSG
              MOVE "GÉNERO DEBE SER H Ó M"
                                        TO MSG(I-MSG)
           END-IF
           .

       VALIDA-DIRECCION.
      *------------------
           MOVE DIRECC OF R01-II TO STRING-CALCULAR
           PERFORM CALCULAR-LARGO-STRING
           IF STRING-LARGO < 5 THEN
              ADD  1                 TO I-MSG
              MOVE "DEBE INGRESAR UNA DIRECCION MINIMO 5 CARACTERES"
                                     TO MSG(I-MSG)
           END-IF
           .

       VALIDA-CIUDAD.
      *--------------
           MOVE CIUDAD OF R01-II TO STRING-CALCULAR
           PERFORM CALCULAR-LARGO-STRING
           IF STRING-LARGO < 5 THEN
              ADD  1                 TO I-MSG
              MOVE "DEBE INGRESAR UNA CIUDAD    MINIMO 5 CARACTERES"
                                     TO MSG(I-MSG)
           END-IF
           .
       VALIDA-TELEFONOS.
      *------------------
           IF TELEFONO1 OF R01-II = SPACES AND
              TELEFONO2 OF R01-II = SPACES
              ADD  1                 TO I-MSG
              MOVE "DEBE INGRESAR AL MENOS 1 TELEFONO"
                                     TO MSG(I-MSG)
           END-IF
           .

       VALIDA-EMAIL.
      *-------------
           IF EMAIL OF R01-II = SPACES THEN
              ADD  1                 TO I-MSG
              MOVE "DEBE INGRESAR EMAIL              "
                                     TO MSG(I-MSG)
           ELSE
            MOVE EMAIL OF R01-II     TO WS-EMAIL
            MOVE ZEROES              TO CUENTA-1
                                        CUENTA-2
                                        CUENTA-3
            INSPECT WS-EMAIL TALLYING CUENTA-1 FOR ALL "@"
            IF CUENTA-1 = 1 THEN
                 UNSTRING WS-EMAIL DELIMITED BY "@"
                 INTO     WS-ANTES-DE-ARROBA,
                          WS-DESPUES-DE-ARROBA
                 IF WS-ANTES-DE-ARROBA(1:3) NOT = SPACES THEN
                    INSPECT WS-DESPUES-DE-ARROBA TALLYING
                    CUENTA-3 FOR ALL "."
                    IF CUENTA-3 = 0 THEN
                       ADD 1         TO I-MSG
                       MOVE
                       "DEBE INGRESAR EMAIL CON FORMATO CORRECTO"
                                       TO MSG(I-MSG)
                    ELSE
                        IF WS-DESPUES-DE-ARROBA(1:3) = SPACES THEN
                           ADD 1     TO I-MSG
                           MOVE
                          "DEBE INGRESAR EMAIL CON FORMATO CORRECTO"
                           TO MSG(I-MSG)
                        END-IF
                    END-IF
                 ELSE
                   ADD 1     TO I-MSG
                   MOVE "DEBE INGRESAR EMAIL CON FORMATO CORRECTO    "
                                            TO MSG(I-MSG)
                 END-IF

             ELSE
                ADD 1     TO I-MSG
                MOVE "DEBE INGRESAR EMAIL CON FORMATO CORRECTO (5)"
                                            TO MSG(I-MSG)
              END-IF
           END-IF
           .
      *
       CALCULAR-LARGO-STRING.
      *-----------------------
           MOVE 0             TO STRING-LARGO
           CALL "LARGOSTR"    USING STRING-CALCULAR STRING-LARGO
           .

      *
       GRABA-REGISTRO.
      *---------------
             INITIALIZE CLIREG OF INP-REGCLI
                        REPLACING ALPHANUMERIC DATA BY SPACES
                                       NUMERIC DATA BY ZEROES
             MOVE CORR R01-I TO CLIREG OF INP-REGCLI
             CALL "GRABARCLI"  USING INP-REGCLI
                                     OUT-REGCLI
             IF ST-REGCLI = "00"
                ADD 1           TO I-MSG
                MOVE "REGISTRO AGREGADO" TO MSG(I-MSG)
             ELSE
                ADD 1           TO I-MSG
                STRING "REGISTRO NO INSERTADO " DELIMITED BY SIZE
                       " STATUS ("              DELIMITED BY SIZE
                       ST-REGCLI                DELIMITED BY SIZE
                       ") "                     DELIMITED BY SIZE
                                          INTO MSG(I-MSG)
             END-IF
             .

