      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 16-07-2025
      * Purpose: ENSELANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Str2JSON.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 VARIABLES-DE-TRABAJO.
           05 CONT-SEP-ARR-I    PIC 9(05).
           05 CONT-SEP-ARR-F    PIC 9(05).
           05 CONT-SEP-ELE-I    PIC 9(05).
           05 CONT-SEP-ELE-F    PIC 9(05).
           05 CONT-SEP-PYC      PIC 9(05).

           05 POS-I-CAMPO       PIC 9(05).
           05 POS-I-ARR         PIC 9(05).
           05 POS-I-ELE         PIC 9(05).
           05 POS-F-ARR         PIC 9(05).
           05 POS-F-ELE         PIC 9(05).

           05 REC-STRING        PIC 9(05).
           05 PFIN              PIC 9(05).
           05 WS-STRING-TRABAJO PIC X(500).
           05 WS-STRING-CAMPO   PIC X(500).
           05 WS-STRING-VALOR   PIC X(500).
           05 WS-STRING-ARMADO  PIC X(500).
           05 VALOR             PIC X.
           05 LARGO-ARR         PIC 9(05).
           05 TIT-ARR           PIC X(50).
           05 I-ELEM            PIC 9(05).

       01 INP-POSCARSTR.
           05 POSCARSTR-STRING-BUSCAR    PIC X(10000).
           05 POSCARSTR-CARACTER-BUSCAR  PIC X.
       01 OUT-POSCARSTR.
           05 POSCARSTR-POSICION         PIC 9(05).

       01 INPUT-LARGOSTR10000.
           05 STRING-CALCULAR   PIC X(10000).
       01 OUTPUT-LARGOSTR10000.
           05 LK-LARGO-STR     PIC 9(05).

       01 INPUT-LARGOSTR500.
           05 STRING-CALCULAR500   PIC X(500).
       01 OUTPUT-LARGOSTR500.
           05 LK-LARGO-STR500      PIC 9(03).

      *LINKAGE SECTION.
      *ESTRUCTURA SIMPLE
      *    RUT=1;NOMBRE=NOMBRE RUT 1;DIRECCION=DIR RUT 1
      *ESTRUCTURA CON ARREGLO
      *    RUT=1;NOMBRE=NOMBRE RUT 1;DIRECCION=DIR RUT 1;
      *    [CuentasCorrientes {Banco=BBVA;Cuenta=45454545454554},
      *                       {Banco=ITAU;Cuenta=2225222524}
      *     ]; CIUDAD=SANTIAGO; REGION=METROPOLITANA
       01 INP-STR2JSON.
           05 STR2JSON-CONVERTIR        PIC X(10000).
           05 STR2JSON-SEP-CAMPO        PIC X(1) VALUE ";".
           05 STR2JSON-SEP-VALOR        PIC X(1) VALUE "=".
           05 STR2JSON-SEP-INI-ELEMENTO PIC X(1) VALUE "{".
           05 STR2JSON-SEP-FIN-ELEMENTO PIC X(1) VALUE "}".
           05 STR2JSON-SEP-ELEMENTO     PIC X(1) VALUE ",".
           05 STR2JSON-SEP-INI-ARRAY    PIC X(1) VALUE "[".
           05 STR2JSON-SEP-FIN-ARRAY    PIC X(1) VALUE "]".
       01 OUT-STR2JSON.
           05 STR2JSON-CODRET           PIC X.
           05 STR2JSON-CONVERTIDO       PIC X(20000).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE ZEROES            TO CONT-SEP-ARR-I
                                     CONT-SEP-ARR-F
                                     CONT-SEP-ELE-I
                                     CONT-SEP-ELE-F
           MOVE "RUT=1;NOMBRE=NOMBRE RUT 1;DIRECCION=DIR RUT 1"
                TO STR2JSON-CONVERTIR

           INSPECT STR2JSON-CONVERTIR TALLYING CONT-SEP-ARR-I
                            FOR ALL STR2JSON-SEP-INI-ARRAY

           INSPECT STR2JSON-CONVERTIR TALLYING CONT-SEP-ARR-F
                            FOR ALL STR2JSON-SEP-FIN-ARRAY

           INSPECT STR2JSON-CONVERTIR TALLYING CONT-SEP-ELE-I
                            FOR ALL STR2JSON-SEP-INI-ELEMENTO

           INSPECT STR2JSON-CONVERTIR TALLYING CONT-SEP-ELE-F
                            FOR ALL STR2JSON-SEP-FIN-ELEMENTO

      *    VERIFICAMOS CON LOS INICIOS Y CIERRES DE ARRAY Y ELEMENTOS
      *    CONCUERDEN.


           IF CONT-SEP-ARR-I = CONT-SEP-ARR-F AND
              CONT-SEP-ELE-I = CONT-SEP-ELE-F THEN
               MOVE SPACES             TO STR2JSON-CONVERTIDO
               MOVE "{"                TO STR2JSON-CONVERTIDO(1:1)


               MOVE STR2JSON-CONVERTIR TO STRING-CALCULAR
               MOVE ZEROES             TO LK-LARGO-STR
               CALL "LargoString10000" USING INPUT-LARGOSTR10000
                                             OUTPUT-LARGOSTR10000

               MOVE 2  TO PFIN
               MOVE 1  TO REC-STRING

               PERFORM PROCESAR-STRING UNTIL REC-STRING > LK-LARGO-STR

               MOVE "}"                TO STR2JSON-CONVERTIDO(PFIN:1)
           END-IF

           GOBACK.

      *
       PROCESAR-STRING.
      *----------------
           MOVE STR2JSON-CONVERTIR(REC-STRING:)
                TO POSCARSTR-STRING-BUSCAR

           MOVE STR2JSON-SEP-CAMPO   TO POSCARSTR-CARACTER-BUSCAR
           CALL "PosCarString" USING INP-POSCARSTR
                                     OUT-POSCARSTR

           MOVE POSCARSTR-POSICION   TO POS-I-CAMPO

           MOVE STR2JSON-SEP-INI-ARRAY TO POSCARSTR-CARACTER-BUSCAR
           CALL "PosCarString" USING INP-POSCARSTR
                                     OUT-POSCARSTR

           MOVE POSCARSTR-POSICION   TO POS-I-ARR

           MOVE STR2JSON-SEP-INI-ELEMENTO TO POSCARSTR-CARACTER-BUSCAR
           CALL "PosCarString" USING INP-POSCARSTR
                                     OUT-POSCARSTR

           MOVE POSCARSTR-POSICION   TO POS-I-ELE

           IF POS-I-ARR > 0 AND POS-I-ELE > 0 THEN
              IF POS-I-CAMPO < POS-I-ARR AND
                 POS-I-CAMPO < POS-I-ELE THEN
                 PERFORM PROCESAR-CAMPO
                 ADD POS-I-CAMPO  TO REC-STRING
              ELSE
                 PERFORM PROCESAR-ARREGLO
              END-IF
           ELSE
              PERFORM PROCESAR-CAMPO
              ADD POS-I-CAMPO  TO REC-STRING
           END-IF

           .

       PROCESAR-CAMPO.
           IF POS-I-CAMPO NOT = 0 THEN
             MOVE STR2JSON-CONVERTIR(REC-STRING:POS-I-CAMPO - 1)
             TO WS-STRING-TRABAJO
           ELSE
             MOVE STR2JSON-CONVERTIR(REC-STRING:)
             TO WS-STRING-TRABAJO

             MOVE WS-STRING-TRABAJO TO STRING-CALCULAR500
             MOVE ZEROES            TO LK-LARGO-STR500
             CALL "LargoString" USING INPUT-LARGOSTR500
                                      OUTPUT-LARGOSTR500

             ADD LK-LARGO-STR500    TO POS-I-CAMPO
           END-IF

           UNSTRING WS-STRING-TRABAJO DELIMITED BY STR2JSON-SEP-VALOR
                    INTO WS-STRING-CAMPO
                         WS-STRING-VALOR

           MOVE WS-STRING-VALOR   TO STRING-CALCULAR500
           MOVE ZEROES            TO LK-LARGO-STR500
           CALL "LargoString" USING INPUT-LARGOSTR500
                                    OUTPUT-LARGOSTR500


           MOVE SPACES       TO WS-STRING-ARMADO
           STRING '"'             DELIMITED SIZE
                  WS-STRING-CAMPO DELIMITED BY " "
                  '" : "'         DELIMITED SIZE
                  WS-STRING-VALOR(1:LK-LARGO-STR500) DELIMITED SIZE
                  '"'             DELIMITED SIZE
                               INTO WS-STRING-ARMADO

           MOVE WS-STRING-ARMADO  TO STRING-CALCULAR500
           MOVE ZEROES            TO LK-LARGO-STR500
           CALL "LargoString" USING INPUT-LARGOSTR500
                                    OUTPUT-LARGOSTR500

      * RUT=1;NOMBRE=NOMBRE RUT 1;DIRECCION=DIR RUT 1

           MOVE WS-STRING-ARMADO(1:LK-LARGO-STR500)  TO
                STR2JSON-CONVERTIDO(PFIN:LK-LARGO-STR500)
           ADD LK-LARGO-STR500    TO PFIN

           IF (REC-STRING + POS-I-CAMPO) < LK-LARGO-STR THEN
               MOVE ", " TO STR2JSON-CONVERTIDO(PFIN:2)
               ADD 2     TO PFIN
           END-IF
           .

       PROCESAR-ARREGLO.

      *    BUSCAMOS DONDE TERMINA EL ARREGLO
           MOVE STR2JSON-SEP-FIN-ARRAY   TO POSCARSTR-CARACTER-BUSCAR
           CALL "PosCarString" USING INP-POSCARSTR
                                     OUT-POSCARSTR

           MOVE POSCARSTR-POSICION   TO POS-F-ARR

      *     SACAMOS LA LONGITUD DEL ARREGLO
           COMPUTE LARGO-ARR = POS-F-ARR - POS-I-ARR

      *    EXTRAEMOS LA PORCION DEL ARREGLO A TRABAJAR
           MOVE STR2JSON-CONVERTIR(POS-I-ARR + 1:( LARGO-ARR - 1 ))
             TO WS-STRING-TRABAJO

      *    RESCATAMOS EL NOMBRE DEL ARRELO QUE DEBE ESTAR ENTRE [ {
           MOVE SPACES                TO TIT-ARR
           UNSTRING WS-STRING-TRABAJO
                    DELIMITED BY STR2JSON-SEP-INI-ELEMENTO
                         INTO    TIT-ARR

           MOVE TIT-ARR           TO STRING-CALCULAR500
           MOVE ZEROES            TO LK-LARGO-STR500
           CALL "LargoString" USING INPUT-LARGOSTR500
                                    OUTPUT-LARGOSTR500
           IF LK-LARGO-STR500 > 0 THEN
               MOVE ' "' TO STR2JSON-CONVERTIDO(PFIN:2)
               ADD 2      TO PFIN
               MOVE TIT-ARR(1:LK-LARGO-STR500) TO
                    STR2JSON-CONVERTIDO(PFIN:LK-LARGO-STR500)
               ADD LK-LARGO-STR500    TO PFIN
               MOVE '" : ['            TO
                    STR2JSON-CONVERTIDO(PFIN:5)
               ADD 5                  TO PFIN
           ELSE
               MOVE '[  ' TO STR2JSON-CONVERTIDO(PFIN:3)
               ADD 3      TO PFIN
           END-IF

           INSPECT WS-STRING-TRABAJO TALLYING CONT-SEP-ELE-I
                            FOR ALL STR2JSON-SEP-INI-ELEMENTO

           INSPECT WS-STRING-TRABAJO TALLYING CONT-SEP-ELE-F
                            FOR ALL STR2JSON-SEP-FIN-ELEMENTO

      *    VERIFICAMOS QUE LOS ELEMENTOS ESTEN CUADRADOS
           IF CONT-SEP-ELE-I = CONT-SEP-ELE-F THEN
              PERFORM PROCESA-ELEMENTO VARYING I-ELEM FROM 1
                      BY 1 UNTIL I-ELEM > CONT-SEP-ELE-I
           END-IF

           MOVE ' ]' TO STR2JSON-CONVERTIDO(PFIN:2)
           ADD 2     TO PFIN
           .

      *
       PROCESA-ELEMENTO.
      *-----------------
           MOVE WS-STRING-TRABAJO
                TO POSCARSTR-STRING-BUSCAR

           MOVE STR2JSON-SEP-CAMPO   TO POSCARSTR-CARACTER-BUSCAR
           CALL "PosCarString" USING INP-POSCARSTR
                                     OUT-POSCARSTR

           MOVE POSCARSTR-POSICION   TO POS-I-CAMPO


           IF POS-I-ARR > 0 AND POS-I-ELE > 0 THEN
              IF POS-I-CAMPO < POS-I-ARR AND
                 POS-I-CAMPO < POS-I-ELE THEN
                 PERFORM PROCESAR-CAMPO
                 ADD POS-I-CAMPO  TO REC-STRING
              ELSE
                 PERFORM PROCESAR-ARREGLO
              END-IF
           ELSE
              PERFORM PROCESAR-CAMPO
              ADD POS-I-CAMPO  TO REC-STRING
           END-IF


           .



       END PROGRAM Str2JSON.
