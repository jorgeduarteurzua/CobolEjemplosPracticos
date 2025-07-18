      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 17-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. ValidarEmail.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 VARIABLES-DE-TRABAJO.
           05 CUENTA-1     PIC 9(03).
           05 CUENTA-2     PIC 9(03).
           05 WS-ANTES-DE-ARROBA   PIC X(150).
           05 WS-DESPUES-DE-ARROBA PIC X(150).
           05 I-CAR-VAL            PIC 9(03).
           05 I                    PIC 9(03).
           05 TOT-CAR-VAL          PIC 9(03).
           05 WS-CAR-VALIDOS PIC X(54) VALUE
           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@.".
           05 FS-CAR-V           PIC X.

       01 INPUT-LARGOSTR500.
           05 STRING-CALCULAR500   PIC X(500).
       01 OUTPUT-LARGOSTR500.
           05 LK-LARGO-STR500      PIC 9(03).

       LINKAGE SECTION.
       01 INP-VALIDA-EMAIL.
           05 WS-EMAIL     PIC X(150).
       01 OUT-VALIDA-EMAIL.
           05 MSG-ERROR   PIC X(50).

       PROCEDURE DIVISION USING INP-VALIDA-EMAIL
                                OUT-VALIDA-EMAIL.
       MAIN-PROCEDURE.
           MOVE "EMAIL CORRECTO "     TO MSG-ERROR

           MOVE ZEROES                TO CUENTA-1
                                         CUENTA-2

           IF WS-EMAIL = SPACES
              MOVE "DEBE INGRESAR EMAIL " TO MSG-ERROR
           ELSE
              PERFORM VALIDAR-CAR-VALIDOS
              IF FS-CAR-V = "N" THEN
                  MOVE "DEBE INGRESAR EMAIL CON CARACETERES VALIDOS "
                       TO MSG-ERROR

              ELSE
                  INSPECT WS-EMAIL TALLYING CUENTA-1 FOR ALL "@"

                  IF CUENTA-1 = 1 THEN
                     UNSTRING WS-EMAIL DELIMITED BY "@"
                     INTO     WS-ANTES-DE-ARROBA,
                              WS-DESPUES-DE-ARROBA

                     IF WS-ANTES-DE-ARROBA(1:1) NOT = SPACES AND
                        WS-ANTES-DE-ARROBA(2:1) NOT = SPACES AND
                        WS-ANTES-DE-ARROBA(3:1) NOT = SPACES
                         THEN
                        INSPECT WS-DESPUES-DE-ARROBA TALLYING
                        CUENTA-2 FOR ALL "."
                        IF CUENTA-2 = 0 THEN
                           MOVE
                           "DEBE INGRESAR EMAIL CON FORMATO CORRECTO "
                                                TO MSG-ERROR
                        ELSE
                          IF WS-DESPUES-DE-ARROBA(1:1) = SPACES AND
                             WS-DESPUES-DE-ARROBA(2:1) = SPACES AND
                             WS-DESPUES-DE-ARROBA(3:1) = SPACES
                              THEN
                             MOVE
                             "DEBE INGRESAR EMAIL CON FORMATO CORRECTO "
                             TO MSG-ERROR
                          END-IF
                        END-IF
                     ELSE
                        MOVE "DEBE INGRESAR EMAIL CON FORMATO CORRECTO"
                                                TO MSG-ERROR
                     END-IF
                  ELSE
                     MOVE "DEBE INGRESAR EMAIL CON FORMATO CORRECTO"
                                                TO MSG-ERROR
                  END-IF
              END-IF

           END-IF
           GOBACK
           .

       VALIDAR-CAR-VALIDOS.

           MOVE WS-EMAIL       TO STRING-CALCULAR500
           MOVE ZEROES         TO LK-LARGO-STR500
           CALL "LargoString" USING INPUT-LARGOSTR500
                                    OUTPUT-LARGOSTR500
           DISPLAY "LK-LARGO-STR500.. : " LK-LARGO-STR500

           MOVE "S"            TO FS-CAR-V
           MOVE ZEROES         TO TOT-CAR-VAL
           PERFORM VARYING I-CAR-VAL FROM 1 BY 1
                   UNTIL   I-CAR-VAL > LK-LARGO-STR500

                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > 54
                     IF WS-EMAIL(I-CAR-VAL:1) =
                        WS-CAR-VALIDOS(I:1)
                        ADD 1    TO TOT-CAR-VAL
                        ADD 54 TO I
                     END-IF
                 END-PERFORM
           END-PERFORM

           IF TOT-CAR-VAL NOT = LK-LARGO-STR500 THEN
              MOVE "N"   TO FS-CAR-V
           END-IF

           .

       END PROGRAM ValidarEmail.
