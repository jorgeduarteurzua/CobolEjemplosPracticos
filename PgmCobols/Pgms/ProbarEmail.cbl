      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 17-07-2025
      * Purpose: ENSEÑANZA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProbarEmail.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 INP-VALIDA-EMAIL.
           05 WS-EMAIL     PIC X(150).
       01 OUT-VALIDA-EMAIL.
           05 MSG-ERROR   PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE SPACES           TO WS-EMAIL
            MOVE SPACES           TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "correo"          TO WS-EMAIL
            MOVE SPACES            TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "correo@"          TO WS-EMAIL
            MOVE SPACES             TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "correo@gmail"      TO WS-EMAIL
            MOVE SPACES              TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "correo@gmail.com"      TO WS-EMAIL
            MOVE SPACES                  TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "uno.raya.nueve@gmail.com"      TO WS-EMAIL
            MOVE SPACES                          TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "uno#raya.nueve@gmail.com"      TO WS-EMAIL
            MOVE SPACES                          TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"

            MOVE "uno,raya,nueve@gmail.com"      TO WS-EMAIL
            MOVE SPACES                          TO MSG-ERROR
            CALL "ValidarEmail" USING INP-VALIDA-EMAIL
                                      OUT-VALIDA-EMAIL

            DISPLAY "EMAIL A VALIDAR : [" WS-EMAIL "]"
            DISPLAY "MENSAJE RUTINA  : [" MSG-ERROR "]"
            STOP RUN.
       END PROGRAM ProbarEmail.
