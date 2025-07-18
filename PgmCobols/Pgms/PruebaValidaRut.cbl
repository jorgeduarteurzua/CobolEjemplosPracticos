      ******************************************************************
      * Author: JORGE DUARTE
      * Date: 07-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PruebaValidaRut.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 PAR-INPUT-VALIDA-RUT.
           10 WS-RUT PIC 9(10) VALUE 12961577.
           10 WS-DV  PIC X    VALUE '8'.
       01 PAR-OUTPUT-VALIDA-RUT.
           10 WS-CODRET PIC 9.

       01 PAR-INPUT-CALCULA-DV.
           10 WS-RUT-DV PIC 9(10) VALUE 12961577.
       01 PAR-OUTPUT-CALCULA-DV.
           10 WS-CODRET-DV PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE 0   TO WS-CODRET
           CALL "ValidaRut" USING PAR-INPUT-VALIDA-RUT
                                  PAR-OUTPUT-VALIDA-RUT

           DISPLAY "Respuesta ValidaRut Rut : " WS-RUT " - " WS-DV
                    " Retorno Rutina : "  WS-CODRET


            CALL "CalculaDVRut" USING PAR-INPUT-CALCULA-DV
                                      PAR-OUTPUT-CALCULA-DV

           DISPLAY "Respuesta CalculaDVRut Rut : " WS-RUT-DV
                    " Digito Verificador Calculado : "  WS-CODRET-DV

            STOP RUN.
       END PROGRAM PruebaValidaRut.
