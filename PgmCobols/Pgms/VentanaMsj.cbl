      ******************************************************************
      * Author:JORGE DUARTE
      * Date: 07-07-2025
      * Purpose: Enseñanza
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VentanaMsj.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-ENTER PIC X.



       LINKAGE SECTION.
       01 MENSAJE-MOSTRAR PIC X(50).

       SCREEN SECTION.
       01  PANTALLA-VENTANA.
          03 LINE 16 COLUMN 10 VALUE
           "+--------------------------------------------------------+".
          03 LINE 17 COLUMN 10 VALUE
           "|                                                        |".
          03 LINE 18 COLUMN 10 VALUE
           "| ".
          03 LINE 18 COLUMN 12 PIC X(50) FROM MENSAJE-MOSTRAR.
          03 LINE 18 COLUMN 63 VALUE
           "    |".
          03 LINE 19 COLUMN 10 VALUE
           "|                                                        |".
          03 LINE 20 COLUMN 10 VALUE
           "+--------------------------------------------------------+".
          03 LINE 21 COLUMN 10 PIC X USING WS-ENTER.

       PROCEDURE DIVISION USING MENSAJE-MOSTRAR.
       MAIN-PROCEDURE.

            DISPLAY PANTALLA-VENTANA
            ACCEPT  PANTALLA-VENTANA

           GOBACK.

       END PROGRAM VentanaMsj.
