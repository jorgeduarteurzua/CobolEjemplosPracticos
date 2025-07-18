      ******************************************************************
      * Author: Jorge Duarte
      * Date: 02-07-2025
      * Purpose: Estudio
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MENU-CHOICE PIC 9 VALUE 0.
       01  SCREEN-LINES.
           05  LINE-1 PIC X(80) VALUE "MENU PRINCIPAL".
           05  LINE-2 PIC X(80) VALUE "1. Opción 1".
           05  LINE-3 PIC X(80) VALUE "2. Opción 2".
           05  LINE-4 PIC X(80) VALUE "3. Salir".
           05  LINE-5 PIC X(80) VALUE "Ingrese su opción: ".
       PROCEDURE DIVISION.
       MAIN-MENU.
           DISPLAY LINE-1.
           DISPLAY LINE-2.
           DISPLAY LINE-3.
           DISPLAY LINE-4.
           DISPLAY LINE-5.
           ACCEPT MENU-CHOICE.
           EVALUATE MENU-CHOICE
             WHEN 1
               PERFORM OPTION-1
             WHEN 2
               PERFORM OPTION-2
             WHEN 3
               PERFORM EXIT-PROGRAM
             WHEN OTHER
               DISPLAY "Opción inválida. Intente de nuevo."
               GO TO MAIN-MENU
           END-EVALUATE.

       OPTION-1.
           DISPLAY "Has elegido la Opción 1".
           GO TO MAIN-MENU.

       OPTION-2.
           DISPLAY "Has elegido la Opción 2".
           GO TO MAIN-MENU.

       EXIT-PROGRAM.
           DISPLAY "Saliendo del programa.".
       STOP RUN.
