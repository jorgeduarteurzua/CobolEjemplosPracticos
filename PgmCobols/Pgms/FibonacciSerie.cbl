       IDENTIFICATION DIVISION.
       PROGRAM-ID. FibonacciSerie.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIBO-COUNT        PIC 9(2) VALUE 10.
       01  FIBO-INDEX        PIC 9(2) VALUE 1.
       01  FIBO-NUM1         PIC 9(10) VALUE 0.
       01  FIBO-NUM2         PIC 9(10) VALUE 1.
       01  FIBO-NUM          PIC 9(10).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Los primeros 10 números de la serie Fibonacci: ".

           PERFORM VARYING FIBO-INDEX FROM 1 BY 1
              UNTIL FIBO-INDEX > FIBO-COUNT
               IF FIBO-INDEX = 1 THEN
                   MOVE FIBO-NUM1 TO FIBO-NUM
               ELSE IF FIBO-INDEX = 2 THEN
                   MOVE FIBO-NUM2 TO FIBO-NUM
               ELSE
                   COMPUTE FIBO-NUM = FIBO-NUM1 + FIBO-NUM2
                   MOVE FIBO-NUM2 TO FIBO-NUM1
                   MOVE FIBO-NUM TO FIBO-NUM2
               END-IF

               DISPLAY FIBO-NUM
           END-PERFORM.

           STOP RUN.
