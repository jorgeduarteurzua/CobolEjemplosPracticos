       IDENTIFICATION DIVISION.
       PROGRAM-ID. FibonacciSerieArr.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIBO-COUNT        PIC 9(3) VALUE 50.
       01  FIBO-INDEX        PIC 9(10) VALUE 1.
       01  FIBO-NUM1         PIC 9(10) VALUE 0.
       01  FIBO-NUM2         PIC 9(10) VALUE 1.
       01  FIBO-NUM          PIC 9(10).
       01 ARREGLO-FIBONNACI.
           05 SERIE-FIBO OCCURS 100 TIMES.
              10 S-FIB   PIC 9(18).

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "Los primeros 50 números de la serie Fibonacci: ".

           PERFORM VARYING FIBO-INDEX FROM 1 BY 1
              UNTIL FIBO-INDEX > FIBO-COUNT
               IF FIBO-INDEX = 1 THEN
                   MOVE FIBO-NUM1 TO S-FIB(FIBO-INDEX)
               ELSE IF FIBO-INDEX = 2 THEN
                   MOVE FIBO-NUM2 TO S-FIB(FIBO-INDEX)
               ELSE
                   COMPUTE S-FIB(FIBO-INDEX) =
                           S-FIB(FIBO-INDEX - 2) +
                           S-FIB(FIBO-INDEX - 1)
               END-IF
           END-PERFORM
           PERFORM VARYING FIBO-INDEX FROM 1 BY 1 UNTIL
                   FIBO-INDEX > FIBO-COUNT
                   DISPLAY S-FIB(FIBO-INDEX)
           END-PERFORM.

           STOP RUN.
