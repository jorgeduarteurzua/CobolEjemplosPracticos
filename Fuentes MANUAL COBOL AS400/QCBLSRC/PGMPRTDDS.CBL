       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID. PGMPRTDDS.
       AUTHOR. JORGE DUARTE.
      *---------------------------------------------------------------**
      * JORGE DUARTE URZUA                        11/12/2024          **
      *                                                               **
      * GENERA REFERENCIA DE PROGRAMAS HASTA 7 NIVELES                **
      *---------------------------------------------------------------**
      *
       ENVIRONMENT DIVISION.
      *--------------------*
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.      IBM-AS400.
       OBJECT-COMPUTER.      IBM-AS400.
       SPECIAL-NAMES.        LOCAL-DATA IS LOCAL-DATA-AREA .
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DDSFIELDS  ASSIGN       TO DATABASE-DDSFIELDS
                             ORGANIZATION IS SEQUENTIAL
                             FILE STATUS  IS FS-DDSFIELDS.

           SELECT DDSKEYS    ASSIGN       TO DATABASE-DDSKEYS
                             ORGANIZATION IS SEQUENTIAL
                             FILE STATUS  IS FS-DDSKEYS.

           SELECT INFORME    ASSIGN TO FORMATFILE-PRTDDS
                             FILE STATUS FS-INFORME.

       DATA DIVISION.
      *--------------*
       FILE SECTION.
      *-------------*
       FD  DDSFIELDS LABEL RECORD STANDARD.
       01  REG-DDSFIELDS.
           COPY  DDS-ALL-FORMAT OF DDSFIELDS.

       FD  DDSKEYS   LABEL RECORD STANDARD.
       01  REG-DDSKEYS.
           COPY  DDS-ALL-FORMAT OF DDSKEYS.

       FD  INFORME.
       01  R-INFORME       PIC X(132).

       WORKING-STORAGE SECTION.
      *------------------------*

       77 IMP-HEAD             PIC X(1).
       77 WS-LOGICO            PIC X(10) VALUE SPACES.

       01 VARIABLES-DE-STATUS.
           05 FS-DDSFIELDS       PIC X(02).
           05 FS-DDSKEYS         PIC X(02).
           05 FS-INFORME         PIC X(02).


       01 TITULO-OO.
          COPY DDS-TITULO-O     OF PRTDDS .
       01 DETALLE-OO.
          COPY DDS-DETALLE-O    OF PRTDDS .
       01 TKEYS-OO.
          COPY DDS-TKEYS-O      OF PRTDDS .
       01 LLAVES-OO.
          COPY DDS-LLAVES-O     OF PRTDDS .

       LINKAGE SECTION.
       01 LK-BIBLIOTECA       PIC X(10).
       01 LK-ARCHIVO          PIC X(10).

       PROCEDURE DIVISION USING LK-BIBLIOTECA LK-ARCHIVO.
      *-----------------------------------------------------

           OPEN INPUT  DDSFIELDS DDSKEYS
           OPEN OUTPUT INFORME

           PERFORM PRINT-HEAD
           PERFORM IMPRIME-CAMPOS UNTIL FS-DDSFIELDS NOT = "00"

           PERFORM IMPRIME-KEYS   UNTIL FS-DDSKEYS   NOT = "00"

           CLOSE INFORME  DDSFIELDS DDSKEYS

           GOBACK.

      *
       IMPRIME-CAMPOS.
      *---------------
           READ DDSFIELDS NEXT RECORD END-READ
           IF FS-DDSFIELDS = "00"
              MOVE WHFLDE OF DDSFIELDS TO CAMPO OF DETALLE-OO
              MOVE WHFTXT OF DDSFIELDS TO DESCAMPO  OF DETALLE-OO
              MOVE ZEROES              TO LARGO     OF DETALLE-OO
                                          DEC       OF DETALLE-OO

      * WHFLDD, WHFLDP
              EVALUATE WHFLDT OF DDSFIELDS
                WHEN "A" MOVE "CHAR"    TO TIPO    OF DETALLE-OO
                        MOVE WHFLDB OF DDSFIELDS TO LARGO OF DETALLE-OO
                WHEN "P" MOVE "DEC "    TO TIPO    OF DETALLE-OO
                        MOVE WHFLDD OF DDSFIELDS TO LARGO OF DETALLE-OO
                        MOVE WHFLDP OF DDSFIELDS TO DEC   OF DETALLE-OO
                WHEN "S" MOVE "NUM "    TO TIPO    OF DETALLE-OO
                        MOVE WHFLDD OF DDSFIELDS TO LARGO OF DETALLE-OO
                        MOVE WHFLDP OF DDSFIELDS TO DEC   OF DETALLE-OO
                WHEN OTHER
                        MOVE "OTRO"    TO TIPO    OF DETALLE-OO
                        MOVE WHFLDB OF DDSFIELDS TO TIPO OF DETALLE-OO
              END-EVALUATE
              WRITE R-INFORME FROM DETALLE-OO FORMAT "DETALLE"
                    AT END-OF-PAGE PERFORM PRINT-HEAD
              END-WRITE
           END-IF
           .

      *
       IMPRIME-TKEYS.
      *--------------
           WRITE R-INFORME FROM TKEYS-OO   FORMAT "TKEYS"
           END-WRITE.
      *
       IMPRIME-KEYS.
      *---------------
           READ DDSKEYS NEXT RECORD END-READ
           IF FS-DDSKEYS   = "00" THEN
              IF WS-LOGICO NOT = APFILE OF DDSKEYS THEN
                 MOVE APFILE OF DDSKEYS TO LOGICO OF TKEYS-OO
                                           WS-LOGICO
                 PERFORM IMPRIME-TKEYS
              END-IF
              MOVE APKEYF OF DDSKEYS   TO CAMPO OF LLAVES-OO
              MOVE APKSEQ OF DDSKEYS   TO ORDEN OF LLAVES-OO
              WRITE R-INFORME FROM LLAVES-OO  FORMAT "LLAVES"
                    AT END-OF-PAGE PERFORM IMPRIME-TKEYS
              END-WRITE
           END-IF
           .


      *
       PRINT-HEAD.
      *-----------
           MOVE LK-BIBLIOTECA          TO BIBLIOTECA OF TITULO-OO
           MOVE LK-ARCHIVO             TO ARCHIVO   OF TITULO-OO

           WRITE R-INFORME FROM TITULO-OO  FORMAT "TITULO"
                            END-WRITE
           .
