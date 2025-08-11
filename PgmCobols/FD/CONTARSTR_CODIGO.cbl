           MOVE ZEROES           TO TOTAL-STR

           IF CONTARSTR-STRING-BUSCAR = SPACES
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LARGO-STR-REV
                    IF CONTARSTR-STRING-REVISAR(I:1) = " "
                        ADD 1            TO TOTAL-STR
                    END-IF
               END-PERFORM
           ELSE
               MOVE CONTARSTR-STRING-REVISAR TO STRING-CALCULAR
               MOVE ZEROES                   TO LK-LARGO-STR
               CALL "LargoString10000" USING INPUT-LARGOSTR10000
                                             OUTPUT-LARGOSTR10000
               MOVE LK-LARGO-STR             TO WS-LARGO-STR-REV

               MOVE CONTARSTR-STRING-BUSCAR TO STRING-CALCULAR
               MOVE ZEROES                   TO LK-LARGO-STR
               CALL "LargoString10000" USING INPUT-LARGOSTR10000
                                             OUTPUT-LARGOSTR10000
               MOVE LK-LARGO-STR             TO WS-LARGO-STR-BUS

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-LARGO-STR-REV
                    IF CONTARSTR-STRING-REVISAR(I:WS-LARGO-STR-BUS) =
                        CONTARSTR-STRING-BUSCAR(1:WS-LARGO-STR-BUS)
                        ADD 1            TO TOTAL-STR
                    END-IF
               END-PERFORM
           END-IF

           GOBACK.