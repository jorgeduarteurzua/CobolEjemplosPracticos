        PGM  PARM(&USR &FILESEL &JOBSEL)

         /*---------------------------------------------------------*/
         /* &FILE = *ALL (TODOS)                                    */
         /* &JOB  = *ALL (TODOS)                                    */
         /*---------------------------------------------------------*/
             DCL VAR(&USR) TYPE(*CHAR) LEN(10)

             DCL VAR(&ESTADO) TYPE(*CHAR) LEN(3)
             DCL VAR(&FILE)   TYPE(*CHAR) LEN(10)
             DCL VAR(&FILESEL) TYPE(*CHAR) LEN(10)
             DCL VAR(&NFILE)  TYPE(*DEC ) LEN(06 0)
             DCL VAR(&JOB)    TYPE(*CHAR) LEN(10)
             DCL VAR(&JOBSEL) TYPE(*CHAR) LEN(10)
             DCL VAR(&NUM)    TYPE(*CHAR) LEN(06)
             DCLF       FILE(QGPL/DTLSPOOLF)

             WRKSPLF    SELECT(&USR) OUTPUT(*PRINT)

             DLTF       FILE(QTEMP/DTLSPOOLF)
             MONMSG     MSGID(CPF0000)

             CRTDUPOBJ  OBJ(DTLSPOOLF) FROMLIB(QGPL) +
                          OBJTYPE(*FILE) TOLIB(QTEMP) DATA(*NO)

             CPYSPLF    FILE(QPRTSPLF) TOFILE(QTEMP/DTLSPOOLF) +
                          SPLNBR(*LAST) MBROPT(*REPLACE)
             OVRDBF     FILE(DTLSPOOLF) TOFILE(QTEMP/DTLSPOOLF)
        LEER:
             RCVF
             MONMSG   CPF0864  EXEC(GOTO FIN)

             CHGVAR     VAR(&ESTADO) VALUE(%SST(&DTLSPOOLF 46 3))
             IF         COND((&ESTADO *EQ 'RDY') *OR (&ESTADO *EQ +
                          'HLD')) THEN(DO)

                  CHGVAR     VAR(&FILE  ) VALUE(%SST(&DTLSPOOLF 2 10))
                  CHGVAR     VAR(&JOB   ) VALUE(%SST(&DTLSPOOLF 108 10))
                  CHGVAR     VAR(&NUM   ) VALUE(%SST(&DTLSPOOLF 119 6))
                  GOTO CMDLBL(EVALUA)

             ENDDO
             GOTO       CMDLBL(LEER)

        EVALUA:
             IF         COND((&FILESEL *EQ '*ALL') *AND (&JOBSEL *EQ +
                          '*ALL')) THEN(GOTO CMDLBL(BORRAR))

             IF         COND((&FILESEL *NE '*ALL') *AND (&JOBSEL *EQ +
                          '*ALL')) THEN(DO)
                  IF COND(&FILESEL *NE ' ') THEN(DO)
                     IF COND(&FILESEL *EQ &FILE) THEN(GOTO CMDLBL(BORRAR))
                  ENDDO
             ENDDO
             IF         COND((&FILESEL *EQ '*ALL') *AND (&JOBSEL *NE +
                          '*ALL')) THEN(DO)
                  IF COND(&JOBSEL *NE ' ') THEN(DO)
                     IF COND(&JOBSEL *EQ &JOB) THEN(GOTO CMDLBL(BORRAR))
                  ENDDO
             ENDDO
             GOTO       CMDLBL(LEER)
        BORRAR:
             CHGVAR     VAR(&NFILE ) VALUE(%SST(&DTLSPOOLF 101 6))
             DLTSPLF    FILE(&FILE) JOB(&NUM/&USR/&JOB) SPLNBR(&NFILE)
             MONMSG     MSGID(CPF0000)
             GOTO LEER

        FIN:
             DLTSPLF    FILE(QPRTSPLF) SPLNBR(*LAST)
             MONMSG     MSGID(CPF0000)
             ENDPGM
