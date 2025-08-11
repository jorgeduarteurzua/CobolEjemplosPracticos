         PGM PARM(&BIB &FILE)
             DCL VAR(&BIB)    TYPE(*CHAR)  LEN(10)
             DCL VAR(&FILE)   TYPE(*CHAR)  LEN(10)
             DCL VAR(&OBJATR) TYPE(*CHAR)  LEN(10)
             DCLF FILE(LIBJDU/DDSREFFILE)

             ADDLIBLE   LIB(QGPL)   POSITION(*LAST)
             MONMSG     MSGID(CPF0000)

             RTVOBJD    OBJ(&BIB/&FILE) OBJTYPE(*FILE) OBJATR(&OBJATR)
             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(FIN))

             DSPFD      FILE(&BIB/&FILE) TYPE(*ACCPTH) +
                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/DDSKEYS) +
                          OUTMBR(&FILE *REPLACE)

             IF COND(&OBJATR *EQ 'PF') THEN(DO)
                DSPDBR     FILE(&BIB/&FILE) OUTPUT(*OUTFILE) +
                          OUTFILE(QTEMP/DDSREFFILE)
                OVRDBF     FILE(DDSREFFILE) TOFILE(QTEMP/DDSREFFILE)
                GOTO CMDLBL(RECFILE)
             ENDDO
             GOTO CMDLBL(CONTINUAR)

        RECFILE:
             RCVF       RCDFMT(QWHDRDBR)
             MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(CONTINUAR))

             DSPFD      FILE(&WHRELI/&WHREFI) TYPE(*ACCPTH) +
                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/DDSKEYS) +
                          OUTMBR(&FILE *ADD)

             GOTO CMDLBL(RECFILE)

        CONTINUAR:
             DSPFFD     FILE(&BIB/&FILE) OUTPUT(*OUTFILE) +
                          OUTFILE(QTEMP/DDSFIELDS) OUTMBR(&FILE +
                          *REPLACE)

             OVRDBF     FILE(DDSFIELDS) TOFILE(QTEMP/DDSFIELDS) +
                          MBR(&FILE)

             OVRDBF     FILE(DDSKEYS) TOFILE(QTEMP/DDSKEYS) MBR(&FILE)


             CALL       PGM(QGPL/PGMPRTDDS) PARM(&BIB &FILE)


         FIN: ENDPGM
