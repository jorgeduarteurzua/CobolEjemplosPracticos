         PGM PARM(&BIB &PGM)
             DCL VAR(&BIB)    TYPE(*CHAR)  LEN(10)
             DCL VAR(&PGM)    TYPE(*CHAR)  LEN(10)

             ADDLIBLE   LIB(QGPL) POSITION(*LAST)
             MONMSG     MSGID(CPF0000)


             CALL       PGM(QGPL/PGMREF) PARM(&BIB &PGM)


         FIN: ENDPGM
