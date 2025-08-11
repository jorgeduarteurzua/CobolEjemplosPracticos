         PGM (&BIB &OBJ &TYPE &DESC &ATR)
             DCL VAR(&BIB)   TYPE(*CHAR)  LEN(10)
             DCL VAR(&OBJ)   TYPE(*CHAR)  LEN(10)
             DCL VAR(&TYPE)  TYPE(*CHAR)  LEN(10)
             DCL VAR(&DESC)  TYPE(*CHAR)  LEN(50)
             DCL VAR(&ATR)   TYPE(*CHAR)  LEN(10)


             RTVOBJD    OBJ(&BIB/&OBJ) OBJTYPE(&TYPE) OBJATR(&ATR) +
                          TEXT(&DESC)
             MONMSG     MSGID(CPF0000) EXEC(CHGVAR VAR(&DESC) +
                          VALUE('No se pudo rescatar Descripción'))

         ENDPGM
