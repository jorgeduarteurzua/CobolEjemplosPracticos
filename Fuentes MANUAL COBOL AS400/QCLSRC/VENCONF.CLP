           PGM PARM(&TIT &MEN &BOT)
             DCLF       FILE(VENCONF)
             DCL        VAR(&TIT)   TYPE(*CHAR) LEN(30)
             DCL        VAR(&MEN)   TYPE(*CHAR) LEN(120)
             DCL        VAR(&BOT)   TYPE(*DEC) LEN(2 0)

             CHGVAR VAR(&TITULO) VALUE(&TIT)
             CHGVAR VAR(&MENSAJE) VALUE(&MEN)
             CHGVAR VAR(&BOTONCNF) VALUE(0)
         PEDIR:
             SNDRCVF    RCDFMT(W01)
             IF COND(&BOTONCNF *NE 0) THEN(DO)
                CHGVAR VAR(&BOT) VALUE(&BOTONCNF)
                GOTO CMDLBL(FIN)
             ENDDO
             GOTO CMDLBL(PEDIR)

         FIN: ENDPGM
