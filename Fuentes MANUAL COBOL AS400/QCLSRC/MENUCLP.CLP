      PGM
             DCLF       FILE(LIBJDU/MENU01) RCDFMT(M01 P01)
             DCL  VAR(&FECHA6)   TYPE(*CHAR) LEN(6) /* DMY */
             DCL  VAR(&FECHA8)   TYPE(*CHAR) LEN(8) /* DMYY */
             DCL  VAR(&FECINP)   TYPE(*CHAR) LEN(6) /* MMYYYY */
             DCL  VAR(&FECOUT)   TYPE(*CHAR) LEN(8) /* YYYYMMDD */

             DCL        VAR(&TIT)   TYPE(*CHAR) LEN(30)
             DCL        VAR(&MEN)   TYPE(*CHAR) LEN(120)
             DCL        VAR(&BOT)   TYPE(*DEC) LEN(2 0)

            /* VARIABLES PARA CENTRAR TEXTO */
            DCL VAR(&INPCEN) TYPE(*CHAR) LEN(103)
            DCL VAR(&OUTCEN) TYPE(*CHAR) LEN(101)
            DCL VAR(&TXCEN ) TYPE(*CHAR) LEN(100)
            DCL VAR(&TXCTDO) TYPE(*CHAR) LEN(100)
            DCL VAR(&TLC   ) TYPE(*CHAR) LEN(30)
            DCL VAR(&TXCODR) TYPE(*CHAR) LEN(1)

      INICIO:
              SNDRCVF RCDFMT(M01) WAIT(*YES)
              IF COND(&IN03 *EQ '1') THEN(DO)
                 GOTO CMDLBL(FIN)
              ENDDO
              IF COND(&MNB001 *EQ 1) THEN(DO)
                 GOTO CMDLBL(PP01)
              ENDDO
              GOTO CMDLBL(INICIO)
     PP01:
          RCVF RCDFMT(P01) WAIT(*YES)
          IF COND(&OPCP01 *EQ 1) THEN(DO)
            CALL PGM(LISCLI)
          ENDDO
          IF COND(&OPCP01 *EQ 2) THEN(DO)
             RTVJOBA    DATE(&FECHA6)
             CVTDAT     DATE(&FECHA6) TOVAR(&FECHA8) FROMFMT(*DMY) +
                          TOFMT(*DMYY) TOSEP(*NONE)
             CHGVAR VAR(&FECINP) VALUE(%SST(&FECHA8 3 6))
             CHGVAR VAR(&FECOUT) VALUE('00000000')
             CALL PGM(VERCALEND) PARM(&FECINP &FECOUT)
          ENDDO
          IF COND(&OPCP01 *EQ 3) THEN(DO)

             CHGVAR VAR(&TXCEN)  VALUE('CONFIRMAR ELIMINACION')
             CHGVAR VAR(&TLC)    VALUE('030')
             CHGVAR VAR(&INPCEN) VALUE(&TXCEN *CAT &TLC)
             CHGVAR VAR(&OUTCEN) VALUE(' ')
             CALL  PGM(CENTRARSTR) PARM(&INPCEN &OUTCEN)
             CHGVAR VAR(&TIT) VALUE(%SST(&OUTCEN 2 30))

             CHGVAR     VAR(&MEN) VALUE('Para Eliminación del +
                          Cliente, presione  <ACEPTAR>, en caco +
                          contrario presione   <CANCELAR>')

             CHGVAR VAR(&BOT) VALUE(0)
             CALL       PGM(VENCONF) PARM(&TIT &MEN &BOT)
          ENDDO
          IF COND(&OPCP01 *EQ 4) THEN(DO)
             ?WRKSPLF
          ENDDO
          IF COND(&OPCP01 *EQ 5) THEN(DO)
             ?WRKUSRJOB
          ENDDO
          IF COND(&OPCP01 *EQ 6) THEN(DO)
             ?WRKJOB
          ENDDO
          IF COND(&OPCP01 *EQ 7) THEN(DO)
             GOTO CMDLBL(FIN)
          ENDDO
          GOTO CMDLBL(INICIO)

     FIN: ENDPGM
