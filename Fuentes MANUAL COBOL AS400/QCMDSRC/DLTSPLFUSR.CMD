             CMD        PROMPT('ELIMINA SPOOL DE UN USUARIO')

             PARM       KWD(USER) TYPE(*CHAR) LEN(10) MIN(1) +
                          PROMPT('Usuario')
             PARM       KWD(FILE) TYPE(*CHAR) LEN(10) MIN(1) +
                          PROMPT('Archivo (*ALL / Nombre)')
             PARM       KWD(JOB) TYPE(*CHAR) LEN(10) MIN(1) +
                          PROMPT('Job (*ALL / Nombre)')
