       01  r-calendario.
           05  calendario-fecha   PIC 9(08).
           05  r-calendario-fecha redefines calendario-fecha.
               10 fecha-ano       PIC 9(04).
               10 fecha-mes       PIC 9(02).
               10 fecha-dia       PIC 9(02).
           05  calendario-dia-sem PIC 9.
      *        1 = Domingo
      *        2 = Lunes
      *        3 = Martes
      *        4 = Miercoles
      *        5 = Jueves
      *        6 = Viernes
      *        7 = Sabado
           05  calendario-feriado PIC X.
      *        N = No
      *        S = Si
	       05  calendario-numdia  PIC 9(06).
