      *-----------------------------------------------------------*
      * COPY COBOL DEL ARCHIVO DE MATERIAS                        *
      * LREC = 80                                                 *
      *-----------------------------------------------------------*
       01  MATERIAS-REGISTRO.
           03  MATERIAS-CLAVE.
               05  MATERIAS-CODIGO                 PIC X(08).
           03  MATERIA-NOMBRE                      PIC X(50).
           03  FILLER                              PIC X(22).
