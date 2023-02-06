      *-----------------------------------------------------------*
      * COPY COBOL DEL ARCHIVO DE ALUMNOS                         *
      * LREC = 120                                                *
      *-----------------------------------------------------------*
       01  ALU-REGISTRO.
           03  ALU-CLAVE.
               05  ALU-CODIGO                      PIC 9(06).
           03  ALU-NOMBRE                          PIC X(50).
           03  ALU-APELLIDO                        PIC X(50).
           03  ALU-FECNAC.
               05  ALUMNO-FECHA-AAAA               PIC 9(04).
               05  ALUMNO-FECHA-MM                 PIC 9(02).
               05  ALUMNO-FECHA-DD                 PIC 9(02).
           03  FILLER                              PIC X(06).
