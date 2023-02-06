      *---------------------------------------------------------------
      *COPY COBOL DEL ARCHIVO DE CALIFICACIONES STCALIF
      *LONGITUD DEL REGISTRO = 80
      *---------------------------------------------------------------
       01  STCALIF-RECORD.
           03  STCALIF-COD-ESTUDIANTE                      PIC 9(06).
           03  STCALIF-COD-MATERIA                         PIC X(08).
           03  STCALIF-NOTA                                PIC 9(03).
           03  STCALIF-FECHA-EXAMEN.
               05  STCALIF-FECHA-EXAMEN-PERIODO.
                   07  STCALIF-FECHA-EXAMEN-AAAA           PIC 9(04).
                   07  STCALIF-FECHA-EXAMEN-MM             PIC 9(02).
               05  STCALIF-FECHA-EXAMEN-DD                 PIC 9(02).
           03  STCALIF-INSTITUTO                           PIC X(50).
           03  FILLER                                      PIC X(05).
