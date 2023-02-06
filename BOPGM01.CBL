       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID. BOPGM01.
      *AUTHOR. MARIA EMILIA LUDUENA
      *
      *-----------------------------------------------------------
      * ESTE PROGRAMA REALIZA LA LECTURA DEL ARCHIVO STCALIF
      * ORGANIZATION: SECUENCIAL
      *-----------------------------------------------------------
      * EL OBJETIVO ES OBTENER LA NOTA MINIMA Y LA NOTA MAXIMA
      * CONTENIDA EN EL ARCHIVO Y MOSTRAR POR FIN DE ARCHIVO
      * LA NOTA MINIMA LEIDA, LA NOTA MAXIMA LEIDA, EL CODIGO DEL
      * ALUMNO Y LA FECHA.
      *-----------------------------------------------------------

       ENVIRONMENT DIVISION.
      *--------------------

       CONFIGURATION SECTION.
      *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
      *--------------------

       FILE-CONTROL.
      *------------

           SELECT STCALIF          ASSIGN       TO
             'C:\Users\Lenovo\Desktop\HOST-COBOL\EJERCICIOS\STCALIF.txt'
                                   ORGANIZATION IS SEQUENTIAL
                                   ACCESS MODE  IS SEQUENTIAL
                                   FILE STATUS  IS FS-STCALIF.

           SELECT MATERIAS         ASSIGN       TO
            'C:\Users\Lenovo\Desktop\HOST-COBOL\EJERCICIOS\MATERIAS.DAT'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS RANDOM
                                   RECORD KEY   IS COD-MATERIAS-FD
                                   FILE STATUS  IS FS-MATERIAS.

           SELECT ALUMNOS          ASSIGN       TO
             'C:\Users\Lenovo\Desktop\HOST-COBOL\EJERCICIOS\ALUMNOS.DAT'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS RANDOM
                                   RECORD KEY   IS COD-ALUMNOS-FD
                                   FILE STATUS  IS FS-ALUMNOS.

           SELECT LISTADO          ASSIGN       TO
             'C:\Users\Lenovo\Desktop\HOST-COBOL\EJERCICIOS\LISTADO.txt'
                                   ORGANIZATION IS SEQUENTIAL
                                   ACCESS MODE  IS SEQUENTIAL
                                   FILE STATUS  IS FS-LISTADO.

       DATA DIVISION.
      *-------------

       FILE SECTION.
      *------------

       FD  STCALIF
           RECORDING MODE IS F.
       01  STCALIF-RECORD-FD             PIC  X(80).

       FD  MATERIAS.
       01  COD-MATERIAS-DATOS-FD.
           03  COD-MATERIAS-FD           PIC X(08).
           03  FILLER                    PIC X(72).

       FD  ALUMNOS.
       01  NRO-LEGAJO-ALUMNO-FD.
           03  COD-ALUMNOS-FD.
               05  ALUMNO-COD-FD         PIC 9(06).
           03  FILLER                    PIC X(114).

       FD  LISTADO
           RECORDING MODE IS F.
       01  REG-LISTADO-FD                PIC  X(132).


       WORKING-STORAGE SECTION.
      *-----------------------
       77  CTE-PROGRAMA                  PIC  X(20) VALUE 'BOPGM01'.

       77  FS-STCALIF                    PIC  X(02) VALUE ' '.
           88 88-FS-STCALIF-OK                      VALUE '00'.
           88 88-FS-STCALIF-EOF                     VALUE '10'.

       77  FS-LISTADO                    PIC  X(02) VALUE ' '.
           88 88-FS-LISTADO-OK                      VALUE '00'.
           88 88-FS-LISTADO-EOF                     VALUE '10'.

       77  FS-MATERIAS                   PIC X(02)  VALUE ' '.
           88 88-FS-MATERIAS-OK                     VALUE '00'.
           88 88-FS-MATERIAS-EOF                    VALUE '10'.
           88 88-FS-MATERIAS-INVKEY                 VALUE '21'.
           88 88-FS-MATERIAS-DUPKEY                 VALUE '22'.
           88 88-FS-MATERIAS-NOKEY                  VALUE '23'.

       77  FS-ALUMNOS                    PIC X(02)  VALUE ' '.
           88 88-FS-ALUMNOS-OK                      VALUE '00'.
           88 88-FS-ALUMNOS-EOF                     VALUE '10'.
           88 88-FS-ALUMNOS-INVKEY                  VALUE '21'.
           88 88-FS-ALUMNOS-DUPKEY                  VALUE '22'.
           88 88-FS-ALUMNOS-NOKEY                   VALUE '23'.

       77  WS-OPEN-STCALIF               PIC X      VALUE 'N'.
           88 88-OPEN-STCALIF-SI                    VALUE 'S'.
           88 88-OPEN-STCALIF-NO                    VALUE 'N'.

       77  WS-OPEN-LISTADO               PIC X      VALUE 'N'.
           88 88-OPEN-LISTADO-SI                    VALUE 'S'.
           88 88-OPEN-LISTADO-NO                    VALUE 'N'.

       77  WS-OPEN-MATERIAS              PIC X      VALUE 'N'.
           88 88-OPEN-MATERIAS-SI                   VALUE 'S'.
           88 88-OPEN-MATERIAS-NO                   VALUE 'N'.

       77  WS-OPEN-ALUMNOS               PIC X      VALUE 'N'.
           88 88-OPEN-ALUMNOS-SI                    VALUE 'S'.
           88 88-OPEN-ALUMNOS-NO                    VALUE 'N'.

       77  WS-LEIDOS-STCALIF             PIC 9(09)  VALUE 0.

      * 77  WS-COD-ESTUDIANTE-ED          PIC ZZZZZ9.

       77  REG-LISTADO                   PIC X(132).
       77  WS-GRABADOS-LISTADO           PIC 9(09)  VALUE 0.
       77  WS-GRABADOS-LISTADO-ED        PIC ZZZ.ZZZ.ZZ9.

       77  WS-PAGINA                     PIC 9(05).
       77  WS-PAGINA-ED                  PIC ZZ.ZZZ.

       77  WS-LINEA                      PIC 9(03)  VALUE 090.
       77  WS-LINEAS-MAX                 PIC 9(02)  VALUE 66.

       77  WS-SALTO-HOJA                 PIC X      VALUE 'S'.
           88  88-SALTO-HOJA-SI                     VALUE 'S'.
           88  88-SALTO-HOJA-NO                     VALUE 'N'.

       77  WS-LIMITE-NOTA                     PIC 9(03)  VALUE 060.

       77  WS-MIN-NOTA                   PIC 9(03)  VALUE 0.
       77  WS-MAX-NOTA                   PIC 9(03)  VALUE 0.

       77  WS-NOTA-PROMEDIO              PIC 9(03)  VALUE 0.
       77  WS-NOTA-PROMEDIO-ED           PIC ZZ9.
       77  WS-ACUM-NOTAS                 PIC 9(10)  VALUE 0.

       01  WS-CURRENT-DATE.
           03 WS-CURRENT-DATE-DATE.
              05 WS-CURRENT-DATE-YYYY    PIC 9(04)  VALUE 0.
              05 WS-CURRENT-DATE-MM      PIC 9(02)  VALUE 0.
              05 WS-CURRENT-DATE-DD      PIC 9(02)  VALUE 0.
           03 WS-CURRENT-DATE-TIME.
              05 WS-CURRENT-DATE-HS      PIC 9(02)  VALUE 0.
              05 WS-CURRENT-DATE-MS      PIC 9(02)  VALUE 0.
              05 WS-CURRENT-DATE-SS      PIC 9(02)  VALUE 0.

      *-------------------------------------------------------------*
      *     DEFINICION DEL AREA COMUN DE IMPRESION DEL LISTADO      *
      *-------------------------------------------------------------*
       01  TIT-01.
           03  FILLER                    PIC X(05)  VALUE 'PGM: '.
           03  FILLER                    PIC X(08)  VALUE 'BOPGM01'.
           03  FILLER                    PIC X(34)  VALUE ' '.
           03  FILLER                    PIC X(38)  VALUE
                'LISTADO DE NOTAS OBTENIDAS POR ALUMNOS'.
           03  FILLER                    PIC X(28)  VALUE ' '.
           03  FILLER                    PIC X(07)  VALUE 'FECHA '.
           03  TIT01-FECHA.
               05  TIT01-FECHA-DD        PIC 99     VALUE 0.
               05  FILLER                PIC X      VALUE '/'.
               05  TIT01-FECHA-MM        PIC 99     VALUE 0.
               05  FILLER                PIC X      VALUE '/'.
               05  TIT01-FECHA-AAAA      PIC 9(04)  VALUE 0.

       01  TIT-02.
           03  FILLER                    PIC X(08)  VALUE 'PAGINA: '.
           03  TIT-2-PAG                 PIC ZZZ9.
           03  FILLER                    PIC X(35)  VALUE ' '.
           03  FILLER                    PIC X(38)  VALUE '-'.
           03  FILLER                    PIC X(29)  VALUE ' '.
           03  FILLER                    PIC X(06)  VALUE 'HORA: '.
           03  TIT02-HORA.
               05  TIT02-HORA-HH         PIC 99     VALUE 0.
               05  FILLER                PIC X      VALUE ':'.
               05  TIT02-HORA-MM         PIC 99     VALUE 0.
               05  FILLER                PIC X      VALUE ':'.
               05  TIT02-HORA-SS         PIC 99     VALUE 0.

       01  TIT-03.
           03  FILLER                    PIC X(132) VALUE SPACES.

       01  TIT-04.
           03  FILLER                    PIC X(07)  VALUE ' '.
           03  FILLER                    PIC X(17)  VALUE
                                            'NOMBRE ESTUDIANTE'.
           03  FILLER                    PIC X(40)  VALUE ' '.
           03  FILLER                    PIC X(14)  VALUE
                                            'NOMBRE MATERIA'.
           03  FILLER                    PIC X(13)  VALUE ' '.
           03  FILLER                    PIC X(04)  VALUE 'NOTA'.
           03  FILLER                    PIC X(03)  VALUE ' '.
           03  FILLER                    PIC X(05)  VALUE 'FECHA'.
           03  FILLER                    PIC X(08)  VALUE ' '.
           03  FILLER                    PIC X(11)  VALUE
                                             'OBSERVACION'.

       01  TIT-05.
           03  FILLER                    PIC X(49)  VALUE ALL '-'.
           03  FILLER                    PIC X      VALUE ' '.
           03  FILLER                    PIC X(40)  VALUE ALL '-'.
           03  FILLER                    PIC X      VALUE ' '.
           03  FILLER                    PIC X(04)  VALUE ALL '-'.
           03  FILLER                    PIC X      VALUE ' '.
           03  FILLER                    PIC X(10)  VALUE ALL '-'.
           03  FILLER                    PIC X      VALUE ' '.
           03  FILLER                    PIC X(20)  VALUE ALL '-'.

       01  INFO-ESTUDIANTE.
           03  ESTUDIANTE-NOMBRE         PIC X(50)  VALUE ' '.
           03  FILLER                    PIC X(01)  VALUE ' '.
           03  ESTUDIANTE-MATERIA        PIC X(40)  VALUE ' '.
           03  FILLER                    PIC X(01)  VALUE ' '.
           03  ESTUDIANTE-NOTA                  PIC ZZ9.
           03  FILLER                    PIC X(01)  VALUE ' '.
           03  ESTUDIANTE-FECHA.
               05  ESTUDIANTE-FECHA-DD   PIC 9(02)  VALUE 0.
               05  FILLER                PIC X(01)  VALUE '/'.
               05  ESTUDIANTE-FECHA-MM   PIC 9(02)  VALUE 0.
               05  FILLER                PIC X(01)  VALUE '/'.
               05  ESTUDIANTE-FECHA-AAAA PIC 9(04)  VALUE 0.
           03  FILLER                    PIC X(01)  VALUE ' '.
           03  ALU-OBSERVACION           PIC X(20)  VALUE ' '.

       01  TIT-06.
           03  FILLER                    PIC X(18)  VALUE
                                             'TOTALES DE CONTROL'.
       01  TIT-07.
           03  FILLER                    PIC X(23)  VALUE
                                             'CANT. REGISTOS LEIDOS: '.
           03  WS-LEIDOS-STCALIF-ED      PIC ZZZ.ZZZ.ZZ9.

       01  TIT-08.
           03  FILLER                    PIC X(13)  VALUE
                                             'NOTA MINIMA: '.
           03  WS-MIN-NOTA-ED            PIC ZZ9.
           03  FILLER                    PIC X(09)  VALUE
                                             ' ALUMNO: '.
           03  WS-MIN-ALUMNO             PIC X(20).
           03  FILLER                    PIC X(15)  VALUE
                                             ' FECHA EXAMEN: '.
           03  WS-MIN-FECHA.
               05  WS-MIN-FECHA-AAAA     PIC 9(04)  VALUE 0.
               05  FILLER                PIC X(01)  VALUE '/'.
               05  WS-MIN-FECHA-MM       PIC 9(02)  VALUE 0.
               05  FILLER                PIC X(01)  VALUE '/'.
               05  WS-MIN-FECHA-DD       PIC 9(02)  VALUE 0.

       01  TIT-09.
           03  FILLER                    PIC X(13)  VALUE
                                             'NOTA MAXIMA: '.
           03  WS-MAX-NOTA-ED            PIC ZZ9.
           03  FILLER                    PIC X(09)  VALUE
                                             ' ALUMNO: '.
           03  WS-MAX-ALUMNO             PIC X(20).
           03  FILLER                    PIC X(15)  VALUE
                                             ' FECHA EXAMEN: '.
           03  WS-MAX-FECHA.
               05  WS-MAX-FECHA-AAAA     PIC 9(04)  VALUE 0.
               05  FILLER                PIC X(01)  VALUE '/'.
               05  WS-MAX-FECHA-MM       PIC 9(02)  VALUE 0.
               05  FILLER                PIC X(01)  VALUE '/'.
               05  WS-MAX-FECHA-DD       PIC 9(02)  VALUE 0.

       01  TIT-10.
           03  FILLER                    PIC X(15)  VALUE
                                            'NOTA PROMEDIO: '.
           03  TOTAL-NOTA-PROMEDIO       PIC ZZ9.
           03  FILLER                    PIC X(114) VALUE SPACES.

      *------------------------------------*
      * DEFINICION DE ARCHIVOS             *
      *------------------------------------*
       COPY WSTCALIF.
       COPY WMATERIA.
       COPY WALUMNOS.

       PROCEDURE DIVISION.
      *------------------

       00000-CUERPO-PRINCIPAL.
      *-----------------------

           PERFORM 10000-INICIO.

           PERFORM 20000-PROCESO
             UNTIL 88-FS-STCALIF-EOF.

           PERFORM 30000-FINALIZO.

           STOP RUN.

       10000-INICIO.
      *-------------

           PERFORM 10100-ABRO-ARCHIVOS.

           PERFORM 10200-1ERA-LECTURA-STCALIF.

       10100-ABRO-ARCHIVOS.
      *-------------------

           OPEN INPUT STCALIF.

           EVALUATE FS-STCALIF
               WHEN '00'
                    SET 88-OPEN-STCALIF-SI TO TRUE

               WHEN OTHER
                    DISPLAY '--------------------------------'
                    DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                    DISPLAY 'PARRAFO: 10100-ABRO-ARCHIVOS'
                    DISPLAY 'ERROR EN OPEN INPUT STCALIF'
                    DISPLAY 'FILE STATUS       :' FS-STCALIF
                    DISPLAY 'SE CANCELA EL PROCESO'
                    DISPLAY '--------------------------------'
                    PERFORM 99999-CANCELO
           END-EVALUATE.
      *----------------------------------------------------------

           OPEN I-O     MATERIAS.

           EVALUATE FS-MATERIAS
               WHEN '00'
                   SET 88-OPEN-MATERIAS-SI TO TRUE

                WHEN OTHER
                   DISPLAY '--------------------------------'
                   DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                   DISPLAY 'PARRAFO: 10100-ABRO-ARCHIVOS'
                   DISPLAY 'ERROR EN OPEN I-O   MATERIAS'
                   DISPLAY 'FILE STATUS       :' FS-MATERIAS
                   DISPLAY 'SE CANCELA EL PROCESO'
                   DISPLAY '--------------------------------'
                   PERFORM 99999-CANCELO
           END-EVALUATE.
      *----------------------------------------------------------

           OPEN I-O     ALUMNOS.

           EVALUATE FS-ALUMNOS
               WHEN '00'
                   SET 88-OPEN-ALUMNOS-SI  TO TRUE

                WHEN OTHER
                   DISPLAY '--------------------------------'
                   DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                   DISPLAY 'PARRAFO: 10100-ABRO-ARCHIVOS'
                   DISPLAY 'ERROR EN OPEN I-O   MATERIAS'
                   DISPLAY 'FILE STATUS       :' FS-ALUMNOS
                   DISPLAY 'SE CANCELA EL PROCESO'
                   DISPLAY '--------------------------------'
                   PERFORM 99999-CANCELO
           END-EVALUATE.
      *----------------------------------------------------------

           OPEN OUTPUT  LISTADO.

           EVALUATE FS-LISTADO
               WHEN '00'
                    SET 88-OPEN-LISTADO-SI TO TRUE

               WHEN OTHER
                    DISPLAY '--------------------------------'
                    DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                    DISPLAY 'PARRAFO: 10100-ABRO-ARCHIVOS'
                    DISPLAY 'ERROR EN OPEN INPUT LISTADO'
                    DISPLAY 'FILE STATUS       :' FS-LISTADO
                    DISPLAY 'SE CANCELA EL PROCESO'
                    DISPLAY '--------------------------------'
                    PERFORM 99999-CANCELO
           END-EVALUATE.

       10200-1ERA-LECTURA-STCALIF.
      *--------------------------
           PERFORM 11100-READ-STCALIF.

           EVALUATE TRUE
               WHEN 88-FS-STCALIF-OK
                    MOVE STCALIF-NOTA           TO WS-MIN-NOTA
                                                   WS-MAX-NOTA
                    MOVE STCALIF-COD-ESTUDIANTE TO WS-MIN-ALUMNO
                                                   WS-MAX-ALUMNO
                    MOVE STCALIF-FECHA-EXAMEN   TO WS-MAX-FECHA
                                                   WS-MIN-FECHA
               WHEN OTHER
                    CONTINUE
           END-EVALUATE.

       11100-READ-STCALIF.
      *-------------------

           INITIALIZE STCALIF-RECORD.

           READ STCALIF INTO STCALIF-RECORD.

           EVALUATE TRUE
               WHEN 88-FS-STCALIF-OK
                    ADD 1              TO WS-LEIDOS-STCALIF
                    DISPLAY 'LEYO    ' STCALIF-COD-ESTUDIANTE

               WHEN 88-FS-STCALIF-EOF
                    DISPLAY 'EOF     ' STCALIF-COD-ESTUDIANTE

               WHEN OTHER
                    DISPLAY '--------------------------------'
                    DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                    DISPLAY 'PARRAFO: 11100-READ-STCALIF '
                    DISPLAY 'ERROR EN READ       STCALIF'
                    DISPLAY 'FILE STATUS       :' FS-STCALIF
                    DISPLAY 'SE CANCELA EL PROCESO'
                    DISPLAY '--------------------------------'
                    PERFORM 99999-CANCELO

           END-EVALUATE.


       20000-PROCESO.
      *-------------

           PERFORM 21000-IMPRIMO-DETALLE.

           IF  STCALIF-NOTA < WS-MIN-NOTA
               MOVE STCALIF-NOTA         TO WS-MIN-NOTA
               MOVE STCALIF-COD-ESTUDIANTE
                                        TO WS-MIN-ALUMNO
               MOVE STCALIF-FECHA-EXAMEN
                                        TO WS-MIN-FECHA
           END-IF.

           IF  STCALIF-NOTA > WS-MAX-NOTA
               MOVE STCALIF-NOTA         TO WS-MAX-NOTA
               MOVE STCALIF-COD-ESTUDIANTE
                                        TO WS-MAX-ALUMNO
               MOVE STCALIF-FECHA-EXAMEN
                                        TO WS-MAX-FECHA
           END-IF.

           PERFORM 11100-READ-STCALIF.

       21000-IMPRIMO-DETALLE.
      *------------------------

           PERFORM 21010-RECUPERO-DATA-ALUMNOS.
           PERFORM 21020-RECUPERO-DATA-MATERIAS.
           PERFORM 21100-CONTROL-DE-LINEA.

           MOVE STCALIF-NOTA              TO ESTUDIANTE-NOTA.
           MOVE STCALIF-FECHA-EXAMEN-AAAA TO ESTUDIANTE-FECHA-AAAA.
           MOVE STCALIF-FECHA-EXAMEN-MM   TO ESTUDIANTE-FECHA-MM.
           MOVE STCALIF-FECHA-EXAMEN-DD   TO ESTUDIANTE-FECHA-DD.

           IF  STCALIF-NOTA > WS-LIMITE-NOTA
                      MOVE 'APROBADO'     TO ALU-OBSERVACION
           ELSE
                      MOVE 'REPROBADO'    TO ALU-OBSERVACION
           END-IF.

           MOVE INFO-ESTUDIANTE          TO REG-LISTADO.
           PERFORM  21300-ESCRIBO-LISTADO.
           ADD 1                          TO WS-LINEA.

       21010-RECUPERO-DATA-ALUMNOS.
      *------------------------

           INITIALIZE ALU-REGISTRO
                      NRO-LEGAJO-ALUMNO-FD.

           MOVE STCALIF-COD-ESTUDIANTE    TO ALUMNO-COD-FD
                                             ALU-CODIGO.

           READ ALUMNOS INTO ALU-REGISTRO.

           EVALUATE TRUE
                WHEN  88-FS-ALUMNOS-OK
                      MOVE    ' '         TO ESTUDIANTE-NOMBRE
                      STRING
                             ALU-NOMBRE   DELIMITED BY '  '
                             ' '          DELIMITED BY SIZE
                             ALU-APELLIDO DELIMITED BY '  '
                         INTO
                             ESTUDIANTE-NOMBRE
                      END-STRING
                WHEN  88-FS-ALUMNOS-NOKEY
                      MOVE  'ALUMNO NO ENCONTRADO'
                                          TO ESTUDIANTE-NOMBRE
                WHEN OTHER
                      DISPLAY '--------------------------------'
                      DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                      DISPLAY 'PARRAFO: 21010-RECUPERO-DATA-ALUMNOS '
                      DISPLAY 'ERROR EN RECUPERO DATA ALUMNOS'
                      DISPLAY 'FILE STATUS       :' FS-ALUMNOS
                      DISPLAY 'SE CANCELA EL PROCESO'
                      DISPLAY '--------------------------------'
                      PERFORM 99999-CANCELO
           END-EVALUATE.

       21020-RECUPERO-DATA-MATERIAS.
      *------------------------

           INITIALIZE MATERIAS-REGISTRO
                      COD-MATERIAS-DATOS-FD.

           MOVE STCALIF-COD-MATERIA       TO COD-MATERIAS-FD
                                             MATERIAS-CODIGO.

           READ MATERIAS INTO MATERIAS-REGISTRO.

           EVALUATE TRUE
                WHEN  88-FS-MATERIAS-OK
                      MOVE  MATERIA-NOMBRE TO ESTUDIANTE-MATERIA
                WHEN  88-FS-MATERIAS-NOKEY
                      MOVE  'MATERIA NO ENCONTRADA'
                                            TO ESTUDIANTE-MATERIA
                WHEN OTHER
                      DISPLAY '--------------------------------'
                      DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                      DISPLAY 'PARRAFO: 21020-RECUPERO-DATA-MATERIAS '
                      DISPLAY 'ERROR EN RECUPERO DATA MATERIAS'
                      DISPLAY 'FILE STATUS       :' FS-MATERIAS
                      DISPLAY 'SE CANCELA EL PROCESO'
                      DISPLAY '--------------------------------'
                      PERFORM 99999-CANCELO
           END-EVALUATE.

       21100-CONTROL-DE-LINEA.
      *------------------------

           IF  WS-LINEA > WS-LINEAS-MAX
               PERFORM 21200-IMPRIMO-TITULOS
           END-IF.

       21200-IMPRIMO-TITULOS.
      *-----------------------

           SET  88-SALTO-HOJA-SI TO TRUE.
           PERFORM  21220-FECHA-ACTUAL
           ADD  1 TO WS-PAGINA.
           MOVE WS-PAGINA TO TIT-2-PAG.

           MOVE  TIT-01 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-02 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-03 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-04 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-05 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE 5 TO WS-LINEA.

       21220-FECHA-ACTUAL.
      *-----------------

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.

           MOVE WS-CURRENT-DATE-YYYY  TO TIT01-FECHA-AAAA
           MOVE WS-CURRENT-DATE-MM    TO TIT01-FECHA-MM
           MOVE WS-CURRENT-DATE-DD    TO TIT01-FECHA-DD
           MOVE WS-CURRENT-DATE-HS    TO TIT02-HORA-HH
           MOVE WS-CURRENT-DATE-MS    TO TIT02-HORA-MM
           MOVE WS-CURRENT-DATE-SS    TO TIT02-HORA-SS.

       21300-ESCRIBO-LISTADO.
      *---------------------

           IF 88-SALTO-HOJA-SI
              SET 88-SALTO-HOJA-NO    TO TRUE
              WRITE REG-LISTADO-FD FROM REG-LISTADO AFTER PAGE
           ELSE
              WRITE REG-LISTADO-FD FROM REG-LISTADO AFTER 1 LINE

           EVALUATE TRUE
              WHEN 88-FS-LISTADO-OK
                   ADD 1              TO  WS-GRABADOS-LISTADO

              WHEN OTHER
                   DISPLAY '--------------------------------'
                   DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                   DISPLAY 'PARRAFO: 21300-ESCRIBO-LISTADO'
                        DISPLAY 'ERROR EN ESCRIBO-LISTADO'
                        DISPLAY 'FILE STATUS       :' FS-LISTADO
                        DISPLAY 'SE CANCELA EL PROCESO'
                        DISPLAY '--------------------------------'
                        PERFORM 99999-CANCELO
           END-EVALUATE.

       30000-FINALIZO.
      *--------------

           PERFORM 30100-TOTALES-CONTROL.

           PERFORM 31000-CIERRO-ARCHIVOS.

       30100-TOTALES-CONTROL.
      *---------------------

           MOVE WS-LEIDOS-STCALIF     TO WS-LEIDOS-STCALIF-ED
           IF WS-LEIDOS-STCALIF > 0
              COMPUTE
                WS-NOTA-PROMEDIO = (WS-ACUM-NOTAS / WS-LEIDOS-STCALIF)
           END-IF.
           MOVE WS-NOTA-PROMEDIO      TO WS-NOTA-PROMEDIO-ED.
           DISPLAY ' '.
           DISPLAY '*******************************************'
           DISPLAY 'TOTALES DE CONTROL PGM: BOPGM01            '
           DISPLAY '*******************************************'
           DISPLAY '*                                         *'
           DISPLAY '* CANT. REGISTROS LEIDOS STCALIF   :       '
                                          WS-LEIDOS-STCALIF-ED
           DISPLAY '* DATOS DE LA NOTA MINIMA            '
           DISPLAY '* COD ESTUDIANTE :'   WS-MIN-ALUMNO
           DISPLAY '* NOTA           :'   WS-MIN-NOTA
           DISPLAY '* FECHA          :'   WS-MIN-FECHA(7:2)
                                          '-'
                                          WS-MIN-FECHA(5:2)
                                          '-'
                                          WS-MIN-FECHA(1:4)
           DISPLAY '*                                         *'
           DISPLAY '* DATOS DE LA NOTA MAXIMA                  '
           DISPLAY '* COD ESTUDIANTE :'   WS-MAX-ALUMNO
           DISPLAY '* NOTA           :'   WS-MAX-NOTA
           DISPLAY '* FECHA          :'   WS-MAX-FECHA(7:2)
                                          '-'
                                          WS-MAX-FECHA(5:2)
                                          '-'
                                          WS-MAX-FECHA(1:4)
           DISPLAY '* PROMEDIO DE NOTAS:' WS-NOTA-PROMEDIO-ED.
           DISPLAY '*                                          *'
           DISPLAY '*******************************************'
           DISPLAY ' '.

           PERFORM 30210-ORG-ESTRUCTURA-PAG.

       30210-ORG-ESTRUCTURA-PAG.
      *------------------------

           PERFORM 21220-FECHA-ACTUAL
           ADD   1           TO WS-PAGINA.
           MOVE  WS-PAGINA   TO TIT-2-PAG.

           MOVE  TIT-03      TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-01      TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-02      TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-03      TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  WS-MIN-NOTA TO WS-MIN-NOTA-ED
           MOVE  WS-MAX-NOTA TO WS-MAX-NOTA-ED
           MOVE  WS-NOTA-PROMEDIO  TO TOTAL-NOTA-PROMEDIO.

           STRING
                  WS-MIN-FECHA-AAAA    DELIMITED BY ' '
                  '/'                  DELIMITED BY SIZE
                  WS-MIN-FECHA-MM      DELIMITED BY ' '
                  '/'                  DELIMITED BY SIZE
                  WS-MIN-FECHA-DD      DELIMITED BY '  '
              INTO
                  WS-MIN-FECHA
           END-STRING.
           STRING
                  WS-MAX-FECHA-AAAA    DELIMITED BY ' '
                  '/'                  DELIMITED BY SIZE
                  WS-MAX-FECHA-MM      DELIMITED BY ' '
                  '/'                  DELIMITED BY SIZE
                  WS-MAX-FECHA-DD      DELIMITED BY '  '
              INTO
                  WS-MAX-FECHA
           END-STRING.

           MOVE  TIT-06 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-07 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-08 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO

           MOVE  TIT-09 TO REG-LISTADO.
           PERFORM 21300-ESCRIBO-LISTADO.

       31000-CIERRO-ARCHIVOS.
      *---------------------

           IF 88-OPEN-STCALIF-SI
              SET 88-OPEN-STCALIF-NO  TO TRUE
              CLOSE STCALIF
              EVALUATE TRUE
                  WHEN 88-FS-STCALIF-OK
                       CONTINUE
                  WHEN OTHER
                       DISPLAY '--------------------------------'
                       DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                       DISPLAY 'PARRAFO: 31000-CIERRO-ARCHIVOS'
                       DISPLAY 'ERROR EN CLOSE STCALIF'
                       DISPLAY 'FILE STATUS       :' FS-STCALIF
                       DISPLAY 'SE CANCELA EL PROCESO'
                       DISPLAY '--------------------------------'
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-MATERIAS-SI
               CLOSE MATERIAS
               SET 88-OPEN-MATERIAS-NO TO TRUE
               EVALUATE TRUE
                   WHEN 88-FS-MATERIAS-OK
                        CONTINUE
                   WHEN OTHER
                        DISPLAY '--------------------------------'
                        DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                        DISPLAY 'PARRAFO: 31000-CIERRO-ARCHIVOS'
                        DISPLAY 'ERROR EN CLOSE MATERIAS'
                        DISPLAY 'FILE STATUS       :' FS-MATERIAS
                        DISPLAY 'SE CANCELA EL PROCESO'
                        DISPLAY '--------------------------------'
                        PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-ALUMNOS-SI
               CLOSE ALUMNOS
               SET 88-OPEN-ALUMNOS-NO TO TRUE
               EVALUATE TRUE
                   WHEN 88-FS-ALUMNOS-OK
                        CONTINUE
                   WHEN OTHER
                        DISPLAY '--------------------------------'
                        DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                        DISPLAY 'PARRAFO: 31000-CIERRO-ARCHIVOS'
                        DISPLAY 'ERROR EN CLOSE ALUMNOS'
                        DISPLAY 'FILE STATUS       :' FS-ALUMNOS
                        DISPLAY 'SE CANCELA EL PROCESO'
                        DISPLAY '--------------------------------'
                        PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

           IF 88-OPEN-LISTADO-SI
              SET 88-OPEN-LISTADO-NO  TO TRUE
              CLOSE LISTADO
              EVALUATE TRUE
                  WHEN 88-FS-LISTADO-OK
                       CONTINUE
                  WHEN OTHER
                       DISPLAY '--------------------------------'
                       DISPLAY 'PROGRAMA          :' CTE-PROGRAMA
                       DISPLAY 'PARRAFO: 31000-CIERRO-ARCHIVOS'
                       DISPLAY 'ERROR EN CLOSE LISTADO'
                       DISPLAY 'FILE STATUS       :' FS-LISTADO
                       DISPLAY 'SE CANCELA EL PROCESO'
                       DISPLAY '--------------------------------'
                       PERFORM 99999-CANCELO
              END-EVALUATE
           END-IF.

       99999-CANCELO.

           PERFORM 31000-CIERRO-ARCHIVOS.

           STOP RUN.
