      ******************************************************************
      * Author: MARIA EMILIA LUDUENA
      * Date:
      * Purpose: CREAR Y POBLAR EL VSAM DE MATERIAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAMALUM.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
      *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS         ASSIGN       TO
            'C:\Users\Lenovo\Desktop\HOST-COBOL\EJERCICIOS\ALUMNOS.DAT'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS SEQUENTIAL
                                   RECORD KEY   IS COD-ALUMNOS-FD
                                   FILE STATUS  IS FS-ALUMNOS.
       DATA DIVISION.
       FILE SECTION.

       FD  ALUMNOS.
       01  NRO-LEGAJO-ALUMNO-FD.
           03  COD-ALUMNOS-FD.
               05  ALUMNO-COD-FD         PIC 9(06).
           03  FILLER                    PIC X(114).

       WORKING-STORAGE SECTION.

       77  FS-ALUMNOS                    PIC X(02)  VALUE ' '.
           88 88-FS-ALUMNOS-OK                      VALUE '00'.
           88 88-FS-ALUMNOS-EOF                     VALUE '10'.
           88 88-FS-ALUMNOS-INVKEY                  VALUE '21'.
           88 88-FS-ALUMNOS-DUPKEY                  VALUE '22'.
           88 88-FS-ALUMNOS-NOKEY                   VALUE '23'.

       77  WS-REG-ALUMNOS-GRABADOS       PIC 9(09)  VALUE 0.

      *------------------------------------
      *DEFINICION DEL ARCHIVO DE ALUMNOS
      *------------------------------------

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

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN OUTPUT ALUMNOS.

           EVALUATE FS-ALUMNOS
               WHEN '00'
                    CONTINUE

               WHEN OTHER
                    DISPLAY 'ERROR OPEN ALUMNOS FS: ' FS-ALUMNOS
                    STOP RUN
           END-EVALUATE.

      * GRABACIONES DE REGISTROS
      *-------------------------
           INITIALIZE ALU-REGISTRO
           MOVE 028445                    TO ALU-CODIGO.
           MOVE 'LUDUENA'                 TO ALU-APELLIDO.
           MOVE 'EMILIA'                  TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 061359                    TO ALU-CODIGO.
           MOVE 'FALLOTICO'               TO ALU-APELLIDO.
           MOVE 'AGUSTINA'                TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 130891                    TO ALU-CODIGO.
           MOVE 'ROMERO'                  TO ALU-APELLIDO.
           MOVE 'CONSTANZA '              TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 513241                    TO ALU-CODIGO.
           MOVE 'GOMEZ'                   TO ALU-APELLIDO.
           MOVE 'JULIETA'                 TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 551216                    TO ALU-CODIGO.
           MOVE 'SAYAGO'                  TO ALU-APELLIDO.
           MOVE 'CAMILA '                 TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.

           INITIALIZE ALU-REGISTRO
           MOVE 591981                    TO ALU-CODIGO.
           MOVE 'GONZALEZ'                TO ALU-APELLIDO.
           MOVE 'FEDERICO'                TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 664418                    TO ALU-CODIGO.
           MOVE 'PEREZ'                   TO ALU-APELLIDO.
           MOVE 'TOMAS'                   TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 701579                    TO ALU-CODIGO.
           MOVE 'MONDINO'                 TO ALU-APELLIDO.
           MOVE 'MATIAS'                  TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 785457                    TO ALU-CODIGO.
           MOVE 'CABRERA'                 TO ALU-APELLIDO.
           MOVE 'NICOLAS'                 TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           INITIALIZE ALU-REGISTRO
           MOVE 959810                    TO ALU-CODIGO.
           MOVE 'ALVAREZ'                 TO ALU-APELLIDO.
           MOVE 'PABLO'                   TO ALU-NOMBRE.
           PERFORM 100-WRITE-ALUMNOS.


           CLOSE ALUMNOS.

           EVALUATE FS-ALUMNOS
              WHEN '00'
                   CONTINUE

              WHEN OTHER
                   DISPLAY 'ERROR CLOSE ALUMNOS FS: ' FS-ALUMNOS
                   STOP RUN

           END-EVALUATE.

           STOP RUN.

       100-WRITE-ALUMNOS.

           WRITE COD-ALUMNOS-FD               FROM ALU-REGISTRO.

           EVALUATE FS-ALUMNOS
               WHEN '00'
                    ADD 1 TO WS-REG-ALUMNOS-GRABADOS

               WHEN OTHER
                    DISPLAY 'ERROR WRITE MATERIAS FS: ' FS-ALUMNOS
                    STOP RUN
           END-EVALUATE.
