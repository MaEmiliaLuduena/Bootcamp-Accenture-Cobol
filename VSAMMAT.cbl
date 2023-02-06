      ******************************************************************
      * Author: MARIA EMILIA LUDUENA
      * Date:
      * Purpose: CREAR Y POBLAR EL VSAM DE MATERIAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAMMAT.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
      *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATERIAS         ASSIGN       TO
            'C:\Users\Lenovo\Desktop\HOST-COBOL\EJERCICIOS\MATERIAS.DAT'
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE  IS SEQUENTIAL
                                   RECORD KEY   IS COD-MATERIAS-FD
                                   FILE STATUS  IS FS-MATERIAS.
       DATA DIVISION.
       FILE SECTION.

       FD  MATERIAS.
       01  COD-MATERIAS-DATOS-FD.
           03  COD-MATERIAS-FD           PIC X(08).
           03  FILLER                    PIC X(72).

       WORKING-STORAGE SECTION.

       77  FS-MATERIAS                   PIC X(02)  VALUE ' '.
           88 88-FS-MATERIAS-OK                     VALUE '00'.
           88 88-FS-MATERIAS-EOF                    VALUE '10'.
           88 88-FS-MATERIAS-INVKEY                 VALUE '21'.
           88 88-FS-MATERIAS-DUPKEY                 VALUE '22'.
           88 88-FS-MATERIAS-NOKEY                  VALUE '23'.

       77  WS-REG-MATERIAS-GRABADOS      PIC 9(09) VALUE 0.

      *------------------------------------
      *DEFINICION DEL ARCHIVO DE MATERIAS
      *------------------------------------

       01 MATERIAS-REGISTRO.
           03 MATERIAS-CLAVE.
               05 MATERIAS-CODIGO             PIC X(08).
           03 MATERIAS-NOMBRE                 PIC X(50).
           03 FILLER                          PIC X(22).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN OUTPUT MATERIAS.

           EVALUATE FS-MATERIAS
               WHEN '00'
                    CONTINUE

               WHEN OTHER
                    DISPLAY 'ERROR OPEN MATERIAS FS: ' FS-MATERIAS
                    STOP RUN
           END-EVALUATE.

      * GRABACIONES DE REGISTROS
      *-------------------------
           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH2581'                       TO MATERIAS-CODIGO.
           MOVE 'CALCULO AVANZADO'               TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.


           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH2989'                       TO MATERIAS-CODIGO.
           MOVE 'MATEMATICA DISCRETA'            TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.


           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH3909'                       TO MATERIAS-CODIGO.
           MOVE 'SISTEMAS DE INFORMACION'        TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.


           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH4755'                       TO MATERIAS-CODIGO.
           MOVE 'SISTEMAS OPERATIVOS'            TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.


           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH5103'                       TO MATERIAS-CODIGO.
           MOVE 'ARQUITECTURA DEL ORDENADOR'     TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.

           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH6535'                       TO MATERIAS-CODIGO.
           MOVE 'INTRODUCCION A LA PROGRAMACION' TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.

           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH6633'                       TO MATERIAS-CODIGO.
           MOVE 'PROGRAMACION WEB'               TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.

           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH6857'                       TO MATERIAS-CODIGO.
           MOVE 'INGLES'                         TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.

           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH7897'                       TO MATERIAS-CODIGO.
           MOVE 'PRACTICA PROFESIONALIZANTE'     TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.

           INITIALIZE MATERIAS-REGISTRO
           MOVE 'MATH9076'                       TO MATERIAS-CODIGO.
           MOVE 'BASE DE DATOS'                  TO MATERIAS-NOMBRE.
           PERFORM 100-WRITE-MATERIAS.

           CLOSE MATERIAS.

           EVALUATE FS-MATERIAS
              WHEN '00'
                   CONTINUE

              WHEN OTHER
                   DISPLAY 'ERROR CLOSE MATERIAS FS: ' FS-MATERIAS
                   STOP RUN

           END-EVALUATE.

           STOP RUN.

       100-WRITE-MATERIAS.

           WRITE COD-MATERIAS-FD              FROM MATERIAS-REGISTRO.

           EVALUATE FS-MATERIAS
               WHEN '00'
                    ADD 1 TO WS-REG-MATERIAS-GRABADOS

               WHEN OTHER
                    DISPLAY 'ERROR WRITE MATERIAS FS: ' FS-MATERIAS
                    STOP RUN
           END-EVALUATE.
