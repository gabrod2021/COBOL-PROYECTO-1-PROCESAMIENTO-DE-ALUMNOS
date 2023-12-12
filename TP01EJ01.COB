      *----------------------------------------------------------------*
      *Author:  GABRIELA RODRIGUEZ                                     *
      *         CLAUDIA SANTILLAN                                      *
      *         GABY OSELLA                                            *
      *         GUILLERMO RODRIGUEZ                                    *
      *         EDDISON MORA GOMEZ                                     *
      *Date:    12/10/2023
      *Purpose: TRABAJO PRACTICO NUMERO 1 (TP1)
      *                                                                *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. TP01EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ALUMNOS
           ASSIGN TO '../ALUMNOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ALUMNOS.

       SELECT DESCARTADOS
           ASSIGN TO '../DESCARTADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-DESCARTADOS.

       SELECT ERRORES
           ASSIGN TO '../ERRORES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERRORES.

       SELECT HONOR
           ASSIGN TO '../HONOR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-HONOR.

       SELECT PROMEDIOS
           ASSIGN TO '../PROMEDIOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PROMEDIOS.


       DATA DIVISION.

       FILE SECTION.

       COPY "ALUMNOS.CPY".

       COPY "DESCARTADOS.CPY".

       COPY "ERRORES.CPY".

       COPY "HONOR.CPY".

       COPY "PROMEDIOS.CPY".

       WORKING-STORAGE SECTION.

       01 WS-CONTADORES.
          05 WS-CONT-ALUMNOS            PIC 9(05) VALUE 0.
          05 WS-CONT-MATERIAS           PIC 9(05) VALUE 0.
          05 WS-CONT-NOTAS              PIC 9(05) VALUE 0.
          05 WS-CONT-ALUMPROM           PIC 9(05) VALUE 0.
          05 WS-CANT-DESCARTADOS        PIC 9(05) VALUE 0.
          05 WS-CANT-ALUM-ERROR         PIC 9(05) VALUE 0.
          05 WS-CONT-ALUM-HONOR         PIC 9(05) VALUE 0.
          05 WS-CONT-REG-LEIDOS         PIC 9(05) VALUE 0.

       01 WS-CORTE-CONTROL.
          05 WS-CC-ALUM-ANT                PIC X(40).
          05 WS-CC-MATE-ANT                PIC X(30).

       01 WS-ACUMULADORES.
          05 WS-CC-NOTA-ACUM                PIC 9(5)V9(3).
          05 WS-CC-MAY-PROM                 PIC 9(5)V9(3).


       01 WS-VARIABLES-AUX.

          05 WS-PROMEDIO                 PIC 9(5)V9(3).
          05 WS-NOM-MAX                  PIC X(40).
          05 WS-MAT-MAX                  PIC X(30).
          05 WS-MAT-EVA                  PIC X(30).
          05 WS-TOTAL-PROCESADOS         PIC 9(5).
          05 WS-PROM-AUX           PIC ZZ.ZZ9,999.
          05 WS-INDICE                   PIC 9(2).

       01  FS-STATUS.
          05 FS-ALUMNOS                          PIC X(2).
             88 FS-ALUMNOS-OK                    VALUE '00'.
             88 FS-ALUMNOS-EOF                   VALUE '10'.
             88 FS-ALUMNOS-NFD                   VALUE '35'.

          05 FS-PROMEDIOS                        PIC X(2).
             88 FS-PROMEDIOS-OK                  VALUE '00'.
             88 FS-PROMEDIOS-EOF                 VALUE '10'.
             88 FS-PROMEDIOS-NFD                 VALUE '35'.

          05  FS-DESCARTADOS                      PIC X(2).
             88 FS-DESCARTADOS-OK                 VALUE '00'.
             88 FS-DESCARTADOS-EOF                VALUE '10'.
             88 FS-DESCARTADOS-NFD                VALUE '35'.

          05  FS-ERRORES                         PIC X(2).
             88 FS-ERRORES-OK                    VALUE '00'.
             88 FS-ERRORES-EOF                   VALUE '10'.
             88 FS-ERRORES-NFD                   VALUE '35'.

          05  FS-HONOR                           PIC X(2).
             88 FS-HONOR-OK                      VALUE '00'.
             88 FS-HONOR-EOF                     VALUE '10'.
             88 FS-HONOR-NFD                     VALUE '35'.

      ******************************************************************
      *    TABLA CUADRO DE HONOR.

       01  WS-TABLA.
          03 WS-TABLA-HONOR  OCCURS 20.
              05 WS-TABLA-MAT     PIC X(30).
              05 WS-TABLA-NOMBRE  PIC X(40).
              05 WS-TABLA-PROM    PIC 9(5)V9(3) VALUE ZERO.



      *----------------------------------------------------------------*
      *****LISTADOS:----------------------------------------------------*


       01 WS-LISTADO-PROMEDIO.
          05 WS-LIS-SEPARADOR           PIC X(86) VALUE ALL '='.

          05 WS-LIS-HEADER.
             10 FILLER             PIC X(40) VALUE 'Apellido y Nombre'.
             10 FILLER             PIC X(03) VALUE ' | '.
             10 FILLER             PIC X(30) VALUE 'Materia'.
             10 FILLER             PIC X(03) VALUE ' | '.
             10 FILLER             PIC X(8) VALUE 'Promedio'.
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-APELLNOM           PIC X(40).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-MATERIA           PIC X(30).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-PROMEDIO           PIC ZZ.ZZ9,999.
          05 WS-LIS-TOTALES-1.
             10 FILLER  PIC X(35) VALUE 'Cantidad de registros leidos:'.
             10 WS-LIS-D-LEIDOS             PIC 9(5).
          05 WS-LIS-TOTALES-2.
             10 FILLER  PIC X(35)
                VALUE 'Cantidad de registros procesados: '.

             10 WS-LIS-D-CANT-REG-PROC         PIC 9(05).
          05 WS-LIS-TOTALES-3.
             10 FILLER  PIC X(35)
               VALUE 'Cantidad de registros descartados: '.

             10 WS-LIS-D-DESCARTADOS       PIC 9(05).
          05 WS-LIS-TOTALES-4.
             10 FILLER  PIC X(35)
             VALUE 'Cantidad de registros con error: '.

             10 WS-LIS-D-ERROR        PIC 9(05).

      ******************************************************************

       01 WS-LISTADO-HONOR.
          05 WS-LIS-SEPARADOR-2            PIC X(86) VALUE ALL '='.

          05 WS-LIS-HEADER2.
             10 FILLER             PIC X(30) VALUE 'Materia'.
             10 FILLER             PIC X(03) VALUE ' | '.
            10 FILLER             PIC X(40) VALUE '  Apellido y Nombre'.
             10 FILLER             PIC X(03) VALUE ' | '.
             10 FILLER             PIC X(10) VALUE '  Promedio'.
          05 WS-LIS-DETALLE2.
             10 WS-LIS-D-MATERIAH           PIC X(30).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-APELLNOMH           PIC X(40).
             10 FILLER                     PIC X(03) VALUE ' | '.
             10 WS-LIS-D-PROMEDIOH           PIC ZZ.ZZ9,999.

      ******************************************************************

       01 WS-LISTADO-DESCARTADOS.
          05 WS-LIS-SEPARADOR-3    PIC X(86) VALUE ALL '='.

         05 WS-LIS-HEADER3.
             10 FILLER PIC X(40) VALUE 'Apellido y Nombre'.
             10 FILLER             PIC X(03) VALUE ' | '.
             10 FILLER             PIC X(30) VALUE 'Materia'.
             10 FILLER             PIC X(03) VALUE ' | '.
             10 FILLER             PIC X(10) VALUE 'Cantidad'.
          05 WS-LIS-DETALLE3.
             10 WS-LIS-D-APELLNOMD         PIC X(40).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-MATERIAD            PIC X(30).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-CANTIDAD           PIC 9(2).
      ******************************************************************

      *----------------------------------------------------------------*

       PROCEDURE DIVISION.

             PERFORM 1000-INICIALIZAR-PROGRAMA
                THRU 1000-INICIALIZAR-PROGRAMA-EXIT.

               IF FS-ALUMNOS-OK

                   PERFORM 2000-PROCESAR-ARCHIVO
                   THRU 2000-PROCESAR-ARCHIVO-EXIT
                   UNTIL FS-ALUMNOS-EOF

                   PERFORM 2660-MOVER-DATOS-HONOR
                   THRU 2660-MOVER-DATOS-HONOR-EXIT

               END-IF.

               PERFORM 3000-FINALIZAR-PROGRAMA
                THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*

       1000-INICIALIZAR-PROGRAMA.


             INITIALIZE WS-CONTADORES WS-ACUMULADORES.

               PERFORM 1100-ABRIR-ARCH-ALUMNOS
                THRU 1100-ABRIR-ARCH-ALUMNOS-EXIT.

               PERFORM 1200-ABRIR-ARCH-PROMEDIOS
                THRU 1200-ABRIR-ARCH-PROMEDIOS-EXIT.

               PERFORM 1300-ABRIR-ARCH-HONOR
                THRU 1300-ABRIR-ARCH-HONOR-EXIT.

               PERFORM 1400-ABRIR-ARCH-ERROR
                THRU 1400-ABRIR-ARCH-ERROR-EXIT.

               PERFORM 1500-ABRIR-AR-DESCARTADOS
                THRU 1500-ABRIR-AR-DESCARTADOS-EXIT.


         1000-INICIALIZAR-PROGRAMA-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       1100-ABRIR-ARCH-ALUMNOS.

           OPEN INPUT ALUMNOS.

           EVALUATE TRUE
               WHEN FS-ALUMNOS-OK
                    PERFORM 1600-LEER-ALUMNOS
                       THRU 1600-LEER-ALUMNOS-EXIT
               WHEN FS-ALUMNOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO ALUMNOS'
                    DISPLAY 'FILE STATUS: ' FS-ALUMNOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ALUMNOS
           END-EVALUATE.

       1100-ABRIR-ARCH-ALUMNOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       1200-ABRIR-ARCH-PROMEDIOS.

           OPEN OUTPUT PROMEDIOS.

           EVALUATE TRUE
               WHEN FS-PROMEDIOS-OK

                   PERFORM 2450-MOVER-TIT-PROMEDIO
                     THRU 2450-MOVER-TIT-PROMEDIO-EXIT

               WHEN FS-PROMEDIOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE PROMEDIOS'
                     DISPLAY 'FILE STATUS: ' FS-PROMEDIOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-PROMEDIOS
                    STOP RUN
           END-EVALUATE.

       1200-ABRIR-ARCH-PROMEDIOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       1300-ABRIR-ARCH-HONOR.

           OPEN OUTPUT HONOR.

           EVALUATE TRUE
               WHEN FS-HONOR-OK
                  PERFORM 2630-MOVER-TIT-HONOR
                  THRU 2630-MOVER-TIT-HONOR-EXIT
                    WHEN FS-HONOR-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-HONOR
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-HONOR
           END-EVALUATE.

       1300-ABRIR-ARCH-HONOR-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       1400-ABRIR-ARCH-ERROR.

           OPEN OUTPUT ERRORES.

           EVALUATE TRUE
               WHEN FS-ERRORES-OK
               CONTINUE
               WHEN FS-ERRORES-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
               WHEN OTHER
                    PERFORM 2700-GRABAR-ARCHIVO-ERROR
                    THRU 2700-GRABAR-ARCH-ERROR-EXIT
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
           END-EVALUATE.

       1400-ABRIR-ARCH-ERROR-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       1500-ABRIR-AR-DESCARTADOS.

           OPEN OUTPUT DESCARTADOS.

            EVALUATE FS-DESCARTADOS
               WHEN '00'
                    PERFORM 2850-MOVER-TIT-DESCARTADOS
                    THRU 2850-MOVER-TIT-DESCARTADOS-EXIT
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO SALIDA DESCARTO'
                    DISPLAY 'FILE STATUS: ' FS-DESCARTADOS
                    STOP RUN
           END-EVALUATE.

       1500-ABRIR-AR-DESCARTADOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*

       1600-LEER-ALUMNOS.

           READ ALUMNOS.

           EVALUATE FS-ALUMNOS
               WHEN '00'
               ADD 1                        TO WS-CONT-REG-LEIDOS
                WHEN '10'
                    SET FS-ALUMNOS-EOF     TO TRUE
               WHEN OTHER
                    SET FS-ALUMNOS-EOF     TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ALUMNOS'
                    DISPLAY 'FILE STATUS: ' FS-ALUMNOS
           END-EVALUATE.

       1600-LEER-ALUMNOS-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       1700-VALIDACION-ENTRADA.

           IF ALUMNO-NOMAPELL-ENTRADA EQUAL SPACES

               PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

      *................................................................*

           IF ALUMNO-MATERIA-ENTRADA EQUAL SPACES THEN

               PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

      *................................................................*

           IF ALUMNO-NOTA-ENTRADA IS NOT NUMERIC THEN

               PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

      *................................................................*

           IF ALUMNO-NOMAPELL-ENTRADA EQUAL LOW-VALUES OR
           ALUMNO-MATERIA-ENTRADA  EQUAL LOW-VALUES

               PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

      *................................................................*

           IF ALUMNO-NOMAPELL-ENTRADA OR
           ALUMNO-MATERIA-ENTRADA  EQUAL HIGH-VALUE

               PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

      *................................................................*

           IF ALUMNO-NOTA-ENTRADA EQUAL LOW-VALUE OR EQUAL HIGH-VALUE

               PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

      *................................................................*

           IF ALUMNO-FECHA-ENTRADA EQUAL LOW-VALUE OR EQUAL HIGH-VALUE

                PERFORM 2760-MOVER-DATOS-ERROR
               THRU 2760-MOVER-DATOS-ERROR-EXIT

               PERFORM 2700-GRABAR-ARCHIVO-ERROR
               THRU 2700-GRABAR-ARCH-ERROR-EXIT

               PERFORM 1600-LEER-ALUMNOS
               THRU 1600-LEER-ALUMNOS-EXIT

           END-IF.

       1700-VALIDACION-ENTRADA-EXIT.

            EXIT.

      *----------------------------------------------------------------*

       2000-PROCESAR-ARCHIVO.

             PERFORM 1700-VALIDACION-ENTRADA
             THRU 1700-VALIDACION-ENTRADA-EXIT.

             MOVE ZEROS      TO    WS-CONT-MATERIAS.


             MOVE ALUMNO-NOMAPELL-ENTRADA   TO  WS-CC-ALUM-ANT.

             ADD 1                          TO WS-CONT-ALUMNOS.


             PERFORM 2100-PROCESAR-CORTE-ALUMNO
             THRU 2100-PROCESAR-CORTE-ALUMNO-EXIT
             UNTIL FS-ALUMNOS-EOF
             OR ALUMNO-NOMAPELL-ENTRADA NOT EQUAL WS-CC-ALUM-ANT.


             MOVE WS-LIS-SEPARADOR-2 TO PROMED-ALUMNOS
             PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
             THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.


       2000-PROCESAR-ARCHIVO-EXIT.

           EXIT.

      *-----------------------------------------------------------------*

       2100-PROCESAR-CORTE-ALUMNO.


              MOVE ALUMNO-MATERIA-ENTRADA     TO WS-CC-MATE-ANT.

              MOVE ZEROS                      TO WS-CONT-MATERIAS.

              MOVE ZEROS                      TO WS-CC-NOTA-ACUM.


               PERFORM 2200-PROCESO-CORTE-MATERIA
               THRU 2200-PROCESO-CORTE-MATERIA-EXIT
               UNTIL FS-ALUMNOS-EOF
               OR ALUMNO-NOMAPELL-ENTRADA NOT EQUAL WS-CC-ALUM-ANT
               OR ALUMNO-MATERIA-ENTRADA NOT EQUAL WS-CC-MATE-ANT.

             IF WS-CONT-MATERIAS = 3 THEN

                   PERFORM 2300-PROCESO-PROMEDIOS
                   THRU 2300-PROCESO-PROMEDIOS-EXIT

                   PERFORM 2460-MOVER-DATOS-PROMEDIO
                   THRU 2460-MOVER-DATOS-PROMEDIO-EXIT

                   PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
                   THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT

                   PERFORM 2420-MOVER-A-CUADRO-HONOR
                   THRU 2420-MOVER-A-CUADRO-HONOR-EXIT

             ELSE

                   PERFORM 2900-MOVER-DATOS-DESCARTADOS
                   THRU 2900-MOVER-DATOS-DESCAR-EXIT

                   PERFORM 2800-GRABAR-AR-DESCARTADOS
                   THRU 2800-GRABAR-AR-DESCARTADOS-EXIT

             END-IF.

       2100-PROCESAR-CORTE-ALUMNO-EXIT.

           EXIT.

      *-----------------------------------------------------------------*

       2200-PROCESO-CORTE-MATERIA.


              MOVE ALUMNO-NOMAPELL-ENTRADA   TO   WS-CC-ALUM-ANT

              MOVE ALUMNO-MATERIA-ENTRADA    TO   WS-CC-MATE-ANT

              ADD 1                          TO   WS-CONT-MATERIAS

              ADD ALUMNO-NOTA-ENTRADA       TO    WS-CC-NOTA-ACUM


                        PERFORM 2400-MAYOR-X-MATERIA
                        THRU 2400-MAYOR-X-MATERIA-EXIT.

                       MOVE ' '            TO WS-NOM-MAX.
                       MOVE ' '            TO WS-MAT-MAX.
                       MOVE ZEROES         TO WS-CC-MAY-PROM.


                        PERFORM 1600-LEER-ALUMNOS
                        THRU 1600-LEER-ALUMNOS-EXIT.


                        PERFORM 1700-VALIDACION-ENTRADA
                        THRU 1700-VALIDACION-ENTRADA-EXIT.


       2200-PROCESO-CORTE-MATERIA-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2300-PROCESO-PROMEDIOS.

                   DIVIDE   WS-CC-NOTA-ACUM BY  WS-CONT-MATERIAS
                   GIVING WS-PROMEDIO

                   ADD 1 TO WS-TOTAL-PROCESADOS.

       2300-PROCESO-PROMEDIOS-EXIT.

            EXIT.

      *----------------------------------------------------------------*

       2400-MAYOR-X-MATERIA.

           MOVE WS-MAT-MAX TO  WS-MAT-EVA.

           EVALUATE TRUE

            WHEN WS-MAT-EVA = "Economía"
                IF WS-TABLA-PROM(1)< WS-CC-MAY-PROM
                 MOVE WS-CC-MAY-PROM TO WS-TABLA-PROM(1)
                 MOVE WS-NOM-MAX TO WS-TABLA-NOMBRE(1)
                 MOVE WS-MAT-MAX  TO WS-TABLA-MAT(1)
                END-IF

           WHEN WS-MAT-EVA = "Física"
                IF WS-TABLA-PROM(2)< WS-CC-MAY-PROM
                 MOVE WS-CC-MAY-PROM TO WS-TABLA-PROM(2)
                 MOVE WS-NOM-MAX TO WS-TABLA-NOMBRE(2)
                 MOVE WS-MAT-MAX  TO WS-TABLA-MAT(2)
                END-IF

           WHEN WS-MAT-EVA ="Informática"
                IF WS-TABLA-PROM(3)< WS-CC-MAY-PROM
                 MOVE WS-CC-MAY-PROM TO WS-TABLA-PROM(3)
                 MOVE WS-NOM-MAX TO WS-TABLA-NOMBRE(3)
                 MOVE WS-MAT-MAX  TO WS-TABLA-MAT(3)
                END-IF

           WHEN WS-MAT-EVA = "Inglés"
                IF WS-TABLA-PROM(4)< WS-CC-MAY-PROM
                 MOVE WS-CC-MAY-PROM TO WS-TABLA-PROM(4)
                 MOVE WS-NOM-MAX TO WS-TABLA-NOMBRE(4)
                 MOVE WS-MAT-MAX  TO WS-TABLA-MAT(4)
                END-IF

           WHEN WS-MAT-EVA ="Matemáticas"
                IF WS-TABLA-PROM(5)< WS-CC-MAY-PROM
                 MOVE WS-CC-MAY-PROM TO WS-TABLA-PROM(5)
                 MOVE WS-NOM-MAX TO WS-TABLA-NOMBRE(5)
                 MOVE WS-MAT-MAX  TO WS-TABLA-MAT(5)
                END-IF

           WHEN WS-MAT-EVA = "Química"
                IF WS-TABLA-PROM(6)< WS-CC-MAY-PROM
                 MOVE WS-CC-MAY-PROM TO WS-TABLA-PROM(6)
                 MOVE WS-NOM-MAX TO WS-TABLA-NOMBRE(6)
                 MOVE WS-MAT-MAX  TO WS-TABLA-MAT(6)
                END-IF

           WHEN OTHER

           CONTINUE

           END-EVALUATE.

       2400-MAYOR-X-MATERIA-EXIT.

           EXIT.

      *-----------------------------------------------------------------*

       2420-MOVER-A-CUADRO-HONOR.

               IF WS-CC-MAY-PROM < WS-PROMEDIO
                 MOVE WS-PROMEDIO TO WS-CC-MAY-PROM
                 MOVE WS-CC-ALUM-ANT TO WS-NOM-MAX
                 MOVE WS-CC-MATE-ANT TO WS-MAT-MAX
               END-IF.

       2420-MOVER-A-CUADRO-HONOR-EXIT.

            EXIT.

      *----------------------------------------------------------------*

       2450-MOVER-TIT-PROMEDIO.

               MOVE WS-LISTADO-PROMEDIO TO PROMED-ALUMNOS.

               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE SPACE TO PROMED-ALUMNOS.

               MOVE WS-LIS-HEADER TO PROMED-ALUMNOS.

               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE WS-LISTADO-PROMEDIO TO PROMED-ALUMNOS.

               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE SPACE TO PROMED-ALUMNOS.

       2450-MOVER-TIT-PROMEDIO-EXIT.

           EXIT.

      *-----------------------------------------------------------------*

       2460-MOVER-DATOS-PROMEDIO.

               MOVE WS-CC-ALUM-ANT TO WS-LIS-D-APELLNOM.

               MOVE WS-CC-MATE-ANT TO WS-LIS-D-MATERIA.

               MOVE WS-PROMEDIO TO WS-LIS-D-PROMEDIO.

               MOVE WS-LIS-DETALLE TO PROMED-ALUMNOS.

       2460-MOVER-DATOS-PROMEDIO-EXIT.

            EXIT.

      *-----------------------------------------------------------------*

       2500-GRABAR-ARCHIVO-PROMEDIO.

           WRITE PROMED-ALUMNOS.

           EVALUATE FS-PROMEDIOS
               WHEN '00'
                    ADD 1 TO WS-CONT-ALUMPROM
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO PROMEDIO'
                    DISPLAY 'FILE STATUS: ' FS-PROMEDIOS
           END-EVALUATE.

       2500-GRABAR-ARCH-PROMEDIO-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2600-GRABAR-ARCH-HONOR.

           WRITE HONOR-ALUMNOS.

           EVALUATE FS-HONOR
               WHEN '00'
                    ADD 1 TO WS-CONT-ALUM-HONOR
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO HONOR'
                    DISPLAY 'FILE STATUS: ' FS-HONOR
           END-EVALUATE.

       2600-GRABAR-ARCH-HONOR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2630-MOVER-TIT-HONOR.

               MOVE WS-LISTADO-HONOR TO HONOR-ALUMNOS.
               PERFORM 2600-GRABAR-ARCH-HONOR
               THRU 2600-GRABAR-ARCH-HONOR-EXIT.

               MOVE WS-LIS-HEADER2 TO HONOR-ALUMNOS.
               PERFORM 2600-GRABAR-ARCH-HONOR
               THRU 2600-GRABAR-ARCH-HONOR-EXIT.

               MOVE WS-LISTADO-HONOR TO HONOR-ALUMNOS.
               PERFORM 2600-GRABAR-ARCH-HONOR
               THRU 2600-GRABAR-ARCH-HONOR-EXIT.

               MOVE SPACE TO HONOR-ALUMNOS.

       2630-MOVER-TIT-HONOR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2660-MOVER-DATOS-HONOR.

           PERFORM 2661-RECORRER-TABLA
           THRU 2661-RECORRER-TABLA UNTIL WS-INDICE = 20.

       2660-MOVER-DATOS-HONOR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2661-RECORRER-TABLA.

             ADD 1 TO WS-INDICE
             MOVE WS-TABLA-MAT(WS-INDICE)    TO WS-LIS-D-MATERIAH.
             MOVE WS-TABLA-NOMBRE(WS-INDICE) TO WS-LIS-D-APELLNOMH.
             MOVE WS-TABLA-PROM (WS-INDICE)  TO WS-PROM-AUX
             MOVE WS-PROM-AUX                TO WS-LIS-D-PROMEDIOH
             MOVE WS-LIS-DETALLE2 TO HONOR-ALUMNOS.
             PERFORM 2600-GRABAR-ARCH-HONOR
             THRU 2600-GRABAR-ARCH-HONOR-EXIT.

       2661-RECORRER-TABLA-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2700-GRABAR-ARCHIVO-ERROR.

           WRITE REGISTRO-ERRORES.

           EVALUATE FS-ERRORES
               WHEN '00'
                    ADD 1 TO WS-CANT-ALUM-ERROR
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERRORES
           END-EVALUATE.

       2700-GRABAR-ARCH-ERROR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2760-MOVER-DATOS-ERROR.

                MOVE ALUMNO-FECHA-ENTRADA TO ERR-FECHA-ENTRADA

                MOVE WS-CC-ALUM-ANT TO ERR-NOMAPELL-ENTRADA

                MOVE WS-CC-MATE-ANT TO ERR-MATERIA-ENTRADA

                MOVE WS-PROMEDIO TO ERR-NOTA-ENTRADA

                MOVE REGISTRO-ALUMNOS TO REGISTRO-ERRORES.

       2760-MOVER-DATOS-ERROR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2800-GRABAR-AR-DESCARTADOS.

           WRITE REG-DESCARTADOS.

           EVALUATE FS-DESCARTADOS
               WHEN '00'
                    ADD 1 TO WS-CANT-DESCARTADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DESCARTO'
                    DISPLAY 'FILE STATUS: ' FS-DESCARTADOS
           END-EVALUATE.

       2800-GRABAR-AR-DESCARTADOS-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2850-MOVER-TIT-DESCARTADOS.

                   MOVE WS-LISTADO-DESCARTADOS TO REG-DESCARTADOS.

                   PERFORM 2800-GRABAR-AR-DESCARTADOS
                   THRU 2800-GRABAR-AR-DESCARTADOS-EXIT.

                   MOVE SPACE TO REG-DESCARTADOS.

                   MOVE WS-LIS-HEADER3 TO REG-DESCARTADOS.

                   PERFORM 2800-GRABAR-AR-DESCARTADOS
                   THRU 2800-GRABAR-AR-DESCARTADOS-EXIT.

                   MOVE WS-LISTADO-DESCARTADOS TO REG-DESCARTADOS.

                   PERFORM 2800-GRABAR-AR-DESCARTADOS
                   THRU 2800-GRABAR-AR-DESCARTADOS-EXIT.

                   MOVE SPACE TO REG-DESCARTADOS.

       2850-MOVER-TIT-DESCARTADOS-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       2900-MOVER-DATOS-DESCARTADOS.

               MOVE WS-CC-ALUM-ANT TO WS-LIS-D-APELLNOMD

               MOVE WS-CC-MATE-ANT TO WS-LIS-D-MATERIAD

               MOVE WS-CONT-MATERIAS TO WS-LIS-D-CANTIDAD

               MOVE WS-LIS-DETALLE3 TO REG-DESCARTADOS.

       2900-MOVER-DATOS-DESCAR-EXIT.

           EXIT.

      *-----------------------------------------------------------------

       2910-MOVER-DATOS-DESCARTADOS.

            MOVE  ALUMNO-NOMAPELL-ENTRADA TO WS-LIS-D-APELLNOMD

               MOVE ALUMNO-MATERIA-ENTRADA TO WS-LIS-D-MATERIAD

               MOVE WS-CONT-MATERIAS     TO WS-LIS-D-CANTIDAD

               MOVE WS-LIS-DETALLE3 TO REG-DESCARTADOS.

       2910-MOVER-DATOS-DESCAR-EXIT.

            EXIT.

      *----------------------------------------------------------------*

       3000-FINALIZAR-PROGRAMA.

             MOVE WS-CONT-REG-LEIDOS  TO WS-LIS-D-LEIDOS.
             MOVE WS-TOTAL-PROCESADOS TO WS-LIS-D-CANT-REG-PROC.
             MOVE WS-CANT-DESCARTADOS TO WS-LIS-D-DESCARTADOS.
             MOVE WS-CANT-ALUM-ERROR  TO WS-LIS-D-ERROR.

               MOVE  WS-LIS-TOTALES-1   TO LINEA-PROM.
               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE  WS-LIS-TOTALES-2   TO LINEA-PROM.
               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE  WS-LIS-TOTALES-3   TO LINEA-PROM.
               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE  WS-LIS-TOTALES-4   TO LINEA-PROM.
               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE WS-LIS-SEPARADOR    TO  LINEA-PROM
               PERFORM 2500-GRABAR-ARCHIVO-PROMEDIO
               THRU 2500-GRABAR-ARCH-PROMEDIO-EXIT.

               MOVE WS-LIS-SEPARADOR-2  TO REG-DESCARTADOS.
               PERFORM 2800-GRABAR-AR-DESCARTADOS
               THRU 2800-GRABAR-AR-DESCARTADOS-EXIT.

               MOVE WS-LIS-SEPARADOR-2  TO HONOR-ALUMNOS.
               PERFORM 2600-GRABAR-ARCH-HONOR
               THRU 2600-GRABAR-ARCH-HONOR-EXIT.

               PERFORM 3100-CERRAR-ARCH-ALUMNOS
               THRU 3100-CERRAR-ARCH-ALUMNOS-EXIT.

               PERFORM 3200-CERRAR-ARCH-PROMEDIO
               THRU 3200-CERRAR-ARCH-PROMEDIO-EXIT.

               PERFORM 3300-CERRAR-ARCH-HONOR
               THRU 3300-CERRAR-ARCH-HONOR-EXIT.

               PERFORM 3400-CERRAR-ARCH-ERROR
               THRU 3400-CERRAR-ARCH-ERROR-EXIT.

               PERFORM 3500-CERRAR-AR-DESCARTADOS
               THRU 3500-CERRAR-AR-DESCARTADOS-EXIT.

         3000-FINALIZAR-PROGRAMA-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       3100-CERRAR-ARCH-ALUMNOS.

           CLOSE ALUMNOS.

           IF NOT FS-ALUMNOS-OK
              DISPLAY 'ERROR EN CLOSE DE ALUMNOS: ' FS-ALUMNOS
           END-IF.

       3100-CERRAR-ARCH-ALUMNOS-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       3200-CERRAR-ARCH-PROMEDIO.

           CLOSE PROMEDIOS.

           IF NOT FS-PROMEDIOS-OK
              DISPLAY 'ERROR EN CLOSE DE VENTASB: ' FS-PROMEDIOS
           END-IF.

       3200-CERRAR-ARCH-PROMEDIO-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       3300-CERRAR-ARCH-HONOR.

           CLOSE HONOR.

           IF NOT FS-HONOR-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-HONOR
           END-IF.

       3300-CERRAR-ARCH-HONOR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       3400-CERRAR-ARCH-ERROR.

           CLOSE ERRORES.

           IF NOT FS-ERRORES-OK

              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-ERRORES
           END-IF.

       3400-CERRAR-ARCH-ERROR-EXIT.

           EXIT.

      *----------------------------------------------------------------*

       3500-CERRAR-AR-DESCARTADOS.

           CLOSE DESCARTADOS.

           IF NOT FS-DESCARTADOS-OK
              DISPLAY 'ERROR EN CLOSE DE DESCARTO: ' FS-DESCARTADOS
           END-IF.

       3500-CERRAR-AR-DESCARTADOS-EXIT.

           EXIT.

      *----------------------------------------------------------------*
           END PROGRAM TP01EJ01.
      *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
