      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAMEN-NEGOCIO-VENTAS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS ASSIGN TO "..\ventas.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT FACTURAS ASSIGN TO "..\facturas.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT ARTICULOS ASSIGN TO "..\articulos.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT VENTAS-SORT ASSIGN TO "SORTWORK".
           SELECT VENTAS-ACT ASSIGN TO "..\ventas-act.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT FACTURAS-SORT ASSIGN TO "sortwork".
           SELECT FACTURAS-ACT ASSIGN TO "..\facturas-act.txt"
               ORGANIZATION is line SEQUENTIAL.
           SELECT LISTADO ASSIGN TO
               PRINTER, "..\impre.txt"
               ORGANIZATION is line SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  VENTAS.
       01  ven-reg.
           03 ven-fecha            pic 9(8).
           03 ven-factura          pic 9(8).
       FD  FACTURAS.
       01  fac_reg_cab.
           03 fac_cab_tiporeg      pic X.
           03 fac_cab_factura      pic 9(8).
       01  fac_reg_det.
           03 fac_det_tiporeg      pic X.
           03 fac_det_art          pic 99.
           03 fac_det_cant         pic 999.
       FD  ARTICULOS.
       01  art_reg.
           03 art_cod              pic 99.
           03 art_nombre           pic X(20).
       FD  VENTAS-ACT.
       01  act-ven-reg.
           03 act-ven-fecha        pic 9(8).
           03 act-ven-factura      pic 9(8).
       FD  LISTADO
           LINAGE is 61 LINES
               with FOOTING at 59
               lines at top 3
               lines at bottom 3.
       01  lis-reg pic x(80).
       SD  VENTAS-SORT.
       01  srt-ven-reg.
           03 srt-ven-fecha        pic 9(8).
           03 srt-ven-factura      pic 9(8).
       SD  FACTURAS-SORT.
       01  srt-fac_reg.
           03 srt-fac_det_art      pic 99.
           03 srt-fac_det_cant     pic 999.
       WORKING-STORAGE SECTION.
       77  flagVentasAct           pic X.
       77  flagArticulos           pic X.
       77  flagFacturas            pic X.
       77  flagSrtFacturas         pic X.
       77  acu-cant-art            pic 9999.
       01  fac_cab_factura_ant     pic 9(8).
       01  srt-fac_det_art_ant     pic 99.
       01  w-cont-paginas          pic 99.
      *
      ************************* LINEAS DE IMPRESION ********************
      *
       01  cabecera1.
           03 filler       pic x(2).
           03 filler       pic x(55) value "LISTADO DE ARTICULOS DE LAS
      -         "FACTURAS EMITIDAS EN VENTAS".
           03 filler       pic x(5)  value spaces.
           03 filler       pic x(13) value "NRO. DE PAG. ".
           03 l-nro-pag    pic 99.
           03 filler       pic x(3).
       01  cabecera2.
           03 filler       pic x(80) value all "-".
       01  cabecera3.
           03 filler       pic x(18) value spaces.
           03 filler       pic x(6)  value "CODIGO".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(6)  value "NOMBRE".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(8)  value "CANTIDAD".
           03 filler       pic x(18) value spaces.
       01  cabecera4.
           03 filler       pic x(18) value spaces.
           03 filler       pic x(6)  value all "-".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(6)  value all "-".
           03 filler       pic x(12)  value spaces.
           03 filler       pic x(8)  value all "-".
           03 filler       pic x(18) value spaces.
       01  detalle.
           03 filler       pic x(20) value spaces.
           03 l-cod        pic x(2)  value spaces.
           03 filler       pic x(7)  value spaces.
           03 l-nombre     pic x(20).
           03 filler       pic x(5)  value spaces.
           03 l-cantidad   pic z.zz9.
           03 filler       pic x(22) value spaces.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT FACTURAS-SORT
               ASCENDING srt-fac_det_art
               INPUT PROCEDURE IS 0300-INPUT-PROCEDURE
               OUTPUT PROCEDURE IS 0400-OUTPUT-PROCEDURE.
      *
            STOP RUN.
      *
      ********************** RUTINAS OUTPUT PRECEDURE ******************
      *
       0300-INPUT-PROCEDURE.
           PERFORM 0310-INICIO-GENERAL-INP.
           PERFORM 0320-LEER-REGISTRO-VENTAS-ACT.
           PERFORM 0330-LEER-REGISTRO-FACTURAS.
           PERFORM UNTIL flagVentasAct = "s"
               PERFORM 0340-INICIO-CAB-C
               PERFORM UNTIL flagVentasAct = "s" OR flagFacturas = "s"
                   OR act-ven-factura <> fac_cab_factura
                       PERFORM 0350-INICIO-CAB-D
                       PERFORM 0330-LEER-REGISTRO-FACTURAS
                       PERFORM UNTIL flagFacturas = "s"
                           OR fac_det_tiporeg <> "D"
                           PERFORM 0360-MUEVO-SORT
                           PERFORM 0330-LEER-REGISTRO-FACTURAS
                       END-PERFORM
                       PERFORM 0370-FIN-CAB-D
                       PERFORM 0320-LEER-REGISTRO-VENTAS-ACT
               END-PERFORM
               PERFORM 0380-FIN-CAB-C
           END-PERFORM
           PERFORM 0390-FIN-GENERAL-INP.
      *
       0310-INICIO-GENERAL-INP.
           PERFORM 0313-ORDENO-VENTAS.
           PERFORM 0316-INICIO-VAR-GRAL-INP.
           PERFORM 0319-ABRO-ARCHIVOS-INP.
      *
       0313-ORDENO-VENTAS.
           SORT VENTAS-SORT
               ASCENDING KEY srt-ven-factura
               USING VENTAS
               GIVING VENTAS-ACT.
      *
       0316-INICIO-VAR-GRAL-INP.
           MOVE "n" TO flagVentasAct.
           MOVE "n" TO flagFacturas.
      *
       0319-ABRO-ARCHIVOS-INP.
           OPEN INPUT VENTAS-ACT.
           OPEN INPUT FACTURAS.
      *
       0320-LEER-REGISTRO-VENTAS-ACT.
           READ VENTAS-ACT AT END MOVE "s" TO flagVentasAct.
      *
       0330-LEER-REGISTRO-FACTURAS.
           READ FACTURAS AT END MOVE "s" TO flagFacturas.
      *
       0340-INICIO-CAB-C.
      *
       0350-INICIO-CAB-D.
      *
       0360-MUEVO-SORT.
           MOVE fac_det_art TO srt-fac_det_art.
           MOVE fac_det_cant TO srt-fac_det_cant.
           RELEASE srt-fac_reg.
      *
       0370-FIN-CAB-D.
      *
       0380-FIN-CAB-C.
           IF flagFacturas <> "s"
                   PERFORM 0330-LEER-REGISTRO-FACTURAS
                   PERFORM UNTIL flagFacturas = "s"
                       OR fac_det_tiporeg <> "D"
                       PERFORM 0330-LEER-REGISTRO-FACTURAS
                   END-PERFORM.
      *
       0390-FIN-GENERAL-INP.
           PERFORM 0393-CIERRO-ARCHIVOS-INP.
      *
       0393-CIERRO-ARCHIVOS-INP.
           CLOSE VENTAS-ACT.
           CLOSE FACTURAS.
      *
      ********************** RUTINAS OUTPUT PRECEDURE ******************
      *
       0400-OUTPUT-PROCEDURE.
           PERFORM 0410-INICIO-GRAL-OUT.
           PERFORM 0420-LEER-REGISTRO-SRT-FAC.
           PERFORM UNTIL flagSrtFacturas = "s"
               PERFORM 0430-INICIO-ART
               PERFORM UNTIL flagSrtFacturas = "s"
                   OR srt-fac_det_art <> srt-fac_det_art_ant
                   PERFORM 0440-SUMO-CANTIDAD-ART
                   PERFORM 0420-LEER-REGISTRO-SRT-FAC
               END-PERFORM
               PERFORM 0450-FIN-ART
           END-PERFORM.
           PERFORM 0490-FIN-GENERAL-OUT.
      *
       0410-INICIO-GRAL-OUT.
           PERFORM 0413-INICIO-VAR-GRAL-OUT.
           PERFORM 0416-ABRO-ARCHIVOS-OUT.
           PERFORM 0419-LISTAR-ENCABEZADO.
      *
       0413-INICIO-VAR-GRAL-OUT.
           MOVE "n" TO flagSrtFacturas.
           MOVE "n" TO flagArticulos.
           MOVE ZEROES TO w-cont-paginas.
      *
       0416-ABRO-ARCHIVOS-OUT.
           OPEN INPUT ARTICULOS.
           OPEN OUTPUT LISTADO.
      *
       0419-LISTAR-ENCABEZADO.
           ADD 1 TO w-cont-paginas.
           MOVE w-cont-paginas to l-nro-pag.
           IF w-cont-paginas = 1
               WRITE lis-reg FROM cabecera1
               DISPLAY lis-reg
           ELSE
               WRITE lis-reg FROM cabecera1 AFTER ADVANCING PAGE
           END-IF.
           WRITE lis-reg FROM cabecera2 AFTER 2.
           DISPLAY lis-reg
           WRITE lis-reg FROM cabecera3 AFTER 2.
           DISPLAY lis-reg.
           WRITE lis-reg FROM cabecera4.
           DISPLAY lis-reg.
      *
       0420-LEER-REGISTRO-SRT-FAC.
           RETURN FACTURAS-SORT AT END MOVE "s" TO flagSrtFacturas.
      *
       0423-LEER-REGISTRO-ARTICULOS.
           READ ARTICULOS AT END MOVE "s" TO flagArticulos.
      *
       0430-INICIO-ART.
           MOVE srt-fac_det_art TO srt-fac_det_art_ant.
           MOVE ZEROES TO acu-cant-art.
      *
       0440-SUMO-CANTIDAD-ART.
           ADD srt-fac_det_cant TO acu-cant-art.
      *
       0450-FIN-ART.
           IF LINAGE-COUNTER = 56
               PERFORM 0419-LISTAR-ENCABEZADO.
           PERFORM 0453-GENERAR-LINEA.
           PERFORM 0459-LISTAR-DETALLE.
      *
       0453-GENERAR-LINEA.
           PERFORM 0456-TRAIGO-NOMBRE.
           MOVE srt-fac_det_art_ant    TO l-cod.
           MOVE art_nombre             TO l-nombre.
           MOVE acu-cant-art           TO l-cantidad.
      *
       0456-TRAIGO-NOMBRE.
           PERFORM 0423-LEER-REGISTRO-ARTICULOS.
           PERFORM UNTIL flagArticulos = "s"
               OR art_cod = srt-fac_det_art_ant
               PERFORM 0423-LEER-REGISTRO-ARTICULOS
           END-PERFORM.
      *
       0459-LISTAR-DETALLE.
           WRITE lis-reg FROM detalle AFTER ADVANCING 1 LINE
               AT END-OF-PAGE PERFORM 0419-LISTAR-ENCABEZADO.
           DISPLAY lis-reg.
      *
       0490-FIN-GENERAL-OUT.
           PERFORM 0493-CIERRO-ARCHIVOS-OUT.
      *
       0493-CIERRO-ARCHIVOS-OUT.
           CLOSE ARTICULOS.
           CLOSE LISTADO.
      *
       END PROGRAM EXAMEN-NEGOCIO-VENTAS.
