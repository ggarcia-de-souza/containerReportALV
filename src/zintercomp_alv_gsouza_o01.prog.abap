*&---------------------------------------------------------------------*
*& Include          ZINTERCOMP_ALV_GSOUZA_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ST_9001'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INICIALIZATION_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE inicialization_9001 OUTPUT.

  IF o_docking IS NOT BOUND.

* Criação de do Objeto docking que vai a tela chamada como referencia.
    CREATE OBJECT o_docking
      EXPORTING
*       parent                      =     " Parent container
        repid                       = sy-repid
        " Report to Which This Docking Control is Linked
        dynnr                       = sy-dynnr
        " Screen to Which This Docking Control is Linked
        extension                   = 9999    " Control Extension
        side                        = cl_gui_docking_container=>dock_at_top
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
*  Aqui está o segredo, esse objeto usa o Docking criado acima como referencia
* e já divide o mesmo em duas linhas e duas colunas ( 4 partições ).
*    BREAK-POINT.
*&---------------------------------------------------------------------*
    CREATE OBJECT o_splitter
      EXPORTING
*       link_dynnr        =     " Screen Number
*       link_repid        =     " Report Name
*       shellstyle        =     " Window Style
*       left              =     " Left-aligned
*       top               =     " top
*       width             =     " NPlWidth
*       height            =     " Hght
*       metric            = CNTL_METRIC_DYNPRO    " Metric
*       align             = 15    " Alignment
        parent            = o_docking    " Parent Container
        rows              = 2   " Number of Rows to be displayed
        columns           = 2  " Number of Columns to be Displayed
*       no_autodef_progid_dynnr =     " Don't Autodefined Progid and Dynnr?
*       name              =     " Name
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
  ENDIF.

* Definição das medidas dos containers
  PERFORM f_define_container_extension
*         Row column Height Width
    USING '1' '1'    '10'   '10'.

  PERFORM f_define_container_extension
*         Row column Height Width
    USING '2' '1'    '70'   '40'.


*  Eu tentei criar esses processo de forma generica porque existem varias
* partições e todos tem o processo similar.

*&---------------------------------------------------------------------*
* Container Header ( 1,1 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '1' '1'     o_container_spfli_h.
  PERFORM f_define_customer_header.

*&---------------------------------------------------------------------*
* Container Header ( 2,1 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '2' '1'     o_container_spfli.
  PERFORM f_define_customer_report.

*&---------------------------------------------------------------------*
* Container Item ( 1,2 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '1' '2'     o_container_sflight_h.
  PERFORM f_define_supplier_header.

*&---------------------------------------------------------------------*
* Container Item ( 1,2 )
*&---------------------------------------------------------------------*
  PERFORM f_create_container
*         Row Column  Container
    USING '2' '2'     o_container_sflight.
  PERFORM f_define_supplier_report.
ENDMODULE.
