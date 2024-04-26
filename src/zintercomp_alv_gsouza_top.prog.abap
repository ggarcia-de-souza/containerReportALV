*&---------------------------------------------------------------------*
*& Include ZINTERCOMP_ALV_GSOUZA_TOP                - PoolMóds.        ZINTERCOMP_ALV_GSOUZA
*&---------------------------------------------------------------------*
PROGRAM zintercomp_alv_gsouza.

TABLES: knb1, bsid, bsik, vbrk, bsak.

*Classe
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id.
ENDCLASS. "lcl_event_handler DEFINITION

*Tipos
*&---------------------------------------------------------------------*
TYPES :
  BEGIN OF ty_spfli,
    carrid   TYPE scarr-carrid,
    carrname TYPE scarr-carrname,
    connid   TYPE spfli-connid,
    cityfrom TYPE spfli-cityfrom,
    cityto   TYPE spfli-cityto,
  END OF   ty_spfli
  .
TYPES:
  BEGIN OF ty_saida,
    bukrs     TYPE bsid-bukrs,
    kunnr     TYPE bsid-kunnr,
    belnr_cus TYPE bsid-belnr,
    xblnr     TYPE vbrk-xblnr,
*    vbeln     TYPE vbrk-vbeln,
    zuonr     TYPE bsid-zuonr,
    bldat     TYPE bsid-bldat,
    dmbtr     TYPE bsid-dmbtr,
    blart     TYPE bsid-blart,
    gjahr     TYPE bsid-gjahr,
    zlspr     TYPE bsid-zlspr,
  END OF ty_saida,

  BEGIN OF ty_saida2,
    bukrs_sup TYPE bsik-bukrs,
    lifnr     TYPE bsik-lifnr,
    belnr_sup TYPE bsik-belnr,
    xblnr_sup TYPE rbkp-xblnr,
*    invoice   TYPE accit-xblnr,
    zuonr_sup TYPE bsik-zuonr,
    bldat_sup TYPE bsik-bldat,
    dmbtr_sup TYPE bsik-dmbtr,
    blart_sup TYPE bsik-blart,
    gjahr_sup TYPE bsik-gjahr,
    zlspr_sup TYPE bsid-zlspr,
  END OF ty_saida2,

  BEGIN OF ty_bkpf_aux,
    belnr TYPE bkpf-belnr,      " Accounting Document Number
    gjahr TYPE bkpf-gjahr,      " Fiscal Year
  END OF ty_bkpf_aux,

  BEGIN OF ty_excel,
    bukrs     TYPE bsid-bukrs,
    kunnr     TYPE bsid-kunnr,
    belnr_cus TYPE bsid-belnr,
    xblnr     TYPE vbrk-xblnr,
*    vbeln     TYPE vbrk-vbeln,
    zuonr     TYPE bsid-zuonr,
    bldat     TYPE bsid-bldat,
    dmbtr     TYPE bsid-dmbtr,
    blart     TYPE bsid-blart,
    gjahr     TYPE bsid-gjahr,
    zlspr     TYPE bsid-zlspr,
    bukrs_sup TYPE bsik-bukrs,
    lifnr     TYPE bsik-lifnr,
    belnr_sup TYPE bsik-belnr,
    xblnr_sup TYPE rbkp-xblnr,
*    invoice   TYPE accit-xblnr,
    zuonr_sup TYPE bsik-zuonr,
    bldat_sup TYPE bsik-bldat,
    dmbtr_sup TYPE bsik-dmbtr,
    blart_sup TYPE bsik-blart,
    gjahr_sup TYPE bsik-gjahr,
    zlspr_sup TYPE bsid-zlspr,
  END OF ty_excel.


* Tabelas internas
*&---------------------------------------------------------------------*
DATA :
  t_spfli        TYPE TABLE OF ty_spfli,
  lt_bsid        TYPE TABLE OF bsid,
  lt_bsad        TYPE TABLE OF bsad,
  lt_bkpf        TYPE TABLE OF bkpf,
  ls_bkpf        TYPE bkpf,
  lt_bkpf_aux    TYPE TABLE OF ty_bkpf_aux, " Accounting Document Header
  ls_bkpf_aux    TYPE ty_bkpf_aux,
  lt_bsik        TYPE TABLE OF bsik,
  lt_vbrk        TYPE TABLE OF vbrk,
  lt_bsak        TYPE TABLE OF bsak,
  lt_rbkp        TYPE TABLE OF rbkp,
  lt_saida       TYPE TABLE OF ty_saida,
  ls_saida       TYPE ty_saida,
  lt_saida2      TYPE TABLE OF ty_saida2,
  ls_saida2      TYPE ty_saida2,
  lt_doc         TYPE TABLE OF j_1bnfdoc,
  lv_string      TYPE string,
  lv_len         TYPE i,
  t_fcat_spfli   TYPE TABLE OF lvc_s_fcat,
  s_fcat_spfli   TYPE  lvc_s_fcat,
  t_sflight      TYPE TABLE OF sflight,
  t_fcat_sflight TYPE TABLE OF  lvc_s_fcat,
  s_fcat_sflight TYPE  lvc_s_fcat,
  line_header    TYPE sdydo_text_element,
  lt_bdcdata     TYPE TABLE OF bdcdata,
  wa_bdcdata     LIKE LINE OF lt_bdcdata.


* Objetos
*&---------------------------------------------------------------------*
DATA :
  o_docking  TYPE REF TO cl_gui_docking_container,
  o_splitter TYPE REF TO cl_gui_splitter_container
  .
DATA :
  o_container_spfli     TYPE REF TO cl_gui_container,
  o_container_spfli_h   TYPE REF TO cl_gui_container,
  o_container_sflight   TYPE REF TO cl_gui_container,
  o_container_sflight_h TYPE REF TO cl_gui_container,
  o_alv_spfli           TYPE REF TO cl_gui_alv_grid,
  o_alv_spfli_h         TYPE REF TO cl_gui_alv_grid,
  o_alv_sflight         TYPE REF TO cl_gui_alv_grid,
  o_alv_sflight_h       TYPE REF TO cl_gui_alv_grid,
  gr_event_handler      TYPE REF TO lcl_event_handler,
  o_document            TYPE REF TO cl_dd_document.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.

*    READ TABLE itab INTO itab INDEX e_row_id-index.
*    IF sy-subrc = 0.
*      SET PARAMETER ID 'MAT' FIELD itab-matnr.
*      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*    ENDIF.
*
* Verificar se é a coluna clicada
    CLEAR: ls_saida2, wa_bdcdata, lt_bdcdata.

    IF e_column_id EQ 'BELNR_CUS'.

      READ TABLE lt_saida INTO ls_saida INDEX e_row_id-index.

      CLEAR wa_bdcdata.
      wa_bdcdata-program  = 'SAPMF05L'.
      wa_bdcdata-dynpro   = '100'.
      wa_bdcdata-dynbegin = 'X'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'BDC_CURSOR'.
      wa_bdcdata-fval = 'RF05L-BELNR'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'RF05L-BELNR'.
      wa_bdcdata-fval = ls_saida-belnr_cus.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'BDC_CURSOR'.
      wa_bdcdata-fval = 'RF05L-BUKRS'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'RF05L-BUKRS'.
      wa_bdcdata-fval = ls_saida-bukrs.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'BDC_CURSOR'.
      wa_bdcdata-fval = 'RF05L-GJAHR'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'RF05L-GJAHR'.
      wa_bdcdata-fval = ls_saida-gjahr.
      APPEND wa_bdcdata TO lt_bdcdata.

      CALL TRANSACTION 'FB03' USING lt_bdcdata MODE 'E'.

    ENDIF.

    READ TABLE lt_saida2 INTO ls_saida2 INDEX e_row_id-index.

    IF sy-subrc EQ 0.

      CLEAR wa_bdcdata.
      wa_bdcdata-program  = 'SAPMF05L'.
      wa_bdcdata-dynpro   = '100'.
      wa_bdcdata-dynbegin = 'X'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'BDC_CURSOR'.
      wa_bdcdata-fval = 'RF05L-BELNR'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'RF05L-BELNR'.
      wa_bdcdata-fval = ls_saida2-belnr_sup.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'BDC_CURSOR'.
      wa_bdcdata-fval = 'RF05L-BUKRS'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'RF05L-BUKRS'.
      wa_bdcdata-fval = ls_saida2-bukrs_sup.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'BDC_CURSOR'.
      wa_bdcdata-fval = 'RF05L-GJAHR'.
      APPEND wa_bdcdata TO lt_bdcdata.

      CLEAR wa_bdcdata.
      wa_bdcdata-fnam = 'RF05L-GJAHR'.
      wa_bdcdata-fval = ls_saida2-gjahr_sup.
      APPEND wa_bdcdata TO lt_bdcdata.

      CALL TRANSACTION 'FB03' USING lt_bdcdata MODE 'E'.

    ENDIF.

  ENDMETHOD. "handle_hotspot_click

ENDCLASS. "lcl_event_handler IMPLEMENTATION


SELECTION-SCREEN BEGIN OF SCREEN 1001.
  SELECTION-SCREEN BEGIN OF BLOCK customer WITH FRAME TITLE TEXT-001.
    PARAMETERS: pa_bukrs  LIKE knb1-bukrs. "Customer Company
    PARAMETERS: pa_bukr2  LIKE knb1-bukrs. "Supplier Company
    PARAMETERS: pa_kunnr  LIKE knb1-kunnr. "Customer
    PARAMETERS: pa_lifnr  LIKE bsik-lifnr. "Supplier
    SELECT-OPTIONS: so_zuonr FOR bsid-zuonr. "Assignment
    SELECT-OPTIONS: so_xblnr FOR vbrk-xblnr. "Reference / Nota Fiscal
    SELECT-OPTIONS: so_blart FOR bsid-blart. "Document Type
    SELECT-OPTIONS: so_bldat FOR bsid-bldat. "Document Date
  SELECTION-SCREEN END OF BLOCK customer.

  SELECTION-SCREEN BEGIN OF BLOCK status WITH FRAME TITLE TEXT-003.
*   open items:
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS x_opsel LIKE itemset-xopsel RADIOBUTTON GROUP rad1.
      SELECTION-SCREEN COMMENT 3(20) TEXT-004 FOR FIELD x_opsel.
    SELECTION-SCREEN END OF LINE.
*    PARAMETERS pa_stida LIKE rfpdo-allgstid DEFAULT sy-datlo.
    SELECTION-SCREEN SKIP.
*   cleared items:
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS x_clsel LIKE itemset-xclsel RADIOBUTTON GROUP rad1.
      SELECTION-SCREEN COMMENT 3(25) TEXT-005 FOR FIELD x_clsel.
    SELECTION-SCREEN END OF LINE.
*    SELECT-OPTIONS so_augdt FOR bsid-augdt NO DATABASE SELECTION.
*    PARAMETERS pa_stid2 LIKE rfpdo-allgstid.
    SELECTION-SCREEN SKIP.
*   all items:
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS x_aisel LIKE itemset-xaisel RADIOBUTTON GROUP rad1.
      SELECTION-SCREEN COMMENT 3(20) TEXT-006 FOR FIELD x_aisel.
    SELECTION-SCREEN END OF LINE.
*    SELECT-OPTIONS so_budat FOR bsid-budat NO DATABASE SELECTION.
  SELECTION-SCREEN END OF BLOCK status.


  SELECTION-SCREEN BEGIN OF BLOCK excel WITH FRAME TITLE TEXT-007.

    SELECTION-SCREEN PUSHBUTTON 1(50) p_but1 USER-COMMAND but1.

  SELECTION-SCREEN END OF BLOCK excel.

SELECTION-SCREEN END OF SCREEN 1001.

INITIALIZATION.
  p_but1 = 'Download'.

AT SELECTION-SCREEN.
*
*  SELECT COUNT(*)
*    FROM kna1 UP TO 1 ROWS
*    WHERE kunnr EQ pa_kunnr
*      AND ktokd EQ 'ICBF'.
*
*  IF sy-subrc <> 0.
*
*    MESSAGE 'Transação somente disponível para operações intercompany' TYPE 'E'.
*
*  ENDIF.

  CASE sy-ucomm.
    WHEN 'BUT1'.

      PERFORM f_selects.
      PERFORM f_excel.
      EXIT.
  ENDCASE.

  PERFORM f_selects.
*  PERFORM f_monta_saida.
  CALL SCREEN 9001.
