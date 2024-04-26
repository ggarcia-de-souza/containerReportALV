*&---------------------------------------------------------------------*
*& Include          ZINTERCOMP_ALV_GSOUZA_F01
*&---------------------------------------------------------------------*

* Inicio do processo
START-OF-SELECTION.

*  PERFORM f_selects.
*
*  CALL SCREEN 9001.
*&---------------------------------------------------------------------*
*& Form f_create_container
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> O_CONTAINER_SPFLI
*&---------------------------------------------------------------------*
FORM f_create_container  USING row       TYPE i
                               col       TYPE i
                               container TYPE REF TO cl_gui_container.

* Aqui é feito a criação do container com referencia a uma das partições.
  o_splitter->get_container(
    EXPORTING
      row    = row
      column = col
    RECEIVING
      container = container
      ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_define_container_extension
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_define_container_extension USING row    TYPE i
                                        column TYPE i
                                        height TYPE i
                                        width  TYPE i.
* Altura
  IF row IS NOT INITIAL
    AND height IS NOT INITIAL.
    o_splitter->set_row_height(
      EXPORTING
        id     = row
        height = height
        ).
  ENDIF.

* Largura
  IF column IS NOT INITIAL
    AND width IS NOT INITIAL.
    o_splitter->set_column_width(
      EXPORTING
        id    = column
        width = width
    ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_define_customer_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_define_customer_header .

* Cabeçalho

*  CREATE OBJECT o_docking
*    EXPORTING
*      repid     = sy-repid
*      dynnr     = '9001'
*      ratio     = 30
*      extension = 9999.    " Control Extension

** Cria o ALV OO
*    CREATE OBJECT o_alv_spfli
*      EXPORTING
*        i_parent          = o_container_spfli   " Parent Container
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.

  CLEAR o_document.
  CREATE OBJECT o_document.

  CONCATENATE 'Day :' sy-datum ' Time :' sy-uzeit INTO line_header RESPECTING BLANKS.

  CALL METHOD o_document->add_text
    EXPORTING
      text         = line_header
      sap_fontsize = cl_dd_document=>medium
      sap_emphasis = cl_dd_document=>strong.

  CALL METHOD o_document->display_document
    EXPORTING
      parent = o_container_spfli_h.

****************
*
* data:
*    lst_layout type lvc_s_layo.
*
*  IF o_docking IS BOUND
*    AND o_alv_spfli_h IS NOT BOUND.
*
** Cria o ALV OO
*    CREATE OBJECT o_alv_spfli_h
*      EXPORTING
*        i_parent          = o_container_spfli_h   " Parent Container
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
*
*    PERFORM f_fieldcat
*      TABLES  t_fcat_spfli
*      USING : 'BUKRS' 'LT_BSID' 'BUKRS' 'BSID',
*              'KUNNR' 'LT_BSID' 'KUNNR' 'BSID',
*              'BELNR' 'LT_BSID' 'BELNR' 'BSID',
*              'XBLNR' 'LT_VBRK' 'XBLNR' 'VBRK',
*              'VBELN' 'LT_VBRK' 'VBELN' 'VBRK',
*              'ZUONR' 'LT_BSID' 'ZUONR' 'BSID',
*              'BLDAT' 'LT_BSID' 'BLDAT' 'BSID',
*              'DMBTR' 'LT_BSID' 'DMBTR' 'BSID',
*              'BLART' 'LT_BSID' 'BLART' 'BSID'.
*
**    PERFORM f_fieldcat
**      TABLES  t_fcat_spfli
**      USING : 'CARRID'   'T_SPFLI' 'CARRID'   'SCARR',
**              'CARRNAME' 'T_SPFLI' 'CARRNAME' 'SCARR',
**              'CONNID'   'T_SPFLI' 'CONNID'   'SPFLI',
**              'CITYFROM' 'T_SPFLI' 'CITYFROM' 'SPFLI',
**              'CITYTO'   'T_SPFLI' 'CITYTO'   'SPFLI'.
*
*
*    lst_layout-zebra      = 'X'.
*    lst_layout-cwidth_opt = 'X'.
*
** Evento do hotspot
*
*    LOOP AT t_fcat_spfli INTO s_fcat_spfli.
*      IF s_fcat_spfli-fieldname = 'BELNR'.
*        s_fcat_spfli-hotspot = 'x'.
*      ENDIF.
*
*      MODIFY t_fcat_spfli FROM s_fcat_spfli.
*    ENDLOOP.
*
** Exibe o ALV OO
*    o_alv_spfli_h->set_table_for_first_display(
*      EXPORTING
*        is_layout                     =  lst_layout   " Layout
*      CHANGING
*        it_outtab                     =  lt_saida        " Output Table
*        it_fieldcatalog               =  t_fcat_spfli   " Field Catalog
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        OTHERS                        = 4
*    ).
*
*    CREATE OBJECT gr_event_handler.
*    SET HANDLER gr_event_handler->handle_hotspot_click FOR o_alv_spfli_h.
*
*  ELSE.
*
** Atualiza o ALV OO
*    o_alv_spfli_h->refresh_table_display(
*      EXCEPTIONS
*        finished       = 1
*        OTHERS         = 2
*    ).
*    endif.
*****************
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_define_customer_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_define_customer_report .

  DATA:
    lst_layout TYPE lvc_s_layo.

  IF o_docking IS BOUND
    AND o_alv_spfli IS NOT BOUND.

* Cria o ALV OO
    CREATE OBJECT o_alv_spfli
      EXPORTING
        i_parent          = o_container_spfli   " Parent Container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM f_fieldcat
      TABLES  t_fcat_spfli
      USING : 'BUKRS' 'LT_BSID' 'BUKRS' 'BSID',
              'KUNNR' 'LT_BSID' 'KUNNR' 'BSID',
              'BELNR_CUS' 'LT_BSID' 'BELNR' 'BSID',
              'XBLNR' 'LT_VBRK' 'XBLNR' 'VBRK',
*              'VBELN' 'LT_VBRK' 'VBELN' 'VBRK',
              'ZUONR' 'LT_BSID' 'ZUONR' 'BSID',
              'BLDAT' 'LT_BSID' 'BLDAT' 'BSID',
              'DMBTR' 'LT_BSID' 'DMBTR' 'BSID',
              'BLART' 'LT_BSID' 'BLART' 'BSID',
              'ZLSPR' 'LT_BSID' 'ZLSPR' 'BSID'.

*    PERFORM f_fieldcat
*      TABLES  t_fcat_spfli
*      USING : 'CARRID'   'T_SPFLI' 'CARRID'   'SCARR',
*              'CARRNAME' 'T_SPFLI' 'CARRNAME' 'SCARR',
*              'CONNID'   'T_SPFLI' 'CONNID'   'SPFLI',
*              'CITYFROM' 'T_SPFLI' 'CITYFROM' 'SPFLI',
*              'CITYTO'   'T_SPFLI' 'CITYTO'   'SPFLI'.


    lst_layout-zebra      = 'X'.
    lst_layout-cwidth_opt = 'X'.

* Evento do hotspot

    LOOP AT t_fcat_spfli INTO s_fcat_spfli.
      IF s_fcat_spfli-fieldname = 'BELNR_CUS'.
        s_fcat_spfli-hotspot = 'x'.
      ENDIF.

      MODIFY t_fcat_spfli FROM s_fcat_spfli.
    ENDLOOP.

* Exibe o ALV OO
    o_alv_spfli->set_table_for_first_display(
      EXPORTING
        is_layout                     =  lst_layout   " Layout
      CHANGING
        it_outtab                     =  lt_saida        " Output Table
        it_fieldcatalog               =  t_fcat_spfli   " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).

    CREATE OBJECT gr_event_handler.
    SET HANDLER gr_event_handler->handle_hotspot_click FOR o_alv_spfli.

  ELSE.

* Atualiza o ALV OO
    o_alv_spfli->refresh_table_display(
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2
    ).

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_selects
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_selects .


* Seleção para da alv Principal.
  SELECT a~carrid a~carrname
         b~connid b~cityfrom b~cityto
    INTO TABLE t_spfli
    FROM scarr AS a
    INNER JOIN spfli AS b
    ON  a~carrid = b~carrid.


  CASE 'X'.
    WHEN x_opsel.

      SELECT *
        INTO TABLE lt_bsid
        FROM bsid               " Accounting: Secondary Index for Customers
        WHERE bukrs EQ pa_bukrs "151
        AND kunnr EQ pa_kunnr
        AND zuonr IN so_zuonr
        AND xblnr IN so_xblnr
        AND blart IN so_blart
        AND bldat IN so_bldat.
      SORT lt_bsid BY bukrs kunnr.

      IF lt_bsid[] IS NOT INITIAL.

        SELECT *
         INTO TABLE lt_vbrk
         FROM vbrk " Billing Document: Header Data
         FOR ALL ENTRIES IN lt_bsid
         WHERE zuonr EQ lt_bsid-zuonr
         AND   bukrs EQ lt_bsid-bukrs.
        SORT lt_vbrk BY vbeln.
      ENDIF. " IF lt_bsid[] IS NOT INITIAL

      SELECT *
        INTO TABLE lt_bsik
        FROM bsik               " Accounting: Secondary Index for Vendors
        WHERE bukrs EQ pa_bukr2 "148
        AND lifnr EQ pa_lifnr.
      SORT lt_bsik BY bukrs lifnr.

      IF lt_bsik[] IS NOT INITIAL.
        SELECT *
          INTO TABLE lt_bkpf
          FROM bkpf " Accounting Document Header
          FOR ALL ENTRIES IN lt_bsik
          WHERE bukrs EQ lt_bsik-bukrs
          AND   belnr EQ lt_bsik-belnr
          AND   gjahr EQ lt_bsik-gjahr.
        SORT lt_bkpf BY bukrs belnr gjahr.

        LOOP AT lt_bkpf INTO ls_bkpf.
          lv_len = strlen( ls_bkpf-awkey ).
          lv_len = lv_len - 4.
*          lv_string = ls_bkpf-awkey(lv_len).

          ls_bkpf_aux-belnr = ls_bkpf-awkey(lv_len).
          ls_bkpf_aux-gjahr = ls_bkpf-gjahr.

          APPEND ls_bkpf_aux TO lt_bkpf_aux.

        ENDLOOP. " LOOP AT lt_bkpf INTO DATA(ls_bkpf)

        IF lt_bkpf_aux[] IS NOT INITIAL.
          SELECT *
            INTO TABLE lt_rbkp
            FROM rbkp " Document Header: Invoice Receipt
            FOR ALL ENTRIES IN lt_bkpf_aux
            WHERE belnr = lt_bkpf_aux-belnr
              AND gjahr = lt_bkpf_aux-gjahr.
          SORT lt_rbkp BY belnr gjahr.
        ENDIF. " IF lt_bkpf_aux IS NOT INITIAL

        IF lt_bkpf[] IS NOT INITIAL.
          SELECT *
            INTO TABLE lt_doc
            FROM j_1bnfdoc " Nota Fiscal Header
            FOR ALL ENTRIES IN lt_bkpf
            WHERE belnr = lt_bkpf-belnr
              AND gjahr = lt_bkpf-gjahr.
        ENDIF. " IF lt_bkpf IS NOT INITIAL
      ENDIF. " IF lt_bsik[] IS NOT INITIAL


      PERFORM f_monta_saida TABLES lt_bsid lt_bsik.


    WHEN x_clsel.

      SELECT *
        INTO TABLE lt_bsad
        FROM bsad               " Accounting: Secondary Index for Customers (Cleared Items)
        WHERE bukrs EQ pa_bukrs "151
        AND kunnr EQ pa_kunnr.
      SORT lt_bsad BY bukrs kunnr.

      IF lt_bsad[] IS NOT INITIAL.
        SELECT *
          INTO TABLE lt_vbrk
          FROM vbrk " Billing Document: Header Data
          FOR ALL ENTRIES IN lt_bsad
          WHERE zuonr EQ lt_bsad-zuonr
          AND   bukrs EQ lt_bsad-bukrs.
        SORT lt_vbrk BY vbeln.
      ENDIF. " IF lt_bsad[] IS NOT INITIAL

      SELECT *
        INTO TABLE lt_bsak
        FROM bsak               " Accounting: Secondary Index for Vendors (Cleared Items)
        WHERE bukrs EQ pa_bukr2 "148
        AND lifnr EQ pa_lifnr.
      SORT lt_bsak BY bukrs lifnr.

      IF lt_bsak[] IS NOT INITIAL.
        SELECT *
          INTO TABLE lt_bkpf
          FROM bkpf " Accounting Document Header
          FOR ALL ENTRIES IN lt_bsak
          WHERE bukrs EQ lt_bsak-bukrs
          AND   belnr EQ lt_bsak-belnr
          AND   gjahr EQ lt_bsak-gjahr.
        SORT lt_bkpf BY bukrs belnr gjahr.

        LOOP AT lt_bkpf INTO ls_bkpf.
          lv_len = strlen( ls_bkpf-awkey ).
          lv_len = lv_len - 4.
*          lv_string = ls_bkpf-awkey(lv_len).

          ls_bkpf_aux-belnr = ls_bkpf-awkey(lv_len).
          ls_bkpf_aux-gjahr = ls_bkpf-gjahr.

          APPEND ls_bkpf_aux TO lt_bkpf_aux.

        ENDLOOP. " LOOP AT lt_bkpf INTO DATA(ls_bkpf)

        IF lt_bkpf_aux[] IS NOT INITIAL.
          SELECT *
            INTO TABLE lt_rbkp
            FROM rbkp " Document Header: Invoice Receipt
            FOR ALL ENTRIES IN lt_bkpf_aux
            WHERE belnr = lt_bkpf_aux-belnr
              AND gjahr = lt_bkpf_aux-gjahr.
          SORT lt_rbkp BY belnr gjahr.
        ENDIF. " IF lt_bkpf_aux IS NOT INITIAL

* Tentar esse select se for incluir a j_1bnfdoc
*        IF lt_bkpf[] IS NOT INITIAL.
*          SELECT *
*            INTO TABLE lt_doc
*            FROM j_1bnfdoc " Nota Fiscal Header
*            FOR ALL ENTRIES IN lt_bkpf
*            WHERE belnr = lt_bkpf_aux-belnr
*              AND gjahr = lt_bkpf_aux-gjahr
*              AND bukrs = lt_bkpf_aux-bukrs.
*        ENDIF. " IF lt_bkpf IS NOT INITIAL


        IF lt_bkpf[] IS NOT INITIAL.
          SELECT *
            INTO TABLE lt_doc
            FROM j_1bnfdoc " Nota Fiscal Header
            FOR ALL ENTRIES IN lt_bkpf
            WHERE belnr = lt_bkpf-belnr
              AND gjahr = lt_bkpf-gjahr.
        ENDIF. " IF lt_bkpf IS NOT INITIAL
      ENDIF. " IF lt_bsak[] IS NOT INITIAL

      PERFORM f_monta_saida TABLES lt_bsad lt_bsak.

    WHEN x_aisel.

      SELECT *
        INTO TABLE lt_bsid
        FROM bsid               " Accounting: Secondary Index for Customers
        WHERE bukrs EQ pa_bukrs "151
        AND kunnr EQ pa_kunnr.
      SORT lt_bsid BY bukrs kunnr.

      SELECT *
        APPENDING TABLE @lt_bsid
        FROM bsad " Accounting: Secondary Index for Customers (Cleared Items)
        WHERE bukrs EQ @pa_bukrs
        AND kunnr EQ @pa_kunnr.
      SORT lt_bsid BY bukrs kunnr.

      IF lt_bsid[] IS NOT INITIAL.
        SELECT *
         INTO TABLE lt_vbrk
         FROM vbrk " Billing Document: Header Data
         FOR ALL ENTRIES IN lt_bsid
         WHERE zuonr EQ lt_bsid-zuonr
         AND   bukrs EQ lt_bsid-bukrs.
        SORT lt_vbrk BY vbeln.
      ENDIF. " IF lt_bsid[] IS NOT INITIAL


      SELECT *
        INTO TABLE lt_bsik
        FROM bsik               " Accounting: Secondary Index for Vendors
        WHERE bukrs EQ pa_bukr2 "148
        AND lifnr EQ pa_lifnr.
      SORT lt_bsik BY bukrs lifnr.

      SELECT *
        APPENDING TABLE @lt_bsik
        FROM bsak " Accounting: Secondary Index for Vendors (Cleared Items)
        WHERE bukrs EQ @pa_bukr2
        AND lifnr EQ @pa_lifnr.
      SORT lt_bsik BY bukrs lifnr.

      IF lt_bsik[] IS NOT INITIAL.
        SELECT *
          INTO TABLE lt_bkpf
          FROM bkpf " Accounting Document Header
          FOR ALL ENTRIES IN lt_bsik
          WHERE bukrs EQ lt_bsik-bukrs
          AND   belnr EQ lt_bsik-belnr
          AND   gjahr EQ lt_bsik-gjahr.
        SORT lt_bkpf BY bukrs belnr gjahr.

        LOOP AT lt_bkpf INTO ls_bkpf.
          lv_len = strlen( ls_bkpf-awkey ).
          lv_len = lv_len - 4.
*          lv_string = ls_bkpf-awkey(lv_len).

          ls_bkpf_aux-belnr = ls_bkpf-awkey(lv_len).
          ls_bkpf_aux-gjahr = ls_bkpf-gjahr.

          APPEND ls_bkpf_aux TO lt_bkpf_aux.

        ENDLOOP. " LOOP AT lt_bkpf INTO DATA(ls_bkpf)

        IF lt_bkpf_aux[] IS NOT INITIAL.
          SELECT *
            INTO TABLE lt_rbkp
            FROM rbkp " Document Header: Invoice Receipt
            FOR ALL ENTRIES IN lt_bkpf_aux
            WHERE belnr = lt_bkpf_aux-belnr
              AND gjahr = lt_bkpf_aux-gjahr.
          SORT lt_rbkp BY belnr gjahr.
        ENDIF. " IF lt_bkpf_aux IS NOT INITIAL

        IF lt_bkpf[] IS NOT INITIAL.
          SELECT *
            INTO TABLE lt_doc
            FROM j_1bnfdoc " Nota Fiscal Header
            FOR ALL ENTRIES IN lt_bkpf
            WHERE belnr = lt_bkpf-belnr
              AND gjahr = lt_bkpf-gjahr.
        ENDIF. " IF lt_bkpf IS NOT INITIAL

      ENDIF. " IF lt_bsik[] IS NOT INITIAL

      PERFORM f_monta_saida TABLES lt_bsid lt_bsik.

  ENDCASE.


*  SELECT *
*    INTO TABLE lt_vbrk
*    FROM vbrk
*    FOR ALL ENTRIES IN lt_bsid
*    WHERE zuonr EQ lt_bsid-zuonr
*    AND   bukrs EQ lt_bsid-bukrs.
*  SORT lt_vbrk BY vbeln.

*  SELECT *
*    INTO TABLE lt_bkpf
*    FROM bkpf
*    FOR ALL ENTRIES IN lt_bsik
*    WHERE bukrs EQ lt_bsik-bukrs
*    AND   belnr EQ lt_bsik-belnr
*    AND   gjahr EQ lt_bsik-gjahr.
*  SORT lt_bkpf BY bukrs belnr gjahr.
*
*  LOOP AT lt_bkpf INTO ls_bkpf.
*    lv_len = strlen( ls_bkpf-awkey ).
*    lv_len = lv_len - 4.
*    lv_string = ls_bkpf-awkey(lv_len).
*
*    SELECT *
*      INTO TABLE lt_rbkp
*      FROM rbkp
*      FOR ALL ENTRIES IN lt_bkpf
*      WHERE belnr = lv_string
*        AND gjahr = lt_bkpf-gjahr.
*    SORT lt_rbkp BY belnr gjahr.
*
*  ENDLOOP.
*
*  SELECT *
*    INTO TABLE lt_doc
*    FROM j_1bnfdoc
*    FOR ALL ENTRIES IN lt_bkpf
*    WHERE belnr = lt_bkpf-belnr
*      AND gjahr = lt_bkpf-gjahr.

*  SELECT *
*    INTO TABLE @lt_rbkp
*    FROM rbkp
*    FOR ALL ENTRIES IN @lt_bkpf
*    WHERE concat( belnr , gjahr ) = @lt_bkpf-awkey.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_FCAT_SPFLI
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_fieldcat TABLES fieldcat
  USING fieldname tabname ref_field ref_table .

* Criação do Fieldcat
  DATA :
    s_fcat TYPE lvc_s_fcat.

  s_fcat-fieldname  = fieldname.
  s_fcat-tabname    = tabname.
  s_fcat-ref_field  = ref_field.
  s_fcat-ref_table  = ref_table.

  IF fieldname = 'XBLNR'.
    CLEAR s_fcat-ref_table.
    s_fcat-scrtext_s = s_fcat-scrtext_m = s_fcat-scrtext_l = 'Nota Fiscal'.
  ELSEIF fieldname = 'INVOICE'.
    CLEAR s_fcat-ref_table.
    s_fcat-scrtext_s = s_fcat-scrtext_m = s_fcat-scrtext_l = 'Invoice'.
  ENDIF.

  APPEND s_fcat TO fieldcat.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_define_supplier_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_define_supplier_header .

* Cabeçalho

  CLEAR o_document.
  CREATE OBJECT o_document.

  CONCATENATE 'Day :' sy-datum 'Time :' sy-uzeit INTO line_header RESPECTING BLANKS.

  CALL METHOD o_document->add_text
    EXPORTING
      text         = line_header
      sap_fontsize = cl_dd_document=>medium
      sap_emphasis = cl_dd_document=>strong.

  CALL METHOD o_document->display_document
    EXPORTING
      parent = o_container_sflight_h.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_define_supplier_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_define_supplier_report .


  DATA:
    lst_layout TYPE lvc_s_layo.

  IF o_docking IS BOUND
    AND o_alv_sflight IS NOT BOUND.

    CREATE OBJECT o_alv_sflight
      EXPORTING
        i_parent          = o_container_sflight   " Parent Container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

*    PERFORM f_fieldcat
*      TABLES  t_fcat_sflight
*      USING : 'CARRID'     'T_SFLIGHT' 'CARRID'     'SFLIGHT',
*              'CONNID'     'T_SFLIGHT' 'CONNID'     'SFLIGHT',
*              'FLDATE'     'T_SFLIGHT' 'FLDATE'     'SFLIGHT',
*              'PRICE'      'T_SFLIGHT' 'PRICE'      'SFLIGHT',
*              'CURRENCY'   'T_SFLIGHT' 'CURRENCY'   'SFLIGHT',
*              'PLANETYPE'  'T_SFLIGHT' 'PLANETYPE'  'SFLIGHT',
*              'PAYMENTSUM' 'T_SFLIGHT' 'PAYMENTSUM' 'SFLIGHT'
    .

    PERFORM f_fieldcat
      TABLES  t_fcat_sflight
      USING : 'BUKRS_SUP' 'LT_BSIK' 'BUKRS' 'BSIK',
              'LIFNR' 'LT_BSIK' 'LIFNR' 'BSIK',
              'BELNR_SUP' 'LT_BSIK' 'BELNR' 'BSIK',
              'XBLNR_SUP' 'LT_RBKP' 'XBLNR' 'RBKP',
*              'INVOICE' 'LT_DOC' 'XBLNR' 'ACCIT',
              'ZUONR_SUP' 'LT_BSIK' 'ZUONR' 'BSIK',
              'BLDAT_SUP' 'LT_BSIK' 'BLDAT' 'BSIK',
              'DMBTR_SUP' 'LT_BSIK' 'DMBTR' 'BSIK',
              'BLART_SUP' 'LT_BSIK' 'BLART' 'BSIK',
              'ZLSPR_SUP' 'LT_BSIK' 'ZLSPR' 'BSID'.

    lst_layout-zebra      = 'X'.
    lst_layout-cwidth_opt = 'X'.

* Evento do hotspot
    LOOP AT t_fcat_sflight INTO s_fcat_sflight.
      IF s_fcat_sflight-fieldname = 'BELNR_SUP'.
        s_fcat_sflight-hotspot = 'x'.
      ENDIF.
      MODIFY t_fcat_sflight FROM s_fcat_sflight.
    ENDLOOP.

* Display ALV
    o_alv_sflight->set_table_for_first_display(
      EXPORTING
        is_layout                     =  lst_layout   " Layout
      CHANGING
        it_outtab                     = lt_saida2    " Output Table
        it_fieldcatalog               = t_fcat_sflight    " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).

    CREATE OBJECT gr_event_handler.
    SET HANDLER gr_event_handler->handle_hotspot_click FOR o_alv_sflight.


  ELSE.
    o_alv_sflight->refresh_table_display(
  EXCEPTIONS
    finished       = 1
    OTHERS         = 2
).
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_saida
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_saida TABLES lt_dados_cliente STRUCTURE bsid
                          lt_dados_fornecedor STRUCTURE bsik.

* LOOP AT lt_bsid INTO DATA(ls_bsid).
*
*    ls_saida-bukrs = ls_bsid-bukrs.
*    ls_saida-kunnr = ls_bsid-kunnr.
*    ls_saida-belnr = ls_bsid-belnr.
*    ls_saida-zuonr = ls_bsid-zuonr.
*    ls_saida-bldat = ls_bsid-bldat.
*    ls_saida-dmbtr = ls_bsid-dmbtr.
*    ls_saida-blart = ls_bsid-blart.
*
*    READ TABLE lt_vbrk INTO DATA(ls_vbrk) WITH KEY vbeln = ls_bsid-vbeln.
*    IF sy-subrc EQ 0.
*
*      ls_saida-xblnr = ls_vbrk-xblnr.
*      ls_saida-vbeln = ls_vbrk-vbeln.
*    ENDIF. " IF sy-subrc EQ 0
*
*    APPEND ls_saida TO lt_saida.
*  ENDLOOP. " LOOP AT lt_bsid INTO DATA(ls_bsid)
*
*  LOOP AT lt_bsik INTO DATA(ls_bsik).
*
*    ls_saida2-bukrs = ls_bsik-bukrs.
*    ls_saida2-lifnr = ls_bsik-lifnr.
*    ls_saida2-belnr = ls_bsik-belnr.
*    ls_saida2-zuonr = ls_bsik-zuonr.
*    ls_saida2-bldat = ls_bsik-bldat.
*    ls_saida2-dmbtr = ls_bsik-dmbtr.
*    ls_saida2-blart = ls_bsik-blart.
*
*    READ TABLE lt_rbkp INTO DATA(ls_rbkp) WITH KEY belnr = ls_bsik-belnr.
*    IF sy-subrc EQ 0.
*
*      ls_saida2-xblnr = ls_rbkp-xblnr.
**      ls_saida2-vbeln = ls_rbkp-vbeln.
*    ENDIF. " IF sy-subrc EQ 0
*
*    READ TABLE lt_doc INTO DATA(ls_doc) WITH KEY bukrs = ls_bsik-bukrs
*                                                 belnr = ls_bsik-belnr
*                                                 gjahr = ls_bsik-gjahr.
*    IF sy-subrc EQ 0.
*
*      CONCATENATE ls_doc-nfenum ls_doc-series INTO ls_saida2-invoice.
*    ENDIF. " IF sy-subrc EQ 0
*
*    APPEND ls_saida2 TO lt_saida2.
*  ENDLOOP. " LOOP AT lt_bsik INTO DATA(ls_bsik)


  LOOP AT lt_dados_cliente INTO DATA(ls_dados_cliente).

    ls_saida-bukrs = ls_dados_cliente-bukrs.
    ls_saida-kunnr = ls_dados_cliente-kunnr.
    ls_saida-belnr_cus = ls_dados_cliente-belnr.
    ls_saida-zuonr = ls_dados_cliente-zuonr.
    ls_saida-bldat = ls_dados_cliente-bldat.
    ls_saida-dmbtr = ls_dados_cliente-dmbtr.
    ls_saida-blart = ls_dados_cliente-blart.
    ls_saida-gjahr = ls_dados_cliente-gjahr.
    ls_saida-zlspr = ls_dados_cliente-zlspr.

    IF ls_dados_cliente-shkzg EQ 'H'.
      ls_saida-dmbtr = abs( ls_saida-dmbtr ) .
    ENDIF.

    READ TABLE lt_vbrk INTO DATA(ls_vbrk) WITH KEY vbeln = ls_dados_cliente-vbeln.
    IF sy-subrc EQ 0.

      ls_saida-xblnr = ls_vbrk-xblnr.
*      ls_saida-vbeln = ls_vbrk-vbeln.
    ENDIF.

    APPEND ls_saida TO lt_saida.
  ENDLOOP.

  LOOP AT lt_dados_fornecedor INTO DATA(ls_dados_fornecedor).

    ls_saida2-bukrs_sup = ls_dados_fornecedor-bukrs.
    ls_saida2-lifnr = ls_dados_fornecedor-lifnr.
    ls_saida2-belnr_sup = ls_dados_fornecedor-belnr.
    ls_saida2-zuonr_sup = ls_dados_fornecedor-zuonr.
    ls_saida2-bldat_sup = ls_dados_fornecedor-bldat.
*    ls_saida2-dmbtr = ls_dados_fornecedor-dmbtr.
    ls_saida2-dmbtr_sup = COND #( WHEN ls_dados_fornecedor-shkzg EQ 'H'
                         THEN ls_dados_fornecedor-dmbtr * -1
                         ELSE ls_dados_fornecedor-dmbtr ).
    ls_saida2-blart_sup = ls_dados_fornecedor-blart.
    ls_saida2-gjahr_sup = ls_dados_fornecedor-gjahr.
    ls_saida2-zlspr_sup = ls_dados_fornecedor-zlspr.

    READ TABLE lt_rbkp INTO DATA(ls_rbkp) WITH KEY belnr = ls_dados_fornecedor-belnr.
    IF sy-subrc EQ 0.

      ls_saida2-xblnr_sup = ls_rbkp-xblnr.
*      ls_saida2-vbeln = ls_rbkp-vbeln.
    ENDIF.

*    READ TABLE lt_doc INTO DATA(ls_doc) WITH KEY bukrs = ls_dados_fornecedor-bukrs
*                                                 belnr = ls_dados_fornecedor-belnr
*                                                 gjahr = ls_dados_fornecedor-gjahr.
*    IF sy-subrc EQ 0.
*
**      CONCATENATE ls_doc-nfenum ls_doc-series INTO ls_saida2-invoice.
*    ENDIF.

    APPEND ls_saida2 TO lt_saida2.
  ENDLOOP.


*  LT_BSIK LT_RBKP

ENDFORM.

*&---------------------------------------------------------------------*
*& Form PREPAR_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepar_catalog .

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = sy-repid
*      i_internal_tabname     = 'LT_SAIDA'
*      i_client_never_display = 'X'
*      i_inclname             = sy-repid
*    CHANGING
*      ct_fieldcat            = lt_fieldcat_slis.
*
*  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
*    EXPORTING
*      it_fieldcat_alv = lt_fieldcat_slis
*    IMPORTING
*      et_fieldcat_lvc = t_fcat_spfli[]
*    TABLES
*      it_data         = itab[]
*    EXCEPTIONS
*      it_data_missing = 1
*      OTHERS          = 2.


  LOOP AT t_fcat_spfli INTO s_fcat_spfli.
    IF s_fcat_spfli-fieldname = 'BELNR_SUPPLIER'.
      s_fcat_spfli-hotspot = 'x'.
    ENDIF.

    MODIFY t_fcat_spfli FROM s_fcat_spfli.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form event_top_of_page
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DG_DYNDOC_ID
*&---------------------------------------------------------------------*
FORM event_top_of_page USING   dg_dyndoc_id TYPE REF TO cl_dd_document.
  "this is more clear.....check it
  "first add text, then pass it to comentry write fm
  DATA : dl_text(255) TYPE c.  "Text
* Populating header to top-of-page
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text      = 'Test Report'
      sap_style = cl_dd_area=>heading.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.

  CLEAR : dl_text.
* Move program ID
  CONCATENATE 'Program Name :' sy-repid
         INTO dl_text SEPARATED BY space.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_excel .

  DATA: d_filename TYPE string,

        d_filepath TYPE string,

        d_fullpath TYPE string.

  DATA: lt_excel TYPE TABLE OF ty_excel,
        ls_excel TYPE ty_excel.

  DATA: lr_excel_structure      TYPE REF TO data,                "  class
        lr_excel_structure2     TYPE REF TO data,  " Runtime Type Services
        lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,  " Runtime Type Services
        lo_table_row_descriptor TYPE REF TO cl_abap_structdescr, " Runtime Type Services
        lv_content              TYPE xstring,
        lt_binary_tab           TYPE TABLE OF sdokcntasc,        " SDOK: line of text document content for Web server
        lt_binary_tab2          TYPE TABLE OF sdokcntasc,        " SDOK: line of text document content for Web server
        lv_length               TYPE i,                          " Length of type Integers
        lv_length2              TYPE i,                          " Length of type Integers
        lv_rows                 TYPE i,                          " Length of type Integers
        lv_rows2                TYPE i.                          " Length of type Integers


  GET REFERENCE OF lt_saida INTO lr_excel_structure.


  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
  lo_table_row_descriptor ?= lo_source_table_descr->get_table_line_type( ).

  DATA(lo_tools_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                               EXPORTING r_data = lr_excel_structure ).

  DATA(lo_config) = lo_tools_xls->configuration( ).

  lo_config->add_column( EXPORTING header_text = 'Empresa Cliente'
                                   field_name = 'BUKRS'
                                   display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column( EXPORTING header_text = 'Cliente'
                             field_name = 'KUNNR'
                             display_type = if_salv_bs_model_column=>uie_text_view ).


  TRY.
      lo_tools_xls->read_result( IMPORTING content = lv_content ).
    CATCH cx_root.
  ENDTRY.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_content
    IMPORTING
      output_length = lv_length
    TABLES
      binary_tab    = lt_binary_tab.

  CLEAR lv_content.
  CLEAR lr_excel_structure.

  GET REFERENCE OF lt_saida2 INTO lr_excel_structure.
*
*  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
*  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
*  lo_table_row_descriptor ?= lo_source_table_descr->get_table_line_type( ).
*
*  DATA(lo_tools_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
*                               EXPORTING r_data = lr_excel_structure ).
*
*  DATA(lo_config) = lo_tools_xls->configuration( ).

  lo_itab_services = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
  lo_table_row_descriptor ?= lo_source_table_descr->get_table_line_type( ).

  lo_tools_xls = cl_salv_export_tool_ats_xls=>create_for_excel(
                               EXPORTING r_data = lr_excel_structure ).

  lo_config = lo_tools_xls->configuration( ).

  lo_config->add_column( EXPORTING header_text = 'Empresa Fornecedora'
                               field_name = 'BUKRS_SUP'
                               display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column( EXPORTING header_text = 'Fornecedor'
                             field_name = 'LIFNR'
                             display_type = if_salv_bs_model_column=>uie_text_view ).

  TRY.
      lo_tools_xls->read_result( IMPORTING content = lv_content ).
    CATCH cx_root.
  ENDTRY.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_content
    IMPORTING
      output_length = lv_length2
    TABLES
      binary_tab    = lt_binary_tab2.



  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = 'XLSX'
*      prompt_on_overwrite  = 'X'
    CHANGING
      filename             = d_filename
      path                 = d_filepath
      fullpath             = d_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_length
      filename                = d_fullpath
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_binary_tab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      OTHERS                  = 11.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_length2
      filename                = d_fullpath
      filetype                = 'BIN'
      append                  = 'X'
    CHANGING
      data_tab                = lt_binary_tab2
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      OTHERS                  = 11.

ENDFORM.
