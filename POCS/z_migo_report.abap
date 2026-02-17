*&---------------------------------------------------------------------*
*& Report Z_MIGO_REPORT
*&---------------------------------------------------------------------*
*& Description: Report to get material documents (101, 105) against PO
*&              for material type HALB and material groups 10, 20, 30.
*&              Includes Batch Characteristics retrieval.
*&---------------------------------------------------------------------*
REPORT z_migo_report.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: matdoc.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_data,
         mblnr           TYPE matdoc-mblnr,
         mjahr           TYPE matdoc-mjahr,
         zeile           TYPE matdoc-zeile,
         werks           TYPE matdoc-werks,
         matnr           TYPE matdoc-matnr,
         lgort           TYPE matdoc-lgort,
         charg           TYPE matdoc-charg,
         bwart           TYPE matdoc-bwart,
         budat           TYPE matdoc-budat,
         ebeln           TYPE matdoc-ebeln,
         ebelp           TYPE matdoc-ebelp,
         matkl           TYPE mara-matkl,
         mtart           TYPE mara-mtart,
         " Batch Characteristics
         container       TYPE agmem, " Using generic type, adjust as needed
         grade           TYPE agmem,
         lot_no          TYPE agmem,
         packtype        TYPE agmem,
         prod_type       TYPE agmem,
         gross_weight    TYPE agmem,
         tare_weight     TYPE agmem,
         net_weight      TYPE agmem,
       END OF ty_data.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_werks FOR matdoc-werks OBLIGATORY,
                  s_matnr FOR matdoc-matnr,
                  s_budat FOR matdoc-budat,
                  s_ebeln FOR matdoc-ebeln. " Purchase Order
SELECTION-SCREEN END OF BLOCK b1.

* Initialization
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM get_batch_characteristics.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data.

  " Select Data from MATDOC (or MSEG if S/4 view logic preferred, but MATDOC is direct)
  " Joining with MARA for material attributes
  
  SELECT m~mblnr, m~mjahr, m~zeile, m~werks, m~matnr, m~lgort, m~charg, m~bwart,
         m~budat, m~ebeln, m~ebelp, a~matkl, a~mtart
    FROM matdoc AS m
    INNER JOIN mara AS a ON m~matnr = a~matnr
    INTO CORRESPONDING FIELDS OF TABLE @gt_data
    WHERE m~werks IN @s_werks
      AND m~matnr IN @s_matnr
      AND m~budat IN @s_budat
      AND m~ebeln IN @s_ebeln
      AND m~ebeln NE @space       " Must be against Purchase Order
      AND m~bwart IN ('101', '105')
      AND m~record_type = 'MDOC'  " Material Documents
      AND a~mtart EQ 'HALB'
      AND a~matkl IN ('10', '20', '30').

  IF sy-subrc NE 0.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_batch_characteristics
*&---------------------------------------------------------------------*
FORM get_batch_characteristics.
  DATA: lt_char_values TYPE TABLE OF bapi_char_values,
        ls_char_values TYPE bapi_char_values,
        lt_return      TYPE TABLE OF bapiret2,
        lv_objkey      TYPE bapi1003_key-object,
        lv_batch       TYPE mcha-charg,
        lv_matnr       TYPE mcha-matnr,
        lv_werks       TYPE mcha-werks.

  FIELD-SYMBOLS: <fs_data> TYPE ty_data.

  LOOP AT gt_data ASSIGNING <fs_data>.
    CHECK <fs_data>-charg IS NOT INITIAL.

    " Construct Object Key for Batch Classification
    " Usually Material(18) + Plant(4) + Batch(10) depending on Batch Level definition (Plant vs Client)
    " Assuming Batch Level at Material/Plant for this logic, passing specific params to helper if needed
    " However, BAPI_OBJCL_GETDETAIL expects generic object key.
    " For Batches (MCH1/MCHA), lookup object number often needed, but easy way:
    " Search for class type 023 (Batch)

    " Using helper function to get characteristics is easier.
    " Or use standard BAPI_OBJCL_GETDETAIL. 
    " ObjectKey for Batch is often Concatenation of MATNR and CHARG (and WERKS) depends on Class Setup.
    
    " Let's use simple function module 'BAPI_BATCH_GET_DETAIL' which returns Classification data nicely.
    
    DATA: lt_char_val TYPE TABLE OF bapibatchatt,
          ls_char_val TYPE bapibatchatt.
          
    CALL FUNCTION 'BAPI_BATCH_GET_DETAIL'
      EXPORTING
        material       = <fs_data>-matnr
        batch          = <fs_data>-charg
        plant          = <fs_data>-werks
      TABLES
        batchattend    = lt_char_val
        return         = lt_return.
        
    IF lt_char_val IS NOT INITIAL.
      LOOP AT lt_char_val INTO ls_char_val.
        CASE ls_char_val-charact.
          WHEN 'CONTAINER'.
             <fs_data>-container = ls_char_val-char_value.
          WHEN 'GRADE'.
             <fs_data>-grade = ls_char_val-char_value.
          WHEN 'LOT_NO' OR 'LOT NO'. " Handling variations
             <fs_data>-lot_no = ls_char_val-char_value.
          WHEN 'PACKTYPE'.
             <fs_data>-packtype = ls_char_val-char_value.
          WHEN 'PRODUCTION_TYPE' OR 'PRODUCTION TYPE'.
             <fs_data>-prod_type = ls_char_val-char_value.
          WHEN 'GROSS_WEIGHT' OR 'GROSS WEIGHT'.
             <fs_data>-gross_weight = ls_char_val-char_value.
          WHEN 'TARE_WEIGHT' OR 'TARE WEIGHT'.
             <fs_data>-tare_weight = ls_char_val-char_value.
          WHEN 'NET_WEIGHT' OR 'NET WEIGHT'.
             <fs_data>-net_weight = ls_char_val-char_value.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    
    REFRESH: lt_char_val, lt_return.
    
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
FORM display_data.
  DATA: lo_alv TYPE REF TO cl_salv_table,
        lo_funcs TYPE REF TO cl_salv_functions_list,
        lo_cols TYPE REF TO cl_salv_columns_table,
        lr_col  TYPE REF TO cl_salv_column.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_data ).
    CATCH cx_salv_msg.
  ENDTRY.

  lo_funcs = lo_alv->get_functions( ).
  lo_funcs->set_all( abap_true ).

  lo_cols = lo_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).
  
  " Optional: Rename Columns if needed
  TRY.
      lr_col = lo_cols->get_column( 'MBLNR' ).
      lr_col->set_medium_text( 'Mat. Doc.' ).
      
      lr_col = lo_cols->get_column( 'CONTAINER' ).
      lr_col->set_medium_text( 'Container' ).
      
      " ... Set other headers as needed ...
    CATCH cx_salv_not_found.
  ENDTRY.

  lo_alv->display( ).

ENDFORM.
