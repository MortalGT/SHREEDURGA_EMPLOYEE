*&---------------------------------------------------------------------*
*& Report Z_CALL_RFC_SAP_TO_MES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_call_rfc_sap_to_mes.

DATA: lv_res_no     TYPE string VALUE '1000136649',
      lv_plant      TYPE string VALUE 'KM10',
      lv_order_no   TYPE string VALUE '0000011261',
      lv_rec_ind    TYPE c LENGTH 1 VALUE 'C',
      lv_material   TYPE string VALUE 'RE615T2B2 G12R',
      lv_quantity   TYPE p DECIMALS 3 VALUE '20.000',
      lv_unit       TYPE string VALUE 'EA',
      lv_short_text TYPE string VALUE 'L002*C132 EXTREME-G2G(TRIAL-210)',
      lv_mat_long   TYPE string VALUE 'RE615T2B2 G12R',
      lv_priority   TYPE string VALUE '5',
      lv_location   TYPE string VALUE 'REKM',
      lv_msg        TYPE string.

" You may need to define variables for EXPORT parameters or TABLES if the FM returns data
" DATA: ...

PARAMETERS: p_dest TYPE rfcdest DEFAULT 'SAP_TO_MES'.

START-OF-SELECTION.

  IF p_dest IS INITIAL.
    CALL FUNCTION 'ZAPI_SAP_TO_MES'
      EXPORTING
        res_no     = lv_res_no
        plant      = lv_plant
        order_no   = lv_order_no
        rec_ind    = lv_rec_ind
        material   = lv_material
        quantity   = lv_quantity
        unit       = lv_unit
        short_text = lv_short_text
        mat_long   = lv_mat_long
        priority   = lv_priority
        location   = lv_location
      EXCEPTIONS
        OTHERS     = 1.
  ELSE.
    CALL FUNCTION 'ZAPI_SAP_TO_MES'
      DESTINATION p_dest
      EXPORTING
        res_no                = lv_res_no
        plant                 = lv_plant
        order_no              = lv_order_no
        rec_ind               = lv_rec_ind
        material              = lv_material
        quantity              = lv_quantity
        unit                  = lv_unit
        short_text            = lv_short_text
        mat_long              = lv_mat_long
        priority              = lv_priority
        location              = lv_location
      EXCEPTIONS
        system_failure        = 1 MESSAGE lv_msg
        communication_failure = 2 MESSAGE lv_msg
        OTHERS                = 3.
  ENDIF.

  IF sy-subrc <> 0.
    WRITE: / 'Error calling RFC. Subrc:', sy-subrc.
    IF lv_msg IS NOT INITIAL.
      WRITE: / 'Message:', lv_msg.
    ENDIF.
  ELSE.
    WRITE: / 'RFC called successfully'.
  ENDIF.
