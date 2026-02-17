CLASS zcl_reservation_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_reservation_run IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    MODIFY ENTITIES OF i_reservationdocumenttp
      ENTITY reservationdocument
        CREATE FROM VALUE #( ( %cid = 'CID_001'
                               goodsmovementtype = '311'
                               reservationdate = sy-datum
                               issuingorreceivingplant = '1200'
                               issuingorreceivingstorageloc = '1219'

                               " costcenter = '000100100' " Not typically required for 311
                               " ResvnVerificationCompanyCode = '1010' " Adjust if needed for Plant 1200
                               issuingorreceivingplant = '1200'
                               issuingorreceivingstorageloc = '1219'
                               %control-goodsmovementtype = cl_abap_behv=>flag_changed
                               %control-reservationdate = cl_abap_behv=>flag_changed
                               %control-issuingorreceivingplant = cl_abap_behv=>flag_changed
                               %control-issuingorreceivingstorageloc = cl_abap_behv=>flag_changed
                               " %control-costcenter = cl_abap_behv=>flag_changed
                               " %control-ResvnVerificationCompanyCode = cl_abap_behv=>flag_changed
                             ) )
      ENTITY reservationdocument
        CREATE BY \_reservationdocumentitemtp
        FROM VALUE #( (
                        %cid_ref = 'CID_001'
                        %target = VALUE #( ( %cid = 'CID_ITM_001'
                                             plant = '1200'
                                             product = '1000000025'
                                             storagelocation = '1292'                  " Issuing Storage Location
                                             batch = '0000000099'
                                             resvnitmrequiredqtyinentryunit = '46.684'
                                             entryunit = 'KG'
                                             ResvnItmIssuingOrReceivingPlnt = '1200'   " Receiving Plant
                                             ResvnItmIssuingOrReceivingStor = '1219'   " Receiving Storage Location
                                             %control-plant = cl_abap_behv=>flag_changed
                                             %control-product = cl_abap_behv=>flag_changed
                                             %control-storagelocation = cl_abap_behv=>flag_changed
                                             %control-batch = cl_abap_behv=>flag_changed
                                             %control-resvnitmrequiredqtyinentryunit = cl_abap_behv=>flag_changed
                                             %control-entryunit = cl_abap_behv=>flag_changed
                                             %control-ResvnItmIssuingOrReceivingPlnt = cl_abap_behv=>flag_changed
                                             %control-ResvnItmIssuingOrReceivingStor = cl_abap_behv=>flag_changed
                                           ) )
                      ) )
      MAPPED DATA(ls_create_mapped)
      FAILED DATA(ls_create_failed)
      REPORTED DATA(ls_create_reported).

    IF ls_create_failed IS INITIAL.
      COMMIT ENTITIES
        RESPONSE OF i_reservationdocumenttp
        FAILED DATA(ls_commit_failed)
        REPORTED DATA(ls_commit_reported).

      IF ls_commit_failed IS INITIAL.
        out->write( 'Reservation created successfully.' ).
        LOOP AT ls_create_mapped-reservationdocument INTO DATA(ls_res_doc).
          out->write( |Reservation Number: { ls_res_doc-reservation }| ).
        ENDLOOP.
      ELSE.
        out->write( 'Commit failed.' ).
        out->write( ls_commit_reported ).
      ENDIF.

    ELSE.
      out->write( 'Creation failed.' ).
      out->write( ls_create_reported ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
