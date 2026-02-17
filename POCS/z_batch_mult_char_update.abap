*&---------------------------------------------------------------------*
*& Report Z_BATCH_MULT_CHAR_UPDATE
*&---------------------------------------------------------------------*
*& Description: Example of updating multiple characteristics for one batch
*&---------------------------------------------------------------------*
REPORT z_batch_mult_char_update.

" To update multiple characteristics for the SAME batch, 
" you must provide multiple entries in the main table of the MODIFY statement.
" Each entry attempts to modify values for a distinct Characteristic ID.

MODIFY ENTITIES OF I_BatchTP_2
  ENTITY BatchCharacteristic
  CREATE BY \_BatchCharacteristicValueTP
  FROM VALUE #(
    " 1st Characteristic Update
    (
      Material        = 'MATERIAL_A'
      Batch           = 'BATCH_1'
      CharcInternalID = '0000008540'   " ID for Characteristic 1 (e.g. Color)
      %target         = VALUE #(
        ( %cid                    = 'CID_1'
          Material                = 'MATERIAL_A'
          Batch                   = 'BATCH_1'
          CharcInternalID         = '0000008540'
          CharcValue              = 'RED'
          %control-Material       = cl_abap_behv=>flag_changed
          %control-Batch          = cl_abap_behv=>flag_changed
          %control-CharcInternalID = cl_abap_behv=>flag_changed
          %control-CharcValue     = cl_abap_behv=>flag_changed
        )
      )
    )
    " 2nd Characteristic Update
    (
      Material        = 'MATERIAL_A'
      Batch           = 'BATCH_1'
      CharcInternalID = '0000008541'   " ID for Characteristic 2 (e.g. Size)
      %target         = VALUE #(
        ( %cid                    = 'CID_2'
          Material                = 'MATERIAL_A'
          Batch                   = 'BATCH_1'
          CharcInternalID         = '0000008541'
          CharcValue              = 'XL'
          %control-Material       = cl_abap_behv=>flag_changed
          %control-Batch          = cl_abap_behv=>flag_changed
          %control-CharcInternalID = cl_abap_behv=>flag_changed
          %control-CharcValue     = cl_abap_behv=>flag_changed
        )
      )
    )
  )
  MAPPED DATA(mapped)
  FAILED DATA(failed)
  REPORTED DATA(reported).

IF failed IS NOT INITIAL.
  " Handle errors
  WRITE: / 'Update failed'.
ELSE.
  COMMIT ENTITIES.
  WRITE: / 'Update successful'.
ENDIF.
