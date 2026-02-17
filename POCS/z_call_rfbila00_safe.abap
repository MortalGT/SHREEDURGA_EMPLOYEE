*&---------------------------------------------------------------------*
*& Report Z_CALL_RFBILA00_SAFE
*&---------------------------------------------------------------------*
*& Description: Safe call to RFBILA00 avoiding subscreen errors
*&---------------------------------------------------------------------*
REPORT z_call_rfbila00_safe.

PARAMETERS: p_rldnr TYPE fagl_rldnr DEFAULT '0L',
            p_cyear TYPE gjahr DEFAULT '2025'.

START-OF-SELECTION.

  " The error 'SET SCREEN not allowed in subscreens' happens when
  " VIA SELECTION-SCREEN is used in a background or subscreen context.
  " We remove it to execute the report directly with the supplied parameters.

  SUBMIT rfbila00
    WITH rldnr    EQ p_rldnr
    WITH bilavers EQ '1001'
    WITH bilaspra EQ 'EN'
    WITH bilbjahr EQ '2026'
    WITH bilvjahr EQ '2025'
*    WITH bilvjahr EQ p_cyear " Alternative using parameter if needed
    WITH bilagrid EQ 'X'      " Grid Display
    AND RETURN.               " Returns control to this program after execution

  WRITE: / 'RFBILA00 called successfully'.
