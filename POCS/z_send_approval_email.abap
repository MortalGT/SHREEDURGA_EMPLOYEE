*&---------------------------------------------------------------------*
*& Report Z_SEND_APPROVAL_EMAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_send_approval_email.

PARAMETERS: p_email TYPE ad_smtpadr OBLIGATORY DEFAULT 'recipient@example.com',
            p_url   TYPE string LOWER CASE OBLIGATORY DEFAULT 'https://sapserver:443/sap/bc/zapprove'.

DATA: lo_send_request TYPE REF TO cl_bcs,
      lo_document     TYPE REF TO cl_document_bcs,
      lo_sender       TYPE REF TO cl_sapuser_bcs,
      lo_recipient    TYPE REF TO if_recipient_bcs,
      lt_text         TYPE soli_tab,
      lv_sent_to_all  TYPE os_boolean,
      lx_document_bcs TYPE REF TO cx_document_bcs,
      lv_link_approve TYPE string,
      lv_link_reject  TYPE string.

START-OF-SELECTION.

  TRY.
      " 1. Create the persistent send request
      lo_send_request = cl_bcs=>create_persistent( ).

      " Construct Links
      lv_link_approve = p_url && '?reqid=123&action=APPROVE'.
      lv_link_reject  = p_url && '?reqid=123&action=REJECT'.

      " 2. Create the HTML content
      APPEND '<html>' TO lt_text.
      APPEND '  <body>' TO lt_text.
      APPEND '    <p>Dear Approver,</p>' TO lt_text.
      APPEND '    <p>Please review the request.</p>' TO lt_text.
      APPEND '    <br>' TO lt_text.
      
      " Buttons Container
      APPEND '    <div style="display: flex; gap: 20px;">' TO lt_text.
      
      " Approve Button
      APPEND '      <a href="' && lv_link_approve && '" style="background-color: #4CAF50; color: white; padding: 10px 20px; text-align: center; text-decoration: none; display: inline-block; border-radius: 4px; margin-right: 15px;">' TO lt_text.
      APPEND '        &#x2705; Approve' TO lt_text.
      APPEND '      </a>' TO lt_text.

      " Reject Button
      APPEND '      <a href="' && lv_link_reject && '" style="background-color: #f44336; color: white; padding: 10px 20px; text-align: center; text-decoration: none; display: inline-block; border-radius: 4px;">' TO lt_text.
      APPEND '        &#x274C; Reject' TO lt_text.
      APPEND '      </a>' TO lt_text.
      
      APPEND '    </div>' TO lt_text.
      APPEND '    <br>' TO lt_text.
      APPEND '    <p>Regards,<br/>SAP System</p>' TO lt_text.
      APPEND '  </body>' TO lt_text.
      APPEND '</html>' TO lt_text.

      " 3. Create the document instance
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_text    = lt_text
        i_subject = 'Approval Request - Action Required'
      ).

      " 4. Add document to send request
      lo_send_request->set_document( lo_document ).

      " 5. Set sender (optional - default is current user)
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).
      lo_send_request->set_sender( lo_sender ).

      " 6. Set recipient
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ).
      lo_send_request->add_recipient(
        i_recipient = lo_recipient
        i_express   = 'X'
      ).

      " 7. Send the email
      lv_sent_to_all = lo_send_request->send( i_with_error_screen = 'X' ).

      IF lv_sent_to_all = 'X'.
        COMMIT WORK.
        WRITE: / 'Email sent successfully to', p_email.
      ELSE.
        ROLLBACK WORK.
        WRITE: / 'Error sending email.'.
      ENDIF.

    CATCH cx_document_bcs INTO lx_document_bcs.
      WRITE: / 'Exception occurred:', lx_document_bcs->get_text( ).
  ENDTRY.