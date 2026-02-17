*&---------------------------------------------------------------------*
*& Class ZCL_HTTP_HANDLER_APPROVE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS zcl_http_handler_approve DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_http_handler_approve IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: lv_reqid  TYPE string,
          lv_action TYPE string,
          lv_html   TYPE string.

    " 1. Get Query Parameters
    lv_reqid = server->request->get_form_field( 'reqid' ).
    lv_action = server->request->get_form_field( 'action' ).

    " 2. Process Action (Dummy Logic)
    " In a real scenario, you would call a business logic class here
    " e.g., zcl_approval_logic=>process( reqid = lv_reqid action = lv_action ).

    " 3. Prepare Response HTML
    lv_html = '<html><head><title>Approval Status</title>' &&
              '<style>body { font-family: Arial, sans-serif; text-align: center; margin-top: 50px; }</style>' &&
              '</head><body>'.

    IF lv_action = 'APPROVE'.
      lv_html = lv_html &&
                '<h1 style="color: green;">Request ' && lv_reqid && ' Approved &#x2705;</h1>' &&
                '<p>Thank you. The request has been successfully approved.</p>'.
    ELSEIF lv_action = 'REJECT'.
      lv_html = lv_html &&
                '<h1 style="color: red;">Request ' && lv_reqid && ' Rejected &#x274C;</h1>' &&
                '<p>The request has been rejected.</p>'.
    ELSE.
      lv_html = lv_html &&
                '<h1>Invalid Action</h1>' &&
                '<p>Unknown action received.</p>'.
    ENDIF.

    lv_html = lv_html && '</body></html>'.

    " 4. Send Response
    server->response->set_cdata( data = lv_html ).
    server->response->set_header_field( name = 'Content-Type' value = 'text/html' ).
    server->response->set_status( code = 200 reason = 'OK' ).

  ENDMETHOD.
ENDCLASS.
