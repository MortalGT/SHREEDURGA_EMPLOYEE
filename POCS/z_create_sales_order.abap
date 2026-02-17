*&---------------------------------------------------------------------*
*& Report z_create_sales_order
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_create_sales_order.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD main.
    " Data declarations for EML
    DATA: lt_so_header TYPE TABLE FOR CREATE i_salesordertp,
          lt_so_item   TYPE TABLE FOR CREATE i_salesordertp\_Item,
          lt_so_prcd   TYPE TABLE FOR CREATE i_salesorderitemtp\_ItemPricingElement.

    " 1. Prepare Header Data
    lt_so_header = VALUE #(
      ( %cid                 = 'ROOT1'
        SalesOrderType       = 'OR'          " Standard Order
        SalesOrganization    = '1010'
        DistributionChannel  = '10'
        OrganizationDivision = '00'
        SoldToParty          = '0010100001'
        %control = VALUE #(
                     SalesOrderType       = if_abap_behv=>mk-on
                     SalesOrganization    = if_abap_behv=>mk-on
                     DistributionChannel  = if_abap_behv=>mk-on
                     OrganizationDivision = if_abap_behv=>mk-on
                     SoldToParty          = if_abap_behv=>mk-on
                   )
      )
    ).

    " 2. Prepare Item Data
    lt_so_item = VALUE #(
      ( %cid_ref             = 'ROOT1'       " References header %cid
        %target = VALUE #(
          ( %cid                 = 'ITEM1'
            Material             = 'TG11'
            RequestedQuantity    = '10'
            RequestedQuantityUnit = 'PC'
            %control = VALUE #(
                         Material              = if_abap_behv=>mk-on
                         RequestedQuantity     = if_abap_behv=>mk-on
                         RequestedQuantityUnit = if_abap_behv=>mk-on
                       )
          )
        )
      )
    ).

    " 3. Prepare Pricing Data (Optional example)
    lt_so_prcd = VALUE #(
      ( %cid_ref             = 'ITEM1'       " References item %cid
        %target = VALUE #(
          ( %cid                 = 'COND1'
            ConditionType        = 'PPR0'
            ConditionRateValue   = '100.00'
            ConditionCurrency    = 'EUR'
            %control = VALUE #(
                         ConditionType      = if_abap_behv=>mk-on
                         ConditionRateValue = if_abap_behv=>mk-on
                         ConditionCurrency  = if_abap_behv=>mk-on
                       )
          )
        )
      )
    ).

    " 4. Call MODIFY ENTITIES
    MODIFY ENTITIES OF i_salesordertp
           ENTITY SalesOrder
           CREATE FROM lt_so_header
           CREATE BY \_Item
           FROM lt_so_item
           ENTITY SalesOrderItem
           CREATE BY \_ItemPricingElement
           FROM lt_so_prcd
           MAPPED DATA(ls_mapped)
           FAILED DATA(ls_failed)
           REPORTED DATA(ls_reported).

    " 5. Commit or Rollback
    IF ls_failed IS INITIAL.
      COMMIT ENTITIES
        RESPONSE OF i_salesordertp
        FAILED DATA(ls_commit_failed)
        REPORTED DATA(ls_commit_reported).

      IF ls_commit_failed IS initial.
        " Success: Output the new Sales Order ID
        LOOP AT ls_mapped-salesorder INTO DATA(ls_so_mapped).
          cl_demo_output=>write( |Sales Order Created: { ls_so_mapped-SalesOrder }| ).
        ENDLOOP.
      ELSE.
        cl_demo_output=>write( 'Commit Failed.' ).
      ENDIF.
    ELSE.
       " Error Handling
       cl_demo_output=>write( 'Creation Failed.' ).
       
       " It's good practice to loop through reported messages to see what went wrong
       LOOP AT ls_reported-salesorder INTO DATA(ls_rep).
         DATA(lv_msg) = ls_rep-%msg->if_message~get_text( ).
         cl_demo_output=>write( |Error: { lv_msg }| ).
       ENDLOOP.
    ENDIF.

    cl_demo_output=>display( ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_demo=>main( ).
