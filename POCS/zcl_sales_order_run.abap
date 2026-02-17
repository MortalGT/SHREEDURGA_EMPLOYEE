CLASS zcl_sales_order_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sales_order_run IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: lt_so_header TYPE TABLE FOR CREATE i_salesordertp,
          lt_so_item   TYPE TABLE FOR CREATE i_salesordertp\_Item,
          lt_so_prcd   TYPE TABLE FOR CREATE i_salesorderitemtp\_ItemPricingElement.

    TYPES: BEGIN OF ty_data,
             Vkorg      TYPE string,
             Vtweg      TYPE string,
             Spart      TYPE string,
             Kunnr      TYPE string,
             Bstkd      TYPE string,
             Matnr      TYPE string,
             Quantity   TYPE string,
             Unit       TYPE string,
             Plant      TYPE string,
             Batch      TYPE string,
             Sloc       TYPE string,
             BasePrice  TYPE string,
             Currency   TYPE string,
             LineNumber TYPE string,
           END OF ty_data.

    DATA: lt_data TYPE TABLE OF ty_data.
    DATA: lv_date TYPE dats.

    lv_date = cl_abap_context_info=>get_system_date( ).

    " 1. Prepare Sample Data (2 Line Items)
    lt_data = VALUE #(
      ( Vkorg = '1010' Vtweg = '10' Spart = '00' Kunnr = '0000001010' Bstkd = 'TEST_RUN_001'
        Matnr = 'TG11' Quantity = '1' Unit = 'PC' Plant = '1010' Batch = '' Sloc = ''
        BasePrice = '100' Currency = 'EUR' LineNumber = '000010' )
      ( Vkorg = '1010' Vtweg = '10' Spart = '00' Kunnr = '0000001010' Bstkd = 'TEST_RUN_001'
        Matnr = 'TG12' Quantity = '2' Unit = 'PC' Plant = '1010' Batch = '' Sloc = ''
        BasePrice = '150' Currency = 'EUR' LineNumber = '000020' )
    ).

    READ TABLE lt_data INTO DATA(wa_xldata) INDEX 1.
    IF sy-subrc = 0.
      " 2. Prepare Header (Create only once)
      APPEND VALUE #(
        %cid     = 'H001'
        %data    = VALUE #(
                     salesordertype          = 'TA' " Or 'OR'
                     salesorganization       = wa_xldata-Vkorg
                     distributionchannel     = wa_xldata-Vtweg
                     organizationdivision    = wa_xldata-Spart
                     soldtoparty             = wa_xldata-Kunnr
                     purchaseorderbycustomer = wa_xldata-Bstkd
                     CustomerPaymentTerms    = '0001'
                     pricingdate             = lv_date
                     requesteddeliverydate   = lv_date
                   )
        %control = VALUE #(
                     salesordertype          = if_abap_behv=>mk-on
                     salesorganization       = if_abap_behv=>mk-on
                     distributionchannel     = if_abap_behv=>mk-on
                     organizationdivision    = if_abap_behv=>mk-on
                     soldtoparty             = if_abap_behv=>mk-on
                     purchaseorderbycustomer = if_abap_behv=>mk-on
                     pricingdate             = if_abap_behv=>mk-on
                     requesteddeliverydate   = if_abap_behv=>mk-on
                     CustomerPaymentTerms    = if_abap_behv=>mk-on
                   )
      ) TO lt_so_header.
    ENDIF.

    " 3. Loop for Items and Pricing
    LOOP AT lt_data INTO wa_xldata.

      " --- A. Pricing Logic ---
      " We create a temporary target table for the pricing of THIS item
      DATA: lt_pricing_target TYPE TABLE FOR CREATE i_salesorderitemtp\_ItemPricingElement-%target.

      APPEND VALUE #(
        %cid                = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( )
        conditiontype       = 'ZESP'
        conditionrateamount = wa_xldata-BasePrice
        conditioncurrency   = wa_xldata-Currency
        conditionquantity   = '1'
        ConditionQuantityUnit = 'EA'
        %control-conditiontype       = if_abap_behv=>mk-on
        %control-conditionrateamount = if_abap_behv=>mk-on
        %control-conditioncurrency   = if_abap_behv=>mk-on
        %control-conditionquantity   = if_abap_behv=>mk-on
        %control-ConditionQuantityUnit = if_abap_behv=>mk-on
      ) TO lt_pricing_target.

      " Append to the main Pricing Table, linking to the Item's CID (LineNumber)
      APPEND VALUE #(
        %cid_ref       = wa_xldata-LineNumber
        salesorder     = space
        SalesOrderItem = space
        %target        = lt_pricing_target
      ) TO lt_so_prcd.


      " --- B. Item Logic ---
      " We create a temporary target table for THIS item
      DATA: lt_item_target TYPE TABLE FOR CREATE i_salesordertp\_Item-%target.

      APPEND VALUE #(
        %cid                           = wa_xldata-LineNumber " This acts as the CID for the Item
        product                        = wa_xldata-Matnr
        %control-product               = if_abap_behv=>mk-on
        materialbycustomer             = wa_xldata-Matnr
        %control-materialbycustomer    = if_abap_behv=>mk-on
        requestedquantity              = wa_xldata-Quantity
        %control-requestedquantity     = if_abap_behv=>mk-on
        requestedquantityunit          = wa_xldata-Unit
        %control-requestedquantityunit = if_abap_behv=>mk-on
        plant                          = wa_xldata-Plant
        %control-plant                 = if_abap_behv=>mk-on
        Batch                          = wa_xldata-Batch
        %control-Batch                 = if_abap_behv=>mk-on
        storagelocation                = wa_xldata-Sloc
        %control-storagelocation       = if_abap_behv=>mk-on
      ) TO lt_item_target.

      " Append to the main Item Table, linking to the Header's CID ('H001')
      APPEND VALUE #(
        %cid_ref   = 'H001'
        salesorder = space
        %target    = lt_item_target
      ) TO lt_so_item.

    ENDLOOP.

    " 4. Executing MODIFY ENTITIES
    MODIFY ENTITIES OF i_salesordertp
           FORWARDING PRIVILEGED
           ENTITY salesorder
           CREATE
           FROM lt_so_header
           CREATE BY \_item FROM lt_so_item
           ENTITY SalesOrderItem
           CREATE BY \_ItemPricingElement FROM lt_so_prcd
           MAPPED DATA(ls_mapped)
           FAILED DATA(ls_failed)
           REPORTED DATA(ls_reported).

    " 5. Commit or Error Handling
    IF ls_failed IS NOT INITIAL.
      " Output errors
      LOOP AT ls_reported-salesorder ASSIGNING FIELD-SYMBOL(<ls_rep_so>).
        out->write( <ls_rep_so>-%msg->if_message~get_text( ) ).
      ENDLOOP.
      LOOP AT ls_reported-salesorderitem ASSIGNING FIELD-SYMBOL(<ls_rep_item>).
        out->write( <ls_rep_item>-%msg->if_message~get_text( ) ).
      ENDLOOP.
    ELSE.
      COMMIT ENTITIES RESPONSE OF i_salesordertp
        FAILED DATA(ls_commit_failed)
        REPORTED DATA(ls_commit_reported).

      IF ls_commit_failed IS INITIAL.
        out->write( 'Sales Order Created Successfully.' ).
        LOOP AT ls_mapped-salesorder INTO DATA(ls_so_map).
          out->write( |Sales Order ID: { ls_so_map-SalesOrder }| ).
        ENDLOOP.
      ELSE.
        out->write( 'Commit Failed.' ).
        LOOP AT ls_commit_reported-salesorder INTO DATA(ls_crep).
          out->write( ls_crep-%msg->if_message~get_text( ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
