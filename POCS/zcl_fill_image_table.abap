CLASS zcl_fill_image_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_fill_image_table IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA: lt_images TYPE TABLE OF ztt_image.

    " Clear existing data
    DELETE FROM ztt_image.

    " Append new values
    lt_images = VALUE #(
      ( id = '001' text = 'GATE' pic_url = 'https://www.flaticon.com/free-icon/box-truck_1048329' )
    ).

    INSERT ztt_image FROM TABLE @lt_images.

    IF sy-subrc = 0.
      out->write( |{ lines( lt_images ) } entries inserted successfully.| ).
    ELSE.
      out->write( 'Error inserting entries.' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
