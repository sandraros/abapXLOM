CLASS zcl_xlom__pv_range_create DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom_application "INTERSECT
                 zcl_xlom_range
                 zcl_xlom__ex_op_colon
                 "zcl_xlom__ex_op_comma TODO
                 .

  PRIVATE SECTION.
    CLASS-METHODS create_from_top_left_bottom_ri
      IMPORTING worksheet     TYPE REF TO zcl_xlom_worksheet
                top_left      TYPE zcl_xlom=>ts_range_address-top_left
                bottom_right  TYPE zcl_xlom=>ts_range_address-bottom_right
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_range.
ENDCLASS.


CLASS zcl_xlom__pv_range_create IMPLEMENTATION.
  METHOD create_from_top_left_bottom_ri.
    " B2:A1 becomes A1:B2, 4:3 becomes 3:4, etc.
    DATA(correctly_ordered_address) = VALUE zcl_xlom=>ts_range_address( top_left     = top_left
                                                                        bottom_right = bottom_right ).
    IF top_left-column > bottom_right-column.
      correctly_ordered_address-top_left-column = bottom_right-column.
      correctly_ordered_address-bottom_right-column = top_left-column.
    ENDIF.
    IF top_left-row > bottom_right-row.
      correctly_ordered_address-top_left-row = bottom_right-row.
      correctly_ordered_address-bottom_right-row = top_left-row.
    ENDIF.

    " Whole rows or columns.
    " e.g. 3:4 becomes A3:XFD4
    " e.g. A:A becomes A1:A1048576
    " If row = 0, it means the range is a whole column (rows from 1 to 1048576).
    " If column = 0, it means the range is a whole row (columns from 1 to 16384).
    DATA(address) = VALUE zcl_xlom=>ts_range_address(
        top_left     = VALUE #( row    = COND #( WHEN correctly_ordered_address-top_left-row > 0
                                                 THEN correctly_ordered_address-top_left-row
                                                 ELSE 1 )
                                column = COND #( WHEN correctly_ordered_address-top_left-column > 0
                                                 THEN correctly_ordered_address-top_left-column
                                                 ELSE 1 ) )
        bottom_right = VALUE #( row    = COND #( WHEN correctly_ordered_address-bottom_right-row > 0
                                                 THEN correctly_ordered_address-bottom_right-row
                                                 ELSE zcl_xlom__ext_worksheet=>max_rows )
                                column = COND #( WHEN correctly_ordered_address-bottom_right-column > 0
                                                 THEN correctly_ordered_address-bottom_right-column
                                                 ELSE zcl_xlom__ext_worksheet=>max_columns ) ) ).

    DATA(range_buffer_line) = REF #( zcl_xlom_range=>_range_buffer[ worksheet = worksheet
                                                                    address   = address ] OPTIONAL ).
    IF range_buffer_line IS NOT BOUND.
      DATA(range) = NEW zcl_xlom_range( ).

      range->zif_xlom__va~type               = zif_xlom__va=>c_type-range.
      range->application                     = worksheet->application.
      range->parent                          = worksheet.
      range->_address                        = address.
      range->column                          = address-top_left-column.
      range->row                             = address-top_left-row.
      range->zif_xlom__va_array~row_count    = address-bottom_right-row - address-top_left-row + 1.
      range->zif_xlom__va_array~column_count = address-bottom_right-column - address-top_left-column + 1.
*      DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( range )-inside_used_range.
      range->zif_xlom__va_array~used_range   = zcl_xlom__ut=>intersect_used_range( range )-inside_used_range.
*      range->zif_xlom__va_array~used_range   = VALUE #( top_left     = VALUE #( )
*                                                        row_count    = range->zif_xlom__va_array~row_count
*                                                        column_count = range->zif_xlom__va_array~column_count ).

      INSERT VALUE #( worksheet = worksheet
                      address   = address
                      object    = range )
             INTO TABLE zcl_xlom_range=>_range_buffer
             REFERENCE INTO range_buffer_line.
    ENDIF.
    result = range_buffer_line->object.
  ENDMETHOD.
ENDCLASS.
