CLASS zcl_xlom__pv_range_create DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom_application
                 zcl_xlom_range
                 zcl_xlom__ex_op_colon.

  PRIVATE SECTION.
*    "! @parameter column_row_collection | By default, the "count" method counts the number of cells.
*    "!                                    It's possible to make it count only the columns or rows
*    "!                                    when the range is created by the methods "columns" or "rows".
    CLASS-METHODS create_from_top_left_bottom_ri
      IMPORTING worksheet     TYPE REF TO zcl_xlom_worksheet
                top_left      TYPE zcl_xlom=>ts_range_address-top_left
                bottom_right  TYPE zcl_xlom=>ts_range_address-bottom_right
*                column_row_collection TYPE ty_column_row_collection DEFAULT c_column_row_collection-none
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_range.
*
*    CLASS-METHODS get_correctly_ordered_address
*      IMPORTING top_left      TYPE zcl_xlom=>ts_range_address_one_cell
*                bottom_right  TYPE zcl_xlom=>ts_range_address_one_cell
*      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.
ENDCLASS.


CLASS zcl_xlom__pv_range_create IMPLEMENTATION.
  METHOD create_from_top_left_bottom_ri.
    DATA range TYPE REF TO zcl_xlom_range.

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
*    DATA(correctly_ordered_address) = get_correctly_ordered_address( top_left     = top_left
*                                                                     bottom_right = bottom_right ).

    " Whole rows or columns.
    " 3:4 becomes A3:XFD4, etc.
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
*                                                    column_row_collection = column_row_collection ] OPTIONAL ).
    IF range_buffer_line IS NOT BOUND.
      range = NEW zcl_xlom_range( ).
*      CASE column_row_collection.
*        WHEN c_column_row_collection-columns.
*          range = NEW zcl_xlom_columns( ).
*        WHEN c_column_row_collection-rows.
*          range = NEW zcl_xlom_rows( ).
*        WHEN c_column_row_collection-none.
*          range = NEW zcl_xlom_range( ).
*        WHEN OTHERS.
*          RAISE EXCEPTION TYPE zcx_xlom_unexpected.
*      ENDCASE.

      range->zif_xlom__va~type               = zif_xlom__va=>c_type-range.
      range->application                     = worksheet->application.
      range->parent                          = worksheet.
      range->_address                        = address.
      range->column                          = range->_address-top_left-column.
      range->row                             = range->_address-top_left-row.
      range->zif_xlom__va_array~row_count    = address-bottom_right-row - address-top_left-row + 1.
      range->zif_xlom__va_array~column_count = address-bottom_right-column - address-top_left-column + 1.

      INSERT VALUE #( worksheet = worksheet
                      address   = address
*                      column_row_collection = column_row_collection
                      object    = range )
             INTO TABLE zcl_xlom_range=>_range_buffer
             REFERENCE INTO range_buffer_line.
    ENDIF.
    result = range_buffer_line->object.
*  ENDMETHOD.
*
*  METHOD get_correctly_ordered_address.
*    result-top_left     = top_left.
*    result-bottom_right = bottom_right.
*    IF top_left-column > bottom_right-column.
*      result-top_left-column = bottom_right-column.
*      result-bottom_right-column = top_left-column.
*    ENDIF.
*    IF top_left-row > bottom_right-row.
*      result-top_left-row = bottom_right-row.
*      result-bottom_right-row = top_left-row.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
