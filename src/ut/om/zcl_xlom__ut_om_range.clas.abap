CLASS zcl_xlom__ut_om_range DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_address
      IMPORTING !range        TYPE REF TO zcl_xlom_range
      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.

    "! Intersection with the "used range" of the range worksheet to reduce the search area (to improve the performance).
    CLASS-METHODS optimize_array_if_range
      IMPORTING array         TYPE REF TO zif_xlom__va_array
      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.
ENDCLASS.



CLASS zcl_xlom__ut_om_range IMPLEMENTATION.
  METHOD get_address.
    result = range->_address.
  ENDMETHOD.

  METHOD optimize_array_if_range.
    IF array->zif_xlom__va~type = array->zif_xlom__va~c_type-range.
      DATA(range) = CAST zcl_xlom_range( array ).
      result = zcl_xlom__ut=>_intersect_2_basis(
                   arg1 = VALUE #( top_left-column     = range->_address-top_left-column
                                   top_left-row        = range->_address-top_left-row
                                   bottom_right-column = range->_address-bottom_right-column
                                   bottom_right-row    = range->_address-bottom_right-row )
                   arg2 = zcl_xlom__ut_om_worksheet=>get_used_range( range->parent ) ).
    ELSE.
      result = VALUE #( top_left-column     = 1
                        top_left-row        = 1
                        bottom_right-column = array->column_count
                        bottom_right-row    = array->row_count ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
