CLASS zcl_xlom__pv_range_optimize DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_fu_filter
                 zcl_xlom__ex_fu_vlookup
                 zcl_xlom__ut.

  PRIVATE SECTION.
    "! Intersection with the "used range" of the range worksheet to reduce the search area (to improve the performance).
    CLASS-METHODS optimize_array_if_range
      IMPORTING array         TYPE REF TO zif_xlom__va_array
      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.
ENDCLASS.


CLASS zcl_xlom__pv_range_optimize IMPLEMENTATION.
  METHOD optimize_array_if_range.
    IF array->zif_xlom__va~type = array->zif_xlom__va~c_type-range.
      DATA(range) = CAST zcl_xlom_range( array ).
      DATA(range_address) = zcl_xlom__ext_range=>get_address( range ).
      result = zcl_xlom__ext_application=>intersect_2_low_level(
                   arg1 = VALUE #( top_left-column     = range_address-top_left-column
                                   top_left-row        = range_address-top_left-row
                                   bottom_right-column = range_address-bottom_right-column
                                   bottom_right-row    = range_address-bottom_right-row )
                   arg2 = zcl_xlom__ext_worksheet=>get_used_range( range->parent ) ).
    ELSE.
      result = VALUE #( top_left-column     = 1
                        top_left-row        = 1
                        bottom_right-column = array->column_count
                        bottom_right-row    = array->row_count ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
