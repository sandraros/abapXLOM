CLASS zcl_xlom__ext_application DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS _intersect_2
      IMPORTING arg1          TYPE REF TO zcl_xlom_range
                arg2          TYPE REF TO zcl_xlom_range
      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.

    CLASS-METHODS _intersect_2_basis
      IMPORTING arg1          TYPE zcl_xlom=>ts_range_address
                arg2          TYPE zcl_xlom=>ts_range_address
      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom__ext_application IMPLEMENTATION.
  METHOD _intersect_2.
    TYPES tt_range TYPE STANDARD TABLE OF REF TO zcl_xlom_range WITH EMPTY KEY.

    DATA(args) = VALUE tt_range( ( arg1 ) ( arg2 ) ).

    LOOP AT args INTO DATA(arg)
         WHERE table_line IS BOUND.
      DATA(address) = zcl_xlom__ext_range=>get_address( arg ).
      result = _intersect_2_basis( arg1 = result
                                   arg2 = VALUE #( top_left-column     = address-top_left-column
                                                   top_left-row        = address-top_left-row
                                                   bottom_right-column = address-bottom_right-column
                                                   bottom_right-row    = address-bottom_right-row ) ).
      IF result IS INITIAL.
        " Empty intersection
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD _intersect_2_basis.
    result = COND #( WHEN arg1 IS NOT INITIAL
                     THEN arg1
                     ELSE VALUE #( top_left-column     = 0
                                   top_left-row        = 0
                                   bottom_right-column = zcl_xlom__ext_worksheet=>max_columns + 1
                                   bottom_right-row    = zcl_xlom__ext_worksheet=>max_rows + 1 ) ).

    IF arg2-top_left-column > result-top_left-column.
      result-top_left-column = arg2-top_left-column.
    ENDIF.
    IF arg2-top_left-row > result-top_left-row.
      result-top_left-row = arg2-top_left-row.
    ENDIF.
    IF arg2-bottom_right-column < result-bottom_right-column.
      result-bottom_right-column = arg2-bottom_right-column.
    ENDIF.
    IF arg2-bottom_right-row < result-bottom_right-row.
      result-bottom_right-row = arg2-bottom_right-row.
    ENDIF.

    IF    result-top_left-column > result-bottom_right-column
       OR result-top_left-row    > result-bottom_right-row.
      " Empty intersection
      result = VALUE #( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
