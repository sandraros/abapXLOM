CLASS zcl_xlom__ut DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_hashed_string,
        string TYPE string,
        row    TYPE i,
        column TYPE i,
      END OF ty_hashed_string.
    TYPES ty_hashed_strings TYPE HASHED TABLE OF ty_hashed_string WITH UNIQUE KEY string.

    CLASS-METHODS get_lookup_range
      IMPORTING lookup_range_array TYPE REF TO zif_xlom__va_array
      RETURNING VALUE(result)      TYPE ty_hashed_strings.

    CLASS-METHODS get_lookup_standard_table
      IMPORTING lookup_range_array  TYPE REF TO zif_xlom__va_array
                optimize_used_range TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(result)       TYPE string_table.

    CLASS-METHODS type
      IMPORTING any_data_object TYPE any
      RETURNING VALUE(result)   TYPE abap_typekind.

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
    TYPES:
      BEGIN OF ty_lookup_range,
        lookup_range_array  TYPE REF TO zif_xlom__va_array,
*        optimized_addresses TYPE zcl_xlom=>ts_range_address,
        hashed_strings      TYPE ty_hashed_strings,
      END OF ty_lookup_range.
    TYPES ty_lookup_ranges TYPE HASHED TABLE OF ty_lookup_range WITH UNIQUE KEY lookup_range_array. "optimized_addresses.

    TYPES:
      BEGIN OF ty_lookup_standard_table_range,
        lookup_range_array TYPE REF TO zif_xlom__va_array,
        string_table       TYPE string_table,
      END OF ty_lookup_standard_table_range.
    TYPES ty_lookup_standard_table_rangs TYPE HASHED TABLE OF ty_lookup_standard_table_range WITH UNIQUE KEY lookup_range_array.

    CLASS-DATA lookup_ranges TYPE ty_lookup_ranges.
    CLASS-DATA lookup_standard_table_ranges TYPE ty_lookup_standard_table_rangs.
ENDCLASS.


CLASS zcl_xlom__ut IMPLEMENTATION.
  METHOD get_lookup_range.
    DATA(lookup_range) = REF #( lookup_ranges[ lookup_range_array  = lookup_range_array ] OPTIONAL ).
    IF lookup_range IS BOUND.
      result = lookup_range->hashed_strings.
      RETURN.
    ENDIF.

    INSERT VALUE #( lookup_range_array  = lookup_range_array
*                    optimized_addresses = optimized_addresses
                    )
           INTO TABLE lookup_ranges
           REFERENCE INTO lookup_range.

    DATA(optimized_addresses) = zcl_xlom__ut_om_range=>optimize_array_if_range( lookup_range_array ).

    DATA(row_number) = optimized_addresses-top_left-row.
    WHILE row_number <= optimized_addresses-bottom_right-row.

      DATA(column_number) = optimized_addresses-top_left-column.
      WHILE column_number <= optimized_addresses-bottom_right-column.

        DATA(cell_value) = zcl_xlom__va=>to_string( lookup_range_array->get_cell_value( column = column_number
                                                                                        row    = row_number )
                                                  )->get_string( ).

        INSERT VALUE #( string = cell_value
                        row    = row_number
                        column = column_number )
               INTO TABLE lookup_range->hashed_strings.

        column_number = column_number + 1.
      ENDWHILE.

      row_number = row_number + 1.
    ENDWHILE.

    result = lookup_range->hashed_strings.
  ENDMETHOD.

  METHOD get_lookup_standard_table.
    DATA(lookup_range) = REF #( lookup_standard_table_ranges[ lookup_range_array  = lookup_range_array ] OPTIONAL ).
    IF lookup_range IS BOUND.
      result = lookup_range->string_table.
      RETURN.
    ENDIF.

    INSERT VALUE #( lookup_range_array  = lookup_range_array
*                    optimized_addresses = optimized_addresses
                    )
           INTO TABLE lookup_standard_table_ranges
           REFERENCE INTO lookup_range.

    IF optimize_used_range = abap_true.
      DATA(optimized_addresses) = zcl_xlom__ut_om_range=>optimize_array_if_range( lookup_range_array ).
    ELSE.
      IF lookup_range_array->zif_xlom__va~type = lookup_range_array->zif_xlom__va~c_type-range.
        optimized_addresses = zcl_xlom__ut_om_range=>get_address( CAST #( lookup_range_array ) ).
      ELSE.
        optimized_addresses = VALUE #( top_left     = VALUE #( row    = 1
                                                               column = 1 )
                                       bottom_right = VALUE #( row    = lookup_range_array->row_count
                                                               column = lookup_range_array->column_count ) ).
      ENDIF.
    ENDIF.

    DATA(row_number) = optimized_addresses-top_left-row.
    WHILE row_number <= optimized_addresses-bottom_right-row.

      DATA(column_number) = optimized_addresses-top_left-column.
      WHILE column_number <= optimized_addresses-bottom_right-column.

        DATA(cell_value) = zcl_xlom__va=>to_string( lookup_range_array->get_cell_value( column = column_number
                                                                                        row    = row_number )
                                                  )->get_string( ).

        INSERT cell_value INTO TABLE lookup_range->string_table.

        column_number = column_number + 1.
      ENDWHILE.

      row_number = row_number + 1.
    ENDWHILE.

    result = lookup_range->string_table.
  ENDMETHOD.

  METHOD type.
    DESCRIBE FIELD any_data_object TYPE result.
  ENDMETHOD.

  METHOD _intersect_2.
    TYPES tt_range TYPE STANDARD TABLE OF REF TO zcl_xlom_range WITH EMPTY KEY.

    DATA(args) = VALUE tt_range( ( arg1 ) ( arg2 ) ).

    LOOP AT args INTO DATA(arg)
         WHERE table_line IS BOUND.
      DATA(address) = zcl_xlom__ut_om_range=>get_address( arg ).
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
                                   bottom_right-column = zcl_xlom__ut_om_worksheet=>max_columns + 1
                                   bottom_right-row    = zcl_xlom__ut_om_worksheet=>max_rows + 1 ) ).

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
