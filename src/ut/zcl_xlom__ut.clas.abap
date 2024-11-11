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

    TYPES:
      BEGIN OF ts_used_range_with_size,
        top_left      TYPE zcl_xlom=>ts_range_address_one_cell,
        bottom_right  TYPE zcl_xlom=>ts_range_address_one_cell,
        column_count  TYPE i,
        row_count     TYPE i,
      END OF ts_used_range_with_size.
    TYPES tt_used_range_with_size type STANDARD TABLE OF ts_used_range_with_size with EMPTY KEY.
    TYPES:
      BEGIN OF ts_intersect_used_range,
        inside_used_range              TYPE ts_used_range_with_size,
        count_cells_outside_used_range TYPE i,
        outside_used_range             TYPE tt_used_range_with_size,
      END OF ts_intersect_used_range.

    CLASS-METHODS get_lookup_range
      IMPORTING lookup_range_array TYPE REF TO zif_xlom__va_array
      RETURNING VALUE(result)      TYPE ty_hashed_strings.

    CLASS-METHODS get_lookup_standard_table
      IMPORTING lookup_range_array  TYPE REF TO zif_xlom__va_array
                optimize_used_range TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(result)       TYPE string_table.

    CLASS-METHODS intersect_used_range
      IMPORTING array         TYPE REF TO zif_xlom__va_array
      RETURNING VALUE(result) TYPE ts_intersect_used_range.

    CLASS-METHODS type
      IMPORTING any_data_object TYPE any
      RETURNING VALUE(result)   TYPE abap_typekind.

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

    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( lookup_range_array ).

    DATA(row_number) = intersect_result-inside_used_range-top_left-row.
    WHILE row_number <= intersect_result-inside_used_range-bottom_right-row.

      DATA(column_number) = intersect_result-inside_used_range-top_left-column.
      WHILE column_number <= intersect_result-inside_used_range-bottom_right-column.

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
      DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( lookup_range_array ).
      DATA(optimized_addresses) = intersect_result-inside_used_range.
    ELSE.
      IF lookup_range_array->zif_xlom__va~type = lookup_range_array->zif_xlom__va~c_type-range.
        optimized_addresses = zcl_xlom__ext_range=>get_address( CAST #( lookup_range_array ) ).
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

  METHOD intersect_used_range.
    IF array->zif_xlom__va~type = array->zif_xlom__va~c_type-range.
      DATA(range) = CAST zcl_xlom_range( array ).
      DATA(range_address) = zcl_xlom__ext_range=>get_address( range ).
      DATA(used_range) = zcl_xlom__ext_worksheet=>get_used_range( range->parent ).
      DATA(intersection) = zcl_xlom__ext_application=>intersect_2_low_level(
                               arg1 = VALUE #( top_left-column     = range_address-top_left-column
                                               top_left-row        = range_address-top_left-row
                                               bottom_right-column = range_address-bottom_right-column
                                               bottom_right-row    = range_address-bottom_right-row )
                               arg2 = used_range ).

      IF intersection IS NOT INITIAL.

        " Values are relative to the original range. If the original range is within the used range, then
        " the intersection equals the original range, and the result will always indicate top row 1 and
        " left column 1 whatever the coordinates of the original range are, because the result contains
        " coordinates relative to the original range.
        result-inside_used_range-top_left-column = intersection-top_left-column - range_address-top_left-column + 1.
        result-inside_used_range-top_left-row    = intersection-top_left-row - range_address-top_left-row + 1.

        result-inside_used_range-bottom_right-column = result-inside_used_range-top_left-column + ( intersection-bottom_right-column - intersection-top_left-column ).
        result-inside_used_range-bottom_right-row    = result-inside_used_range-top_left-row + ( intersection-bottom_right-row - intersection-top_left-row ).

        result-inside_used_range-column_count = result-inside_used_range-bottom_right-column - result-inside_used_range-top_left-column + 1.
        result-inside_used_range-row_count    = result-inside_used_range-bottom_right-row - result-inside_used_range-top_left-row + 1.

      ENDIF.

      DATA(range_address_column_count) = range_address-bottom_right-column - range_address-top_left-column + 1.

      result-count_cells_outside_used_range =   (   ( range_address-bottom_right-column - range_address-top_left-column + 1 )
                                                  * ( range_address-bottom_right-row - range_address-top_left-row + 1 ) )
                                              - ( result-inside_used_range-column_count * result-inside_used_range-row_count ).

      " TOP of area outside the used range
      IF result-inside_used_range-top_left-row > 1.
        INSERT VALUE #( top_left     = VALUE #( column = 1
                                                row    = 1 )
                        bottom_right = VALUE #( column = range_address_column_count
                                                row    = result-inside_used_range-top_left-row - 1 )
                        column_count = range_address_column_count
                        row_count    = result-inside_used_range-top_left-row - 1 )
               INTO TABLE result-outside_used_range.
      ENDIF.

      " MIDDLE LEFT of area outside the used range
      IF result-inside_used_range-top_left-column > 1.
        INSERT VALUE #( top_left     = VALUE #( column = 1
                                                row    = result-inside_used_range-top_left-row )
                        bottom_right = VALUE #( column = result-inside_used_range-top_left-column - 1
                                                row    = result-inside_used_range-bottom_right-row )
                        column_count = result-inside_used_range-top_left-column - 1
                        row_count    = result-inside_used_range-row_count )
               INTO TABLE result-outside_used_range.
      ENDIF.

      " MIDDLE RIGHT of area outside the used range
      IF result-inside_used_range-bottom_right-column < range_address_column_count.
        INSERT VALUE #( top_left     = VALUE #( column = result-inside_used_range-bottom_right-column + 1
                                                row    = result-inside_used_range-top_left-row )
                        bottom_right = VALUE #( column = range_address_column_count
                                                row    = result-inside_used_range-bottom_right-row )
                        column_count = range_address_column_count - result-inside_used_range-bottom_right-column
                        row_count    = result-inside_used_range-row_count )
               INTO TABLE result-outside_used_range.
      ENDIF.

      " BOTTOM of area outside the used range
      DATA(range_address_row_count) = range_address-bottom_right-row - range_address-top_left-row + 1.
      IF result-inside_used_range-bottom_right-row < range_address_row_count.
        INSERT VALUE #( top_left     = VALUE #( column = 1
                                                row    = result-inside_used_range-bottom_right-row + 1 )
                        bottom_right = VALUE #( column = range_address_column_count
                                                row    = range_address_row_count )
                        column_count = range_address_column_count
                        row_count    = range_address_row_count - result-inside_used_range-bottom_right-row )
               INTO TABLE result-outside_used_range.
      ENDIF.
    ELSE.
      result-inside_used_range = VALUE #( top_left-column     = 1
                                          top_left-row        = 1
                                          bottom_right-column = array->column_count
                                          bottom_right-row    = array->row_count
                                          column_count        = array->column_count
                                          row_count           = array->row_count ).
    ENDIF.
  ENDMETHOD.

  METHOD type.
    DESCRIBE FIELD any_data_object TYPE result.
  ENDMETHOD.
ENDCLASS.
