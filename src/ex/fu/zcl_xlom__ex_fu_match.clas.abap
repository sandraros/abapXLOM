"! MATCH(lookup_value, lookup_array, [match_type])
"! https://support.microsoft.com/en-us/office/match-function-e8dffd45-c762-47d6-bf89-533f4a37673a
CLASS zcl_xlom__ex_fu_match DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'MATCH'
                                        type = zif_xlom__ex=>c_type-function-match.

    CLASS-METHODS class_constructor.

    "! MATCH(lookup_value, lookup_array, [match_type])
    "! https://support.microsoft.com/en-us/office/match-function-e8dffd45-c762-47d6-bf89-533f4a37673a
    "! MATCH returns the position of the matched value within lookup_array, not the value itself. For example, MATCH("b",{"a","b","c"},0) returns 2, which is the relative position of "b" within the array {"a","b","c"}.
    "! MATCH does not distinguish between uppercase and lowercase letters when matching text values.
    "! If MATCH is unsuccessful in finding a match, it returns the #N/A error value.
    "! If match_type is 0 and lookup_value is a text string, you can use the wildcard characters — the question mark (?) and asterisk (*) — in the lookup_value argument.
    "! A question mark matches any single character; an asterisk matches any sequence of characters.
    "! If you want to find an actual question mark or asterisk, type a tilde (~) before the character.
    "! @parameter lookup_value | Required. The value that you want to match in lookup_array.
    "!                           For example, when you look up someone's number in a telephone book,
    "!                           you are using the person's name as the lookup value, but the telephone number is the value you want.
    "!                           The lookup_value argument can be a value (number, text, or logical value)
    "!                           or a cell reference to a number, text, or logical value.
    "! @parameter lookup_array | Required. The range of cells being searched.
    "! @parameter match_type   | Optional. The number -1, 0, or 1. The match_type argument specifies how Excel
    "!                           matches lookup_value with values in lookup_array. The default value for this argument is 1.
    "!                           <ul>
    "!                           <li>1 or omitted: MATCH finds the largest value that is less than or equal to lookup_value.
    "!                                             The values in the lookup_array argument must be placed in ascending order, for example:
    "!                                             ...-2, -1, 0, 1, 2, ..., A-Z, FALSE, TRUE.</li>
    "!                           <li>0: MATCH finds the first value that is exactly equal to lookup_value.
    "!                                  The values in the lookup_array argument can be in any order.</li>
    "!                           <li>-1: MATCH finds the smallest value that is greater than or equal tolookup_value. The values in the lookup_array argument
    "!                                   must be placed in descending order, for example: TRUE, FALSE, Z-A, ...2, 1, 0, -1, -2, ..., and so on.</li>
    "!                           </ul>
    "! @parameter result       | MATCH returns the position of the matched value within lookup_array, not the value itself.
    "!                           For example, MATCH("b",{"a","b","c"},0) returns 2, which is the relative position of "b" within the array {"a","b","c"}.
    CLASS-METHODS create
      IMPORTING lookup_value  TYPE REF TO zif_xlom__ex
                lookup_array  TYPE REF TO zif_xlom__ex OPTIONAL
                match_type    TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_match.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_hashed_string,
        string TYPE string,
        row    TYPE i,
        column TYPE i,
      END OF ty_hashed_string.
    TYPES ty_hashed_strings TYPE HASHED TABLE OF ty_hashed_string WITH UNIQUE KEY string.
    TYPES:
      BEGIN OF ty_lookup_range,
        lookup_range_array  TYPE REF TO zif_xlom__va_array,
        optimized_addresses TYPE zcl_xlom=>ts_range_address,
        hashed_strings      TYPE ty_hashed_strings,
      END OF ty_lookup_range.
    TYPES ty_lookup_ranges TYPE HASHED TABLE OF ty_lookup_range WITH UNIQUE KEY lookup_range_array optimized_addresses.

    CONSTANTS:
      BEGIN OF c_arg,
        lookup_value TYPE i VALUE 1,
        lookup_array TYPE i VALUE 2,
        match_type   TYPE i VALUE 3,
      END OF c_arg.

    CONSTANTS:
      BEGIN OF c_match_type,
        exact_match TYPE i VALUE 0,
      END OF c_match_type.

    CLASS-DATA lookup_ranges TYPE ty_lookup_ranges.
    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.

    METHODS get_lookup_range
      IMPORTING lookup_range_array  TYPE REF TO zif_xlom__va_array
                optimized_addresses TYPE zcl_xlom=>ts_range_address
      RETURNING VALUE(result)       TYPE REF TO ty_lookup_range.
ENDCLASS.


CLASS zcl_xlom__ex_fu_match IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'LOOKUP_VALUE' )
                          ( name = 'LOOKUP_ARRAY' not_part_of_result_array = abap_true )
                          ( name = 'MATCH_TYPE  ' default = zcl_xlom__ex_el_number=>create( 1 ) ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_match( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( lookup_value )
                                                          ( lookup_array )
                                                          ( match_type   ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD get_lookup_range.
    result = REF #( lookup_ranges[ lookup_range_array  = lookup_range_array
                                   optimized_addresses = optimized_addresses ] OPTIONAL ).
    IF result IS BOUND.
      RETURN.
    ENDIF.

    INSERT VALUE #( lookup_range_array  = lookup_range_array
                    optimized_addresses = optimized_addresses )
           INTO TABLE lookup_ranges
           REFERENCE INTO result.

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
               INTO TABLE result->hashed_strings.

        column_number = column_number + 1.
      ENDWHILE.

      row_number = row_number + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(lookup_value_result) = arguments[ c_arg-lookup_value ].
        DATA(lookup_range_array) = zcl_xlom__va=>to_array( arguments[ c_arg-lookup_array ] ).
        DATA(match_type_result) = COND i( LET result_num_chars = arguments[ c_arg-match_type ] IN
                                          WHEN result_num_chars IS BOUND
                                          THEN COND #( WHEN result_num_chars->type = result_num_chars->c_type-empty
                                                       THEN 1
                                                       ELSE zcl_xlom__va=>to_number( result_num_chars )->get_integer( ) ) ).

        IF match_type_result <> c_match_type-exact_match.
          RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDIF.

        IF     lookup_range_array->row_count    > 1
           AND lookup_range_array->column_count > 1.
          " MATCH cannot lookup a two-dimension array, it can search either one row or one column.
          result = zcl_xlom__va_error=>na_not_applicable.
        ELSE.
*          DATA(optimized_addresses) = zcl_xlom__ut_om_range=>optimize_array_if_range( lookup_range_array ).
*          IF optimized_addresses IS INITIAL.
*            " The range may be out of the bounds, so no match found.
*            result = zcl_xlom__va_error=>na_not_applicable.
*          ELSE.
          DATA(lookup_range_hashed_strings) = zcl_xlom__ut=>get_lookup_range( lookup_range_array ).
          IF lookup_range_hashed_strings IS INITIAL.
            " The range may be out of the bounds, so no match found.
            result = zcl_xlom__va_error=>na_not_applicable.
          ELSE.
            DATA(lookup_value_string) = zcl_xlom__va=>to_string(
                SWITCH #( lookup_value_result->type
                          WHEN zif_xlom__va=>c_type-array
                            OR zif_xlom__va=>c_type-range
                          THEN CAST zif_xlom__va_array( lookup_value_result )->get_cell_value( column = 1
                                                                                               row    = 1 )
                          ELSE lookup_value_result )
                )->get_string( ).
            DATA(ref_hashed_string) = REF #( lookup_range_hashed_strings[ string = lookup_value_string ] OPTIONAL ).
            IF ref_hashed_string IS BOUND.
              IF lookup_range_array->row_count > 1.
                result = zcl_xlom__va_number=>create( EXACT #( ref_hashed_string->row ) ).
              ELSE.
                result = zcl_xlom__va_number=>create( EXACT #( ref_hashed_string->column ) ).
              ENDIF.
            ENDIF.
            IF result IS NOT BOUND.
              " no match found
              result = zcl_xlom__va_error=>na_not_applicable.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
