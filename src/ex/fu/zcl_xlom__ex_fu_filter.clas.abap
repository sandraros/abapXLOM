"! FILTER(array, include, [if_empty])
"! https://support.microsoft.com/en-us/office/filter-function-f4f7cb66-82eb-4767-8f7c-4877ad80c759
CLASS zcl_xlom__ex_fu_filter DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.

    CLASS-METHODS create
      IMPORTING array         TYPE REF TO zif_xlom__ex
                !include      TYPE REF TO zif_xlom__ex
                if_empty      TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_filter.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        array    TYPE i VALUE 1,
        include  TYPE i VALUE 2,
        if_empty TYPE i VALUE 3,
      END OF c_arg.

    CONSTANTS:
      BEGIN OF c_match_type,
        exact_match TYPE i VALUE 0,
      END OF c_match_type.
ENDCLASS.


CLASS zcl_xlom__ex_fu_filter IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-match.
    zif_xlom__ex~parameters = VALUE #( ( name = 'ARRAY   ' )
                                       ( name = 'INCLUDE ' not_part_of_result_array = abap_true )
                                       ( name = 'IF_EMPTY' default = zcl_xlom__ex_el_number=>create( 1 ) ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_filter( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( array    )
                                                          ( include  )
                                                          ( if_empty ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
*        DATA(array) = CAST zif_xlom__va_array( arguments[ c_arg-array ] ).
*        DATA(include) = zcl_xlom__va=>to_array( arguments[ c_arg-lookup_array ] ).
*        DATA(match_type_result) = COND i( LET result_num_chars = arguments[ c_arg-match_type ] IN
*                                          WHEN result_num_chars IS BOUND
*                                          THEN COND #( WHEN result_num_chars->type = result_num_chars->c_type-empty
*                                                       THEN 1
*                                                       ELSE zcl_xlom__va=>to_number( result_num_chars )->get_integer( ) ) ).
*        IF match_type_result <> c_match_type-exact_match.
*          RAISE EXCEPTION TYPE zcx_xlom_todo.
*        ENDIF.
*        IF     lookup_array_result->row_count    > 1
*           AND lookup_array_result->column_count > 1.
*          " MATCH cannot lookup a two-dimension array, it can search either one row or one column.
*          result = zcl_xlom__va_error=>na_not_applicable.
*        ELSE.
*          DATA(optimized_lookup_array) = zcl_xlom_range=>optimize_array_if_range( lookup_array_result ).
*          IF optimized_lookup_array IS NOT INITIAL.
*            DATA(row_number) = optimized_lookup_array-top_left-row.
*            WHILE row_number <= optimized_lookup_array-bottom_right-row.
*              DATA(column_number) = optimized_lookup_array-top_left-column.
*              WHILE column_number <= optimized_lookup_array-bottom_right-column.
*                DATA(cell_value) = lookup_array_result->get_cell_value( column = column_number
*                                                                        row    = row_number ).
*                DATA(equal) = abap_false.
*                DATA(lookup_value_result_2) = SWITCH #( lookup_value_result->type
*                                                        WHEN zif_xlom__va=>c_type-array
*                                                          OR zif_xlom__va=>c_type-range
*                                                        THEN CAST zif_xlom__va( CAST zif_xlom__va_array( lookup_value_result )->get_cell_value(
*                                                                                    column = 1
*                                                                                    row    = 1 ) )
*                                                        ELSE lookup_value_result ).
*                IF    lookup_value_result_2->type = zif_xlom__va=>c_type-string
*                   OR cell_value->type            = zif_xlom__va=>c_type-string.
*                  equal = xsdbool( zcl_xlom__va=>to_string( lookup_value_result_2 )->get_string( )
*                                   = zcl_xlom__va=>to_string( cell_value )->get_string( ) ).
*                ELSEIF    lookup_value_result_2->type = zif_xlom__va=>c_type-number
*                       OR cell_value->type            = zif_xlom__va=>c_type-number.
*                  equal = xsdbool( zcl_xlom__va=>to_number( lookup_value_result_2 )->get_number( )
*                                   = zcl_xlom__va=>to_number( cell_value )->get_number( ) ).
*                ELSEIF    lookup_value_result_2->type = zif_xlom__va=>c_type-boolean
*                       OR cell_value->type            = zif_xlom__va=>c_type-boolean.
*                  equal = xsdbool( zcl_xlom__va=>to_boolean( lookup_value_result_2 )->boolean_value
*                                   = zcl_xlom__va=>to_boolean( cell_value )->boolean_value ).
*                ELSEIF     lookup_value_result_2->type = zif_xlom__va=>c_type-empty
*                       AND cell_value->type            = zif_xlom__va=>c_type-empty.
*                  equal = abap_true.
*                ELSE.
*                  RAISE EXCEPTION TYPE zcx_xlom_todo.
*                ENDIF.
*                IF equal = abap_true.
*                  IF lookup_array_result->row_count > 1.
*                    result = zcl_xlom__va_number=>create( EXACT #( row_number ) ).
*                  ELSE.
*                    result = zcl_xlom__va_number=>create( EXACT #( column_number ) ).
*                  ENDIF.
*                  " Dummy code to exit the two loops
*                  row_number = lookup_array_result->row_count.
*                  column_number = lookup_array_result->column_count.
*                ENDIF.
*                column_number = column_number + 1.
*              ENDWHILE.
*              row_number = row_number + 1.
*            ENDWHILE.
*          ENDIF.
*          IF result IS NOT BOUND.
*            " no match found
*            result = zcl_xlom__va_error=>na_not_applicable.
*          ENDIF.
*        ENDIF.
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
