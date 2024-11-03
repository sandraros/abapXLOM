"! VLOOKUP(lookup_value, table_array, col_index_num, [range_lookup])
"! https://support.microsoft.com/en-us/office/vlookup-function-0bbc8083-26fe-4963-8ab8-93a18ad188a1
CLASS zcl_xlom__ex_fu_vlookup DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'VLOOKUP'.

    METHODS adjust_evaluated_operands REDEFINITION.

    CLASS-METHODS class_constructor.

    "! @parameter range_lookup | Approximate match (TRUE or omitted) or Exact match (FALSE)
    CLASS-METHODS create
      IMPORTING lookup_value  TYPE REF TO zif_xlom__ex
                table_array   TYPE REF TO zif_xlom__ex
                col_index_num TYPE REF TO zif_xlom__ex
                range_lookup  TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_vlookup.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        lookup_value  TYPE i VALUE 1,
        table_array   TYPE i VALUE 2,
        col_index_num TYPE i VALUE 3,
        range_lookup  TYPE i VALUE 4,
      END OF c_arg.

    CONSTANTS:
      BEGIN OF c_range_lookup,
        approximate_match TYPE abap_bool VALUE abap_true,
        exact_match       TYPE abap_bool VALUE abap_false,
      END OF c_range_lookup.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_vlookup IMPLEMENTATION.
  METHOD adjust_evaluated_operands.
    " VLOOKUP({"a","b"},{"b","a"},{1,2},FALSE)
    " is the same as
    " VLOOKUP({"a","b"},{"b","a"},1,FALSE)
    DATA(lookup_value) = ref #( evaluated_operands[ c_arg-lookup_value ] ).
    DATA(col_index_num) = ref #( evaluated_operands[ c_arg-col_index_num ] ).
    DATA(lookup_value_is_array_range) = SWITCH #( lookup_value->*->type
                                                  WHEN zif_xlom__va=>c_type-array
                                                    OR zif_xlom__va=>c_type-range
                                                  THEN abap_true ).
    DATA(col_index_num_is_array_range) = SWITCH #( col_index_num->*->type
                                                   WHEN zif_xlom__va=>c_type-array
                                                     OR zif_xlom__va=>c_type-range
                                                   THEN abap_true ).
    IF     lookup_value_is_array_range  = abap_true
       AND col_index_num_is_array_range = abap_true.
      col_index_num->* = CAST zif_xlom__va_array( col_index_num->* )->get_cell_value( column = 1
                                                                                      row    = 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
    parameters = VALUE #( ( name = 'LOOKUP_VALUE ' )
                          ( name = 'TABLE_ARRAY  ' not_part_of_result_array = abap_true )
                          ( name = 'COL_INDEX_NUM' )
                          ( name = 'RANGE_LOOKUP ' default = zcl_xlom__ex_el_boolean=>true ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-match.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_vlookup( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( lookup_value  )
                                                          ( table_array   )
                                                          ( col_index_num )
                                                          ( range_lookup  ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(lookup_value) = zcl_xlom__va=>to_string( arguments[ c_arg-lookup_value ] )->get_string( ).
        DATA(table_array) = zcl_xlom__va=>to_array( arguments[ c_arg-table_array ] ).
        DATA(col_index_num) = zcl_xlom__va=>to_number( arguments[ c_arg-col_index_num ] )->get_integer( ).
        DATA(range_lookup) = zcl_xlom__va=>to_boolean( arguments[ c_arg-range_lookup ] )->boolean_value.

        IF range_lookup <> c_range_lookup-exact_match.
          RAISE EXCEPTION TYPE zcx_xlom_todo
            EXPORTING text = 'Only an exact match is currently supported in VLOOKUP'.
        ENDIF.

        DATA(optimized_table_array) = zcl_xlom__pv_range_optimize=>optimize_array_if_range( table_array ).
        IF optimized_table_array IS INITIAL.
          RAISE EXCEPTION TYPE zcx_xlom_todo
            EXPORTING text = ''.
        ELSE.
          DATA(row_number) = optimized_table_array-top_left-row.
          WHILE row_number <= optimized_table_array-bottom_right-row.
            DATA(column_number) = optimized_table_array-top_left-column.
            DATA(table_array_cell_value) = zcl_xlom__va=>to_string( table_array->get_cell_value(
                                                                        column = column_number
                                                                        row    = row_number ) )->get_string( ).
            DATA(equal) = xsdbool( lookup_value = table_array_cell_value ).
            IF equal = abap_true.
              result = table_array->get_cell_value( column = col_index_num
                                                    row    = row_number ).
              " Dummy code to exit the two loops
              row_number = optimized_table_array-bottom_right-row.
            ENDIF.
            row_number = row_number + 1.
          ENDWHILE.
        ENDIF.
        IF result IS NOT BOUND.
          " no match found
          result = zcl_xlom__va_error=>na_not_applicable.
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
