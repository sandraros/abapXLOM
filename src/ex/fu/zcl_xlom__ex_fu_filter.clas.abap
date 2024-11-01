"! FILTER(array, include, [if_empty]) <br/>
"!  &#x2000;
"!     │ A │ B <br/>
"!    ─│ ─ │ ─ <br/>
"!   1 │ a │ b <br/>
"!    ─│ ─ │ ─ <br/>
"!   2 │ m │ n
"! <ul>
"! <li>Vertical search: FILTER(A1:B2,A1:A2="a") → {"a","b"} (1 row, two columns</li>
"! <li>Horizontal search: FILTER(A1:B2,A1:B1="a") → {"a";"m"} (two rows, 1 column)</li>
"! <li>No match without <strong>if_empty</strong>: FILTER(A1:B2,A1:B1="c") → #CALC!</li>
"! <li>No match with <strong>if_empty</strong>: FILTER(A1:B2,A1:B1="c","") → ""</li>
"! <li>Include size is neither one row nor one column of <strong>array</strong>: FILTER(A1:B2,A1="a") → #VALUE!</li>
"! <li>Idem: FILTER(A1:B3,A1:A2="a") → #VALUE!</li>
"! </ul>
"! https://support.microsoft.com/en-us/office/filter-function-f4f7cb66-82eb-4767-8f7c-4877ad80c759
CLASS zcl_xlom__ex_fu_filter DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING array         TYPE REF TO zif_xlom__ex
                !include      TYPE REF TO zif_xlom__ex
                if_empty      TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_filter.

    METHODS zif_xlom__ex~evaluate REDEFINITION.
    METHODS zif_xlom__ex~get_parameters REDEFINITION.

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

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_filter IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( not_part_of_result_array = abap_true
                          ( name = 'ARRAY   ' )
                          ( name = 'INCLUDE ' )
                          ( name = 'IF_EMPTY' default = zcl_xlom__ex_el_empty_argument=>singleton ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-match.
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
        DATA(lookup_array) = zcl_xlom__va=>to_array( arguments[ c_arg-array ] ).
        DATA(filter_condition_aka_include) = zcl_xlom__va=>to_array( arguments[ c_arg-include ] ).
        DATA(if_empty) = arguments[ c_arg-if_empty ].

        " INCLUDE must be one dimension and span over either one row or one column of ARRAY:
        "   - vertical search if INCLUDE is one column,
        "   - horizontal search if INCLUDE is one row.
        IF    (     filter_condition_aka_include->column_count <> 1
                AND filter_condition_aka_include->row_count    <> 1 )
           OR (     filter_condition_aka_include->column_count  = 1
                AND filter_condition_aka_include->row_count    <> lookup_array->row_count )
           OR (     filter_condition_aka_include->row_count     = 1
                AND filter_condition_aka_include->column_count <> lookup_array->column_count ).
          result = zcl_xlom__va_error=>value_cannot_be_calculated.
        ELSE.

          DATA(result_rows) = VALUE zif_xlom__va_array=>tt_row( ).
          DATA(result_row_count) = 0.
          DATA(result_column_count) = 0.

          DATA(optimized_lookup_array) = zcl_xlom__ut_om_range=>optimize_array_if_range( lookup_array ).
          " If outside the used range (initial) -> result is not found
          IF optimized_lookup_array IS NOT INITIAL.

            DATA(row_number) = 1.
            WHILE row_number <= filter_condition_aka_include->row_count.
              DATA(column_number) = 1.
              WHILE column_number <= filter_condition_aka_include->column_count.

                DATA(filter_condition_one_cell_valu) = zcl_xlom__va=>to_boolean(
                    filter_condition_aka_include->get_cell_value( column = column_number
                                                                  row    = row_number ) )->boolean_value.

                IF filter_condition_one_cell_valu = abap_true.
                  IF filter_condition_aka_include->column_count = 1.
                    " Case of a vertical search to extract whole rows from ARRAY.
                    """"""""""""""""""""""""""""""""""""""""""""""""""

                    " First time, count the columns.
                    IF result_rows IS INITIAL.
                      result_column_count = lookup_array->column_count.
                    ENDIF.

                    DATA(lookup_array_one_row) = lookup_array->get_array_value(
                                                     top_left     = VALUE #( column = 1
                                                                             row    = row_number )
                                                     bottom_right = VALUE #( column = lookup_array->column_count
                                                                             row    = row_number ) ).

                    DATA(row) = VALUE zif_xlom__va_array=>ts_row( ).
                    DATA(column_number_2) = 1.
                    WHILE column_number_2 <= result_column_count.
                      INSERT lookup_array_one_row->get_cell_value( column = column_number_2
                                                                   row    = row_number )
                             INTO TABLE row-columns_of_row.
                      column_number_2 = column_number_2 + 1.
                    ENDWHILE.

                    INSERT row INTO TABLE result_rows.
                    result_row_count = result_row_count + 1.

                  ELSE.
                    " Case of a horizontal search to extract whole columns from ARRAY.
                    """"""""""""""""""""""""""""""""""""""""""""""""""

                    " First time, add the rows.
                    IF result_rows IS INITIAL.
                      result_row_count = lookup_array->row_count.
                      DO result_row_count TIMES.
                        INSERT INITIAL LINE INTO TABLE result_rows.
                      ENDDO.
                    ENDIF.

                    DATA(lookup_array_one_column) = lookup_array->get_array_value(
                                                        top_left     = VALUE #( column = column_number
                                                                                row    = 1 )
                                                        bottom_right = VALUE #( column = column_number
                                                                                row    = lookup_array->row_count ) ).
                    DATA(row_number_2) = 1.
                    WHILE row_number_2 <= result_row_count.
                      DATA(row_2) = REF #( result_rows[ sy-index ] ).
                      INSERT lookup_array_one_column->get_cell_value( column = column_number
                                                                      row    = row_number_2 )
                             INTO TABLE row_2->columns_of_row.
                      row_number_2 = row_number_2 + 1.
                    ENDWHILE.

                    result_column_count = result_column_count + 1.
                  ENDIF.
                ENDIF.

                column_number = column_number + 1.
              ENDWHILE.
              row_number = row_number + 1.
            ENDWHILE.
          ENDIF.

          IF result_rows IS NOT INITIAL.
            result = zcl_xlom__va_array=>create_initial( row_count    = result_row_count
                                                         column_count = result_column_count
                                                         rows         = result_rows ).
          ELSE.
            " no match found
            IF if_empty->type = if_empty->c_type-empty.
              result = zcl_xlom__va_error=>calc.
            ELSE.
              result = if_empty.
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
