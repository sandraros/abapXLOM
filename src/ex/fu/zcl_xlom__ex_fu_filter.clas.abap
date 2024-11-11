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
"! <li>If no match, the result depends of the presence of <strong>if_empty</strong>:<ul>
"! <li>Absence : FILTER(A1:B2,A1:B1="c") → #CALC!</li>
"! <li>Presence: FILTER(A1:B2,A1:B1="c","xx") → "xx"</li>
"! </ul></li>
"! <li>If the Include size is neither one row nor one column of <strong>array</strong>: FILTER(A1:B2,A1="a") → #VALUE!</li>
"! <li>Idem: FILTER(A1:B3,A1:A2="a") → #VALUE!</li>
"! </ul>
"! https://support.microsoft.com/en-us/office/filter-function-f4f7cb66-82eb-4767-8f7c-4877ad80c759
CLASS zcl_xlom__ex_fu_filter DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'FILTER'
                                        type = zif_xlom__ex=>c_type-function-filter.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING array         TYPE REF TO zif_xlom__ex
                !include      TYPE REF TO zif_xlom__ex
                if_empty      TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_filter.

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

    METHODS evaluate_single
      IMPORTING
        i_array           TYPE REF TO zif_xlom__va_array
        i_include         TYPE REF TO zif_xlom__va_array
        i_vertical_search TYPE abap_bool
        i_row_number      TYPE i
        i_column_number   TYPE i
      CHANGING
        c_result_cells    TYPE zif_xlom__va_array=>tt_cell.
*        c_result_rows     TYPE zif_xlom__va_array=>tt_row.
ENDCLASS.


CLASS zcl_xlom__ex_fu_filter IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( not_part_of_result_array = abap_true
                          ( name = 'ARRAY   ' )
                          ( name = 'INCLUDE ' )
                          ( name = 'IF_EMPTY' default = zcl_xlom__ex_el_empty_argument=>singleton ) ).
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
        DATA(array) = zcl_xlom__va=>to_array( arguments[ c_arg-array ] ).
        DATA(include) = zcl_xlom__va=>to_array( arguments[ c_arg-include ] ).
        DATA(if_empty) = arguments[ c_arg-if_empty ].

        " INCLUDE must be one dimension, otherwise #VALUE!
        " That will be a vertical search if INCLUDE is one column.
        "     - INCLUDE and ARRAY must have the same number of rows, otherwise #VALUE!
        " That will be a horizontal search if INCLUDE is one row.
        "     - INCLUDE and ARRAY must have the same number of columns, otherwise #VALUE!
        IF    (     include->column_count <> 1
                AND include->row_count    <> 1 )
           OR (     include->column_count  = 1
                AND include->row_count    <> array->row_count )
           OR (     include->row_count     = 1
                AND include->column_count <> array->column_count ).
          result = zcl_xlom__va_error=>value_cannot_be_calculated.
        ELSE.

          IF include->column_count = 1.
            DATA(vertical_search) = abap_true.
          ELSE.
            DATA(horizontal_search) = abap_true.
          ENDIF.

          " Keep the same size of optimized ranges; the maximum size must be used.
          DATA(array_resized_used_range) = array->used_range.
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA(include_resized_used_range) = include->used_range.

          IF vertical_search = abap_true.
*              DATA(row_count) = nmax( val1 = array->used_range-row_count
*                                      val2 = include->used_range-row_count ).
            IF array->used_range-row_count < include->used_range-row_count.
              array_resized_used_range-bottom_right-row = array->used_range-bottom_right-row + include->used_range-row_count - array->used_range-row_count.
              array_resized_used_range-row_count = include->used_range-row_count.
            ELSEIF include->used_range-row_count < array->used_range-row_count.
              include_resized_used_range-bottom_right-row = include->used_range-bottom_right-row + array->used_range-row_count - include->used_range-row_count.
              include_resized_used_range-row_count = array->used_range-row_count.
            ENDIF.
          ELSE.
*              DATA(column_count) = nmax( val1 = optimized_array-column_count
*                                         val2 = optimized_include-column_count ).
            IF array->used_range-column_count < include->used_range-column_count.
              array_resized_used_range-bottom_right-column = array->used_range-bottom_right-column + include->used_range-column_count - array->used_range-column_count.
              array_resized_used_range-column_count = include->used_range-column_count.
            ELSEIF include->used_range-column_count < array->used_range-column_count.
              include_resized_used_range-bottom_right-column = include_resized_used_range-bottom_right-column + array->used_range-column_count - include_resized_used_range-column_count.
              include_resized_used_range-column_count = array->used_range-column_count.
            ENDIF.
          ENDIF.

          DATA(result_cells) = VALUE zif_xlom__va_array=>tt_cell( ).
*          DATA(result_rows) = VALUE zif_xlom__va_array=>tt_row( ).
          DATA(row_number) = include_resized_used_range-top_left-row.
          WHILE row_number <= include_resized_used_range-bottom_right-row.

            DATA(column_number) = include_resized_used_range-top_left-column.
            WHILE column_number <= include_resized_used_range-bottom_right-column.

              evaluate_single( EXPORTING i_array           = array
                                         i_include         = include
                                         i_vertical_search = vertical_search
                                         i_row_number      = row_number
                                         i_column_number   = column_number
                               CHANGING  c_result_cells    = result_cells ).
*                               CHANGING  c_result_rows     = result_rows ).

              column_number = column_number + 1.
            ENDWHILE.

            row_number = row_number + 1.
          ENDWHILE.

          if result_cells IS NOT INITIAL.
            DATA(result_row_count) = result_cells[ lines( result_cells ) ]-row.
            DATA(result_column_count) = result_cells[ lines( result_cells ) ]-column.
*          DATA(result_row_count) = lines( result_rows ).
*          DATA(result_column_count) = COND i( WHEN result_rows IS NOT INITIAL
*                                              THEN lines( result_rows[ 1 ]-columns_of_row ) ).
          ENDIF.

          IF    include_resized_used_range-row_count    < include->row_count
             OR include_resized_used_range-column_count < include->column_count.

            row_number = nmin( val1 = include_resized_used_range-row_count + 1
                               val2 = include->row_count ).
            column_number = nmin( val1 = include_resized_used_range-column_count + 1
                                  val2 = include->column_count ).

            DATA(values_of_other_cells) = VALUE zif_xlom__va_array=>tt_cell( ).
            evaluate_single( EXPORTING i_array           = array
                                       i_include         = include
                                       i_vertical_search = vertical_search
                                       i_row_number      = row_number
                                       i_column_number   = column_number
                             CHANGING  c_result_cells    = values_of_other_cells ).
*                             CHANGING  c_result_rows     = values_of_other_cells ).

            IF values_of_other_cells IS NOT INITIAL.
              IF vertical_search = abap_true.
                result_row_count = result_row_count + array->row_count - array_resized_used_range-row_count.
              ELSE.
                result_column_count = result_column_count + array->column_count - array_resized_used_range-column_count.
              ENDIF.
            ENDIF.
          ENDIF.

          IF result_cells IS NOT INITIAL.
*          IF result_rows IS NOT INITIAL.
            " TODO initialize VALUE_OF_OTHER_CELLS
            result = zcl_xlom__va_array=>create_initial( row_count             = result_row_count
                                                         column_count          = result_column_count
                                                         cells                 = result_cells
                                                         values_of_other_cells = values_of_other_cells ).
*                                                         rows                 = result_rows
*                                                         value_of_other_cells = VALUE #( ) ).
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

  METHOD evaluate_single.
*    DATA result_row_count TYPE i.
    DATA(filter_condition_one_cell_valu) = zcl_xlom__va=>to_boolean(
        i_include->get_cell_value( column = i_column_number
                                   row    = i_row_number ) )->boolean_value.

    IF filter_condition_one_cell_valu = abap_true.
      IF i_vertical_search = abap_true.
        " Case of a vertical search to extract whole rows from ARRAY.
        """"""""""""""""""""""""""""""""""""""""""""""""""

        " First time, count the columns.
        IF c_result_cells IS INITIAL.
*                      result_column_count = optimized_array-column_count.
        ENDIF.

*        DATA(row) = VALUE zif_xlom__va_array=>ts_row( ).
        DATA(column_number_2) = 1.
        WHILE column_number_2 <= i_array->used_range-column_count.
          DATA(cell_value) = i_array->get_cell_value( column = column_number_2
                                                      row    = i_row_number ).
          INSERT VALUE #( row    = i_row_number
                          column = column_number_2
                          value  = cell_value )
                 INTO TABLE c_result_cells.
          column_number_2 = column_number_2 + 1.
        ENDWHILE.

*        INSERT row INTO TABLE c_result_rows.
*        result_row_count = result_row_count + 1.

      ELSE.
        " Case of a horizontal search to extract whole columns from ARRAY.
        """"""""""""""""""""""""""""""""""""""""""""""""""

*        " First time, add the rows.
*        IF c_result_rows IS INITIAL.
*          result_row_count = i_array->row_count.
*          DO result_row_count TIMES.
*            INSERT INITIAL LINE INTO TABLE c_result_rows.
*          ENDDO.
*        ENDIF.

        DATA(lookup_array_one_column) = i_array->get_section_as_array(
                                            top_left     = VALUE #( column = i_column_number
                                                                    row    = 1 )
                                            bottom_right = VALUE #( column = i_column_number
                                                                    row    = i_array->used_range-row_count ) ).
        DATA(row_number_2) = 1.
        WHILE row_number_2 <= i_array->used_range-row_count.
          cell_value = lookup_array_one_column->get_cell_value( column = i_column_number
                                                                row    = row_number_2 ).
          INSERT VALUE #( row    = row_number_2
                          column = i_column_number
                          value  = cell_value )
                 INTO TABLE c_result_cells.
*          DATA(row_2) = REF #( c_result_rows[ sy-index ] ).
*          INSERT lookup_array_one_column->get_cell_value( column = i_column_number
*                                                          row    = row_number_2 )
*                 INTO TABLE row_2->columns_of_row.
          row_number_2 = row_number_2 + 1.
        ENDWHILE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
