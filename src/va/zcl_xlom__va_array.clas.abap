CLASS zcl_xlom__va_array DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__pv_worksheet_array.

  PUBLIC SECTION.
    INTERFACES zif_xlom__va.
    INTERFACES zif_xlom__va_array.

    TYPES:
      BEGIN OF ts_used_range_one_cell,
        column TYPE i,
        row    TYPE i,
      END OF ts_used_range_one_cell.
    TYPES:
      BEGIN OF ts_used_range,
        top_left     TYPE ts_used_range_one_cell,
        bottom_right TYPE ts_used_range_one_cell,
      END OF ts_used_range.

*    DATA used_range TYPE ts_used_range READ-ONLY.

    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized" lang="en">CREATE_INITIAL</p>
    "!
    "! @parameter row_count    | <p class="shorttext synchronized" lang="en">ROW_COUNT</p>
    "! @parameter column_count | <p class="shorttext synchronized" lang="en">COLUMN_COUNT</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">RESULT</p>
    CLASS-METHODS create_initial
      IMPORTING row_count             TYPE i
                column_count          TYPE i
                cells                 TYPE zif_xlom__va_array=>tt_cell OPTIONAL
                values_of_other_cells TYPE zif_xlom__va_array=>tt_cell OPTIONAL
*                !rows                TYPE zif_xlom__va_array=>tt_row OPTIONAL
*                values_of_other_cells TYPE REF TO zif_xlom__va        OPTIONAL
      RETURNING VALUE(result)         TYPE REF TO zcl_xlom__va_array.

  PRIVATE SECTION.
    "! Internal type of cell value (empty, number, string, boolean, error, array, compound data)
    TYPES ty_value_type TYPE i.
    TYPES:
      BEGIN OF ts_cell,
        "! Start from 1
        column     TYPE i,
        "! Start from 1
        row        TYPE i,
        formula    TYPE REF TO zif_xlom__ex,
        calculated TYPE abap_bool,
        value      TYPE REF TO zif_xlom__va,
      END OF ts_cell.
    TYPES tt_cell TYPE SORTED TABLE OF ts_cell WITH UNIQUE KEY row column.

    CONSTANTS:
      BEGIN OF c_value_type,
        "! Needed by ISBLANK formula function. IT CANNOT be replaced with "empty = xsdbool( not line_exists( _cells[ row = ... column = ... ] ) )"
        "! because a cell may exist for data other than value, like number format, background color, and so on. Optionally, there could be two
        "! internal tables, _cells only for values.
        empty         TYPE ty_value_type VALUE 1,
        number        TYPE ty_value_type VALUE 2,
        string        TYPE ty_value_type VALUE 3,
        "! Cell containing the value TRUE or FALSE.
        "! Needed by TYPE formula function (4 = logical value)
        boolean       TYPE ty_value_type VALUE 4,
        "! Needed by TYPE formula function (16 = error)
        error         TYPE ty_value_type VALUE 5,
        "! Needed by TYPE formula function (64 = array)
        array         TYPE ty_value_type VALUE 6,
        "! Needed by TYPE formula function (128 = compound data)
        compound_data TYPE ty_value_type VALUE 7,
      END OF c_value_type.

    CLASS-DATA initial_used_range TYPE zcl_xlom__ut=>ts_used_range_with_size.

    DATA _cells TYPE tt_cell.
    DATA values_of_other_cells TYPE zif_xlom__va_array=>tt_cell.

    "! @parameter row | Start from 1
    "! @parameter column | Start from 1
    METHODS set_cell_value_single
      IMPORTING !row       TYPE i
                !column    TYPE i
                !value     TYPE REF TO zif_xlom__va
                formula    TYPE REF TO zif_xlom__ex OPTIONAL
                calculated TYPE abap_bool           OPTIONAL.
ENDCLASS.


CLASS zcl_xlom__va_array IMPLEMENTATION.
  METHOD class_constructor.
    initial_used_range = VALUE #( top_left     = VALUE #( row    = 1
                                                          column = 1 )
                                  bottom_right = VALUE #( row    = 1
                                                          column = 1 )
                                  row_count    = 1
                                  column_count = 1 ).
  ENDMETHOD.

  METHOD create_initial.
    result = NEW zcl_xlom__va_array( ).
    result->zif_xlom__va~type               = zif_xlom__va=>c_type-array.
    result->zif_xlom__va_array~row_count    = row_count.
    result->zif_xlom__va_array~column_count = column_count.
    result->zif_xlom__va_array~used_range   = initial_used_range.
    result->zif_xlom__va_array~set_array_values( cells                 = cells
*    result->zif_xlom__va_array~set_array_values( rows                 = rows
                                                 values_of_other_cells = values_of_other_cells ).
  ENDMETHOD.

  METHOD set_cell_value_single.
    DATA(cell) = REF #( _cells[ row    = row
                                column = column ] OPTIONAL ).
    IF cell IS NOT BOUND.
      INSERT VALUE #( row    = row
                      column = column )
             INTO TABLE _cells
             REFERENCE INTO cell.
    ENDIF.

    cell->value      = value.
    cell->formula    = formula.
    cell->calculated = calculated.

    IF lines( _cells ) = 1.
      " If it's the first cell defined in the array.
      zif_xlom__va_array~used_range = VALUE #( top_left     = VALUE #( row    = row
                                                                       column = column )
                                               bottom_right = VALUE #( row    = row
                                                                       column = column )
                                               row_count    = 1
                                               column_count = 1 ).
    ELSE.
      DATA(recalculate_row_count) = abap_false.
      DATA(recalculate_column_count) = abap_false.
      IF row < zif_xlom__va_array~used_range-top_left-row.
        zif_xlom__va_array~used_range-top_left-row = row.
        recalculate_row_count = abap_true.
      ENDIF.
      IF column < zif_xlom__va_array~used_range-top_left-column.
        zif_xlom__va_array~used_range-top_left-column = column.
        recalculate_column_count = abap_true.
      ENDIF.
      IF row > zif_xlom__va_array~used_range-bottom_right-row.
        zif_xlom__va_array~used_range-bottom_right-row = row.
        recalculate_row_count = abap_true.
      ENDIF.
      IF column > zif_xlom__va_array~used_range-bottom_right-column.
        zif_xlom__va_array~used_range-bottom_right-column = column.
        recalculate_column_count = abap_true.
      ENDIF.
      IF recalculate_row_count = abap_true.
        zif_xlom__va_array~used_range-row_count = zif_xlom__va_array~used_range-bottom_right-row - zif_xlom__va_array~used_range-top_left-row + 1.
      ENDIF.
      IF recalculate_column_count = abap_true.
        zif_xlom__va_array~used_range-column_count = zif_xlom__va_array~used_range-bottom_right-column - zif_xlom__va_array~used_range-top_left-column + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_xlom__va_array~get_array_values.
    result = VALUE #(
        cells                 = VALUE #( FOR <cell> IN _cells
                                        ( row = <cell>-row
                                          column = <cell>-column
                                          value = <cell>-value ) )
*        rows                  = VALUE #( FOR row_number = 1 WHILE row_number <= zif_xlom__va_array~used_range-bottom_right-row
*                                        LET row_number_2 = row_number IN
*                                        ( columns_of_row = VALUE #( FOR <cell> IN _cells
*                                                                    WHERE ( row = row_number_2 )
*                                                                    ( <cell>-value ) ) ) )
        values_of_other_cells = values_of_other_cells ).
  ENDMETHOD.

  METHOD zif_xlom__va_array~get_cell_value.
    IF    row > zif_xlom__va_array~ROW_count
        or column > zif_xlom__va_array~column_count.
      result = zcl_xlom__va_error=>ref.
    ELSEIF    row    < zif_xlom__va_array~used_range-top_left-row
       OR row    > zif_xlom__va_array~used_range-bottom_right-row.
      IF lines( values_of_other_cells ) = 1.
        " Case of the array of a worksheet.
        result = values_of_other_cells[ 1 ]-value.
      ELSE.
        result = values_of_other_cells[ row    = zif_xlom__va_array~used_range-bottom_right-row + 1
                                        column = column ]-value.
      ENDIF.
    ELSEIF column < zif_xlom__va_array~used_range-top_left-column
       OR column > zif_xlom__va_array~used_range-bottom_right-column.
      IF lines( values_of_other_cells ) = 1.
        " Case of the array of a worksheet.
        result = values_of_other_cells[ 1 ]-value.
      ELSE.
        result = values_of_other_cells[ row    = row
                                        column = zif_xlom__va_array~used_range-bottom_right-column + 1 ]-value.
      ENDIF.
    ELSE.
      DATA(cell) = REF #( _cells[ row    = row
                                  column = column ] OPTIONAL ).
      IF cell IS NOT BOUND.
        " Empty/Blank - Its evaluation depends on its usage (zero or empty string)
        " =1+Empty gives 1, ="a"&Empty gives "a"
        result = zcl_xlom__va_empty=>get_singleton( ).
      ELSE.
        result = cell->value.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_xlom__va_array~get_section_as_array.
    DATA(row_count)    = bottom_right-row - top_left-row + 1.
    DATA(column_count) = bottom_right-column - top_left-column + 1.
    DATA(target_array) = create_initial( row_count    = row_count
                                         column_count = column_count ).
    DATA(row) = 1.
    WHILE row <= row_count.
      DATA(column) = 1.
      WHILE column <= column_count.
        target_array->zif_xlom__va_array~set_cell_value(
            column = column
            row    = row
            value  = zif_xlom__va_array~get_cell_value( column = top_left-column + column - 1
                                                        row    = top_left-row + row - 1 ) ).
        column = column + 1.
      ENDWHILE.
      row = row + 1.
    ENDWHILE.
    result = target_array.
  ENDMETHOD.

  METHOD zif_xlom__va_array~set_array_values.
    _cells = VALUE #( ).
    LOOP AT cells REFERENCE INTO DATA(cell).
      zif_xlom__va_array~set_cell_value( column = cell->column
                                         row    = cell->row
                                         value  = cell->value ).
    ENDLOOP.

    me->values_of_other_cells = values_of_other_cells.
*    DATA(row) = 1.
*    _cells = VALUE #( ).
*    LOOP AT rows REFERENCE INTO DATA(row2).
*      DATA(column) = 1.
*      LOOP AT row2->columns_of_row INTO DATA(cell_value).
*        zif_xlom__va_array~set_cell_value( column = column
*                                           row    = row
*                                           value  = cell_value ).
*        column = column + 1.
*      ENDLOOP.
*      row = row + 1.
*    ENDLOOP.
*    me->value_of_other_cells = COND #( WHEN values_of_other_cells IS BOUND
*                                       THEN values_of_other_cells
*                                       ELSE zcl_xlom__va_empty=>get_singleton( ) ).
  ENDMETHOD.

  METHOD zif_xlom__va_array~set_cell_value.
    IF    row    > zif_xlom__va_array~row_count
       OR row    < 1
       OR column > zif_xlom__va_array~column_count
       OR column < 1.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.
    IF value IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.

    CASE value->type.

      WHEN value->c_type-array
        OR value->c_type-range.

        DATA(source_array) = CAST zif_xlom__va_array( value ).
        DATA(source_array_row) = 1.
        WHILE source_array_row <= source_array->used_range-row_count.
          DATA(source_array_column) = 1.
          WHILE source_array_column <= source_array->used_range-column_count.
            DATA(source_array_cell) = source_array->get_cell_value( column = source_array_column
                                                                    row    = source_array_row ).
            set_cell_value_single( row        = row + source_array_row - 1
                                   column     = column + source_array_column - 1
                                   value      = source_array_cell
                                   formula    = formula
                                   calculated = calculated ).

            source_array_column = source_array_column + 1.
          ENDWHILE.
          source_array_row = source_array_row + 1.
        ENDWHILE.

      WHEN OTHERS.

        set_cell_value_single( row        = row
                               column     = column
                               value      = value
                               formula    = formula
                               calculated = calculated ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_xlom__va~get_value.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
  ENDMETHOD.

  METHOD zif_xlom__va~is_array.
    result = abap_true.
  ENDMETHOD.

  METHOD zif_xlom__va~is_boolean.
    result = abap_false.
  ENDMETHOD.

  METHOD zif_xlom__va~is_equal.
    IF input_result->type = zif_xlom__va=>c_type-array.
      DATA(input_array) = CAST zcl_xlom__va_array( input_result ).
      IF     zif_xlom__va_array~column_count = input_array->zif_xlom__va_array~column_count
         AND zif_xlom__va_array~row_count    = input_array->zif_xlom__va_array~row_count
         AND me->_cells = input_array->_cells.
        result = abap_true.
      ELSE.
        result = abap_false.
      ENDIF.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD zif_xlom__va~is_error.
    result = abap_false.
  ENDMETHOD.

  METHOD zif_xlom__va~is_number.
    result = abap_false.
  ENDMETHOD.

  METHOD zif_xlom__va~is_string.
    result = abap_false.
  ENDMETHOD.
ENDCLASS.
