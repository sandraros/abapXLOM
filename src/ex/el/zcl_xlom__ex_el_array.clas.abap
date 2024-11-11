CLASS zcl_xlom__ex_el_array DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex_array.
    INTERFACES zif_xlom__ex_el.

*    TYPES:
*      BEGIN OF ts_cell,
*        row    TYPE i,
*        column TYPE i,
*        value  TYPE REF TO zif_xlom__ex,
*      END OF ts_cell.
*    TYPES tt_cell TYPE SORTED TABLE OF ts_cell WITH UNIQUE KEY row column.
*
*    DATA cells TYPE tt_cell READ-ONLY.
    TYPES tt_column TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_row,
        columns_of_row TYPE tt_column,
      END OF ts_row.
    TYPES tt_row TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

    DATA rows TYPE tt_row READ-ONLY.

    CLASS-METHODS create
*      IMPORTING cells         TYPE tt_cell
      IMPORTING !rows         TYPE tt_row
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_array.
  PRIVATE SECTION.
    DATA column_count TYPE i.
ENDCLASS.


CLASS zcl_xlom__ex_el_array IMPLEMENTATION.
  METHOD create.
    DATA(column_count) = 0.
    LOOP AT rows REFERENCE INTO DATA(row).
      IF column_count = 0.
        column_count = lines( row->columns_of_row ).
      ELSEIF lines( row->columns_of_row ) <> column_count.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      ENDIF.
    ENDLOOP.

    result = NEW zcl_xlom__ex_el_array( ).
    result->zif_xlom__ex~type = result->zif_xlom__ex~c_type-array.
*    result->cells             = cells.
    result->rows              = rows.
    result->column_count      = column_count.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(cells) = VALUE zif_xlom__va_array=>tt_cell( ).
    DATA(row_number) = 1.
    LOOP AT rows REFERENCE INTO DATA(row).
      DATA(column_number) = 1.
      LOOP AT row->columns_of_row INTO DATA(expression).
        DATA(value) = zcl_xlom__ex_ut_eval=>evaluate_array_operands( expression = expression
                                                                     context    = context ).
        INSERT VALUE #( row    = row_number
                        column = column_number
                        value  = value )
               INTO TABLE cells.
        column_number = column_number + 1.
      ENDLOOP.
      row_number = row_number + 1.
    ENDLOOP.

    result = zcl_xlom__va_array=>create_initial( row_count    = lines( rows )
                                                 column_count = column_count
                                                 cells        = cells ).
*    result = zcl_xlom__va_array=>create_initial(
*                 row_count    = lines( rows )
*                 column_count = REDUCE #( INIT n = 0
*                                               FOR <row> IN rows
*                                               NEXT n = nmax( val1 = n
*                                                              val2 = lines( <row>-columns_of_row ) ) )
*                 rows         = VALUE #( FOR <row> IN rows
*                                         ( columns_of_row = VALUE #( FOR <column> IN <row>-columns_of_row
*                                                                     ( zcl_xlom__ex_ut_eval=>evaluate_array_operands(
*                                                                           expression = <column>
*                                                                           context    = context ) ) ) ) ) ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__ex_el~render.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
  ENDMETHOD.
ENDCLASS.
