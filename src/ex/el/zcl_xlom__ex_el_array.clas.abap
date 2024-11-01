CLASS zcl_xlom__ex_el_array DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.
    INTERFACES zif_xlom__ex_array.

    TYPES tt_column TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_row,
        columns_of_row TYPE tt_column,
      END OF ts_row.
    TYPES tt_row TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.

    DATA rows TYPE tt_row READ-ONLY.

    CLASS-METHODS create
      IMPORTING !rows         TYPE tt_row
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_array.
ENDCLASS.


CLASS zcl_xlom__ex_el_array IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom__ex_el_array( ).
    result->zif_xlom__ex~type = result->zif_xlom__ex~c_type-array.
    result->rows              = rows.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_array=>create_initial(
                 row_count    = lines( rows )
                 column_count = REDUCE #( INIT n = 0
                                               FOR <row> IN rows
                                               NEXT n = nmax( val1 = n
                                                              val2 = lines( <row>-columns_of_row ) ) )
                 rows         = VALUE #( FOR <row> IN rows
                                         ( columns_of_row = VALUE #( FOR <column> IN <row>-columns_of_row
                                                                     ( zcl_xlom__ex_ut_eval=>evaluate_array_operands(
                                                                           expression = <column>
                                                                           context    = context ) ) ) ) ) ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.
ENDCLASS.
