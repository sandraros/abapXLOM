class ZCL_XLOM__EX_EL_ARRAY definition
  public
  final
  create private .

public section.

  interfaces ZIF_XLOM__EX .
  interfaces ZIF_XLOM__EX_ARRAY .

  types:
    tt_column TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY .
  types:
    BEGIN OF ts_row,
        columns_of_row TYPE tt_column,
      END OF ts_row .
  types:
    tt_row TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY .

  class-methods CREATE
    importing
      !ROWS type TT_ROW
    returning
      value(RESULT) type ref to ZCL_XLOM__EX_EL_ARRAY .
  PRIVATE SECTION.
    DATA rows TYPE tt_row.
ENDCLASS.



CLASS ZCL_XLOM__EX_EL_ARRAY IMPLEMENTATION.


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
ENDCLASS.
