CLASS zcl_xlom__ex_el_table_col_rang DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    CLASS-METHODS create
      IMPORTING !from         TYPE REF TO zcl_xlom__ex_el_table_column
                !to           TYPE REF TO zcl_xlom__ex_el_table_column
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_table_col_rang.

  PRIVATE SECTION.
    DATA from TYPE REF TO zcl_xlom__ex_el_table_column.
    DATA to   TYPE REF TO zcl_xlom__ex_el_table_column.
ENDCLASS.


CLASS zcl_xlom__ex_el_table_col_rang IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom__ex_el_table_col_rang( ).
    result->from              = from.
    result->to                = to.
    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_col_rang.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION type zcx_xlom_todo.
*    result = zcl_xlom__va_string=>get( string ).
*    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.
ENDCLASS.
