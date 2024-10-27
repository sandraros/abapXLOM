CLASS zcl_xlom__ex_el_table_column DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    DATA name TYPE string READ-ONLY.

    CLASS-METHODS create
      IMPORTING !name         TYPE csequence
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_table_column.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom__ex_el_table_column IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom__ex_el_table_column( ).
    result->name              = name.
    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_column.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION type zcx_xlom_todo.
*    result = zcl_xlom__va_string=>get( string ).
*    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
