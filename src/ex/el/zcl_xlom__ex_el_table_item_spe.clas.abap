CLASS zcl_xlom__ex_el_table_item_spe DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    CLASS-DATA all      TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA data     TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA headers  TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA this_row TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA totals   TYPE REF TO zcl_xlom__ex_el_table_item_spe.

    CLASS-METHODS class_constructor.
ENDCLASS.


CLASS zcl_xlom__ex_el_table_item_spe IMPLEMENTATION.
  METHOD class_constructor.
    all = NEW zcl_xlom__ex_el_table_item_spe( ).
    all->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
*    all->_all = abap_true.
    data = NEW zcl_xlom__ex_el_table_item_spe( ).
    data->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
*    data->_data = abap_true.
    headers = NEW zcl_xlom__ex_el_table_item_spe( ).
    headers->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
*    headers->_headers = abap_true.
    this_row = NEW zcl_xlom__ex_el_table_item_spe( ).
    this_row->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
*    this_row->_this_row = abap_true.
    totals = NEW zcl_xlom__ex_el_table_item_spe( ).
    totals->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
*    totals->_totals = abap_true.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
*    result = zcl_xlom__va_string=>get( string ).
*    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
