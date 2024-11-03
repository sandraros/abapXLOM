CLASS zcl_xlom__ex_el_table_item_spe DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex_el.

    CLASS-DATA all      TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA data     TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA headers  TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA this_row TYPE REF TO zcl_xlom__ex_el_table_item_spe.
    CLASS-DATA totals   TYPE REF TO zcl_xlom__ex_el_table_item_spe.

    data name type string READ-ONLY.

    CLASS-METHODS class_constructor.

ENDCLASS.


CLASS zcl_xlom__ex_el_table_item_spe IMPLEMENTATION.
  METHOD class_constructor.
    all = NEW zcl_xlom__ex_el_table_item_spe( ).
    all->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
    all->name = '[#All]'.
*    all->_all = abap_true.
    data = NEW zcl_xlom__ex_el_table_item_spe( ).
    data->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
    data->name = '[#Data]'.
*    data->_data = abap_true.
    headers = NEW zcl_xlom__ex_el_table_item_spe( ).
    headers->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
    headers->name = '[#Headers]'.
*    headers->_headers = abap_true.
    this_row = NEW zcl_xlom__ex_el_table_item_spe( ).
    this_row->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
    this_row->name = '[#This Row]'.
*    this_row->_this_row = abap_true.
    totals = NEW zcl_xlom__ex_el_table_item_spe( ).
    totals->zif_xlom__ex~type = zif_xlom__ex=>c_type-table_item_spe.
    totals->name = '[#Totals]'.
*    totals->_totals = abap_true.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
*    result = zcl_xlom__va_string=>get( string ).
*    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__ex_el~render.
    result = name.
  ENDMETHOD.
ENDCLASS.
