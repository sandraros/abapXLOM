CLASS zcl_xlom__ex_el_table DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    TYPES ty_table_name    TYPE c LENGTH 255.
    TYPES ty_row_specifier TYPE i.

    CONSTANTS:
      "! Possible item specifiers, [#All] and [#This Row] can be specified only alone,
      "! [#Headers] and [#Totals] can be specified alone or with [#Data], [#Data] can
      "! be also specified alone. No specifier is the same as [#Data].
      "! Note that [#All] has the same meaning as the combination of [#Headers], [#Data] and [#Totals].
      BEGIN OF c_rows,
        all          TYPE ty_row_specifier VALUE 1,
        data         TYPE ty_row_specifier VALUE 2,
        "! data + totals
        data_totals  TYPE ty_row_specifier VALUE 18,
        headers      TYPE ty_row_specifier VALUE 4,
        "! headers + data
        headers_data TYPE ty_row_specifier VALUE 6,
        this_row     TYPE ty_row_specifier VALUE 8,
        totals       TYPE ty_row_specifier VALUE 16,
      END OF c_rows.

*    CLASS-DATA all      TYPE REF TO zcl_xlom__ex_el_table.
*    CLASS-DATA data     TYPE REF TO zcl_xlom__ex_el_table.
*    CLASS-DATA headers  TYPE REF TO zcl_xlom__ex_el_table.
*    CLASS-DATA this_row TYPE REF TO zcl_xlom__ex_el_table.
*    CLASS-DATA totals   TYPE REF TO zcl_xlom__ex_el_table.
*
*    DATA _all         TYPE abap_bool                             READ-ONLY.
*    DATA _data        TYPE abap_bool                             READ-ONLY.
*    DATA _headers     TYPE abap_bool                             READ-ONLY.
*    DATA _this_row    TYPE abap_bool                             READ-ONLY.
*    DATA _totals      TYPE abap_bool                             READ-ONLY.
    DATA column       TYPE REF TO zcl_xlom__ex_el_table_column   READ-ONLY.
    DATA column_range TYPE REF TO zcl_xlom__ex_el_table_col_rang READ-ONLY.
    DATA rows         TYPE ty_row_specifier                      READ-ONLY.
    DATA name         TYPE ty_table_name                         READ-ONLY.

*    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !name         TYPE ty_table_name
                !column       TYPE REF TO zcl_xlom__ex_el_table_column   OPTIONAL
                !rows         TYPE ty_row_specifier                      OPTIONAL
                column_range  TYPE REF TO zcl_xlom__ex_el_table_col_rang OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_table.
ENDCLASS.


CLASS zcl_xlom__ex_el_table IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom__ex_el_table( ).
    result->name              = name.
    result->column            = column.
    result->rows              = COND #( WHEN rows <> 0
                                        THEN rows
                                        ELSE c_rows-data ).
    result->column_range      = column_range.
    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-table.
*    result->_all  = boolc( rows = c_rows-all ).
*    result->_data = boolc(    rows = c_rows-all
*                           OR rows = c_rows-data
*                           OR rows = c_rows-data_totals
*                           OR rows = c_rows-headers_data ).
*    result->_headers = boolc(    rows = c_rows-all
*                           OR rows = c_rows-headers
*                           OR rows = c_rows-headers_data ).
*    result->_this_row = boolc( rows = c_rows-this_row ).
*    result->_this_row = boolc( rows = c_rows-this_row ).
*  METHOD class_constructor.
*    all = NEW zcl_xlom__ex_el_table( ).
*    all->_all = abap_true.
*    data = NEW zcl_xlom__ex_el_table( ).
*    data->_data = abap_true.
*    headers = NEW zcl_xlom__ex_el_table( ).
*    headers->_headers = abap_true.
*    this_row = NEW zcl_xlom__ex_el_table( ).
*    this_row->_this_row = abap_true.
*    totals = NEW zcl_xlom__ex_el_table( ).
*    totals->_totals = abap_true.
*  ENDMETHOD.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    data(containing_cell) = context->worksheet->cells( row    = context->containing_cell-row
                                  column = context->containing_cell-column ).
*    if containing_cell->current_region IS BOUND.
*    ENDIF.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
*    result = zcl_xlom__va_string=>get( string ).
*    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
