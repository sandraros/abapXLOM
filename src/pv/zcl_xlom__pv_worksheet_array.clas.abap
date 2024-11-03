CLASS zcl_xlom__pv_worksheet_array DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom_range
                 zcl_xlom_worksheet
                 zcl_xlom__ex_fu_concatenate
                 zcl_xlom__va.

  PRIVATE SECTION.
    CLASS-METHODS get_array
      IMPORTING worksheet     TYPE REF TO zcl_xlom_worksheet
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__va_array.

    CLASS-METHODS get_cells
      IMPORTING worksheet     TYPE REF TO zcl_xlom_worksheet
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__va_array=>tt_cell.

    CLASS-METHODS get_used_range
      IMPORTING worksheet     TYPE REF TO zcl_xlom_worksheet
      RETURNING VALUE(result) TYPE zcl_xlom__va_array=>ts_used_range.
ENDCLASS.


CLASS zcl_xlom__pv_worksheet_array IMPLEMENTATION.
  METHOD get_array.
    result = worksheet->_array.
  ENDMETHOD.

  METHOD get_cells.
    result = ref #( worksheet->_array->_cells ).
  ENDMETHOD.

  METHOD get_used_range.
    result = worksheet->_array->used_range.
  ENDMETHOD.
ENDCLASS.
