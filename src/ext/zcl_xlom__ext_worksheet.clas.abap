CLASS zcl_xlom__ext_worksheet DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS max_rows    TYPE i VALUE 1048576.
    CONSTANTS max_columns TYPE i VALUE 16384.

    CLASS-METHODS get_used_range
      IMPORTING worksheet     TYPE REF TO zcl_xlom_worksheet
      RETURNING VALUE(result) TYPE zcl_xlom__va_array=>ts_used_range.
ENDCLASS.


CLASS zcl_xlom__ext_worksheet IMPLEMENTATION.
  METHOD get_used_range.
    result = worksheet->_array->zif_xlom__va_array~used_range.
  ENDMETHOD.
ENDCLASS.
