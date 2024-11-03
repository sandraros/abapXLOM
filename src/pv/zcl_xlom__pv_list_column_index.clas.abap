CLASS zcl_xlom__pv_list_column_index DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom_list_columns.

  PRIVATE SECTION.
    CLASS-METHODS set_index
      IMPORTING list_column TYPE REF TO zcl_xlom_list_column
                !index      TYPE i.
ENDCLASS.


CLASS zcl_xlom__pv_list_column_index IMPLEMENTATION.
  METHOD set_index.
    list_column->index = index.
  ENDMETHOD.
ENDCLASS.
