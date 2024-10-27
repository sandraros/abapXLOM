CLASS zcl_xlom_list_column DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_column.
ENDCLASS.


CLASS zcl_xlom_list_column IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom_list_column( ).
  ENDMETHOD.
ENDCLASS.
