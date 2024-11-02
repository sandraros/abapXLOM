CLASS zcl_xlom__ext_list_column DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS set_index
      IMPORTING
        list_column TYPE REF TO zcl_xlom_list_column
        index       TYPE i.
ENDCLASS.



CLASS zcl_xlom__ext_list_column IMPLEMENTATION.
  METHOD set_index.
    list_column->index = index.
  ENDMETHOD.
ENDCLASS.
