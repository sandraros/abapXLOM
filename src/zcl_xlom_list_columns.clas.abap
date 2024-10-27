CLASS zcl_xlom_list_columns DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_columns TYPE STANDARD TABLE OF REF TO zcl_xlom_list_column WITH EMPTY KEY.

*    "! https://learn.microsoft.com/en-us/office/vba/api/excel.listcolumns.add
*    "!
*    "! @parameter position | Integer
*    "! @parameter result | Added column
*    METHODS add
*      IMPORTING !position     TYPE int2
*      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_column.

    CLASS-METHODS create
      IMPORTING !columns      TYPE ty_columns
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_columns.

  PRIVATE SECTION.
    DATA columns TYPE ty_columns.
ENDCLASS.


CLASS zcl_xlom_list_columns IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom_list_columns( ).
    result->columns = columns.
*  METHOD add.
*    DATA(list_column) = zcl_xlom_list_column=>create( ).
*    INSERT list_column INTO items INDEX position.
*    result = list_column.
*  ENDMETHOD.
  ENDMETHOD.
ENDCLASS.
