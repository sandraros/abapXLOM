CLASS zcl_xlom_list_object DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! In a test in Excel with a table, if typing more than 255 characters in the
    "! table name, the extra characters are automatically removed without warning.
    TYPES ty_name TYPE c LENGTH 255.

*    "! TODO. NOT USED CURRENTLY.
*    "! <p>A Range object specifying a single-cell reference as the destination for the top-left corner of the new list object.
*    "!    If the Range object refers to more than one cell, an error is generated.</p>
*    "! <p>The Destination argument must be specified when SourceType is set to xlSrcExternal.
*    "!    The Destination argument is ignored if SourceType is set to xlSrcRange.</p>
*    "! <p>The destination range must be on the worksheet that contains the ListObjects collection specified by expression.
*    "!    New columns will be inserted at the Destination to fit the new list. Therefore, existing data will not be overwritten.</p>
*    DATA destination      TYPE REF TO zcl_xlom_range                READ-ONLY.
*    "! https://learn.microsoft.com/en-us/office/vba/api/excel.listobject.headerrowrange
*    DATA header_row_range TYPE REF TO zcl_xlom_range                READ-ONLY.
    DATA list_columns TYPE REF TO zcl_xlom_list_columns         READ-ONLY.
*    DATA list_rows    TYPE REF TO zcl_xlom_list_rows            READ-ONLY.
    DATA name         TYPE ty_name                              READ-ONLY.
    "! In my tests, the parent was always ZCL_XLOM_WORKSHEET (ListObject from Range and from Model).
    DATA parent       TYPE REF TO zcl_xlom_sheet                READ-ONLY.
    DATA range        TYPE REF TO zcl_xlom_range                READ-ONLY.
    DATA source_type  TYPE zcl_xlom=>ty_list_object_source_type READ-ONLY.
*    "! TODO. NOT USED CURRENTLY.
*    "! https://learn.microsoft.com/en-us/office/vba/api/excel.listobject.tablestyle
*    DATA table_style      TYPE REF TO zcl_xlom_table_style          READ-ONLY.

    CLASS-METHODS create
      IMPORTING !name            TYPE ty_name
                parent           TYPE REF TO zcl_xlom_sheet
                source_type      TYPE zcl_xlom=>ty_list_object_source_type OPTIONAL
                !source          TYPE any                                  OPTIONAL
                link_source      TYPE abap_bool                            OPTIONAL
                has_headers      TYPE zcl_xlom=>ty_yes_no_guess            OPTIONAL
                !destination     TYPE REF TO zcl_xlom_range                OPTIONAL
                table_style_name TYPE string                               OPTIONAL
      RETURNING VALUE(result)    TYPE REF TO zcl_xlom_list_object.

    "! Rename
    METHODS set_name
      IMPORTING !name TYPE ty_name.
ENDCLASS.


CLASS zcl_xlom_list_object IMPLEMENTATION.
  METHOD create.
    " TODO: parameter LINK_SOURCE is never used (ABAP cleaner)
    " TODO: parameter HAS_HEADERS is never used (ABAP cleaner)

    IF    name             IS INITIAL
       OR source_type      <> zcl_xlom=>c_list_object_source_type-range
       OR has_headers      <> zcl_xlom=>c_yes_no_guess-yes
       OR destination      IS BOUND
       OR table_style_name IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.

    result = NEW zcl_xlom_list_object( ).
    result->name        = name.
    result->source_type = source_type.

    CASE source_type.
      WHEN zcl_xlom=>c_list_object_source_type-range.
        IF source IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_xlom_todo
            EXPORTING
              text = 'When SOURCE_TYPE = xlSrcRange: SOURCE must be a Range object representing the data source'.
        ENDIF.
        TRY.
            result->range = source.
          CATCH cx_sy_move_cast_error ##NO_HANDLER.
            RAISE EXCEPTION TYPE zcx_xlom_todo
              EXPORTING
                text = 'When SOURCE_TYPE = xlSrcRange: SOURCE must be a Range object representing the data source'.
        ENDTRY.
        IF has_headers = zcl_xlom=>c_yes_no_guess-yes.
          DATA(columns) = value zcl_xlom_list_columns=>ty_columns( ).
          DATA(header_row) = result->range->rows( 1 ).
          DATA(column_number) = CONV int2( 1 ).
          DATA(column_count) = header_row->count( ).
          WHILE column_number <= column_count.
*            DATA(column) = result->list_columns->add( position = column_number ).
*            column->set_name( ).
            INSERT VALUE #( ) INTO TABLE columns.
            column_number = column_number + 1.
          ENDWHILE.
          result->list_columns = zcl_xlom_list_columns=>create( columns ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDIF.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD set_name.
    " TODO make sure the new name doesn't exist yet.
    me->name = name.
  ENDMETHOD.
ENDCLASS.
