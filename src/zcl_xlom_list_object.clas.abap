"! https://learn.microsoft.com/en-us/office/vba/api/excel.listobject
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
    DATA parent       TYPE REF TO zcl_xlom_worksheet            READ-ONLY.
    DATA range        TYPE REF TO zcl_xlom_range                READ-ONLY.
    DATA source_type  TYPE zcl_xlom=>ty_list_object_source_type READ-ONLY.
*    "! TODO. NOT USED CURRENTLY.
*    "! https://learn.microsoft.com/en-us/office/vba/api/excel.listobject.tablestyle
*    DATA table_style      TYPE REF TO zcl_xlom_table_style          READ-ONLY.

    "! See the parameter descriptions in the method ZCL_XLOM_LIST_OBJECTS=>ADD.
    CLASS-METHODS create
      IMPORTING !name            TYPE ty_name
                parent           TYPE REF TO zcl_xlom_worksheet
                source_type      TYPE zcl_xlom=>ty_list_object_source_type OPTIONAL
                !source          TYPE any                                  OPTIONAL
                link_source      TYPE abap_bool                            OPTIONAL
                has_headers      TYPE zcl_xlom=>ty_yes_no_guess            DEFAULT zcl_xlom=>c_yes_no_guess-guess
                !destination     TYPE REF TO zcl_xlom_range                OPTIONAL
                table_style_name TYPE string                               OPTIONAL
      RETURNING VALUE(result)    TYPE REF TO zcl_xlom_list_object.

    "! Rename
    METHODS set_name
      IMPORTING !name TYPE ty_name.
  PRIVATE SECTION.
    CLASS-METHODS get_new_header_row_cell_values
      IMPORTING
        old_header_row_cell_values TYPE string_table
      RETURNING
        value(result)              TYPE string_table.
ENDCLASS.


CLASS zcl_xlom_list_object IMPLEMENTATION.
  METHOD create.
    IF    name             IS INITIAL
       OR source_type      <> zcl_xlom=>c_list_object_source_type-range
       OR link_source       = abap_true
       OR has_headers      <> zcl_xlom=>c_yes_no_guess-yes
       OR destination      IS BOUND
       OR table_style_name IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.

    result = NEW zcl_xlom_list_object( ).
    result->name        = name.
    result->parent      = parent.
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
          DATA(header_row) = zcl_xlom_range=>create_from_row_column(
                                 worksheet   = result->range->parent
                                 row         = result->range->row
                                 column      = result->range->column
                                 row_size    = 1
                                 column_size = result->range->zif_xlom__va_array~column_count ).
          data(header_row_cell_values) = zcl_xlom__ut=>get_lookup_standard_table(
                                           lookup_range_array  = header_row
                                           optimize_used_range = abap_false ).
*          DATA(header_row_cell_values) = VALUE string_table( ).
*          DATA(column_number) = result->range->column.
*          DO result->range->zif_xlom__va_array~column_count TIMES.
*            DATA(header_row_cell_value) = zcl_xlom__va=>to_string( parent->cells( row    = result->range->row
*                                                                                  column = column_number
*                                                                       )->value( ) )->get_string( ).
*            INSERT header_row_cell_value INTO TABLE header_row_cell_values.
*          ENDDO.

          DATA(new_header_row_cell_values) = get_new_header_row_cell_values( header_row_cell_values ).

*          " Give a name to empty and non-unique column names
*          " TODO
*          " If a column name is empty, Excel logic is to use "Column1"; if the name exists, "2" is used instead of "1", etc.
*          " If a column name appears twice, Excel logic is to append "2"; if the name exists, "3" is used instead of "2", etc.
*          " The appendage of a number is to be preceded by a truncation if the column name has more than 252 characters. Note that Excel
*          " has a bug because it should be 250 characters because if there are more than 1000 same column names, Excel currently gives
*          " a non-unique name ending with "100" for 1000 to 1009 and 10000 to 10099, "101" for 1010 to 1019 and 10100 to 10199, etc.
*          LOOP AT header_row_cell_values ASSIGNING FIELD-SYMBOL(<header_row_cell_value>).
*            column_number = sy-tabix.
*            IF header_row_cell_value IS INITIAL.
*              DATA(column_prefix) = `Column`.
*              DATA(column_suffix) = 1.
*            ELSE.
*              DATA(count_columns_with_same_name) = 0.
*              LOOP AT header_row_cell_values TRANSPORTING NO FIELDS WHERE table_line = header_row_cell_value.
*                count_columns_with_same_name = count_columns_with_same_name + 1.
*              ENDLOOP.
*              IF count_columns_with_same_name >= 2.
*                column_prefix = COND #( WHEN strlen( header_row_cell_value ) <= 250
*                                        THEN header_row_cell_value
*                                        ELSE substring( val = header_row_cell_value
*                                                        len = 250 ) ).
*                column_suffix = 2.
*              ELSE.
*                " Column name is unique, no need to determine a new name.
*                column_prefix = ``.
*                column_suffix = 0.
*              ENDIF.
*            ENDIF.
*            IF column_prefix IS NOT INITIAL.
*              DO.
*                DATA(new_column_name) = |{ column_prefix }{ column_suffix }|.
*                count_columns_with_same_name = 0.
*                LOOP AT header_row_cell_values TRANSPORTING NO FIELDS WHERE table_line = new_column_name.
*                  count_columns_with_same_name = count_columns_with_same_name + 1.
*                ENDLOOP.
*                IF count_columns_with_same_name = 0.
*                  EXIT.
*                ENDIF.
*                column_suffix = column_suffix + 1.
*              ENDDO.
*              parent->cells( row    = result->range->row
*                             column = column_number
*                           )->set_value( zcl_xlom__va_string=>get( new_column_name ) ).
*            ENDIF.
*            " TODO
*            INSERT VALUE #( ) INTO TABLE columns.
*            column_number = column_number + 1.
*          ENDLOOP.

          DATA(index_number) = 1.
          LOOP AT new_header_row_cell_values ASSIGNING FIELD-SYMBOL(<new_header_row_cell_value>).
            IF <new_header_row_cell_value> <> header_row_cell_values[ index_number ].
              parent->cells( row    = result->range->row
                             column = result->range->column + index_number - 1
                           )->set_value( zcl_xlom__va_string=>get( <new_header_row_cell_value> ) ).
            ENDIF.
            index_number = index_number + 1.
          ENDLOOP.
          result->list_columns = zcl_xlom_list_columns=>create( list_object = result ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDIF.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD get_new_header_row_cell_values.
    result = old_header_row_cell_values.
    " Give a name to empty and non-unique column names
    " TODO
    " If a column name is empty, Excel logic is to use "Column1"; if the name exists, "2" is used instead of "1", etc.
    " If a column name appears twice, Excel logic is to append "2"; if the name exists, "3" is used instead of "2", etc.
    " The appendage of a number is to be preceded by a truncation if the column name has more than 252 characters. Note that Excel
    " has a bug because it should be 250 characters because if there are more than 1000 same column names, Excel currently gives
    " a non-unique name ending with "100" for 1000 to 1009 and 10000 to 10099, "101" for 1010 to 1019 and 10100 to 10199, etc.
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA(column_number) = 0.
    LOOP AT result REFERENCE INTO DATA(header_row_cell_value).
      column_number = column_number + 1.
      IF header_row_cell_value->* IS INITIAL.
        DATA(column_prefix) = `Column`.
        DATA(column_suffix) = 1.
      ELSE.
        DATA(count_columns_with_same_name) = 0.
        LOOP AT old_header_row_cell_values TRANSPORTING NO FIELDS WHERE table_line = header_row_cell_value->*.
          count_columns_with_same_name = count_columns_with_same_name + 1.
        ENDLOOP.
        IF count_columns_with_same_name >= 2.
          column_prefix = COND #( WHEN strlen( header_row_cell_value->* ) <= 250
                                  THEN header_row_cell_value->*
                                  ELSE substring( val = header_row_cell_value->*
                                                  len = 250 ) ).
          column_suffix = 2.
        ELSE.
          " Column name is unique, no need to determine a new name.
          column_prefix = ``.
          column_suffix = 0.
        ENDIF.
      ENDIF.
      IF column_prefix IS NOT INITIAL.
        DO.
          data(new_column_name) = |{ column_prefix }{ column_suffix }|.
          count_columns_with_same_name = 0.
          LOOP AT result TRANSPORTING NO FIELDS WHERE table_line = new_column_name.
            count_columns_with_same_name = count_columns_with_same_name + 1.
          ENDLOOP.
          IF count_columns_with_same_name = 0.
            EXIT.
          ENDIF.
          column_suffix = column_suffix + 1.
        ENDDO.
        header_row_cell_value->* = new_column_name.
*              parent->cells( row    = result->range->row
*                             column = column_number
*                           )->set_value( zcl_xlom__va_string=>get( new_column_name ) ).
      ENDIF.
*            " TODO
*            INSERT VALUE #( ) INTO TABLE columns.
      column_number = column_number + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_name.
    " TODO make sure the new name doesn't exist yet.
    me->name = name.
  ENDMETHOD.
ENDCLASS.
