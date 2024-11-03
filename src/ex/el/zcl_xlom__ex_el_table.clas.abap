CLASS zcl_xlom__ex_el_table DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex_el.

    TYPES ty_table_name    TYPE c LENGTH 255.
    TYPES ty_row_specifier TYPE i.

    CONSTANTS:
      "! Possible item specifiers
      "! <ul>
      "! <li>[#All] and [#This Row] can be specified only alone,</li>
      "! <li>[#Headers] and [#Totals] can be specified alone or with [#Data], [#Data] can be also specified alone.</li>
      "! <li>No specifier is the same as [#Data].</li>
      "! <li>Note that [#All] has the same meaning as the combination of [#Headers], [#Data] and [#Totals].</li>
      "! <li>In all cases, the item specifier(s) must be used with a column name or an interval of columns.</li>
      "! </ul>
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
                !rows         TYPE ty_row_specifier                      DEFAULT c_rows-data
                column_range  TYPE REF TO zcl_xlom__ex_el_table_col_rang OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_table.

  PRIVATE SECTION.
    METHODS find_table
      IMPORTING workbook      TYPE REF TO zcl_xlom_workbook
                table_name    TYPE zcl_xlom__ex_el_table=>ty_table_name
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_object.
ENDCLASS.


CLASS zcl_xlom__ex_el_table IMPLEMENTATION.
  METHOD create.
    IF    column_range IS BOUND
       OR (     rows <> c_rows-data
            AND rows <> c_rows-headers
            AND rows <> c_rows-this_row ).
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.

    result = NEW zcl_xlom__ex_el_table( ).
    result->name              = name.
    result->column            = column.
    result->rows              = rows.
    result->column_range      = column_range.
    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-table.
  ENDMETHOD.

  METHOD find_table.
    DATA(worksheet_item) = 1.
    WHILE worksheet_item <= workbook->worksheets->count.
      DATA(worksheet) = workbook->worksheets->item( worksheet_item ).
      DATA(list_object_item) = 1.
      WHILE list_object_item <= worksheet->list_objects->count.
        DATA(list_object) = worksheet->list_objects->item( list_object_item ).
        IF list_object->name = table_name.
          result = list_object.
          RETURN.
        ENDIF.
        list_object_item = list_object_item + 1.
      ENDWHILE.
      worksheet_item = worksheet_item + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(list_object) = find_table( workbook   = context->worksheet->parent
                                    table_name = name ).
    IF list_object IS NOT BOUND.
      result = zcl_xlom__va_error=>ref.
      RETURN.
    ENDIF.

    CASE rows.
      WHEN c_rows-data.
        result = zcl_xlom_range=>create_from_row_column( worksheet   = context->worksheet
                                                         row         = list_object->range->row + 1
                                                         column      = list_object->range->column
                                                         row_size    = list_object->range->zif_xlom__va_array~row_count
                                                         column_size = 1 ).
      WHEN c_rows-headers.
        result = zcl_xlom_range=>create_from_row_column( worksheet   = context->worksheet
                                                         row         = list_object->range->row
                                                         column      = list_object->range->column
                                                         row_size    = 1
                                                         column_size = 1 ).
      WHEN c_rows-this_row.
        DATA(list_column) = list_object->list_columns->item( column->name ).
        result = context->worksheet->cells( row    = context->containing_cell-row
                                            column = list_object->range->column + list_column->index - 1 ).
    ENDCASE.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__ex_el~render.
    result = |{ name
             }[{
             SWITCH string( rows
                            WHEN c_rows-data         THEN '[#Data]'
                            WHEN c_rows-data_totals  THEN '[#Data],[#Totals]'
                            WHEN c_rows-headers      THEN '[#Headers]'
                            WHEN c_rows-headers_data THEN '[#Headers],[#Data]'
                            WHEN c_rows-this_row     THEN '[#This Row]'
                            WHEN c_rows-totals       THEN '[#Totals]'
                            ELSE                          THROW zcx_xlom_todo( ) )
             }{ COND #( WHEN column IS BOUND
                        THEN |,[{ column->name }]| )
             }]|.
  ENDMETHOD.
ENDCLASS.
