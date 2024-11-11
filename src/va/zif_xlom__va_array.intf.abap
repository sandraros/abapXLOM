INTERFACE zif_xlom__va_array
  PUBLIC.

  INTERFACES zif_xlom__va.

  TYPES:
    BEGIN OF ts_cell,
      row    TYPE i,
      column TYPE i,
      value  TYPE REF TO zif_xlom__va,
    END OF ts_cell.
  TYPES tt_cell   TYPE SORTED TABLE OF ts_cell WITH UNIQUE KEY row column.
  TYPES tt_column TYPE STANDARD TABLE OF REF TO zif_xlom__va WITH EMPTY KEY.
  TYPES:
    BEGIN OF ts_row,
      columns_of_row TYPE tt_column,
    END OF ts_row.
  TYPES tt_row TYPE STANDARD TABLE OF ts_row WITH EMPTY KEY.
  TYPES:
    BEGIN OF ts_address_one_cell,
      "! 0 means that the address is the whole row defined in ROW
      column       TYPE i,
      column_fixed TYPE abap_bool,
      "! 0 means that the address is the whole column defined in COLUMN
      row          TYPE i,
      row_fixed    TYPE abap_bool,
    END OF ts_address_one_cell.
  TYPES:
    BEGIN OF ts_address,
      "! Can also be an internal ID like "1" ([1]Sheet1!A1)
      workbook_name  TYPE string,
      worksheet_name TYPE string,
      range_name     TYPE string,
      top_left       TYPE ts_address_one_cell,
      bottom_right   TYPE ts_address_one_cell,
    END OF ts_address.
  TYPES:
    BEGIN OF ts_array_values,
      cells                 TYPE tt_cell,
*      rows                  TYPE tt_row,
      "! C:C<>"" will return FALSE (value of all rows below the used range).
      "! 1:1<>"" will return FALSE (value of all columns at the right of the used range).
      "! FILTER(C:D,C:C<>"") will return one row of two columns (value of all rows below the used range).
      values_of_other_cells TYPE tt_cell,
*      value_of_other_cells TYPE REF TO zif_xlom__va,
    END OF ts_array_values.

  DATA used_range   TYPE zcl_xlom__ut=>ts_used_range_with_size READ-ONLY.
  DATA row_count    TYPE i                                     READ-ONLY.
  DATA column_count TYPE i                                     READ-ONLY.

  METHODS get_array_values
    RETURNING VALUE(result) TYPE ts_array_values.

  METHODS get_section_as_array
    IMPORTING top_left      TYPE zcl_xlom=>ts_range_address_one_cell
              bottom_right  TYPE zcl_xlom=>ts_range_address_one_cell
    RETURNING VALUE(result) TYPE REF TO zif_xlom__va_array.

  "! @parameter column | Start from 1
  "! @parameter row    | Start from 1
  METHODS get_cell_value
    IMPORTING !column       TYPE i
              !row          TYPE i
    RETURNING VALUE(result) TYPE REF TO zif_xlom__va.

  "! @parameter cells            | Specific values from row 1 and column 1 up to any row and any column
  "! @parameter values_of_other_cells | .
  METHODS set_array_values
    IMPORTING cells                 TYPE tt_cell
              values_of_other_cells TYPE tt_cell OPTIONAL.
*              !rows                TYPE tt_row
*              values_of_other_cells TYPE REF TO zif_xlom__va OPTIONAL.

  "! @parameter column | Start from 1
  "! @parameter row    | Start from 1
  METHODS set_cell_value
    IMPORTING !column    TYPE i
              !row       TYPE i
              !value     TYPE REF TO zif_xlom__va
              formula    TYPE REF TO zif_xlom__ex OPTIONAL
              calculated TYPE abap_bool           OPTIONAL.
ENDINTERFACE.
