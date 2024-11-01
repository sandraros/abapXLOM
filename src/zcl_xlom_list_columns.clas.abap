CLASS zcl_xlom_list_columns DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
*    TYPES ty_columns TYPE STANDARD TABLE OF REF TO zcl_xlom_list_column WITH EMPTY KEY.

    DATA parent TYPE REF TO zcl_xlom_list_object READ-ONLY.

*    "! https://learn.microsoft.com/en-us/office/vba/api/excel.listcolumns.add
*    "!
*    "! @parameter position | Integer
*    "! @parameter result | Added column
*    METHODS add
*      IMPORTING !position     TYPE int2
*      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_column.

    CLASS-METHODS create
      IMPORTING list_object        TYPE REF TO zcl_xlom_list_object
*      IMPORTING header_row    TYPE REF TO zcl_xlom_range
*      IMPORTING !columns      TYPE ty_columns
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_columns.

    METHODS item
      IMPORTING !index        TYPE simple
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_column
      RAISING   zcx_xlom__va.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_list_object,
        INDEX  TYPE i,
        object TYPE REF TO zcl_xlom_list_column,
      END OF ty_list_object.
    TYPES ty_list_objects TYPE hashed TABLE OF ty_list_object WITH UNIQUE KEY index.

    DATA items TYPE ty_list_objects.
    DATA header_row TYPE REF TO zcl_xlom_range.
ENDCLASS.


CLASS zcl_xlom_list_columns IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom_list_columns( ).
    result->parent = list_object.
    DATA(address) = zcl_xlom__ut_om_range=>get_address( list_object->range ).
    result->header_row = zcl_xlom_range=>create_from_row_column(
                           worksheet   = list_object->range->parent
                           row         = address-top_left-row
                           column      = address-top_left-column
                           row_size    = 1
                           column_size = address-bottom_right-column - address-top_left-column + 1 ).
*    result->parent     = header_row->list_object( ).
*    result->columns = columns.
*  METHOD add.
*    DATA(list_column) = zcl_xlom_list_column=>create( ).
*    INSERT list_column INTO items INDEX position.
*    result = list_column.
*  ENDMETHOD.
  ENDMETHOD.

  METHOD item.
    TRY.
        CASE zcl_xlom__ut=>type( index ).
          WHEN cl_abap_typedescr=>typekind_string
            OR cl_abap_typedescr=>typekind_char.
            DATA(header_row_hashed_strings) = zcl_xlom__ut=>get_lookup_range( header_row ).
            DATA(index_2) = header_row_hashed_strings[ string = index ]-column.

          WHEN cl_abap_typedescr=>typekind_int.
            index_2 = index.
*            result = zcl_xlom__va=>to_string( header_row->cells( row    = 1
*                                        column = index )->value( ) )->get_string( ).

          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDCASE.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_xlom__va
          EXPORTING result_error = zcl_xlom__va_error=>ref.
    ENDTRY.

    DATA(list_column) = REF #( items[ index = index_2 ] OPTIONAL ).
    IF list_column IS NOT BOUND.
      INSERT VALUE #( index  = index_2
                      object = zcl_xlom_list_column=>create( ) )
             INTO TABLE items
             REFERENCE INTO list_column.
    ENDIF.
    zcl_xlom__ut_om_list_column=>set_index( list_column = list_column->object
                                            index       = index_2 ).
    result = list_column->object.
  ENDMETHOD.
ENDCLASS.
