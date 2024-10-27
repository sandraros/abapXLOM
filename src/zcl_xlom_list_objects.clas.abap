CLASS zcl_xlom_list_objects DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! In my tests, the parent was always ZCL_XLOM_WORKSHEET (ListObject from Range and from Model).
    DATA parent TYPE REF TO zcl_xlom_sheet READ-ONLY.

    "! Add (SourceType, Source, LinkSource, XlListObjectHasHeaders, Destination, TableStyleName)
    "! https://learn.microsoft.com/en-us/office/vba/api/excel.listobjects.add
    "!
    "! @parameter source_type | Possible values in ZCL_XLOM=>C_LIST_OBJECT_SOURCE_TYPE
    "! @parameter source | <ul>
    "!                     <li>When SourceType = xlSrcRange: A Range object representing the data source. If omitted, the Source will default to the range returned by list range detection code.</li>
    "!                     <li>When others: TODO. NOT SUPPORTED CURRENTLY.</li>
    "!                     </ul>
    "! @parameter link_source | TODO. NOT SUPPORTED CURRENTLY.
    "! @parameter has_headers | TODO. NOT SUPPORTED CURRENTLY.
    "! @parameter destination | TODO. NOT SUPPORTED CURRENTLY.
    "! <p>A Range object specifying a single-cell reference as the destination for the top-left corner of the new list object.
    "!    If the Range object refers to more than one cell, an error is generated.</p>
    "! <p>The Destination argument must be specified when SourceType is set to xlSrcExternal.
    "!    The Destination argument is ignored if SourceType is set to xlSrcRange.</p>
    "! <p>The destination range must be on the worksheet that contains the ListObjects collection specified by expression.
    "!    New columns will be inserted at the Destination to fit the new list. Therefore, existing data will not be overwritten.</p>
    "! @parameter table_style_name | TODO. NOT SUPPORTED CURRENTLY.
    "! @parameter result | The new list object (table, etc.)
    METHODS add
      IMPORTING source_type      TYPE zcl_xlom=>ty_list_object_source_type OPTIONAL
                !source          TYPE any                                  OPTIONAL
                link_source      TYPE abap_bool                            OPTIONAL
                has_headers      TYPE zcl_xlom=>ty_yes_no_guess            OPTIONAL
                !destination     TYPE REF TO zcl_xlom_range                OPTIONAL
                table_style_name TYPE string                               OPTIONAL
      RETURNING VALUE(result)    TYPE REF TO zcl_xlom_LIST_object.

    CLASS-METHODS create
      IMPORTING !parent       TYPE REF TO zcl_xlom_sheet
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_list_objects.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_list_object,
        name   TYPE zcl_xlom_list_object=>ty_name,
        object TYPE REF TO zcl_xlom_list_object,
      END OF ty_list_object.
    TYPES ty_list_objects TYPE SORTED TABLE OF ty_list_object WITH UNIQUE KEY name.

    DATA list_objects            TYPE ty_list_objects.
    DATA list_object_last_number TYPE i VALUE 0.
ENDCLASS.


CLASS zcl_xlom_list_objects IMPLEMENTATION.
  METHOD add.
    list_object_last_number = list_object_last_number + 1.
    DATA(list_object_name) = EXACT zcl_xlom_list_object=>ty_name( |Table{ list_object_last_number }| ).
    DATA(list_object) = zcl_xlom_list_object=>create( name             = list_object_name
                                                      parent           = parent
                                                      source_type      = source_type
                                                      source           = source
                                                      link_source      = link_source
                                                      has_headers      = has_headers
                                                      destination      = destination
                                                      table_style_name = table_style_name ).
    INSERT VALUE #( name   = list_object_name
                    object = list_object )
           INTO TABLE list_objects.
    result = list_object.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom_list_objects( ).
    result->parent = parent.
  ENDMETHOD.
ENDCLASS.
