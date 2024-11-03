"! https://learn.microsoft.com/en-us/office/vba/api/excel.workbooks
CLASS zcl_xlom_workbooks DEFINITION
  PUBLIC
  CREATE PRIVATE
*  GLOBAL FRIENDS zif_xlom__ut_all_friends
  GLOBAL FRIENDS zcl_xlom__pv_workbook_name_chg
  .

  PUBLIC SECTION.
*    INTERFACES zif_xlom__ut_all_friends.

    DATA application TYPE REF TO zcl_xlom_application READ-ONLY.
    DATA count       TYPE i                           READ-ONLY.

    CLASS-METHODS create
      IMPORTING !application  TYPE REF TO zcl_xlom_application
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_workbooks.

    "! Add (Template)
    "! https://learn.microsoft.com/en-us/office/vba/api/excel.workbooks.add
    METHODS add
      IMPORTING template      TYPE any OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_workbook.

    "! @parameter index  | Required    Variant The name or index number of the object.
    METHODS item
      IMPORTING !index        TYPE simple
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_workbook
      RAISING
        zcx_xlom__va.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_workbook,
        name   TYPE zcl_xlom_workbook=>ty_name,
        object TYPE REF TO zcl_xlom_workbook,
      END OF ty_workbook.
    TYPES ty_workbooks TYPE SORTED TABLE OF ty_workbook WITH NON-UNIQUE KEY name
                              WITH UNIQUE SORTED KEY by_object COMPONENTS object.

    DATA items TYPE ty_workbooks.

    METHODS change_workbook_name
      IMPORTING workbook TYPE REF TO zcl_xlom_workbook.

*    METHODS on_saved
*      FOR EVENT saved OF zcl_xlom_workbook
*      IMPORTING sender.
ENDCLASS.


CLASS zcl_xlom_workbooks IMPLEMENTATION.
  METHOD add.
    " TODO: parameter TEMPLATE is never used (ABAP cleaner)

    DATA workbook TYPE ty_workbook.

    workbook-object = zcl_xlom_workbook=>create( application ).
    INSERT workbook INTO TABLE items.
    count = count + 1.

*    SET HANDLER on_saved FOR workbook-object.
    result = workbook-object.
  ENDMETHOD.

  METHOD change_workbook_name.
    items[ KEY by_object COMPONENTS object = workbook ]-name = workbook->name.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom_workbooks( ).
    result->application = application.
  ENDMETHOD.

  METHOD item.
    TRY.
        CASE zcl_xlom__ut=>type( index ).
          WHEN cl_abap_typedescr=>typekind_string
              OR cl_abap_typedescr=>typekind_char.
            result = items[ name = index ]-object.
          WHEN cl_abap_typedescr=>typekind_int.
            result = items[ index ]-object.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDCASE.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_xlom__va
          EXPORTING result_error = zcl_xlom__va_error=>ref.
    ENDTRY.
*  ENDMETHOD.
*
*  METHOD on_saved.
*    items[ KEY by_object COMPONENTS object = sender ]-name = sender->name.
  ENDMETHOD.
ENDCLASS.
