"! https://learn.microsoft.com/en-us/office/vba/api/excel.workbook
CLASS zcl_xlom_workbook DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS "zcl_xlom_workbooks
                 zcl_xlom__pv_worksheet_activtd.

  PUBLIC SECTION.
    TYPES ty_name TYPE string.

    DATA active_sheet TYPE REF TO zcl_xlom_sheet       READ-ONLY.

    DATA application  TYPE REF TO zcl_xlom_application READ-ONLY.

    "! workbook name
    DATA name         TYPE ty_name                     READ-ONLY.

    "! workbook path
    DATA path         TYPE string                      READ-ONLY.

    DATA worksheets   TYPE REF TO zcl_xlom_worksheets  READ-ONLY.

    CLASS-METHODS create
      IMPORTING !application  TYPE REF TO zcl_xlom_application
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_workbook.

    "! SaveAs (FileName, FileFormat, Password, WriteResPassword,
    "!         ReadOnlyRecommended, CreateBackup, AccessMode,
    "!         ConflictResolution, AddToMru, TextCodepage, TextVisualLayout, Local)
    "! https://learn.microsoft.com/en-us/office/vba/api/excel.workbook.saveas
    "!
    "! @parameter file_name | A string that indicates the name of the file to be saved. You can include
    "!                        a full path; if you don't, Microsoft Excel saves the file in the current folder.
    METHODS save_as
      IMPORTING file_name TYPE csequence.

*  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS activate_worksheet
      IMPORTING worksheet TYPE REF TO zcl_xlom_worksheet.
*    EVENTS saved.
ENDCLASS.


CLASS zcl_xlom_workbook IMPLEMENTATION.
  METHOD activate_worksheet.
    active_sheet = worksheet.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom_workbook( ).
    result->application = application.
    result->worksheets  = zcl_xlom_worksheets=>create( workbook = result ).
    result->active_sheet = result->worksheets->add( name = 'Sheet1' ).
  ENDMETHOD.

  METHOD save_as.
    path = file_name.
    zcl_xlom__pv_workbook_name_chg=>change_workbook_name( me ).
  ENDMETHOD.
ENDCLASS.
