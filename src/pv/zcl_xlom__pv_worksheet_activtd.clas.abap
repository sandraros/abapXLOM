"! Workbook SaveAs -> Change workbook name in workbooks collection
CLASS zcl_xlom__pv_worksheet_activtd DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom_worksheets.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS activate_worksheet
      IMPORTING worksheet TYPE REF TO zcl_xlom_worksheet.
ENDCLASS.



CLASS zcl_xlom__pv_worksheet_activtd IMPLEMENTATION.
  METHOD activate_worksheet.
    worksheet->application->activate_worksheet( worksheet ).
    worksheet->parent->activate_worksheet( worksheet ).
  ENDMETHOD.
ENDCLASS.
