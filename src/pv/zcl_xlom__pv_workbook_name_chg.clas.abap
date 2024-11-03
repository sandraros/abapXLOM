"! Workbook SaveAs -> Change workbook name in workbooks collection
CLASS zcl_xlom__pv_workbook_name_chg DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  global friends zcl_xlom_workbook .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    class-methods change_workbook_name
      importing workbook type ref to zcl_xlom_workbook.
ENDCLASS.



CLASS zcl_xlom__pv_workbook_name_chg IMPLEMENTATION.
  METHOD change_workbook_name.
    workbook->application->workbooks->change_workbook_name( workbook ).
  ENDMETHOD.
ENDCLASS.