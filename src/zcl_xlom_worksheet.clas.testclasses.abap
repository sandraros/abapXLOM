*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS cells FOR TESTING RAISING cx_static_check.

    DATA active_worksheet TYPE REF TO zcl_xlom_worksheet.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD cells.
    DATA(range_b3) = active_worksheet->cells( row    = 3
                                              column = 2 ).
    cl_abap_unit_assert=>assert_equals( act = range_b3->address( )
                                        exp = '$B$3' ).
  ENDMETHOD.

  METHOD setup.
    DATA(application) = zcl_xlom_application=>create( ).
    DATA(workbook) = application->workbooks->add( ).
    active_worksheet = CAST zcl_xlom_worksheet( workbook->active_sheet ).
  ENDMETHOD.
ENDCLASS.
