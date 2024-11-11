CLASS zcl_xlom__ex_ut_eval_aunit DEFINITION
  PUBLIC
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PROTECTED SECTION.
    DATA application TYPE REF TO zcl_xlom_application.
    DATA workbook    TYPE REF TO zcl_xlom_workbook.
    DATA worksheet   TYPE REF TO zcl_xlom_worksheet.
    DATA range_a1    TYPE REF TO zcl_xlom_range.
    DATA range_a2    TYPE REF TO zcl_xlom_range.
    DATA range_a3    TYPE REF TO zcl_xlom_range.
    DATA range_a4    TYPE REF TO zcl_xlom_range.
    DATA range_b1    TYPE REF TO zcl_xlom_range.
    DATA range_b2    TYPE REF TO zcl_xlom_range.
    DATA range_b3    TYPE REF TO zcl_xlom_range.
    DATA range_b4    TYPE REF TO zcl_xlom_range.
    DATA range_c1    TYPE REF TO zcl_xlom_range.
    DATA range_c2    TYPE REF TO zcl_xlom_range.
    DATA range_c3    TYPE REF TO zcl_xlom_range.
    DATA range_c4    TYPE REF TO zcl_xlom_range.
    DATA range_d1    TYPE REF TO zcl_xlom_range.
    DATA range_d2    TYPE REF TO zcl_xlom_range.
    DATA range_d3    TYPE REF TO zcl_xlom_range.
    DATA range_d4    TYPE REF TO zcl_xlom_range.
    DATA value       TYPE REF TO zif_xlom__va.

    METHODS setup_default_xlom_objects.
ENDCLASS.


CLASS zcl_xlom__ex_ut_eval_aunit IMPLEMENTATION.
  METHOD setup_default_xlom_objects.
    application = zcl_xlom_application=>create( ).
    workbook = application->workbooks->add( ).
    TRY.
        worksheet = workbook->worksheets->item( 'Sheet1' ).
        range_a1 = worksheet->range( cell1_string = 'A1' ).
        range_a2 = worksheet->range( cell1_string = 'A2' ).
        range_a3 = worksheet->range( cell1_string = 'A3' ).
        range_a4 = worksheet->range( cell1_string = 'A4' ).
        range_b1 = worksheet->range( cell1_string = 'B1' ).
        range_b2 = worksheet->range( cell1_string = 'B2' ).
        range_b3 = worksheet->range( cell1_string = 'B3' ).
        range_b4 = worksheet->range( cell1_string = 'B4' ).
        range_c1 = worksheet->range( cell1_string = 'C1' ).
        range_c2 = worksheet->range( cell1_string = 'C2' ).
        range_c3 = worksheet->range( cell1_string = 'C3' ).
        range_c4 = worksheet->range( cell1_string = 'C4' ).
        range_d1 = worksheet->range( cell1_string = 'D1' ).
        range_d2 = worksheet->range( cell1_string = 'D2' ).
        range_d3 = worksheet->range( cell1_string = 'D3' ).
        range_d4 = worksheet->range( cell1_string = 'D4' ).
      CATCH zcx_xlom__va.
        cl_abap_unit_assert=>fail( 'unexpected' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
