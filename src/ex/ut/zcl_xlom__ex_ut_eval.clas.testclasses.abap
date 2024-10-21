*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.

  PRIVATE SECTION.
    METHODS array                      FOR TESTING RAISING cx_static_check.

    METHODS complex_1                  FOR TESTING RAISING cx_static_check.
    METHODS complex_2                  FOR TESTING RAISING cx_static_check.

    METHODS error                      FOR TESTING RAISING cx_static_check.

    METHODS function_optional_argument FOR TESTING RAISING cx_static_check.









    METHODS number                     FOR TESTING RAISING cx_static_check.

    METHODS range_a1_plus_one          FOR TESTING RAISING cx_static_check.
    METHODS range_two_sheets           FOR TESTING RAISING cx_static_check.
    METHODS string                     FOR TESTING RAISING cx_static_check.

    DATA application TYPE REF TO zcl_xlom_application.
    DATA workbook    TYPE REF TO zcl_xlom_workbook.
    DATA worksheet   TYPE REF TO zcl_xlom_worksheet.
    DATA range_a1    TYPE REF TO zcl_xlom_range.
    DATA range_a2    TYPE REF TO zcl_xlom_range.
    DATA range_b1    TYPE REF TO zcl_xlom_range.
    DATA range_b2    TYPE REF TO zcl_xlom_range.
    DATA range_c1    TYPE REF TO zcl_xlom_range.
    DATA range_d1    TYPE REF TO zcl_xlom_range.

*    METHODS assert_equals
*      IMPORTING act           TYPE REF TO zif_xlom__va
*                exp           TYPE REF TO zif_xlom__va
*      RETURNING VALUE(result) TYPE abap_bool.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD array.
    range_a1->set_formula2( value = `{1,2}` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_a1 )->get_number( )
                                        exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_b1 )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

*  METHOD assert_equals.
*    cl_abap_unit_assert=>assert_true( xsdbool( exp->is_equal( act ) ) ).
*  ENDMETHOD.

  METHOD complex_1.
    range_a2->set_formula2(
        value = `"'"&RIGHT(CELL("filename",A1),LEN(CELL("filename",A1))-FIND("]",CELL("filename",A1)))&" (2)'!$1:$1"` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a2->value( ) )->get_string( )
                                        exp = `'Sheet1 (2)'!$1:$1` ).
  ENDMETHOD.

  METHOD complex_2.
    DATA dummy_ref_to_iferror   TYPE REF TO zcl_xlom__ex_fu_iferror ##NEEDED.
    DATA dummy_ref_to_t         TYPE REF TO zcl_xlom__ex_fu_t ##NEEDED.
    DATA dummy_ref_to_ampersand TYPE REF TO zcl_xlom__ex_op_ampersand ##NEEDED.
    DATA dummy_ref_to_index     TYPE REF TO zcl_xlom__ex_fu_index ##NEEDED.
    DATA dummy_ref_to_offset    TYPE REF TO zcl_xlom__ex_fu_offset ##NEEDED.
    DATA dummy_ref_to_indirect  TYPE REF TO zcl_xlom__ex_fu_indirect ##NEEDED.
    DATA dummy_ref_to_minus     TYPE REF TO zcl_xlom__ex_op_minus ##NEEDED.
    DATA dummy_ref_to_row       TYPE REF TO zcl_xlom__ex_fu_row ##NEEDED.
    DATA dummy_ref_to_match     TYPE REF TO zcl_xlom__ex_fu_match ##NEEDED.

    DATA(worksheet_bkpf) = workbook->worksheets->add( 'BKPF' ).
    worksheet_bkpf->range( cell1_string = 'A1' )->set_value( zcl_xlom__va_string=>create( 'ID_REF_TEST' ) ).
    worksheet_bkpf->range( cell1_string = 'B2' )->set_value( zcl_xlom__va_string=>create( `'BKPF (2)'!$1:$1` ) ).

    DATA(worksheet_bkpf_2) = workbook->worksheets->add( 'BKPF (2)' ).
    worksheet_bkpf_2->range( cell1_string = 'A1' )->set_value( zcl_xlom__va_string=>create( 'ID_REF_TEST' ) ).
    worksheet_bkpf_2->range( cell1_string = 'A3' )->set_value( zcl_xlom__va_string=>create( 'MY_TEST' ) ).

    DATA(range_bkpf_a3) = worksheet_bkpf->range( cell1_string = 'A3' ).
    range_bkpf_a3->set_formula2(
        value = `IFERROR(T(""&INDEX(OFFSET(INDIRECT(BKPF!$B$2),ROW()-1,0),1,MATCH(A$1,INDIRECT(BKPF!$B$2),0))),"")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_bkpf_a3->value( ) )->get_string( )
                                        exp = `MY_TEST` ).
  ENDMETHOD.

  METHOD error.
    range_a1->set_formula2( value = `#N/A` ).
    cl_abap_unit_assert=>assert_equals( act = range_a1->value( )->type
                                        exp = zif_xlom__va=>c_type-error ).
  ENDMETHOD.

  METHOD function_optional_argument.
    range_a1->set_formula2( value = `RIGHT("hello",0)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = '' ).
    range_a1->set_formula2( value = `RIGHT("hello",)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = '' ).
  ENDMETHOD.

  METHOD number.
    range_a1->set_formula2( value = `1` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_a1->value( ) )->get_number( )
                                        exp = 1 ).
*    assert_equals( act = range_a1->value( )
*                   exp = zcl_xlom__va_number=>get( 1 ) ).

    range_a1->set_formula2( value = `-1` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_a1->value( ) )->get_number( )
                                        exp = -1 ).
*    assert_equals( act = range_a1->value( )
*                   exp = zcl_xlom__va_number=>get( -1 ) ).
  ENDMETHOD.

  METHOD range_a1_plus_one.
    range_a1->set_value( zcl_xlom__va_number=>create( 10 ) ).
    DATA(range_a2) = worksheet->range( cell1_string = 'A2' ).
    range_a2->set_formula2( 'A1+1' ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_a2->value( ) )->get_number( )
                                        exp = 11 ).
  ENDMETHOD.

  METHOD range_two_sheets.
    DATA dummy_ref_to_offset TYPE REF TO zcl_xlom__ex_fu_offset ##NEEDED.

    range_a1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).

    DATA(worksheet_2) = workbook->worksheets->add( 'Sheet2' ).
    DATA(range_sheet2_b2) = worksheet_2->range( cell1_string = 'B2' ).
    range_sheet2_b2->set_formula2( |"C"&Sheet1!A1| ).

    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_sheet2_b2->value( ) )->get_string( )
                                        exp = `CHello` ).
  ENDMETHOD.

  METHOD setup.
    application = zcl_xlom_application=>create( ).
    workbook = application->workbooks->add( ).
    TRY.
        worksheet = workbook->worksheets->item( 'Sheet1' ).
        range_a1 = worksheet->range( cell1_string = 'A1' ).
        range_a2 = worksheet->range( cell1_string = 'A2' ).
        range_b1 = worksheet->range( cell1_string = 'B1' ).
        range_b2 = worksheet->range( cell1_string = 'B2' ).
        range_c1 = worksheet->range( cell1_string = 'C1' ).
        range_d1 = worksheet->range( cell1_string = 'D1' ).
      CATCH zcx_xlom__va INTO DATA(error). " TODO: variable is assigned but never used (ABAP cleaner)
        cl_abap_unit_assert=>fail( 'unexpected' ).
    ENDTRY.
  ENDMETHOD.

  METHOD string.
    range_a1->set_formula2( value = `"1"` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( range_a1->value( ) )->get_string( )
                                        exp = '1' ).
  ENDMETHOD.
ENDCLASS.
