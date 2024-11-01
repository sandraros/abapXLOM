*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.

  PRIVATE SECTION.
    METHODS array_evaluation           FOR TESTING RAISING cx_static_check.
    METHODS complex_1                  FOR TESTING RAISING cx_static_check.
    METHODS complex_2                  FOR TESTING RAISING cx_static_check.
    METHODS function_optional_argument FOR TESTING RAISING cx_static_check.
    METHODS operand_is_an_error        FOR TESTING RAISING cx_static_check.
    METHODS range_a1_plus_one          FOR TESTING RAISING cx_static_check.
    METHODS range_two_sheets           FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD array_evaluation.
    value = application->evaluate( `ADDRESS({1,2},{1,2;3,4},{1,4},TRUE,{"s","t"})` ).
    " same as:  ADDRESS(1,1,1,TRUE,"s")  ADDRESS(2,2,4,TRUE,"t")
    "           ADDRESS(1,3,1,TRUE,"s")  ADDRESS(2,4,4,TRUE,"t")
    " expects:  s!$A$1  t!B2
    "           s!$C$1  t!D2
    DATA(value_array) = CAST zif_xlom__va_array( value ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value_array->get_cell_value(
                                                                            column = 1
                                                                            row    = 1 ) )->get_string( )
                                        exp = `s!$A$1` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value_array->get_cell_value(
                                                                            column = 2
                                                                            row    = 1 ) )->get_string( )
                                        exp = `t!B2` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value_array->get_cell_value(
                                                                            column = 1
                                                                            row    = 2 ) )->get_string( )
                                        exp = `s!$C$1` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value_array->get_cell_value(
                                                                            column = 2
                                                                            row    = 2 ) )->get_string( )
                                        exp = `t!D2` ).
  ENDMETHOD.

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

  METHOD function_optional_argument.
    range_a1->set_formula2( value = `RIGHT("hello",0)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = '' ).
    range_a1->set_formula2( value = `RIGHT("hello",)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = '' ).
  ENDMETHOD.

  METHOD operand_is_an_error.
    DATA(value) = application->evaluate( `OFFSET(INDIRECT("aa"&#VALUE!),#N/A,1)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>value_cannot_be_calculated ).
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
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
