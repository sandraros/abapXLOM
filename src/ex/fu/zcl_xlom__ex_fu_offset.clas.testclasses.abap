*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS a1_1_1 FOR TESTING RAISING cx_static_check.
    METHODS a1_2_0 FOR TESTING RAISING cx_static_check.
    METHODS a1_2_2 FOR TESTING RAISING cx_static_check.
    METHODS empty_height_and_width FOR TESTING RAISING cx_static_check.
    METHODS negative FOR TESTING RAISING cx_static_check.
    METHODS ref FOR TESTING RAISING cx_static_check.
    METHODS resize FOR TESTING RAISING cx_static_check.
    METHODS whole_row FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD a1_1_1.
    value = application->evaluate( `OFFSET(A1,1,1)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$B$2' ).
  ENDMETHOD.

  METHOD a1_2_0.
    value = application->evaluate( `OFFSET(A1,2,0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$A$3' ).
  ENDMETHOD.

  METHOD a1_2_2.
    value = application->evaluate( `OFFSET(A1,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$C$3' ).
  ENDMETHOD.

  METHOD empty_height_and_width.
    value = application->evaluate( `OFFSET(A1,1,1,,)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$B$2' ).
  ENDMETHOD.

  METHOD negative.
    value = application->evaluate( `OFFSET(C2,-1,-2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$A$1' ).
  ENDMETHOD.

  METHOD ref.
    " height=0 leads to #REF!
    value = application->evaluate( `OFFSET(A1,1,0,0)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>ref ).
  ENDMETHOD.

  METHOD resize.
    value = application->evaluate( `OFFSET(A1,1,1,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$B$2:$C$3' ).
  ENDMETHOD.

  METHOD whole_row.
    value = application->evaluate( `OFFSET(1:1,1,0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$2:$2' ).
  ENDMETHOD.
ENDCLASS.
