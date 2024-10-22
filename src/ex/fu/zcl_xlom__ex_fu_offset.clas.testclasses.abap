*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD test.
    value = application->evaluate( `OFFSET(A1,1,1)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$B$2' ).

    value = application->evaluate( `OFFSET(A1,2,0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$A$3' ).

    value = application->evaluate( `OFFSET(A1,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$C$3' ).

    value = application->evaluate( `OFFSET(C2,-1,-2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$A$1' ).

    value = application->evaluate( `OFFSET(A1,1,1,,)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$B$2' ).

    value = application->evaluate( `OFFSET(A1,1,1,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$B$2:$C$3' ).

    value = application->evaluate( `OFFSET(1:1,1,0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$2:$2' ).
  ENDMETHOD.
ENDCLASS.
