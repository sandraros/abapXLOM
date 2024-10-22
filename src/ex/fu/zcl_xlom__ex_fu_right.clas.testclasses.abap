*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_1 FOR TESTING RAISING cx_static_check.
    METHODS test_2 FOR TESTING RAISING cx_static_check.
    METHODS test_3 FOR TESTING RAISING cx_static_check.
    METHODS test_4 FOR TESTING RAISING cx_static_check.
    METHODS test_5 FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.

CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD test_1.
    value = application->evaluate( `RIGHT("Hello",2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = 'lo' ).
  ENDMETHOD.

  METHOD test_2.
    value = application->evaluate( `RIGHT(25,1)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = '5' ).
  ENDMETHOD.

  METHOD test_3.
    value = application->evaluate( `RIGHT("hello")` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = 'o' ).
  ENDMETHOD.

  METHOD test_4.
    value = application->evaluate( `RIGHT("hello",0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = '' ).
  ENDMETHOD.

  METHOD test_5.
    " RIGHT("hello",) is the same result as RIGHT("hello",0), it differs from RIGHT("hello"), which is the same as RIGHT("hello",1).
    value = application->evaluate( `RIGHT("hello",)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = '' ).
  ENDMETHOD.
ENDCLASS.
