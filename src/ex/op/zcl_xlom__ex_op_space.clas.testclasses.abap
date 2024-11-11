*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS null  FOR TESTING RAISING cx_static_check.
    METHODS test  FOR TESTING RAISING cx_static_check.
    METHODS test2 FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD null.
    value = application->evaluate( `A1 A2` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>null ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD test.
    value = application->evaluate( `C:C` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$C:$C' ).
  ENDMETHOD.

  METHOD test2.
    value = application->evaluate( `3:3` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( value )->address( )
                                        exp = '$3:$3' ).
  ENDMETHOD.
ENDCLASS.
