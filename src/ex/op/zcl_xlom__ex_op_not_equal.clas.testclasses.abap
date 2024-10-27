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
    DATA(value) = application->evaluate( `1<>2` ).
    cl_abap_unit_assert=>assert_true( CAST zcl_xlom__va_boolean( value )->boolean_value ).
  ENDMETHOD.
ENDCLASS.
