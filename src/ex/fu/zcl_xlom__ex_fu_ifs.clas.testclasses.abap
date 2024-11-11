*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS all_false FOR TESTING RAISING cx_static_check.
    METHODS nominal FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD all_false.
    value = application->evaluate( `IFS(0=1,"a")` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>na_not_applicable ).
  ENDMETHOD.

  METHOD nominal.
    value = application->evaluate( `IFS(0=1,"a",0=1,"b",1=1,"c",TRUE,"d")` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = 'c' ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
