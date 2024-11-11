*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS array_number FOR TESTING RAISING cx_static_check.
    METHODS array_string_of_not_a_number FOR TESTING RAISING cx_static_check.
    METHODS array_string_of_number FOR TESTING RAISING cx_static_check.
    METHODS nominal FOR TESTING RAISING cx_static_check.
    METHODS string_of_not_a_number FOR TESTING RAISING cx_static_check.
    METHODS string_of_number FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD array_number.
    value = application->evaluate( `MAX({2,3;5,4},1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 5 ).
  ENDMETHOD.

  METHOD array_string_of_not_a_number.
    value = application->evaluate( `MAX({"2"},1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD array_string_of_number.
    value = application->evaluate( `MAX({"2"},1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD nominal.
    value = application->evaluate( `MAX(1,5,3)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 5 ).
  ENDMETHOD.

  METHOD string_of_not_a_number.
    value = application->evaluate( `MAX("T",1)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>value_cannot_be_calculated ).
  ENDMETHOD.

  METHOD string_of_number.
    value = application->evaluate( `MAX("2",1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
