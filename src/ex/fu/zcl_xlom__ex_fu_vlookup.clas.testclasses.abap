*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS adjust_evaluated_operands FOR TESTING RAISING cx_static_check.
    METHODS nominal FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD adjust_evaluated_operands.
    value = application->evaluate( `VLOOKUP({"a","b"},{"b","t";"a","u"},{2,1},FALSE)` ).
    " Only the first value of {2,1} is considered because {"a","b"} is an array (one or the other, but not both)
    DATA(value_array) = CAST zif_xlom__va_array( value ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value_array->get_cell_value(
                                                                            column = 1
                                                                            row    = 1 ) )->get_string( )
                                        exp = `u` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value_array->get_cell_value(
                                                                            column = 2
                                                                            row    = 1 ) )->get_string( )
                                        exp = `t` ).
  ENDMETHOD.

  METHOD nominal.
    value = application->evaluate( `VLOOKUP("b",{"a","t";"b","u";"c","v"},2,FALSE)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
                                        exp = 'u' ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
