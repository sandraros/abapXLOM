*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS adjust_evaluated_operands FOR TESTING RAISING cx_static_check.
    METHODS nominal                   FOR TESTING RAISING cx_static_check.
    METHODS nominal_not_found         FOR TESTING RAISING cx_static_check.
    METHODS null                      FOR TESTING RAISING cx_static_check.
    METHODS ref                       FOR TESTING RAISING cx_static_check.
    METHODS whole_column              FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD adjust_evaluated_operands.
    value = application->evaluate( `VLOOKUP({"a","b"},{"b","t";"a","u"},{2,1},FALSE)` ).
    " Only the first value 2 of {2,1} is considered because {"a","b"} is an array (one or the other, but not both)
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

  METHOD nominal_not_found.
    value = application->evaluate( `VLOOKUP("d",{"a","t";"b","u";"c","v"},2,FALSE)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>na_not_applicable ).
  ENDMETHOD.

  METHOD null.
    value = application->evaluate( `VLOOKUP("a",A1 B1,1,FALSE)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>null ).
  ENDMETHOD.

  METHOD ref.
    value = application->evaluate( `VLOOKUP("a",{"a"},2,FALSE)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>ref ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD whole_column.
    range_a2->set_value( zcl_xlom__va_number=>create( 1 ) ).
    range_b2->set_value( zcl_xlom__va_number=>create( 2 ) ).
    value = application->evaluate( `VLOOKUP(1,A:B,2,FALSE)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_number=>create( 2 ) ).
  ENDMETHOD.
ENDCLASS.
