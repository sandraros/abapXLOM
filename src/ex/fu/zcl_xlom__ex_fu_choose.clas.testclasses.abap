*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS nominal FOR TESTING RAISING cx_static_check.
    METHODS value_cannot_be_calculated FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD nominal.
    range_a1->set_formula2( value = `CHOOSE(2,"a","b")` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( range_a1->value( ) )->get_string( )
                                        exp = 'b' ).
  ENDMETHOD.

  METHOD value_cannot_be_calculated.
    range_a1->set_formula2( value = `CHOOSE(3,1,2)` ).
    cl_abap_unit_assert=>assert_equals( act = range_a1->value( )
                                        exp = zcl_xlom__va_error=>value_cannot_be_calculated ).
  ENDMETHOD.
ENDCLASS.
