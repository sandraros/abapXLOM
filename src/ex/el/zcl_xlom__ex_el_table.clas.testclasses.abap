*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS string FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD string.
    range_a1->set_formula2( value = `"1"` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( range_a1->value( ) )->get_string( )
                                        exp = '1' ).
  ENDMETHOD.
ENDCLASS.
