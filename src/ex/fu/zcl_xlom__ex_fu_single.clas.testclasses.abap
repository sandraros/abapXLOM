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
    range_a1->set_formula2( value = `ROW(B2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_a1->value( ) )->get_number( )
                                        exp = 2 ).

    range_a1->set_formula2( value = `ROW()` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_a1->value( ) )->get_number( )
                                        exp = 1 ).
  ENDMETHOD.
ENDCLASS.
