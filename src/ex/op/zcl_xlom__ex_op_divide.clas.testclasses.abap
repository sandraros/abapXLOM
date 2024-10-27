*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_app IMPLEMENTATION.
  METHOD test.
    setup_default_xlom_objects( ).
    range_a1->set_formula2( value = `2*3*4` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_a1->value( ) )->get_number( )
                                        exp = 24 ).
  ENDMETHOD.
ENDCLASS.
