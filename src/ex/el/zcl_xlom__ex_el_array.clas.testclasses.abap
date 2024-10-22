*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS array FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD array.
    range_a1->set_formula2( value = `{1,2}` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_a1 )->get_number( )
                                        exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_b1 )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
