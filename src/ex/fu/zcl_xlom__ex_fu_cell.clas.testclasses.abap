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

* TODO not very clear what CELL should do without the Reference argument...
*    range_a1->set_formula2( value = `CELL("filename")` ).
*    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va_converter=>to_string( range_a1->value( ) )->get_string( )
*                                        exp = `\[]Sheet1` ).
    range_a2->set_value( zcl_xlom__va_string=>create( '' ) ).
    range_a1->set_formula2( value = `CELL("filename",A2)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = `\[]Sheet1` ).
  ENDMETHOD.
ENDCLASS.
