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
    range_b1->set_formula2( value = `MATCH("a",{"b","a"},0)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_b1->value( ) )->get_number( )
                                        exp = 2 ).

    range_a1->set_value( zcl_xlom__va_string=>create( `Hello ` ) ).
    range_a2->set_value( zcl_xlom__va_string=>create( `world` ) ).
    range_b1->set_formula2( value = `MATCH("world",A1:A2,0)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_b1->value( ) )->get_number( )
                                        exp = 2 ).

    range_a1->set_value( zcl_xlom__va_string=>create( `Hello ` ) ).
    range_a2->set_value( zcl_xlom__va_string=>create( `world` ) ).
    range_b1->set_formula2( value = `MATCH("world",A:A,0)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_b1->value( ) )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.
ENDCLASS.
