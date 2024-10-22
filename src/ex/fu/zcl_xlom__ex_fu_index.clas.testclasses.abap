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
    range_b2->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_a1->set_formula2( value = `INDEX(A1:C3,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = `Hello` ).

    range_a1->set_value( zcl_xlom__va_string=>create( `Hello ` ) ).
    range_b2->set_value( zcl_xlom__va_string=>create( `world` ) ).

    range_c1->set_formula2( value = `INDEX(A1:B2,{2,1},{2,1})` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_c1->value( ) )->get_string( )
                                        exp = `world` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_d1->value( ) )->get_string( )
                                        exp = `Hello ` ).

    range_a1->set_formula2( value = `INDEX({"a","b"},1,2)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = `b` ).

    range_a1->set_formula2( value = `INDEX({"a","b";"c","d"},{2,1},{2,1})` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_a1->value( ) )->get_string( )
                                        exp = `d` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( range_b1->value( ) )->get_string( )
                                        exp = `a` ).
  ENDMETHOD.
ENDCLASS.
