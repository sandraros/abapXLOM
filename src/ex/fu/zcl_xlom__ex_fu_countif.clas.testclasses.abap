*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS contains_e               FOR TESTING RAISING cx_static_check.
    METHODS empty_argument           FOR TESTING RAISING cx_static_check.
    METHODS equal_empty              FOR TESTING RAISING cx_static_check.
    METHODS equal_empty_whole_column FOR TESTING RAISING cx_static_check.
    METHODS not_equal_empty          FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD contains_e.
    range_a1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_a2->set_value( zcl_xlom__va_string=>create( `world` ) ).
    range_b1->set_value( zcl_xlom__va_string=>create( `peace` ) ).
    range_b2->set_value( zcl_xlom__va_string=>create( `love` ) ).
    range_c1->set_formula2( value = `COUNTIF(A1:B2,"*e*")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_c1->value( ) )->get_integer( )
                                        exp = 3 ).
  ENDMETHOD.

  METHOD empty_argument.
    " always return 0
    application->evaluate( `COUNTIF(A1,)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_c1->value( ) )->get_integer( )
                                        exp = 0 ).
  ENDMETHOD.

  METHOD equal_empty.
    range_a1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_b1->set_value( zcl_xlom__va_string=>create( `peace` ) ).
    range_b2->set_value( zcl_xlom__va_string=>create( `love` ) ).
    range_c1->set_formula2( value = `COUNTIF(A1:B2,"")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_c1->value( ) )->get_integer( )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD equal_empty_whole_column.
    value = application->evaluate( `COUNTIF(A:A,"")` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( value )->get_integer( )
                                        exp = 1048576 ).
  ENDMETHOD.

  METHOD not_equal_empty.
    range_a1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_b1->set_value( zcl_xlom__va_string=>create( `peace` ) ).
    range_b2->set_value( zcl_xlom__va_string=>create( `love` ) ).
    range_c1->set_formula2( value = `COUNTIF(A1:B2,"<>")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_c1->value( ) )->get_integer( )
                                        exp = 3 ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
