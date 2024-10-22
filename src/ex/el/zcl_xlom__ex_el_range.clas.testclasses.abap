*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS range FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD range.
    range_a1->set_value( zcl_xlom__va_number=>create( 10 ) ).
    range_a2->set_formula2( 'A1' ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( range_a2->value( ) )->get_number( )
                                        exp = 10 ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
