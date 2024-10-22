*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS with_reference FOR TESTING RAISING cx_static_check.
    METHODS without_reference FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD with_reference.
    range_a1->set_formula2( value = `COLUMN(B1)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_a1->value( ) )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

  METHOD without_reference.
    range_b1->set_formula2( value = `COLUMN()` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_b1->value( ) )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.
ENDCLASS.
