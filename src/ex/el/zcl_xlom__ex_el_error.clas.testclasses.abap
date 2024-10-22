*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS error FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD error.
    range_a1->set_formula2( value = `#N/A` ).
    cl_abap_unit_assert=>assert_equals( act = range_a1->value( )->type
                                        exp = zif_xlom__va=>c_type-error ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
