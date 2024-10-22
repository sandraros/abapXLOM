*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS value_if_error FOR TESTING RAISING cx_static_check.
    METHODS value_if_not_error FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD value_if_error.
    value = application->evaluate( `IFERROR(#N/A,1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 1 ).

    " IFERROR(#N/A,1)
    value = zcl_xlom__ex_fu_iferror=>create( value          = zcl_xlom__ex_el_error=>value_cannot_be_calculated
                                             value_if_error = zcl_xlom__ex_el_number=>create( 1 )
                                           )->evaluate( ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD value_if_not_error.
    value = application->evaluate( `IFERROR(2,1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.
ENDCLASS.
