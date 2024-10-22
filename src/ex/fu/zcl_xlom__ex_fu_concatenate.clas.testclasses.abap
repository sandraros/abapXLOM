*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS arrays            FOR TESTING RAISING cx_static_check.
    METHODS nominal           FOR TESTING RAISING cx_static_check.
    METHODS only_one_argument FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD arrays.
    value = application->evaluate( `CONCATENATE({1,1},{1,1})` ).
    DATA(value_array) = CAST zif_xlom__va_array( value ).
    cl_abap_unit_assert=>assert_equals( act = value_array->column_count
                                        exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = value_array->row_count
                                        exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( value_array->get_cell_value( row    = 1
                                                                     column = 1 ) )->get_string( )
        exp = '11' ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( value_array->get_cell_value( row    = 1
                                                                     column = 2 ) )->get_string( )
        exp = '11' ).
  ENDMETHOD.

  METHOD nominal.
    value = application->evaluate( `CONCATENATE(1,2,3,4,5)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( value )->get_string( )
                                        exp = '12345' ).
  ENDMETHOD.

  METHOD only_one_argument.
    value = application->evaluate( `CONCATENATE(1)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_string( value )->get_string( )
                                        exp = '1' ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
