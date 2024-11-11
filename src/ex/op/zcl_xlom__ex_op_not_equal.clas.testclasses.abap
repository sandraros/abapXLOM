*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test         FOR TESTING RAISING cx_static_check.
    METHODS whole_column FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD test.
    value = application->evaluate( `1<>2` ).
    cl_abap_unit_assert=>assert_true( CAST zcl_xlom__va_boolean( value )->boolean_value ).
  ENDMETHOD.

  METHOD whole_column.
    range_a1->set_value( zcl_xlom__va_string=>create( 'Hello' ) ).
    value = application->evaluate( `A:A<>""` ).
    " The result is an array of 1 column and 1048576 rows equal to {TRUE;FALSE;FALSE;FALSE;FALSE;...}
    DATA(array) = CAST zif_xlom__va_array( value ).
    cl_abap_unit_assert=>assert_equals( act = array->column_count
                                        exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = array->row_count
                                        exp = zcl_xlom__ext_worksheet=>max_rows ).
    cl_abap_unit_assert=>assert_equals(
        act = array->get_array_values( )
        exp = VALUE zif_xlom__va_array=>ts_array_values(
                  cells                 = VALUE #( ( row = 1 column = 1 value = zcl_xlom__va_boolean=>true ) )
                  values_of_other_cells = VALUE #( ( row = 1 column = 1 value = zcl_xlom__va_boolean=>false ) ) ) ).
  ENDMETHOD.
ENDCLASS.
