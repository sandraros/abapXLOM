*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS find_empty_in_empty         FOR TESTING RAISING cx_static_check.
    METHODS find_empty_in_non_empty     FOR TESTING RAISING cx_static_check.
    METHODS find_empty_with_start_num   FOR TESTING RAISING cx_static_check.
    METHODS find_empty_with_start_num_2 FOR TESTING RAISING cx_static_check.
    METHODS find_number_in_number       FOR TESTING RAISING cx_static_check.
    METHODS find_whole_column           FOR TESTING RAISING cx_static_check.
    METHODS nominal_found               FOR TESTING RAISING cx_static_check.
    METHODS nominal_not_found           FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD find_empty_in_empty.
    value = application->evaluate( `FIND("","")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD find_empty_in_non_empty.
    value = application->evaluate( `FIND("","abc")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 1 ).
  ENDMETHOD.

  METHOD find_empty_with_start_num.
    value = application->evaluate( `FIND("","a",2)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

  METHOD find_empty_with_start_num_2.
    value = application->evaluate( `FIND("","a",3)` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>value_cannot_be_calculated ).
  ENDMETHOD.

  METHOD find_number_in_number.
    value = application->evaluate( `FIND(1,51)` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

  METHOD find_whole_column.
    value = application->evaluate( `FIND(A:A,A:A)` ).
    DATA(array) = CAST zif_xlom__va_array( value ).
    cl_abap_unit_assert=>assert_equals(
        act = array->get_array_values( )
        exp = VALUE zif_xlom__va_array=>ts_array_values(
                  cells                 = VALUE #( ( row = 1 column = 1 value = zcl_xlom__va_number=>create( 1 ) ) )
                  values_of_other_cells = VALUE #( ( row = 1 column = 1 value = zcl_xlom__va_number=>create( 1 ) ) ) ) ).
  ENDMETHOD.

  METHOD nominal_found.
    value = application->evaluate( `FIND("b","abc")` ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__va=>to_number( value )->get_number( )
                                        exp = 2 ).
  ENDMETHOD.

  METHOD nominal_not_found.
    value = application->evaluate( `FIND("d","abc")` ).
    cl_abap_unit_assert=>assert_equals( act = value
                                        exp = zcl_xlom__va_error=>value_cannot_be_calculated ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.
ENDCLASS.
