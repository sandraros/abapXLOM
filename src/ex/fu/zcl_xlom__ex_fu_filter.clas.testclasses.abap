*"* use this source file for your ABAP unit test classes

"!  &#x2000;
"!     │ A │ B <br/>
"!    ─│ ─ │ ─ <br/>
"!   1 │ a │ b <br/>
"!    ─│ ─ │ ─ <br/>
"!   2 │ m │ n
"! <ul>
"! <li>Vertical search: FILTER(A1:B2,A1:A2="a") → \{"a","b"\} (1 row, two columns</li>
"! <li>Horizontal search: FILTER(A1:B2,A1:B1="a") → {"a";"m"} (two rows, 1 column)</li>
"! <li>No match without <strong>if_empty</strong>: FILTER(A1:B2,A1:B1="c") → #CALC!</li>
"! <li>No match with <strong>if_empty</strong>: FILTER(A1:B2,A1:B1="c","") → ""</li>
"! <li>Include size is neither one row nor one column of <strong>array</strong>: FILTER(A1:B2,A1="a") → #VALUE!</li>
"! <li>Idem: FILTER(A1:B3,A1:A2="a") → #VALUE!</li>
"! </ul>
CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS array_not_an_array FOR TESTING RAISING cx_static_check.
    METHODS horizontal_nominal FOR TESTING RAISING cx_static_check.
    METHODS vertical_nominal   FOR TESTING RAISING cx_static_check.
    METHODS vertical_optimize FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD array_not_an_array.
    value = application->evaluate( `FILTER("a",TRUE)` ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 1
                                                                                          column = 1 ) )->get_string( )
        exp = 'a' ).
  ENDMETHOD.

  METHOD horizontal_nominal.
    value = application->evaluate( `FILTER({"a","b";"m","n"},{TRUE,FALSE})` ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 1
                                                                                          column = 1 ) )->get_string( )
        exp = 'a' ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 2
                                                                                          column = 1 ) )->get_string( )
        exp = 'm' ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD vertical_nominal.
    value = application->evaluate( `FILTER({"a","b";"m","n"},{TRUE;FALSE})` ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 1
                                                                                          column = 1 ) )->get_string( )
        exp = 'a' ).
    cl_abap_unit_assert=>assert_equals(
        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 1
                                                                                          column = 2 ) )->get_string( )
        exp = 'b' ).
  ENDMETHOD.

  METHOD vertical_optimize.
    range_c1->set_value( zcl_xlom__va_string=>create( 'a' ) ).
    range_d1->set_value( zcl_xlom__va_string=>create( 'b' ) ).
    value = application->evaluate( `FILTER(C:D,C:C="a")` ).
    DATA(array) = CAST zif_xlom__va_array( value ).
    cl_abap_unit_assert=>assert_equals(
        act = array->get_array_values( )
        exp = VALUE zif_xlom__va_array=>ts_array_values(
                  cells                 = VALUE #( ( row = 1 column = 1 value = zcl_xlom__va_string=>create( 'a' ) )
                                                   ( row = 1 column = 2 value = zcl_xlom__va_string=>create( 'b' ) ) )
                  values_of_other_cells = VALUE #( ) ) ).
*    cl_abap_unit_assert=>assert_equals(
*        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 1
*                                                                                          column = 1 ) )->get_string( )
*        exp = 'a' ).
*    cl_abap_unit_assert=>assert_equals(
*        act = CAST zcl_xlom__va_string( CAST zif_xlom__va_array( value )->get_cell_value( row    = 1
*                                                                                          column = 2 ) )->get_string( )
*        exp = 'b' ).
  ENDMETHOD.
ENDCLASS.
