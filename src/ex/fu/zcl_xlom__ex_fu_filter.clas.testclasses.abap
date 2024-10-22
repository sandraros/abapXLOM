**"* use this source file for your ABAP unit test classes
*
*CLASS ltc_app DEFINITION
*  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
*  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
*
*  PRIVATE SECTION.
*    METHODS array_not_an_array FOR TESTING RAISING cx_static_check.
*    METHODS nominal            FOR TESTING RAISING cx_static_check.
*
*    METHODS setup.
*ENDCLASS.
*
*
*CLASS ltc_app IMPLEMENTATION.
*  METHOD array_not_an_array.
*    value = application->evaluate( `FILTER("a",TRUE)` ).
*    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
*                                        exp = 'a' ).
*  ENDMETHOD.
*
*  METHOD nominal.
*    value = application->evaluate( `FILTER({"a","A";"b","B"},{TRUE,TRUE})` ).
*    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_string( value )->get_string( )
*                                        exp = 'a' ).
*  ENDMETHOD.
*
*  METHOD setup.
*    setup_default_xlom_objects( ).
*  ENDMETHOD.
*ENDCLASS.
