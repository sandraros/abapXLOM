*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_app IMPLEMENTATION.
  METHOD test.
    setup_default_xlom_objects( ).

    DATA dummy_ref_to_offset TYPE REF TO zcl_xlom__ex_fu_offset ##NEEDED.
    DATA result              TYPE REF TO zif_xlom__va.

    result = application->evaluate( `OFFSET(A1,1,1)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$B$2' ).

    result = application->evaluate( `OFFSET(A1,2,0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$A$3' ).

    result = application->evaluate( `OFFSET(A1,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$C$3' ).

    result = application->evaluate( `OFFSET(C2,-1,-2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$A$1' ).

    result = application->evaluate( `OFFSET(A1,1,1,,)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$B$2' ).

    result = application->evaluate( `OFFSET(A1,1,1,2,2)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$B$2:$C$3' ).

    result = application->evaluate( `OFFSET(1:1,1,0)` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$2:$2' ).
  ENDMETHOD.
ENDCLASS.
