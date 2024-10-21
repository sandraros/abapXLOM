*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD test.
    DATA result TYPE REF TO zif_xlom__va.

    setup_default_xlom_objects( ).

    result = application->evaluate( `C:C` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$C:$C' ).

    result = application->evaluate( `3:3` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom_range( result )->address( )
                                        exp = '$3:$3' ).
  ENDMETHOD.
ENDCLASS.
