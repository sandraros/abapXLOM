*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD test.
    DATA(list_object) = worksheet->list_objects->add( source_type = zcl_xlom=>c_list_object_source_type-range
                                                      source      = zcl_xlom_range=>create_from_address_or_name(
                                                                        address     = 'A1:B2'
                                                                        relative_to = worksheet )
                                                      has_headers = zcl_xlom=>c_yes_no_guess-yes ).
    range_a2->set_value( zcl_xlom__va_number=>get( 5 ) ).
    range_b2->set_formula2( value = `Table1[[#This Row][Column1]]*2` ).
    cl_abap_unit_assert=>assert_equals( act = CAST zcl_xlom__va_number( range_b2->value( ) )->get_number( )
                                        exp = 10 ).
  ENDMETHOD.
ENDCLASS.
