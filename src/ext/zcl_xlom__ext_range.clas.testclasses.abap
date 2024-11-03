*"* use this source file for your ABAP unit test classes

CLASS ltc_range DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS convert_column_a_xfd_to_number FOR TESTING RAISING cx_static_check.
    METHODS convert_column_number_to_a_xfd FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_range IMPLEMENTATION.
  METHOD convert_column_a_xfd_to_number.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__ext_range=>convert_column_a_xfd_to_number( roman_letters = 'XFD' )
                                        exp = 16384 ).

    TRY.
        zcl_xlom__ext_range=>convert_column_a_xfd_to_number( roman_letters = 'XFE' ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected for XFE - Column does not exist' ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_xlom__ext_range=>convert_column_a_xfd_to_number( roman_letters = 'ZZZZ' ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected for ZZZZ - Column does not exist' ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_xlom__ext_range=>convert_column_a_xfd_to_number( roman_letters = '1' ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected for 1 - Invalid column ID' ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD convert_column_number_to_a_xfd.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom__ext_range=>convert_column_number_to_a_xfd( 16384 )
                                        exp = 'XFD' ).

    TRY.
        zcl_xlom__ext_range=>convert_column_number_to_a_xfd( 16385 ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected for 16385 - Column does not exist' ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_xlom__ext_range=>convert_column_number_to_a_xfd( -1 ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected for -1 - Column does not exist' ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
