*"* use this source file for your ABAP unit test classes

CLASS ltc_range                DEFINITION DEFERRED.
CLASS ltc_decode_valid_address DEFINITION DEFERRED.

CLASS zcl_xlom_range DEFINITION LOCAL FRIENDS ltc_range
                                              ltc_decode_valid_address.

CLASS ltc_range DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS create_from_top_left_bottom_ri FOR TESTING RAISING cx_static_check.
*    METHODS decode_range_address_a1_invali FOR TESTING RAISING cx_static_check.
    METHODS decode_range_address_a1_valid  FOR TESTING RAISING cx_static_check.
*    METHODS decode_range_address_sh_invali FOR TESTING RAISING cx_static_check.
    METHODS decode_range_address_sh_valid  FOR TESTING RAISING cx_static_check.

    TYPES ty_address TYPE zif_xlom__va_array=>ts_address.
ENDCLASS.


CLASS ltc_decode_valid_address DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS a1                 FOR TESTING RAISING cx_static_check.
    METHODS a_dollar_1         FOR TESTING RAISING cx_static_check.
    METHODS dollar_a1          FOR TESTING RAISING cx_static_check.
    METHODS dollar_a_dollar_1  FOR TESTING RAISING cx_static_check.
    METHODS a1_b1              FOR TESTING RAISING cx_static_check.
    METHODS a_a                FOR TESTING RAISING cx_static_check.
    METHODS row_1              FOR TESTING RAISING cx_static_check.
    METHODS sheet1_a1          FOR TESTING RAISING cx_static_check.
    METHODS sheet_quoted_a1    FOR TESTING RAISING cx_static_check.
    METHODS wrong_row_order    FOR TESTING RAISING cx_static_check.
    METHODS wrong_column_order FOR TESTING RAISING cx_static_check.
    METHODS wrong_order_a2_b1  FOR TESTING RAISING cx_static_check.
    METHODS wrong_order_b2_a1  FOR TESTING RAISING cx_static_check.

    TYPES ty_address TYPE zif_xlom__va_array=>ts_address.
ENDCLASS.


CLASS ltc_decode_valid_address IMPLEMENTATION.
  METHOD a1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column = 1
                                                                                        row    = 1 )
                                                                bottom_right = VALUE #( column = 1
                                                                                        row    = 1 ) ) ).
  ENDMETHOD.

  METHOD a_dollar_1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A$1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column    = 1
                                                                                        row       = 1
                                                                                        row_fixed = abap_true )
                                                                bottom_right = VALUE #( column    = 1
                                                                                        row       = 1
                                                                                        row_fixed = abap_true ) ) ).
  ENDMETHOD.

  METHOD dollar_a1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '$A1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1 )
                                                                bottom_right = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1 ) ) ).
  ENDMETHOD.

  METHOD dollar_a_dollar_1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '$A$1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1
                                                                                        row_fixed    = abap_true )
                                                                bottom_right = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1
                                                                                        row_fixed    = abap_true ) ) ).
  ENDMETHOD.

  METHOD a1_b1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A1:B1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column = 1
                                                                                        row    = 1 )
                                                                bottom_right = VALUE #( column = 2
                                                                                        row    = 1 ) ) ).
  ENDMETHOD.

  METHOD a_a.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A:A' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column = 1 )
                                                                bottom_right = VALUE #( column = 1 ) ) ).
  ENDMETHOD.

  METHOD row_1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '1:1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row = 1 )
                                                                bottom_right = VALUE #( row = 1 ) ) ).
  ENDMETHOD.

  METHOD sheet1_a1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'Sheet1!A1' )
                                        exp = VALUE ty_address( worksheet_name = 'Sheet1'
                                                                top_left       = VALUE #( column = 1
                                                                                          row    = 1 )
                                                                bottom_right   = VALUE #( column = 1
                                                                                          row    = 1 ) ) ).
  ENDMETHOD.

  METHOD sheet_quoted_a1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( `'Sheet1 (2)'!A1` )
                                        exp = VALUE ty_address( worksheet_name = 'Sheet1 (2)'
                                                                top_left       = VALUE #( column = 1
                                                                                          row    = 1 )
                                                                bottom_right   = VALUE #( column = 1
                                                                                          row    = 1 ) ) ).
  ENDMETHOD.

  METHOD wrong_row_order.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '4:3' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 3
                                                                                        column = 0 )
                                                                bottom_right = VALUE #( row    = 4
                                                                                        column = 0 ) ) ).
  ENDMETHOD.

  METHOD wrong_column_order.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'B:A' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 0
                                                                                        column = 1 )
                                                                bottom_right = VALUE #( row    = 0
                                                                                        column = 2 ) ) ).
  ENDMETHOD.

  METHOD wrong_order_a2_b1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A2:B1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 1
                                                                                        column = 1 )
                                                                bottom_right = VALUE #( row    = 2
                                                                                        column = 2 ) ) ).
  ENDMETHOD.

  METHOD wrong_order_b2_a1.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'B2:A1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 1
                                                                                        column = 1 )
                                                                bottom_right = VALUE #( row    = 2
                                                                                        column = 2 ) ) ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_range IMPLEMENTATION.
  METHOD create_from_top_left_bottom_ri.
*    zcl_xlom_range=>create_from_address_or_name(
*      EXPORTING
*        address     =                  " ADDRESS
*        relative_to =                  " RELATIVE_TO
**      RECEIVING
**        result      =                  " RESULT
*    ).
**    CATCH zcx_xlom__va.(
*      EXPORTING
*        worksheet             =                  " WORKSHEET
*        top_left              =                  " TOP_LEFT
*        bottom_right          =                  " BOTTOM_RIGHT
**        column_row_collection =                  " COLUMN_ROW_COLLECTION
**      RECEIVING
**        result                =                  " RESULT
*    ).( -1 ).
  ENDMETHOD.

*  METHOD decode_range_address_a1_invali.
*    LOOP AT VALUE string_table( ( `:` ) ( `` ) ( `$` ) ( `A` ) ( `A:` ) ( `$$A1` ) ( `A:A1` ) ( `B2:A1` ) ) INTO DATA(address).
*      TRY.
*          zcl_xlom_range=>decode_range_address_a1( address ).
*          cl_abap_unit_assert=>fail( msg = |Exception expected for address "{ address }"| ).
*        CATCH cx_root ##NO_HANDLER.
*      ENDTRY.
*    ENDLOOP.
*  ENDMETHOD.

  METHOD decode_range_address_a1_valid.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column = 1
                                                                                        row    = 1 )
                                                                bottom_right = VALUE #( column = 1
                                                                                        row    = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A$1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column    = 1
                                                                                        row       = 1
                                                                                        row_fixed = abap_true )
                                                                bottom_right = VALUE #( column    = 1
                                                                                        row       = 1
                                                                                        row_fixed = abap_true ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '$A1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1 )
                                                                bottom_right = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '$A$1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1
                                                                                        row_fixed    = abap_true )
                                                                bottom_right = VALUE #( column       = 1
                                                                                        column_fixed = abap_true
                                                                                        row          = 1
                                                                                        row_fixed    = abap_true ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A1:B1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column = 1
                                                                                        row    = 1 )
                                                                bottom_right = VALUE #( column = 2
                                                                                        row    = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A:A' )
                                        exp = VALUE ty_address( top_left     = VALUE #( column = 1 )
                                                                bottom_right = VALUE #( column = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '1:1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row = 1 )
                                                                bottom_right = VALUE #( row = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'Sheet1!A1' )
                                        exp = VALUE ty_address( worksheet_name = 'Sheet1'
                                                                top_left       = VALUE #( column = 1
                                                                                          row    = 1 )
                                                                bottom_right   = VALUE #( column = 1
                                                                                          row    = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( `'Sheet1 (2)'!A1` )
                                        exp = VALUE ty_address( worksheet_name = 'Sheet1 (2)'
                                                                top_left       = VALUE #( column = 1
                                                                                          row    = 1 )
                                                                bottom_right   = VALUE #( column = 1
                                                                                          row    = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( '4:3' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 3
                                                                                        column = 0 )
                                                                bottom_right = VALUE #( row    = 4
                                                                                        column = 0 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'B:A' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 0
                                                                                        column = 1 )
                                                                bottom_right = VALUE #( row    = 0
                                                                                        column = 2 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A2:B1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 1
                                                                                        column = 1 )
                                                                bottom_right = VALUE #( row    = 2
                                                                                        column = 2 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'A2:B1' )
                                        exp = VALUE ty_address( top_left     = VALUE #( row    = 1
                                                                                        column = 1 )
                                                                bottom_right = VALUE #( row    = 2
                                                                                        column = 2 ) ) ).
  ENDMETHOD.

*  METHOD decode_range_address_sh_invali.
*    LOOP AT VALUE string_table( ( `:` ) ( `` ) ( `$` ) ( `A` ) ( `A:` ) ( `$$A1` ) ( `A:A1` ) ( `B2:A1` ) ) INTO DATA(address).
*      TRY.
*          zcl_xlom_range=>decode_range_address_a1( address ).
*          cl_abap_unit_assert=>fail( msg = |Exception expected for address "{ address }"| ).
*        CATCH cx_root ##NO_HANDLER.
*      ENDTRY.
*    ENDLOOP.
*  ENDMETHOD.

  METHOD decode_range_address_sh_valid.
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( 'BKPF!A:A' )
                                        exp = VALUE ty_address( worksheet_name = 'BKPF'
                                                                top_left       = VALUE #( column = 1 )
                                                                bottom_right   = VALUE #( column = 1 ) ) ).
    cl_abap_unit_assert=>assert_equals( act = zcl_xlom_range=>decode_range_address_a1( `'BKPF (2)'!A:A` )
                                        exp = VALUE ty_address( worksheet_name = 'BKPF (2)'
                                                                top_left       = VALUE #( column = 1 )
                                                                bottom_right   = VALUE #( column = 1 ) ) ).
  ENDMETHOD.
ENDCLASS.
