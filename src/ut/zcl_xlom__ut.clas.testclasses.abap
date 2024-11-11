*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION
  INHERITING FROM zcl_xlom__ex_ut_eval_aunit FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS bottom_outside_used_range FOR TESTING RAISING cx_static_check.
    METHODS inside_used_range FOR TESTING RAISING cx_static_check.
    METHODS left_outside_used_range FOR TESTING RAISING cx_static_check.
    METHODS right_outside_used_range FOR TESTING RAISING cx_static_check.
    METHODS top_outside_used_range FOR TESTING RAISING cx_static_check.
    METHODS used_range_is_in_middle   FOR TESTING RAISING cx_static_check.

    METHODS setup.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD bottom_outside_used_range.
    range_a1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_a2->set_value( zcl_xlom__va_string=>create( `world` ) ).
    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( worksheet->range( cell1_string = 'A1:A3' ) ).
    cl_abap_unit_assert=>assert_equals(
        act = intersect_result
        exp = VALUE zcl_xlom__ut=>ts_intersect_used_range(
                        inside_used_range              = VALUE #( top_left     = VALUE #( row    = 1
                                                                                          column = 1 )
                                                                  bottom_right = VALUE #( row    = 2
                                                                                          column = 1 )
                                                                  row_count    = 2
                                                                  column_count = 1 )
                        count_cells_outside_used_range = 1
                        outside_used_range             = VALUE #( ( top_left     = VALUE #( row    = 3
                                                                                            column = 1 )
                                                                    bottom_right = VALUE #( row    = 3
                                                                                            column = 1 )
                                                                    row_count    = 1
                                                                    column_count = 1 ) ) ) ).
  ENDMETHOD.

  METHOD inside_used_range.
    range_b2->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_b3->set_value( zcl_xlom__va_string=>create( `world` ) ).
    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( worksheet->range( cell1_string = 'B2:B3' ) ).
    cl_abap_unit_assert=>assert_equals(
        act = intersect_result
        exp = VALUE zcl_xlom__ut=>ts_intersect_used_range(
                        inside_used_range              = VALUE #( top_left     = VALUE #( row    = 1
                                                                                          column = 1 )
                                                                  bottom_right = VALUE #( row    = 2
                                                                                          column = 1 )
                                                                  row_count    = 2
                                                                  column_count = 1 )
                        count_cells_outside_used_range = 0 ) ).
  ENDMETHOD.

  METHOD left_outside_used_range.
    range_b1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_c1->set_value( zcl_xlom__va_string=>create( `world` ) ).
    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( worksheet->range( cell1_string = 'A1:C1' ) ).
    cl_abap_unit_assert=>assert_equals(
        act = intersect_result
        exp = VALUE zcl_xlom__ut=>ts_intersect_used_range(
                        inside_used_range              = VALUE #( top_left     = VALUE #( row    = 1
                                                                                          column = 2 )
                                                                  bottom_right = VALUE #( row    = 1
                                                                                          column = 3 )
                                                                  row_count    = 1
                                                                  column_count = 2 )
                        count_cells_outside_used_range = 1
                        outside_used_range             = VALUE #( ( top_left     = VALUE #( row    = 1
                                                                                            column = 1 )
                                                                    bottom_right = VALUE #( row    = 1
                                                                                            column = 1 )
                                                                    row_count    = 1
                                                                    column_count = 1 ) ) ) ).
  ENDMETHOD.

  METHOD right_outside_used_range.
    range_a1->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_b1->set_value( zcl_xlom__va_string=>create( `world` ) ).
    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( worksheet->range( cell1_string = 'A1:C1' ) ).
    cl_abap_unit_assert=>assert_equals(
        act = intersect_result
        exp = VALUE zcl_xlom__ut=>ts_intersect_used_range(
                        inside_used_range              = VALUE #( top_left     = VALUE #( row    = 1
                                                                                          column = 1 )
                                                                  bottom_right = VALUE #( row    = 1
                                                                                          column = 2 )
                                                                  row_count    = 1
                                                                  column_count = 2 )
                        count_cells_outside_used_range = 1
                        outside_used_range             = VALUE #( ( top_left     = VALUE #( row    = 1
                                                                                            column = 3 )
                                                                    bottom_right = VALUE #( row    = 1
                                                                                            column = 3 )
                                                                    row_count    = 1
                                                                    column_count = 1 ) ) ) ).
  ENDMETHOD.

  METHOD setup.
    setup_default_xlom_objects( ).
  ENDMETHOD.

  METHOD top_outside_used_range.
    range_a2->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_a3->set_value( zcl_xlom__va_string=>create( `world` ) ).
    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( worksheet->range( cell1_string = 'A1:A3' ) ).
    cl_abap_unit_assert=>assert_equals(
        act = intersect_result
        exp = VALUE zcl_xlom__ut=>ts_intersect_used_range(
                        inside_used_range              = VALUE #( top_left     = VALUE #( row    = 2
                                                                                          column = 1 )
                                                                  bottom_right = VALUE #( row    = 3
                                                                                          column = 1 )
                                                                  row_count    = 2
                                                                  column_count = 1 )
                        count_cells_outside_used_range = 1
                        outside_used_range             = VALUE #( ( top_left     = VALUE #( row    = 1
                                                                                            column = 1 )
                                                                    bottom_right = VALUE #( row    = 1
                                                                                            column = 1 )
                                                                    row_count    = 1
                                                                    column_count = 1 ) ) ) ).
  ENDMETHOD.

  METHOD used_range_is_in_middle.
    range_c3->set_value( zcl_xlom__va_string=>create( `Hello` ) ).
    range_d4->set_value( zcl_xlom__va_string=>create( `world` ) ).
    DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( worksheet->range( cell1_string = 'B2:F6' ) ).
    cl_abap_unit_assert=>assert_equals(
        act = intersect_result
        exp = VALUE zcl_xlom__ut=>ts_intersect_used_range(
                        inside_used_range              = VALUE #( top_left     = VALUE #( row    = 2
                                                                                          column = 2 )
                                                                  bottom_right = VALUE #( row    = 3
                                                                                          column = 3 )
                                                                  row_count    = 2
                                                                  column_count = 2 )
                        count_cells_outside_used_range = 21
                        outside_used_range             = VALUE #( ( top_left     = VALUE #( row    = 1
                                                                                            column = 1 )
                                                                    bottom_right = VALUE #( row    = 1
                                                                                            column = 5 )
                                                                    row_count    = 1
                                                                    column_count = 5 )
                                                                  ( top_left     = VALUE #( row    = 2
                                                                                            column = 1 )
                                                                    bottom_right = VALUE #( row    = 3
                                                                                            column = 1 )
                                                                    row_count    = 2
                                                                    column_count = 1 )
                                                                  ( top_left     = VALUE #( row    = 2
                                                                                            column = 4 )
                                                                    bottom_right = VALUE #( row    = 3
                                                                                            column = 5 )
                                                                    row_count    = 2
                                                                    column_count = 2 )
                                                                  ( top_left     = VALUE #( row    = 4
                                                                                            column = 1 )
                                                                    bottom_right = VALUE #( row    = 5
                                                                                            column = 5 )
                                                                    row_count    = 2
                                                                    column_count = 5 ) ) ) ).
  ENDMETHOD.
ENDCLASS.
