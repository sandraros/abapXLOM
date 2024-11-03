*&---------------------------------------------------------------------*
*& Report z_xlom_demo_over_abap2xlsx
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_xlom_demo_over_abap2xlsx.

PARAMETERS xlsxpath TYPE string LOWER CASE DEFAULT 'C:\...\name.xlsx'.

START-OF-SELECTION.
*TYPES ty_unique_strings TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

TRY.

    DATA(xlom_application) = zcl_xlom_application=>create( ).
    xlom_application->set_calculation( zcl_xlom=>c_calculation-manual ).

    DATA(xlom_workbook) = xlom_application->workbooks->add( ).

*    DATA(formulas) = VALUE ty_unique_strings( ).

    " CAN'T USE ZCL_EXCEL_READER_HUGE_FILE BECAUSE IT DOESN'T READ TABLES.
    DATA(reader) = NEW zcl_excel_reader_2007( ).

    DATA(workbook) = reader->zif_excel_reader~load_file( xlsxpath ).
    DATA(worksheets_iterator) = workbook->get_worksheets_iterator( ).
    WHILE worksheets_iterator->has_next( ).
      DATA(worksheet) = CAST zcl_excel_worksheet( worksheets_iterator->get_next( ) ).
      DATA(worksheet_name) = worksheet->get_title( ).

      DATA(xlom_worksheet) = xlom_workbook->worksheets->add( worksheet_name ).

      LOOP AT worksheet->sheet_content REFERENCE INTO DATA(cell).
        IF cell->cell_formula IS NOT INITIAL.
*          INSERT cell->cell_formula INTO TABLE formulas.
          xlom_worksheet->range( cell1_string = cell->cell_coords )->set_formula2( cell->cell_formula ).
        ELSEIF cell->data_type = 's'.
          xlom_worksheet->range( cell1_string = cell->cell_coords )->set_value(
                                                                      zcl_xlom__va_string=>get( cell->cell_value ) ).
        ELSE.
          xlom_worksheet->range( cell1_string = cell->cell_coords )->set_value(
              zcl_xlom__va_number=>get( CONV #( cell->cell_value ) ) ).
        ENDIF.
      ENDLOOP.

      DATA(tables_iterator) = worksheet->get_tables_iterator( ).
      WHILE tables_iterator->has_next( ).
        DATA(table) = CAST zcl_excel_table( tables_iterator->get_next( ) ).
        DATA(list_object) = xlom_worksheet->list_objects->add(
            source_type      = zcl_xlom=>c_list_object_source_type-range
            source           = zcl_xlom_range=>create_from_address_or_name(
                                 address     = |{ table->settings-top_left_column }{ table->settings-top_left_row }:{ table->settings-bottom_right_column }{ table->settings-bottom_right_row }|
                                 relative_to = xlom_worksheet )
            has_headers      = zcl_xlom=>c_yes_no_guess-yes ).
        list_object->set_name( EXACT #( table->settings-table_name ) ).
      ENDWHILE.
    ENDWHILE.

*" SAP TABLE DATA
*LOOP AT object_tabs REFERENCE INTO DATA(object_tab).
*  DATA(sheet_name_object_data) = object_tab->name.
*  xlom_worksheet = xlom_workbook->worksheets->add( sheet_name_object_data && ' (2)' ).
*  LOOP AT object_tab->lines REFERENCE INTO DATA(line).
*    DATA(column) = 1.
*    LOOP AT line->cells REFERENCE INTO DATA(cell_2).
*      CASE cell_2->type.
*        WHEN 'str'.
*          xlom_worksheet->cells( row    = line->line_number
*                                 column = column
*                               )->set_value( zcl_xlom__va_string=>get( cell_2->value ) ).
*        WHEN 'num'.
*          xlom_worksheet->cells( row    = line->line_number
*                                 column = column
*                               )->set_value( zcl_xlom__va_number=>get( CONV #( cell_2->value ) ) ).
*        WHEN OTHERS.
*          RAISE EXCEPTION TYPE lcx_xlom_todo.
*      ENDCASE.
*      column = column + 1.
*    ENDLOOP.
*  ENDLOOP.
*ENDLOOP.

    " CALCULATE THE FORMULAS
    xlom_application->calculate( ).

  CATCH cx_root INTO DATA(error).
    ASSERT 1 = 1. " Debug helper to set a break-point
ENDTRY.
