CLASS zcl_xlom__va DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS compare
      IMPORTING VALUE(operand_1) TYPE REF TO zif_xlom__va
                operator         TYPE csequence
                VALUE(operand_2) TYPE REF TO zif_xlom__va
      RETURNING VALUE(result)    TYPE REF TO zif_xlom__va.

    CLASS-METHODS to_boolean
      IMPORTING !input        TYPE REF TO zif_xlom__va
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__va_boolean.

    CLASS-METHODS to_number
      IMPORTING !input        TYPE REF TO zif_xlom__va
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__va_number
      RAISING   zcx_xlom__va.

    CLASS-METHODS to_string
      IMPORTING !input        TYPE REF TO zif_xlom__va
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__va_string
      RAISING   zcx_xlom__va.

    CLASS-METHODS to_range
      IMPORTING !input        TYPE REF TO zif_xlom__va
                worksheet     TYPE REF TO zcl_xlom_worksheet
      RETURNING VALUE(result) TYPE REF TO zcl_xlom_range
      RAISING   zcx_xlom__va.

    CLASS-METHODS to_array
      IMPORTING !input        TYPE REF TO zif_xlom__va
      RETURNING VALUE(result) TYPE REF TO zif_xlom__va_array
      RAISING   zcx_xlom__va.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom__va IMPLEMENTATION.
  METHOD compare.
    FIELD-SYMBOLS <operand_1> TYPE simple.
    FIELD-SYMBOLS <operand_2> TYPE simple.

    IF    operand_1->type = zif_xlom__va=>c_type-array
       OR operand_1->type = zif_xlom__va=>c_type-range.
      operand_1 = CAST zif_xlom__va_array( operand_1 )->get_cell_value( column = 1
                                                                        row    = 1 ).
    ENDIF.

    IF    operand_2->type = zif_xlom__va=>c_type-array
       OR operand_2->type = zif_xlom__va=>c_type-range.
      operand_2 = CAST zif_xlom__va_array( operand_2 )->get_cell_value( column = 1
                                                                        row    = 1 ).
    ENDIF.

    IF operand_1->type = zif_xlom__va=>c_type-error.
      result = operand_1.
    ELSEIF operand_2->type = zif_xlom__va=>c_type-error.
      result = operand_2.
    ELSE.
      IF operand_1->type <> operand_2->type.
        IF operand_1->type = zif_xlom__va=>c_type-empty.
          operand_1 = SWITCH #( operand_2->type
                                WHEN zif_xlom__va=>c_type-boolean THEN zcl_xlom__va_boolean=>false
                                WHEN zif_xlom__va=>c_type-number  THEN zcl_xlom__va_number=>get( 0 )
                                WHEN zif_xlom__va=>c_type-string  THEN zcl_xlom__va_string=>get( '' ) ).
        ENDIF.
        IF operand_2->type = zif_xlom__va=>c_type-empty.
          operand_2 = SWITCH #( operand_2->type
                                WHEN zif_xlom__va=>c_type-boolean THEN zcl_xlom__va_boolean=>false
                                WHEN zif_xlom__va=>c_type-number  THEN zcl_xlom__va_number=>get( 0 )
                                WHEN zif_xlom__va=>c_type-string  THEN zcl_xlom__va_string=>get( '' ) ).
        ENDIF.
      ENDIF.
      IF operand_1->type = operand_2->type.
        CASE operand_1->type.
          WHEN zif_xlom__va=>c_type-boolean.
            ASSIGN CAST zcl_xlom__va_boolean( operand_1 )->boolean_value TO <operand_1>.
            ASSIGN CAST zcl_xlom__va_boolean( operand_2 )->boolean_value TO <operand_2>.
          WHEN zif_xlom__va=>c_type-number.
            DATA(operand_1_number) = CAST zcl_xlom__va_number( operand_1 )->get_number( ).
            DATA(operand_2_number) = CAST zcl_xlom__va_number( operand_2 )->get_number( ).
            ASSIGN operand_1_number TO <operand_1>.
            ASSIGN operand_2_number TO <operand_2>.
          WHEN zif_xlom__va=>c_type-string.
            DATA(operand_1_string) = CAST zcl_xlom__va_string( operand_1 )->get_string( ).
            DATA(operand_2_string) = CAST zcl_xlom__va_string( operand_2 )->get_string( ).
            ASSIGN operand_1_string TO <operand_1>.
            ASSIGN operand_2_string TO <operand_2>.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDCASE.
        CASE operator.
          WHEN '='.
            result = zcl_xlom__va_boolean=>get( xsdbool( <operand_1> = <operand_2> ) ).
          WHEN '<'.
            result = zcl_xlom__va_boolean=>get( xsdbool( <operand_1> < <operand_2> ) ).
          WHEN '>'.
            result = zcl_xlom__va_boolean=>get( xsdbool( <operand_1> > <operand_2> ) ).
          WHEN '>='.
            result = zcl_xlom__va_boolean=>get( xsdbool( <operand_1> >= <operand_2> ) ).
          WHEN '<='.
            result = zcl_xlom__va_boolean=>get( xsdbool( <operand_1> <= <operand_2> ) ).
          WHEN '<>'.
            result = zcl_xlom__va_boolean=>get( xsdbool( <operand_1> <> <operand_2> ) ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDCASE.
      ELSE.
        CASE operator.
          WHEN '='.
            result = zcl_xlom__va_boolean=>false.
          WHEN '<>'.
            result = zcl_xlom__va_boolean=>true.
          WHEN OTHERS.
            DATA(operand_1_greater_th_operand_2) = COND #(
              WHEN operand_1->type = zif_xlom__va=>c_type-string THEN abap_true
              WHEN operand_2->type = zif_xlom__va=>c_type-string THEN abap_false
              WHEN operand_1->type = zif_xlom__va=>c_type-number THEN abap_true
              WHEN operand_2->type = zif_xlom__va=>c_type-number THEN abap_false
              ELSE                                                    THROW zcx_xlom_todo( ) ).
            CASE operator.
              WHEN '<'
                OR '<='.
                result = zcl_xlom__va_boolean=>get( xsdbool( operand_1_greater_th_operand_2 = abap_false ) ).
              WHEN '>'
                OR '>='.
                result = zcl_xlom__va_boolean=>get( xsdbool( operand_1_greater_th_operand_2 = abap_true ) ).
              WHEN OTHERS.
                RAISE EXCEPTION TYPE zcx_xlom_todo.
            ENDCASE.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD to_array.
    CASE input->type.
      WHEN input->c_type-array
          OR input->c_type-range.
        result = CAST #( input ).
*      WHEN input->c_type-range.
*        result = zcl_xlom__pv_worksheet_array=>get_array( CAST zcl_xlom_range( input )->parent ).
      WHEN OTHERS.
        result = zcl_xlom__va_array=>create_initial( row_count    = 1
                                                     column_count = 1
                                                     cells        = VALUE #( ( row = 1 column = 1 value = input ) ) ).
*                     rows         = VALUE #( ( columns_of_row = VALUE #( ( input ) ) ) ) ).
    ENDCASE.
  ENDMETHOD.

  METHOD to_boolean.
    " If source is Number:
    "       FALSE if 0
    "       TRUE if not 0
    "
    "  If source is String:
    "       Language-dependent.
    "       In English:
    "       TRUE if "TRUE"
    "       FALSE if "FALSE"
    CASE input->type.
      WHEN input->c_type-array.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      WHEN input->c_type-boolean.
        result = CAST #( input ).
      WHEN input->c_type-error.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      WHEN input->c_type-number.
        CASE CAST zcl_xlom__va_number( input )->get_number( ).
          WHEN 0.
            result = zcl_xlom__va_boolean=>false.
          WHEN OTHERS.
            result = zcl_xlom__va_boolean=>true.
        ENDCASE.
      WHEN input->c_type-range.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      WHEN input->c_type-string.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD to_number.
    " If source is Boolean:
    "      0 if FALSE
    "      1 if TRUE
    "
    " If source is String:
    "      Language-dependent for decimal separator.
    "      "." if English, "," if French, etc.
    "      Accepted: "-1", "+1", ".5", "1E1", "-.5"
    "      "-1E-1", "1e1", "1e307", "1e05", "1e-309"
    "      Invalid: "", "E1", "1e308", "1e-310"
    "      #VALUE! if invalid decimal separator
    "      #VALUE! if invalid number
    IF     input->type <> input->c_type-array
       AND input->type <> input->c_type-range.
      DATA(cell) = input.
    ELSE.
      DATA(range) = CAST zif_xlom__va_array( input ).
      IF    range->column_count <> 1
         OR range->row_count    <> 1.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      ENDIF.
      cell = range->get_cell_value( column = 1
                                    row    = 1 ).
    ENDIF.

    CASE cell->type.
      WHEN cell->c_type-array.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      WHEN cell->c_type-boolean.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      WHEN cell->c_type-empty.
        result = zcl_xlom__va_number=>get( 0 ).
      WHEN cell->c_type-error.
        RAISE EXCEPTION TYPE zcx_xlom__va
          EXPORTING result_error = CAST #( cell ).
      WHEN cell->c_type-number.
        result = CAST #( cell ).
      WHEN cell->c_type-range.
        " impossible because processed in the previous block.
        RAISE EXCEPTION TYPE zcx_xlom_unexpected.
      WHEN cell->c_type-string.
        TRY.
            DATA(number) = CONV f( CAST zcl_xlom__va_string( cell )->get_string( ) ).
            result = zcl_xlom__va_number=>get( number ).
          CATCH cx_sy_conversion_no_number.
            RAISE EXCEPTION TYPE zcx_xlom__va
              EXPORTING result_error = zcl_xlom__va_error=>value_cannot_be_calculated.
        ENDTRY.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD to_range.
    CASE input->type.
      WHEN input->c_type-error.
        " TODO I didn't check whether it should be #N/A, #REF! or #VALUE!
        RAISE EXCEPTION TYPE zcx_xlom__va
          EXPORTING result_error = zcl_xlom__va_error=>value_cannot_be_calculated.
      WHEN input->c_type-number.
        DATA(number) = CAST zcl_xlom__va_number( input )->get_number( ).
        IF frac( number ) <> 0.
          RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDIF.
        result = zcl_xlom_range=>create_from_address_or_name( address     = |{ number }:{ number }|
                                                              relative_to = worksheet ).
      WHEN input->c_type-range.
        result = CAST #( input ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD to_string.
    CASE input->type.
      WHEN input->c_type-empty.
        result = zcl_xlom__va_string=>create( '' ).
      WHEN input->c_type-error.
        RAISE EXCEPTION TYPE zcx_xlom__va
          EXPORTING result_error = CAST #( input ).
      WHEN input->c_type-number.
        result = zcl_xlom__va_string=>create( |{ CAST zcl_xlom__va_number( input )->get_number( ) }| ).
      WHEN input->c_type-array
            OR input->c_type-range.
        DATA(input_range) = CAST zif_xlom__va_array( input ).
        DATA(cell) = input_range->get_cell_value( column = 1
                                                  row    = 1 ).
        DATA(string) = VALUE string( ).
        CASE cell->type.
          WHEN zif_xlom__va=>c_type-empty.
            string = ''.
          WHEN zif_xlom__va=>c_type-error.
            string = CAST zcl_xlom__va_error( cell )->english_error_name.
          WHEN zif_xlom__va=>c_type-number.
            string = |{ CAST zcl_xlom__va_number( cell )->get_number( ) }|.
          WHEN zif_xlom__va=>c_type-string.
            string = CAST zcl_xlom__va_string( cell )->get_string( ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDCASE.
*        DATA(input_range) = CAST zcl_xlom_range( input ).
*        DATA(input_range_address) = zcl_xlom__ext_range=>get_address( input_range ).
*        IF input_range_address-top_left <> input_range_address-bottom_right.
*          RAISE EXCEPTION TYPE zcx_xlom_todo.
*        ENDIF.
*        DATA(input_range_worksheet_cells) = zcl_xlom__pv_worksheet_array=>get_cells( input_range->parent ).
*        DATA(cell) = REF #( input_range_worksheet_cells->*[ row    = input_range_address-top_left-row
*                                                            column = input_range_address-top_left-column ] OPTIONAL ).
*        DATA(string) = VALUE string( ).
*        IF cell IS BOUND.
*          CASE cell->value->type.
*            WHEN zif_xlom__va=>c_type-number.
*              string = |{ CAST zcl_xlom__va_number( cell->value )->get_number( ) }|.
*            WHEN zif_xlom__va=>c_type-string.
*              string = CAST zcl_xlom__va_string( cell->value )->get_string( ).
*            WHEN zif_xlom__va=>c_type-error.
*              string = CAST zcl_xlom__va_error( cell->value )->english_error_name.
*            WHEN OTHERS.
*              RAISE EXCEPTION TYPE zcx_xlom_todo.
*          ENDCASE.
*        ENDIF.
        result = zcl_xlom__va_string=>create( string ).

      WHEN input->c_type-string.
        result = CAST #( input ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
