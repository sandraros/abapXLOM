CLASS zcl_xlom__ex_ut_parser DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
*    INTERFACES zif_xlom__ut_all_friends.

    CLASS-METHODS create
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_ut_parser.

    METHODS parse
      IMPORTING !tokens       TYPE zcl_xlom__ex_ut_lexer=>tt_token
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex
      RAISING   zcx_xlom__ex_ut_parser.

  PRIVATE SECTION.
    TYPES tt_expr TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    DATA formula_offset      TYPE i.
    DATA current_token_index TYPE sytabix.
    DATA tokens              TYPE zcl_xlom__ex_ut_lexer=>tt_token.

    METHODS get_expression_from_curly_brac
      IMPORTING arguments     TYPE zcl_xlom__ex_ut_parser_item=>tt_item
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.

    METHODS get_expression_from_error
      IMPORTING error_name    TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.

    METHODS get_expression_from_operator
      IMPORTING operator      TYPE string
                arguments     TYPE tt_expr
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.

    METHODS get_expression_from_symbol_nam
      IMPORTING token_value   TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.

    "! Transform parentheses and operators into items
    METHODS parse_expression_item
      CHANGING item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    "! Merge function item with its next item item (arguments in parentheses)
    METHODS parse_expression_item_1
      CHANGING item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    METHODS parse_expression_item_2
      CHANGING item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    METHODS parse_expression_item_3
      CHANGING item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    METHODS parse_expression_item_4
      CHANGING item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    METHODS parse_expression_item_5
      CHANGING item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    METHODS parse_tokens_until
      IMPORTING !until TYPE csequence
      CHANGING  item   TYPE REF TO zcl_xlom__ex_ut_parser_item.
ENDCLASS.


CLASS zcl_xlom__ex_ut_parser IMPLEMENTATION.
  METHOD create.
    result = NEW zcl_xlom__ex_ut_parser( ).
  ENDMETHOD.

  METHOD get_expression_from_curly_brac.
    result = zcl_xlom__ex_el_array=>create(
                 rows = VALUE #( FOR <argument> IN arguments
                                 ( columns_of_row = VALUE #( FOR <subitem> IN <argument>->subitems
                                                             ( <subitem>->expression ) ) ) ) ).
  ENDMETHOD.

  METHOD get_expression_from_error.
    result = zcl_xlom__ex_el_error=>get_from_error_name( error_name ).
  ENDMETHOD.

  METHOD get_expression_from_operator.
    DATA(unary) = xsdbool( lines( arguments ) = 1 ).
    CASE operator.
      WHEN '+'.
        IF unary = abap_false.
          result = zcl_xlom__ex_op_plus=>create( left_operand  = arguments[ 1 ]
                                                 right_operand = arguments[ 2 ] ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDIF.
      WHEN '-'.
        IF unary = abap_false.
          result = zcl_xlom__ex_op_minus=>create( left_operand  = arguments[ 1 ]
                                                  right_operand = arguments[ 2 ] ).
        ELSE.
          result = zcl_xlom__ex_op_minus_unary=>create( operand = arguments[ 1 ] ).
        ENDIF.
      WHEN '*'.
        result = zcl_xlom__ex_op_mult=>create( left_operand  = arguments[ 1 ]
                                               right_operand = arguments[ 2 ] ).
      WHEN '='.
        result = zcl_xlom__ex_op_equal=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN '<'.
        result = zcl_xlom__ex_op_lower=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN '<='.
        result = zcl_xlom__ex_op_lower_equal=>create( left_operand  = arguments[ 1 ]
                                                      right_operand = arguments[ 2 ] ).
      WHEN '>'.
        result = zcl_xlom__ex_op_greater=>create( left_operand  = arguments[ 1 ]
                                                  right_operand = arguments[ 2 ] ).
      WHEN '>='.
        result = zcl_xlom__ex_op_greater_equal=>create( left_operand  = arguments[ 1 ]
                                                        right_operand = arguments[ 2 ] ).
      WHEN '<>'.
        result = zcl_xlom__ex_op_not_equal=>create( left_operand  = arguments[ 1 ]
                                                    right_operand = arguments[ 2 ] ).
      WHEN '&'.
        result = zcl_xlom__ex_op_ampersand=>create( left_operand  = arguments[ 1 ]
                                                    right_operand = arguments[ 2 ] ).
      WHEN ':'.
        result = zcl_xlom__ex_op_colon=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD get_expression_from_symbol_nam.
    IF token_value CP 'true'.
      result = zcl_xlom__ex_el_boolean=>create( boolean_value = abap_true ).
    ELSEIF token_value CP 'false'.
      result = zcl_xlom__ex_el_boolean=>create( boolean_value = abap_false ).
    ELSE.
      result = zcl_xlom__ex_el_range=>create( token_value ).
    ENDIF.
  ENDMETHOD.

  METHOD parse.
    current_token_index = 1.
    me->tokens = tokens.

    DATA(initial_item) = zcl_xlom__ex_ut_parser_item=>create( type = '(' ).
    current_token_index = 0.

    " Determine the groups for the parentheses (expression grouping,
    " function arguments), curly brackets (arrays) and square brackets (tables).
    parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-parenthesis_close
                        CHANGING  item  = initial_item ).

    " Determine the items for the operators.
    parse_expression_item_2( CHANGING item = initial_item ).

*    " Merge function item with its next item (arguments in parentheses) and ignore commas between arguments.
    " ignore commas between arguments of functions and tables.
    parse_expression_item_1( CHANGING item = initial_item ).

    " arrays
    parse_expression_item_5( CHANGING item = initial_item ).

    " Remove useless items of one item.
    parse_expression_item_3( CHANGING item = initial_item ).

    " Determine the expressions for each item.
    parse_expression_item_4( CHANGING item = initial_item ).

    result = initial_item->expression.
  ENDMETHOD.

  METHOD parse_expression_item.
    WHILE current_token_index < lines( tokens ).
      current_token_index = current_token_index + 1.
      DATA(current_token) = REF #( tokens[ current_token_index ] ).
      DATA(subitem) = NEW zcl_xlom__ex_ut_parser_item( ).
      subitem->type  = current_token->type.
      subitem->value = current_token->value.
      CASE current_token->type.
        WHEN zcl_xlom__ex_ut_lexer=>c_type-parenthesis_open
          OR zcl_xlom__ex_ut_lexer=>c_type-function_name.
          parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-parenthesis_close
                              CHANGING  item  = subitem ).
        WHEN zcl_xlom__ex_ut_lexer=>c_type-curly_bracket_open.
          parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-curly_bracket_close
                              CHANGING  item  = subitem ).
        WHEN zcl_xlom__ex_ut_lexer=>c_type-table_name.
          parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-square_bracket_close
                              CHANGING  item  = subitem ).
      ENDCASE.
      INSERT subitem INTO TABLE item->subitems.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_expression_item_1.
    LOOP AT item->subitems INTO DATA(subitem).
      parse_expression_item_1( CHANGING item = subitem ).
    ENDLOOP.

    CASE item->type.
      WHEN zcl_xlom__ex_ut_lexer=>c_type-function_name
            OR zcl_xlom__ex_ut_lexer=>c_type-table_name.
        LOOP AT item->subitems INTO subitem.
          DATA(subitem_index) = sy-tabix.
          IF subitem->type <> zcl_xlom__ex_ut_lexer=>c_type-comma.
            CONTINUE.
          ENDIF.

          IF    subitem_index = lines( item->subitems )
             OR (     subitem_index < lines( item->subitems )
                  AND item->subitems[ subitem_index + 1 ]->type = zcl_xlom__ex_ut_lexer=>c_type-comma ).
            " Need to differentiate the two cases RIGHT("hello",) and RIGHT("hello"):
            " RIGHT("hello",) (means RIGHT("hello",0)) -> arguments "hello" and empty value
            " RIGHT("hello") (means RIGHT("hello",1)) -> arguments "hello" and empty argument
            INSERT zcl_xlom__ex_ut_parser_item=>create( type = zcl_xlom__ex_ut_lexer=>c_type-empty_argument )
                   INTO item->subitems
                   INDEX subitem_index + 1.
          ENDIF.
          DELETE item->subitems USING KEY loop_key.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

  METHOD parse_expression_item_2.
    TYPES to_expression TYPE REF TO zif_xlom__ex.
    TYPES:
      BEGIN OF ts_work,
        position   TYPE sytabix,
        token      TYPE REF TO zcl_xlom__ex_ut_lexer=>ts_token,
        expression TYPE REF TO zif_xlom__ex,
        operator   TYPE REF TO zcl_xlom__ex_ut_operator,
        priority   TYPE i,
      END OF ts_work.
    TYPES tt_work TYPE SORTED TABLE OF ts_work WITH NON-UNIQUE KEY position
                    WITH NON-UNIQUE SORTED KEY by_priority COMPONENTS priority position.
    TYPES:
      BEGIN OF ts_subitem_by_priority,
        operator      TYPE REF TO zcl_xlom__ex_ut_operator,
        priority      TYPE i,
        subitem_index TYPE i,
        subitem       TYPE REF TO zcl_xlom__ex_ut_parser_item,
      END OF ts_subitem_by_priority.
    TYPES tt_subitem_by_priority TYPE SORTED TABLE OF ts_subitem_by_priority WITH NON-UNIQUE KEY priority.
    TYPES tt_operand_positions   TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA priorities           TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA subitems_by_priority TYPE tt_subitem_by_priority.
    DATA item_index           TYPE syst-tabix.

    LOOP AT item->subitems INTO DATA(subitem).
      parse_expression_item_2( CHANGING item = subitem ).
    ENDLOOP.

    LOOP AT item->subitems INTO subitem
         WHERE table_line->type = zcl_xlom__ex_ut_lexer=>c_type-operator.
      item_index = sy-tabix.
      DATA(unary) = COND abap_bool( WHEN item_index = 1
                                    THEN abap_true
                                    ELSE SWITCH #( item->subitems[ item_index - 1 ]->type
                                                   WHEN zcl_xlom__ex_ut_lexer=>c_type-operator
                                                     OR zcl_xlom__ex_ut_lexer=>c_type-comma
                                                     OR zcl_xlom__ex_ut_lexer=>c_type-parenthesis_open
                                                     OR zcl_xlom__ex_ut_lexer=>c_type-semicolon
                                                   THEN abap_true ) ).
      DATA(operator) = zcl_xlom__ex_ut_operator=>get( operator = subitem->value
                                                      unary    = unary ).
      DATA(priority) = operator->get_priority( ).
      INSERT priority INTO TABLE priorities.
      INSERT VALUE #( priority      = priority
                      operator      = operator
                      subitem_index = item_index
                      subitem       = subitem )
             INTO TABLE subitems_by_priority.
    ENDLOOP.

    " Process operators with priority 1 first, then 2, etc.
    " The priority 0 corresponds to functions, tables, boolean values, numeric literals and text literals.
    LOOP AT priorities INTO priority.
      LOOP AT subitems_by_priority REFERENCE INTO DATA(subitem_by_priority)
           WHERE priority = priority.

        DATA(operand_position_offsets) = subitem_by_priority->operator->get_operand_position_offsets( ).

        subitem_by_priority->subitem->subitems = VALUE #( ).
        LOOP AT operand_position_offsets INTO DATA(operand_position_offset).
          INSERT item->subitems[ subitem_by_priority->subitem_index + operand_position_offset ]
                 INTO TABLE subitem_by_priority->subitem->subitems.
        ENDLOOP.

        DATA(positions_of_operands_to_delet) = VALUE tt_operand_positions(
            FOR <operand_position_offset> IN operand_position_offsets
            ( subitem_by_priority->subitem_index + <operand_position_offset> ) ).
        SORT positions_of_operands_to_delet BY table_line DESCENDING.
        LOOP AT positions_of_operands_to_delet INTO DATA(position).
          DATA(subitem_to_delete) = item->subitems[ position ].
          LOOP AT item->subitems ASSIGNING FIELD-SYMBOL(<subitem>)
               WHERE table_line = subitem_to_delete.
            <subitem> = subitem_by_priority->subitem.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM item->subitems COMPARING table_line.
  ENDMETHOD.

  METHOD parse_expression_item_3.
    LOOP AT item->subitems REFERENCE INTO DATA(subitem).
      parse_expression_item_3( CHANGING item = subitem->* ).
    ENDLOOP.

    IF     item->type              = zcl_xlom__ex_ut_lexer=>c_type-parenthesis_open
       AND lines( item->subitems ) = 1.
      item = item->subitems[ 1 ].
    ENDIF.
  ENDMETHOD.

  METHOD parse_expression_item_4.
    LOOP AT item->subitems INTO DATA(subitem).
      parse_expression_item_4( CHANGING item = subitem ).
    ENDLOOP.

    CASE item->type.
      WHEN zcl_xlom__ex_ut_lexer=>c_type-curly_bracket_open.
        item->expression = get_expression_from_curly_brac( arguments = item->subitems ).

      WHEN zcl_xlom__ex_ut_lexer=>c_type-empty_argument.
        item->expression = zcl_xlom__ex_el_empty_argument=>singleton.

      WHEN zcl_xlom__ex_ut_lexer=>c_type-error_name.
        item->expression = get_expression_from_error( error_name = item->value ).

      WHEN zcl_xlom__ex_ut_lexer=>c_type-function_name.
        " FUNCTION NAME
        item->expression = zcl_xlom__ex_fu=>create_dynamic( function_name = item->value
                                                            arguments     = VALUE #( FOR <subitem> IN item->subitems
                                                                                     ( <subitem>->expression ) ) ).
      WHEN zcl_xlom__ex_ut_lexer=>c_type-number.
        " NUMBER
        item->expression = zcl_xlom__ex_el_number=>create( CONV #( item->value ) ).

      WHEN zcl_xlom__ex_ut_lexer=>c_type-operator.
        " OPERATOR
        item->expression = get_expression_from_operator( operator  = item->value
                                                         arguments = VALUE #( FOR <subitem> IN item->subitems
                                                                              ( <subitem>->expression ) ) ).
      WHEN zcl_xlom__ex_ut_lexer=>c_type-semicolon.
        " Array row separator. No special processing, it's handled inside curly brackets.
        ASSERT 1 = 1. " Debug helper to set a break-point

      WHEN zcl_xlom__ex_ut_lexer=>c_type-square_bracket_open.
        CASE item->value.
          WHEN `[#All]`.
            item->expression = zcl_xlom__ex_el_table_item_spe=>all.
          WHEN `[#Data]`.
            item->expression = zcl_xlom__ex_el_table_item_spe=>data.
          WHEN `[#Headers]`.
            item->expression = zcl_xlom__ex_el_table_item_spe=>headers.
          WHEN `[#This Row]`.
            item->expression = zcl_xlom__ex_el_table_item_spe=>this_row.
          WHEN `[#Totals]`.
            item->expression = zcl_xlom__ex_el_table_item_spe=>totals.
          WHEN OTHERS.
            " [@column]
            " https://support.microsoft.com/en-us/office/implicit-intersection-operator-ce3be07b-0101-4450-a24e-c1c999be2b34
            "
            " Table[[][column]]
            "
            " https://support.microsoft.com/en-us/office/using-structured-references-with-excel-tables-f5ed2452-2337-4f71-bed3-c8ae6d2b276e
            " Use an escape character for some special characters in column headers
            " Some characters have special meaning and require the use of a single quotation mark (')
            " as an escape character. For example: =DeptSalesFYSummary['#OfItems]
            " Here’s the list of special characters that need an escape character (') in the formula:
            "   - Left bracket ([)
            "   - Right bracket (])
            "   - Pound sign(#)
            "   - Single quotation mark (')
            "   - At sign (@)
            DATA(table_column_name) = replace( val   = item->value
                                               regex = `^\[|'(?!')|\]$`
                                               with  = ''
                                               occ   = 0 ).
            item->expression = zcl_xlom__ex_el_table_column=>create( table_column_name ).
        ENDCASE.

      WHEN zcl_xlom__ex_ut_lexer=>c_type-symbol_name.
        " SYMBOL NAME (range address, range name, boolean constant)
        item->expression = get_expression_from_symbol_nam( item->value ).

      WHEN zcl_xlom__ex_ut_lexer=>c_type-table_name.
        DATA column TYPE REF TO zcl_xlom__ex_el_table_column.
        DATA(item_specifier) = VALUE zcl_xlom__ex_el_table=>ty_row_specifier( ).
        LOOP AT item->subitems INTO subitem.
          CASE subitem->value.
            WHEN '[#All]'.
              item_specifier = item_specifier + zcl_xlom__ex_el_table=>c_rows-all.
            WHEN '[#Data]'.
              item_specifier = item_specifier + zcl_xlom__ex_el_table=>c_rows-data.
            WHEN '[#Headers]'.
              item_specifier = item_specifier + zcl_xlom__ex_el_table=>c_rows-headers.
            WHEN '[#This Row]'.
              item_specifier = item_specifier + zcl_xlom__ex_el_table=>c_rows-this_row.
            WHEN '[#Totals]'.
              item_specifier = item_specifier + zcl_xlom__ex_el_table=>c_rows-totals.
            WHEN OTHERS.
              TRY.
                  column ?= subitem->expression.
                CATCH cx_root.
                  RAISE EXCEPTION TYPE zcx_xlom_todo.
              ENDTRY.
          ENDCASE.
        ENDLOOP.
        IF item_specifier IS INITIAL.
          item_specifier = zcl_xlom__ex_el_table=>c_rows-data.
        ENDIF.
        item->expression = zcl_xlom__ex_el_table=>create( name   = EXACT #( item->value )
                                                          column = column
                                                          rows   = item_specifier ).

      WHEN zcl_xlom__ex_ut_lexer=>c_type-text_literal.
        " TEXT LITERAL
        item->expression = zcl_xlom__ex_el_string=>create( item->value ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.

  METHOD parse_expression_item_5.
    TYPES ty_ref_item TYPE REF TO zcl_xlom__ex_ut_parser_item.

    LOOP AT item->subitems INTO DATA(subitem).
      parse_expression_item_5( CHANGING item = subitem ).
    ENDLOOP.

    LOOP AT item->subitems INTO DATA(array)
         WHERE table_line->type = zcl_xlom__ex_ut_lexer=>c_type-curly_bracket_open.
      DATA(new_array_subitems) = VALUE zcl_xlom__ex_ut_parser_item=>tt_item( ).
      DATA(row) = VALUE ty_ref_item( ).
      LOOP AT array->subitems INTO DATA(array_subitem).
        CASE array_subitem->type.
          WHEN zcl_xlom__ex_ut_lexer=>c_type-comma.
          WHEN zcl_xlom__ex_ut_lexer=>c_type-semicolon.
            INSERT row INTO TABLE new_array_subitems.
            row = VALUE #( ).
          WHEN OTHERS.
            IF row IS NOT BOUND.
              row = NEW zcl_xlom__ex_ut_parser_item( ).
              row->type  = zcl_xlom__ex_ut_lexer=>c_type-semicolon.
              row->value = zcl_xlom__ex_ut_lexer=>c_type-semicolon.
            ENDIF.
            INSERT array_subitem INTO TABLE row->subitems.
        ENDCASE.
      ENDLOOP.
      INSERT row INTO TABLE new_array_subitems.
      array->subitems = new_array_subitems.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_tokens_until.
    WHILE current_token_index < lines( tokens ).
      current_token_index = current_token_index + 1.
      DATA(current_token) = REF #( tokens[ current_token_index ] ).
      DATA(subitem) = NEW zcl_xlom__ex_ut_parser_item( ).
      subitem->type  = current_token->type.
      subitem->value = current_token->value.
      CASE current_token->type.
        WHEN zcl_xlom__ex_ut_lexer=>c_type-parenthesis_open
          OR zcl_xlom__ex_ut_lexer=>c_type-function_name.
          parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-parenthesis_close
                              CHANGING  item  = subitem ).
        WHEN zcl_xlom__ex_ut_lexer=>c_type-curly_bracket_open.
          parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-curly_bracket_close
                              CHANGING  item  = subitem ).
        WHEN zcl_xlom__ex_ut_lexer=>c_type-table_name.
          parse_tokens_until( EXPORTING until = zcl_xlom__ex_ut_lexer=>c_type-square_bracket_close
                              CHANGING  item  = subitem ).
        WHEN until.
          RETURN.
      ENDCASE.
      INSERT subitem INTO TABLE item->subitems.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
