CLASS zcl_xlom__ex_ut DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS are_equal
      IMPORTING expression_1  TYPE REF TO zif_xlom__ex
                expression_2  TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE abap_bool.

    "! Check if mandatory parameters receive arguments,
    "! set default arguments for parameters which are optional with a default value but don't receive an argument,
    "! check if too many arguments are passed,
    "! accept many arguments if it's variadic.
    CLASS-METHODS check_arguments_or_operands
      IMPORTING expression            TYPE REF TO zif_xlom__ex
      CHANGING  arguments_or_operands TYPE zif_xlom__ex=>tt_argument_or_operand.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom__ex_ut IMPLEMENTATION.
  METHOD are_equal.
    IF    (     expression_1 IS NOT BOUND
            AND expression_2 IS BOUND )
       OR (     expression_1 IS BOUND
            AND expression_2 IS NOT BOUND ).
      result = abap_false.
      RETURN.
    ENDIF.

    IF     expression_1 IS NOT BOUND
       AND expression_2 IS NOT BOUND.
      result = abap_true.
      RETURN.
    ENDIF.

    IF expression_1->type <> expression_2->type.
      result = abap_false.
      RETURN.
    ENDIF.

    CASE expression_1->type.
      WHEN expression_1->c_type-array.
        DATA(array_expression_1) = CAST zcl_xlom__ex_el_array( expression_1 ).
        DATA(array_expression_2) = CAST zcl_xlom__ex_el_array( expression_2 ).
        IF lines( array_expression_1->rows ) <> lines( array_expression_2->rows ).
          result = abap_false.
          RETURN.
        ENDIF.
        DO lines( array_expression_1->rows ) TIMES.
          DATA(row_number) = sy-index.
          IF lines( array_expression_1->rows[ row_number ]-columns_of_row )
             <> lines( array_expression_2->rows[ row_number ]-columns_of_row ).
            result = abap_false.
            RETURN.
          ENDIF.
          DO lines( array_expression_1->rows[ row_number ]-columns_of_row ) TIMES.
            IF NOT are_equal( expression_1 = array_expression_1->rows[ row_number ]-columns_of_row[ sy-index ]
                              expression_2 = array_expression_2->rows[ row_number ]-columns_of_row[ sy-index ] ).
              result = abap_true.
              RETURN.
            ENDIF.
          ENDDO.
        ENDDO.
      WHEN expression_1->c_type-empty_argument.
        ASSERT 1 = 1. " Debug helper to set a break-point
      WHEN expression_1->c_type-no_argument.
        ASSERT 1 = 1. " Debug helper to set a break-point
      WHEN expression_1->c_type-boolean.
        IF CAST zcl_xlom__ex_el_boolean( expression_1 )->boolean_value <> CAST zcl_xlom__ex_el_boolean( expression_2 )->boolean_value.
          result = abap_false.
          RETURN.
        ENDIF.
      WHEN expression_1->c_type-error.
        IF expression_1 <> expression_2.
          result = abap_false.
          RETURN.
        ENDIF.
      WHEN expression_1->c_type-number.
        IF CAST zcl_xlom__ex_el_number( expression_1 )->number <> CAST zcl_xlom__ex_el_number( expression_2 )->number.
          result = abap_false.
          RETURN.
        ENDIF.
      WHEN expression_1->c_type-range.
        IF CAST zcl_xlom__ex_el_range( expression_1 )->_address_or_name <> CAST zcl_xlom__ex_el_range( expression_2 )->_address_or_name.
          result = abap_false.
          RETURN.
        ENDIF.
      WHEN expression_1->c_type-string.
        IF CAST zcl_xlom__ex_el_string( expression_1 )->string <> CAST zcl_xlom__ex_el_string( expression_2 )->string.
          result = abap_false.
          RETURN.
        ENDIF.
      WHEN expression_1->c_type-table.
        DATA(table_expression_1) = CAST zcl_xlom__ex_el_table( expression_1 ).
        DATA(table_expression_2) = CAST zcl_xlom__ex_el_table( expression_2 ).
        IF    table_expression_1->name         <> table_expression_2->name
           OR table_expression_1->column->name <> table_expression_2->column->name
           OR table_expression_1->rows         <> table_expression_2->rows.
          result = abap_false.
          RETURN.
        ENDIF.
      WHEN expression_1->c_type-table_column
        OR expression_1->c_type-table_col_rang
        OR expression_1->c_type-table_item_spe.
        RAISE EXCEPTION TYPE zcx_xlom_unexpected.
    ENDCASE.

    IF lines( expression_1->arguments_or_operands ) <> lines( expression_2->arguments_or_operands ).
      result = abap_false.
      RETURN.
    ENDIF.

    LOOP AT expression_1->arguments_or_operands INTO DATA(expression_1_argument_or_opera).
      IF expression_1_argument_or_opera IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_xlom_unexpected.
      ENDIF.
      DATA(expression_2_argument_or_opera) = VALUE #( expression_2->arguments_or_operands[ sy-tabix ] ).
      IF expression_2_argument_or_opera IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_xlom_unexpected.
      ENDIF.
      IF expression_1_argument_or_opera->type <> expression_2_argument_or_opera->type.
        result = abap_false.
        RETURN.
      ENDIF.
      IF NOT are_equal( expression_1 = expression_1_argument_or_opera
                        expression_2 = expression_2_argument_or_opera ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    result = abap_true.
  ENDMETHOD.

  METHOD check_arguments_or_operands.
    DATA(parameters) = expression->get_parameters( ).

    " Only a variadic function may have more arguments than the number of parameters.
    IF     parameters IS NOT INITIAL
       AND lines( arguments_or_operands ) > lines( parameters )
       AND parameters[ lines( parameters ) ]-variadic  = abap_false.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.

    DATA(count_last_arguments_not_bound) = 0.
    DATA(parameter_number) = 0.
    LOOP AT parameters REFERENCE INTO DATA(parameter).
      parameter_number = parameter_number + 1.

      DATA(argument_or_operand) = REF #( arguments_or_operands[ parameter_number ] OPTIONAL ).

      " Check that for a mandatory parameter, an argument is passed.
      IF     parameter->default  IS NOT BOUND
         AND parameter->optional  = abap_false
         AND argument_or_operand IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      ENDIF.

      " Set the default value if one is defined.
      " If an argument is passed empty, don't assign the default value
      " (different behavior in at least RIGHT and LEFT functions).
      IF     parameter->default  IS BOUND
         AND argument_or_operand IS NOT BOUND.
        INSERT parameter->default INTO arguments_or_operands INDEX parameter_number REFERENCE INTO argument_or_operand.
      ENDIF.

      IF argument_or_operand IS BOUND
          AND argument_or_operand->* IS NOT BOUND.
        count_last_arguments_not_bound = count_last_arguments_not_bound + 1.
      ELSE.
        count_last_arguments_not_bound = 0.
      ENDIF.
    ENDLOOP.

    IF count_last_arguments_not_bound > 0.
      DELETE arguments_or_operands FROM lines( arguments_or_operands ) - count_last_arguments_not_bound + 1.
    ENDIF.
    DO lines( parameters ) - lines( arguments_or_operands ) TIMES.
      INSERT zcl_xlom__ex_el_none_argument=>singleton INTO TABLE arguments_or_operands.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
