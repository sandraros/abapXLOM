CLASS zcl_xlom__ex_ut_eval DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! One simple example to understand:
    "!
    "! In E1, INDEX(A1:D4,{1,2;3,4},{1,2;3,4}) will correspond to four values in E1:F2:
    "! E1: INDEX(A1:D4,1,1) (i.e. A1)
    "! F1: INDEX(A1:D4,2,2) (i.e. B2)
    "! E2: INDEX(A1:D4,3,3) (i.e. C3)
    "! F2: INDEX(A1:D4,4,4) (i.e. D4)
    CLASS-METHODS evaluate_array_operands
      IMPORTING expression    TYPE REF TO zif_xlom__ex
                context       TYPE REF TO zcl_xlom__ex_ut_eval_context OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zif_xlom__va.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS evaluate_single_cell
      IMPORTING
        column          TYPE i
        row             TYPE i
        operand_results TYPE zif_xlom__ex=>tt_operand_result
        parameters      TYPE zif_xlom__ex=>tt_parameter
        context         TYPE REF TO zcl_xlom__ex_ut_eval_context
        expression      TYPE REF TO zif_xlom__ex
      RETURNING
        VALUE(result)   TYPE REF TO zif_xlom__va.
ENDCLASS.


CLASS zcl_xlom__ex_ut_eval IMPLEMENTATION.
  METHOD evaluate_array_operands.
    DATA: column_number TYPE i,
          row_number TYPE i.
    " One simple example to understand:
    "
    " In E1, INDEX(A1:D4,{1,2;3,4},{1,2;3,4}) will correspond to four values in E1:F2:
    " E1: INDEX(A1:D4,1,1) (i.e. A1)
    " F1: INDEX(A1:D4,2,2) (i.e. B2)
    " E2: INDEX(A1:D4,3,3) (i.e. C3)
    " F2: INDEX(A1:D4,4,4) (i.e. D4)
    "
    " Below is the generalization.
    "
    "  formula with operation on arrays        result
    "
    "  -> if the left operand is one line high, the line is replicated till max lines of the right operand.
    "  -> if the left operand is one column wide, the column is replicated till max columns of the right operand.
    "  -> if the right operand is one line high, the line is replicated till max lines of the left operand.
    "  -> if the right operand is one column wide, the column is replicated till max columns of the left operand.
    "
    "  -> if the left operand has less lines than the right operand, additional lines are added with #N/A.
    "  -> if the left operand has less columns than the right operand, additional columns are added with #N/A.
    "  -> if the right operand has less lines than the left operand, additional lines are added with #N/A.
    "  -> if the right operand has less columns than the left operand, additional columns are added with #N/A.
    "
    "  -> target array size = max lines of both operands + max columns of both operands.
    "  -> each target cell of the target array is calculated like this:
    "     T(1,1) = L(1,1) op R(1,1)
    "     T(2,1) = L(2,1) op R(2,1)
    "     etc.
    "     If the left cell or right cell is #N/A, the target cell is also #N/A.
    "
    "  Examples where one of the two operands has 1 cell, 1 line or 1 column
    "
    "  a | b | c   op   k | l | m | n          a op k | b op l | c op m | #N/A
    "
    "  a | b | c   op   k                      a op k | b op k | c op k
    "  d | e | f                               d op k | e op k | f op k
    "  g | h | i                               g op k | h op k | i op k
    "
    "  a | b | c   op   k | l | m | n          a op k | b op l | c op m | #N/A
    "  d | e | f                               d op k | e op l | f op m | #N/A
    "  g | h | i                               g op k | h op l | i op m | #N/A
    "
    "  a | b | c   op   k                      a op k | b op k | c op k
    "  d | e | f        l                      d op l | e op l | f op l
    "  g | h | i        m                      g op m | h op m | i op m
    "                   n                      #N/A   | #N/A   | #N/A
    "
    "  a | b | c   op   k                      a op k | b op k | c op k
    "  d | e | f        l                      d op l | e op l | f op l
    "  g | h | i                               #N/A   | #N/A   | #N/A
    "
    "  a | b | c   op   k                      a op k | b op k | c op k
    "                   l                      a op l | b op l | c op l
    "                   m                      a op m | b op m | c op m
    "
    "  Both operands have more than 1 line and more than 1 column
    "
    "  a | b | c   op   k | n                  a op k | b op n | #N/A
    "  d | e | f        l | o                  d op l | e op o | #N/A
    "  g | h | i                               #N/A   | #N/A   | #N/A
    "
    "  a | b | c   op   k | n                  a op k | b op n | #N/A
    "  d | e | f        l | o                  d op l | e op o | #N/A
    "                   m | p                  #N/A   | #N/A   | #N/A
    "
    "  a | b       op   k | n | q              a op k | b op n | #N/A
    "  d | e            l | o | r              d op l | e op o | #N/A
    "  g | h                                   #N/A   | #N/A   | #N/A

    IF expression->type BETWEEN 1 AND 99.
      result = expression->evaluate( arguments = VALUE #( )
                                     context   = context ).
      RETURN.
    ENDIF.

    DATA(parameters) = expression->get_parameters( ).
    DATA(at_least_one_array_or_range) = abap_false.
    DATA(operand_results) = VALUE zif_xlom__ex=>tt_operand_result( ).

    LOOP AT expression->arguments_or_operands INTO DATA(operand).
      IF operand IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_xlom_unexpected.
      ENDIF.

      " OPTIONAL because there can be an operand but no parameter in case the parameter is an endless
      " list of parameters, like in the AND function, only the 2 first parameters are defined.
      DATA(parameter) = REF #( parameters[ sy-tabix ] OPTIONAL ).

      " EVALUATE
      DATA(operand_result) = zcl_xlom__ex_ut_eval=>evaluate_array_operands( expression = operand
                                                                            context    = context ).

      IF     parameter                 IS BOUND
         AND parameter->error_accepted  = abap_false
         AND operand_result->type       = operand_result->c_type-error.
        " e.g. the result of =OFFSET(INDIRECT("aa"&#VALUE!),#N/A,1) is #VALUE!
        result = operand_result.
        RETURN.
      ENDIF.

      INSERT operand_result INTO TABLE operand_results.

      IF     parameter IS BOUND
         AND parameter->not_part_of_result_array  = abap_false
         AND (    operand_result->type = operand_result->c_type-array
               OR operand_result->type = operand_result->c_type-range )
         AND (    CAST zif_xlom__va_array( operand_result )->row_count    > 1
               OR CAST zif_xlom__va_array( operand_result )->column_count > 1 ).
        at_least_one_array_or_range = abap_true.
      ENDIF.
    ENDLOOP.

    IF at_least_one_array_or_range = abap_false.
      "======================
      " EXPRESSION EVALUATION
      "======================
      result = expression->evaluate( arguments = operand_results
                                     context   = context ).
    ELSE.

      " Special logic for a few functions
      IF expression->type between 1000 and 9999.
        CAST zcl_xlom__ex_fu( expression )->adjust_evaluated_operands( CHANGING evaluated_operands = operand_results ).
      ENDIF.

      DATA(max_row_count) = 1.
      DATA(max_column_count) = 1.
      DATA(used_range_row_count) = 1.
      DATA(used_range_column_count) = 1.
      LOOP AT operand_results INTO operand_result.
        " There can be an operand but no parameter in case the parameter is an endless list
        " of parameters, like in the AND function, only the 2 first parameters are defined.
        parameter = REF #( parameters[ sy-tabix ] OPTIONAL ).
        IF    parameter->not_part_of_result_array  = abap_true
           OR operand_result IS NOT BOUND.
          CONTINUE.
        ENDIF.
        CASE operand_result->type.
          WHEN operand_result->c_type-array
            OR operand_result->c_type-range.
            DATA(optimized_array_range) = zcl_xlom__ut=>intersect_used_range( cast #( operand_result ) ).
            used_range_row_count = nmax( val1 = used_range_row_count
                                         val2 = optimized_array_range-inside_used_range-row_count ).
            used_range_column_count = nmax( val1 = used_range_column_count
                                            val2 = optimized_array_range-inside_used_range-column_count ).
            max_row_count = nmax( val1 = max_row_count
                                  val2 = CAST zif_xlom__va_array( operand_result )->row_count ).
            max_column_count = nmax( val1 = max_column_count
                                     val2 = CAST zif_xlom__va_array( operand_result )->column_count ).
        ENDCASE.
      ENDLOOP.

      DATA(cells) = VALUE zif_xlom__va_array=>tt_cell( ).
*      DATA(rows) = VALUE zif_xlom__va_array=>tt_row( ).

      row_number = 1.
      WHILE row_number <= used_range_row_count.

        DATA(row) = VALUE zif_xlom__va_array=>ts_row( ).

        column_number = 1.
        WHILE column_number <= used_range_column_count.
          DATA(single_cell_result) = evaluate_single_cell( column          = column_number
                                                           row             = row_number
                                                           operand_results = operand_results
                                                           parameters      = parameters
                                                           context         = context
                                                           expression      = expression ).
          INSERT VALUE #( row    = row_number
                          column = column_number
                          value  = single_cell_result )
                 into table cells.
          column_number = column_number + 1.
        ENDWHILE.
        row_number = row_number + 1.
      ENDWHILE.

      IF    used_range_row_count    < max_row_count
         OR used_range_column_count < max_column_count.
        DATA(values_of_other_cells_2) = evaluate_single_cell( column          = nmin( val1 = used_range_column_count + 1
                                                                                      val2 = max_column_count )
                                                              row             = nmin( val1 = used_range_row_count + 1
                                                                                      val2 = max_row_count )
                                                              operand_results = operand_results
                                                              parameters      = parameters
                                                              context         = context
                                                              expression      = expression ).
        DATA(values_of_other_cells_3) = zcl_xlom__va=>to_array( values_of_other_cells_2 ).
        DATA(values_of_other_cells) = VALUE zif_xlom__va_array=>tt_cell( ).
        row_number = 1.
        WHILE row_number <= used_range_row_count.
          column_number = 1.
          WHILE column_number <= used_range_column_count.
            INSERT VALUE #( row    = 1
                            column = column_number
                            value  = values_of_other_cells_3->get_cell_value( column = column_number
                                                                              row    = row_number ) )
                   INTO TABLE values_of_other_cells.
            column_number = column_number + 1.
          ENDWHILE.
          row_number = row_number + 1.
        ENDWHILE.
      ENDIF.

      DATA(target_array) = zcl_xlom__va_array=>create_initial( row_count             = max_row_count
                                                               column_count          = max_column_count
                                                               cells                 = cells
                                                               values_of_other_cells = values_of_other_cells ).
*                                                               rows                 = rows
*                                                               value_of_other_cells = value_of_other_cells ).

      result = target_array.
    ENDIF.
  ENDMETHOD.

  METHOD evaluate_single_cell.

    DATA operand_result TYPE REF TO zif_xlom__va.
    DATA parameter TYPE REF TO zif_xlom__ex=>ts_parameter.

    DATA(single_cell_operands) = VALUE zif_xlom__ex=>tt_operand_result( ).
    LOOP AT operand_results INTO operand_result.
      parameter = REF #( parameters[ sy-tabix ] ).
      IF     parameter->not_part_of_result_array  = abap_false
         AND operand_result IS BOUND.
        CASE operand_result->type.
          WHEN operand_result->c_type-array
            OR operand_result->c_type-range.
            DATA(operand_array_range) = CAST zif_xlom__va_array( operand_result ).
            DATA(cell) = operand_array_range->get_cell_value(
                             column = COND #( WHEN operand_array_range->column_count = 1
                                              THEN 1
                                              ELSE column )
                             row    = COND #( WHEN operand_array_range->row_count = 1
                                              THEN 1
                                              ELSE row ) ).
          WHEN OTHERS.
            cell = operand_result.
        ENDCASE.
      ELSE.
        cell = operand_result.
      ENDIF.
      INSERT cell INTO TABLE single_cell_operands.
    ENDLOOP.

    "======================
    " EXPRESSION EVALUATION
    "======================
    result = expression->evaluate( arguments = single_cell_operands
                                   context   = context ).

  ENDMETHOD.

ENDCLASS.
