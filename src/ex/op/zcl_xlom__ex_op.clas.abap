"! TODO should look like ZCL_XLOM__EX_FU
CLASS zcl_xlom__ex_op DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create_dynamic
      IMPORTING operator      TYPE csequence
                arguments     TYPE zif_xlom__ex=>tt_argument_or_operand
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.
ENDCLASS.


CLASS zcl_xlom__ex_op IMPLEMENTATION.
  METHOD create_dynamic.
    " Calculation operators and precedence in Excel
    " https://support.microsoft.com/en-us/office/calculation-operators-and-precedence-in-excel-48be406d-4975-4d31-b2b8-7af9e0e2878a
    DATA(unary) = xsdbool( lines( arguments ) = 1 ).
    CASE operator.
      WHEN '&'.
        result = zcl_xlom__ex_op_ampersand=>create( left_operand  = arguments[ 1 ]
                                                    right_operand = arguments[ 2 ] ).
      WHEN '^'.
        result = zcl_xlom__ex_op_caret=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN ':'.
        result = zcl_xlom__ex_op_colon=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN ','.
        result = zcl_xlom__ex_op_comma=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN '/'.
        result = zcl_xlom__ex_op_divide=>create( left_operand  = arguments[ 1 ]
                                                 right_operand = arguments[ 2 ] ).
      WHEN '='.
        result = zcl_xlom__ex_op_equal=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN '>'.
        result = zcl_xlom__ex_op_greater=>create( left_operand  = arguments[ 1 ]
                                                  right_operand = arguments[ 2 ] ).
      WHEN '>='.
        result = zcl_xlom__ex_op_greater_equal=>create( left_operand  = arguments[ 1 ]
                                                        right_operand = arguments[ 2 ] ).
      WHEN '<'.
        result = zcl_xlom__ex_op_lower=>create( left_operand  = arguments[ 1 ]
                                                right_operand = arguments[ 2 ] ).
      WHEN '<='.
        result = zcl_xlom__ex_op_lower_equal=>create( left_operand  = arguments[ 1 ]
                                                      right_operand = arguments[ 2 ] ).
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
      WHEN '<>'.
        result = zcl_xlom__ex_op_not_equal=>create( left_operand  = arguments[ 1 ]
                                                    right_operand = arguments[ 2 ] ).
      WHEN '%'.
        result = zcl_xlom__ex_op_percent=>create( operand = arguments[ 1 ] ).
      WHEN '+'.
        IF unary = abap_false.
          result = zcl_xlom__ex_op_plus=>create( left_operand  = arguments[ 1 ]
                                                 right_operand = arguments[ 2 ] ).
        ELSE.
          result = zcl_xlom__ex_op_plus_unary=>create( operand = arguments[ 1 ] ).
        ENDIF.
      WHEN ` `.
        result = zcl_xlom__ex_op_space=>create( left_operand  = arguments[ 1 ]
                                                       right_operand = arguments[ 2 ] ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
