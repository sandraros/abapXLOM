class ZCL_XLOM__EX_OP_MINUS definition
  public
  final
  create private

  global friends ZCL_XLOM__EX_OP .

public section.

  interfaces ZIF_XLOM__EX .

  class-methods CREATE
    importing
      !LEFT_OPERAND type ref to ZIF_XLOM__EX
      !RIGHT_OPERAND type ref to ZIF_XLOM__EX
    returning
      value(RESULT) type ref to ZCL_XLOM__EX_OP_MINUS .
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        left_operand  TYPE i VALUE 1,
        right_operand TYPE i VALUE 2,
      END OF c_arg.

    METHODS constructor.
*    DATA left_operand  TYPE REF TO zif_xlom__ex.
*    DATA right_operand TYPE REF TO zif_xlom__ex.
ENDCLASS.



CLASS ZCL_XLOM__EX_OP_MINUS IMPLEMENTATION.


  METHOD constructor.
    zif_xlom__ex~type = zif_xlom__ex=>c_type-operation-minus.
    zif_xlom__ex~parameters = VALUE #( ( name = 'LEFT_OPERAND'  )
                                       ( name = 'RIGHT_OPERAND' ) ).
  ENDMETHOD.


  METHOD create.
    result = NEW zcl_xlom__ex_op_minus( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( LEFT_OPERAND  )
                                                          ( RIGHT_OPERAND ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
*    result->left_operand      = left_operand.
*    result->right_operand     = right_operand.
*    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-operation-minus.
*  ENDMETHOD.
*
*  METHOD zif_xlom__ex~evaluate.
*    DATA(array_evaluation) = zcl_xlom__ex_ut_eval=>evaluate_array_operands(
*                                 expression = me
*                                 context    = context
*                                 operands   = VALUE #( ( name = 'LEFT'  object = left_operand )
*                                                       ( name = 'RIGHT' object = right_operand ) ) ).
*    IF array_evaluation-result IS BOUND.
*      result = array_evaluation-result.
*    ELSE.
*      result = zif_xlom__ex~evaluate_single( arguments = array_evaluation-operand_results
*                                             context   = context ).
*    ENDIF.
  ENDMETHOD.


  METHOD zif_xlom__ex~evaluate.
    TRY.
        result = zcl_xlom__va_number=>create(
                         zcl_xlom__va=>to_number( arguments[ c_arg-left_operand ] )->get_number( )
                       - zcl_xlom__va=>to_number( arguments[ c_arg-right_operand ] )->get_number( ) ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
*  ENDMETHOD.
*
*  METHOD zif_xlom__ex~is_equal.
*    IF     expression       IS BOUND
*       AND expression->type  = zif_xlom__ex=>c_type-operation-minus
*       AND left_operand->is_equal( CAST zcl_xlom__ex_op_minus( expression )->left_operand )
*       AND right_operand->is_equal( CAST zcl_xlom__ex_op_minus( expression )->right_operand ).
*      result = abap_true.
*    ELSE.
*      result = abap_false.
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD zif_xlom__ex~set_result.
*    zif_xlom__ex~result_of_evaluation = value.
*    result = value.
  ENDMETHOD.
ENDCLASS.
