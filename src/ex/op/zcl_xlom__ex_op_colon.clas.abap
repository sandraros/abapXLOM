"! Operator colon (e.g. A1:A2, OFFSET(A1,1,1):OFFSET(A1,2,2), my.B1:my.C1 (range names), etc.)
CLASS zcl_xlom__ex_op_colon DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_op.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.
    INTERFACES zif_xlom__ex.
    INTERFACES zif_xlom__ex_array.

    CLASS-METHODS create
      IMPORTING left_operand  TYPE REF TO zif_xlom__ex
                right_operand TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_op_colon.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        left_operand  TYPE i VALUE 1,
        right_operand TYPE i VALUE 2,
      END OF c_arg.

    METHODS constructor.
ENDCLASS.


CLASS zcl_xlom__ex_op_colon IMPLEMENTATION.
  METHOD constructor.
    zif_xlom__ex~type = zif_xlom__ex=>c_type-operation-colon.
    zif_xlom__ex~parameters = VALUE #( not_part_of_result_array = abap_true
                                       ( name = 'LEFT_OPERAND' )
                                       ( name = 'RIGHT_OPERAND' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_op_colon( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( left_operand  )
                                                          ( right_operand ) ).
*    zcl_xlom__ex_ut=>check_arguments_or_operands(
*      EXPORTING expression            = result
*      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(left_operand) = zcl_xlom__va=>to_range(
                             input     = zif_xlom__ex~arguments_or_operands[ c_arg-left_operand ]->result_of_evaluation
                             worksheet = context->worksheet ).
    DATA(right_operand) = zcl_xlom__va=>to_range(
        input     = zif_xlom__ex~arguments_or_operands[ c_arg-right_operand ]->result_of_evaluation
        worksheet = context->worksheet ).
    TRY.
        result = zcl_xlom_range=>create_from_top_left_bottom_ri( worksheet    = context->worksheet
                                                                 top_left     = left_operand->_address-top_left
                                                                 bottom_right = right_operand->_address-bottom_right ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
