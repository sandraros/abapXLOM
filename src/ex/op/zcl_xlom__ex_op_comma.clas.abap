"! Union operator to concatenate two ranges (e.g. A1,A2,A3 which is the same as A1:A3)
CLASS zcl_xlom__ex_op_comma DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_op.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = ','
                                        type = zif_xlom__ex=>c_type-operation-comma.
    INTERFACES zif_xlom__ex_array.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING left_operand  TYPE REF TO zif_xlom__ex
                right_operand TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_op_comma.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        left_operand  TYPE i VALUE 1,
        right_operand TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_op_comma IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( not_part_of_result_array = abap_true
                          ( name = 'LEFT_OPERAND' )
                          ( name = 'RIGHT_OPERAND' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_op_comma( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( left_operand  )
                                                          ( right_operand ) ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(left_operand) = zcl_xlom__va=>to_range(
                             input     = zif_xlom__ex~arguments_or_operands[ c_arg-left_operand ]->result_of_evaluation
                             worksheet = context->worksheet ).
    DATA(right_operand) = zcl_xlom__va=>to_range(
        input     = zif_xlom__ex~arguments_or_operands[ c_arg-right_operand ]->result_of_evaluation
        worksheet = context->worksheet ).
    TRY.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
*        result = zcl_xlom__pv_range_create=>create_from_top_left_bottom_ri(
*                     worksheet    = context->worksheet
*                     top_left     = zcl_xlom__ext_range=>get_address( left_operand )-top_left
*                     bottom_right = zcl_xlom__ext_range=>get_address( right_operand )-bottom_right ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
