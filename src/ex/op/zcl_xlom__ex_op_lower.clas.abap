CLASS zcl_xlom__ex_op_lower DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_op.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = '<'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING left_operand  TYPE REF TO zif_xlom__ex
                right_operand TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_op_lower.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        left_operand  TYPE i VALUE 1,
        right_operand TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.

    METHODS constructor.
ENDCLASS.


CLASS zcl_xlom__ex_op_lower IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'LEFT_OPERAND'  )
                          ( name = 'RIGHT_OPERAND' ) ).
  ENDMETHOD.

  METHOD constructor.
    zif_xlom__ex~type = zif_xlom__ex=>c_type-operation-lower.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_op_lower( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( left_operand  )
                                                          ( right_operand ) ).
*    zcl_xlom__ex_ut=>check_arguments_or_operands(
*      EXPORTING expression            = result
*      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(left_operand) = arguments[ c_arg-left_operand ].
    DATA(right_operand) = arguments[ c_arg-right_operand ].
    result = zcl_xlom__va=>compare(
               operand_1 = left_operand
               operator  = '<'
               operand_2 = right_operand ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
