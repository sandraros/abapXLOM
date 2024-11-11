CLASS zcl_xlom__ex_op_percent DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_op.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = '%'
                                        type = zif_xlom__ex=>c_type-operation-percent.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING operand       TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_op_percent.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        operand TYPE i VALUE 1,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_op_percent IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'OPERAND' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_op_percent( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( operand ) ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(operand_result) = arguments[ c_arg-operand ].
        result = zcl_xlom__va_number=>create( zcl_xlom__va=>to_number( operand_result )->get_number( ) ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
