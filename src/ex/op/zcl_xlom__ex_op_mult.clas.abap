CLASS zcl_xlom__ex_op_mult DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_op.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = '*'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING left_operand  TYPE REF TO zif_xlom__ex
                right_operand TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_op_mult.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        left_operand  TYPE i VALUE 1,
        right_operand TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.

    METHODS constructor.
ENDCLASS.


CLASS zcl_xlom__ex_op_mult IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'LEFT_OPERAND'  )
                          ( name = 'RIGHT_OPERAND' ) ).
  ENDMETHOD.

  METHOD constructor.
    zif_xlom__ex~type = zif_xlom__ex=>c_type-operation-mult.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_op_mult( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( left_operand  )
                                                          ( right_operand ) ).
*    zcl_xlom__ex_ut=>check_arguments_or_operands(
*      EXPORTING expression            = result
*      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        result = zcl_xlom__va_number=>create( zcl_xlom__va=>to_number( arguments[ c_arg-left_operand ] )->get_number( )
                                            * zcl_xlom__va=>to_number( arguments[ c_arg-right_operand ] )->get_number( ) ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
