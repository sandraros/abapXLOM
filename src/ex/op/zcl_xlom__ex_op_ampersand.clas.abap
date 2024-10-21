CLASS zcl_xlom__ex_op_ampersand DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom__ex_op.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    CLASS-METHODS create
      IMPORTING left_operand  TYPE REF TO zif_xlom__ex
                right_operand TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_op_ampersand.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        left_operand  TYPE i VALUE 1,
        right_operand TYPE i VALUE 2,
      END OF c_arg.

    METHODS constructor.
ENDCLASS.


CLASS zcl_xlom__ex_op_ampersand IMPLEMENTATION.
  METHOD constructor.
    zif_xlom__ex~type = zif_xlom__ex=>c_type-operation-ampersand.
    zif_xlom__ex~parameters = VALUE #( ( name = 'LEFT_OPERAND ' )
                                       ( name = 'RIGHT_OPERAND' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_op_ampersand( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( left_operand  )
                                                          ( right_operand ) ).
*    zcl_xlom__ex_ut=>check_arguments_or_operands(
*      EXPORTING expression            = result
*      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        result = zcl_xlom__va_string=>create( zcl_xlom__va=>to_string( arguments[ c_arg-left_operand ] )->get_string( )
                                           && zcl_xlom__va=>to_string( arguments[ c_arg-right_operand ] )->get_string( ) ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
