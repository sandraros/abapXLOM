CLASS zcl_xlom__ex_fu_if DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'IF'
                                        type = zif_xlom__ex=>c_type-function-if.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !condition    TYPE REF TO zif_xlom__ex
                expr_if_true  TYPE REF TO zif_xlom__ex
                expr_if_false TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_if.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        condition     TYPE i VALUE 1,
        expr_if_true  TYPE i VALUE 2,
        expr_if_false TYPE i VALUE 3,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_if IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'CONDITION    ' )
                          ( name = 'EXPR_IF_TRUE ' not_part_of_result_array = abap_true )
                          ( name = 'EXPR_IF_FALSE' not_part_of_result_array = abap_true  default = zcl_xlom__ex_el_boolean=>false ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_if( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( condition     )
                                                          ( expr_if_true  )
                                                          ( expr_if_false ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(condition_evaluation) = zcl_xlom__va=>to_boolean( arguments[ c_arg-condition ] ).
    result = COND #( WHEN condition_evaluation = zcl_xlom__va_boolean=>true
                     THEN arguments[ c_arg-expr_if_true ]
                     ELSE arguments[ c_arg-expr_if_false ] ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
