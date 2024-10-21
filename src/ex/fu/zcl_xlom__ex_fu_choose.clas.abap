CLASS zcl_xlom__ex_fu_choose DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING !condition    TYPE REF TO zif_xlom__ex
                expr_if_true  TYPE REF TO zif_xlom__ex
                expr_if_false TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_choose.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        condition     TYPE i VALUE 1,
        expr_if_true  TYPE i VALUE 2,
        expr_if_false TYPE i VALUE 3,
      END OF c_arg.
ENDCLASS.


CLASS zcl_xlom__ex_fu_choose IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-if.
    zif_xlom__ex~parameters = VALUE #( ( name = 'CONDITION    ' )
                                       ( name = 'EXPR_IF_TRUE ' )
                                       ( name = 'EXPR_IF_FALSE' default = zcl_xlom__ex_el_number=>create( 1 ) )
                                       ( name = 'A1        ' default = zcl_xlom__ex_el_boolean=>true )
                                       ( name = 'SHEET_TEXT' default = zcl_xlom__ex_el_string=>create( '' ) ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_choose( ).
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
ENDCLASS.
