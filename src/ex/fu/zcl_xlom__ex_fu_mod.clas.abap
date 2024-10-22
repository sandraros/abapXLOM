"! MOD(number, divisor)
"! https://support.microsoft.com/en-us/office/mod-function-9b6cd169-b6ee-406a-a97b-edf2a9dc24f3
CLASS zcl_xlom__ex_fu_mod DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING !number       TYPE REF TO zif_xlom__ex
                divisor       TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_mod.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        number  TYPE i VALUE 1,
        divisor TYPE i VALUE 2,
      END OF c_arg.
ENDCLASS.


CLASS zcl_xlom__ex_fu_mod IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-len.
    zif_xlom__ex~parameters = VALUE #( ( name = 'NUMBER ' )
                                       ( name = 'DIVISOR' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_mod( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( number  )
                                                          ( divisor ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(number) = zcl_xlom__va=>to_number( arguments[ c_arg-number ] )->get_number( ).
        DATA(divisor) = zcl_xlom__va=>to_number( arguments[ c_arg-divisor ] )->get_number( ).
        result = zcl_xlom__va_number=>create( number MOD divisor ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
