"! LEN(text)
"! https://support.microsoft.com/en-us/office/len-lenb-functions-29236f94-cedc-429d-affd-b5e33d2c67cb
CLASS zcl_xlom__ex_fu_mod DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING !text         TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_mod.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        text TYPE i VALUE 1,
      END OF c_arg.
ENDCLASS.


CLASS zcl_xlom__ex_fu_mod IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-len.
    zif_xlom__ex~parameters = VALUE #( ( name = 'TEXT' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_mod( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( text ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        result = zcl_xlom__va_number=>create(
                     strlen( zcl_xlom__va=>to_string( arguments[ c_arg-text ] )->get_string( ) ) ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
