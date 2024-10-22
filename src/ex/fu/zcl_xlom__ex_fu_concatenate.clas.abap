"! CONCATENATE(text1, [text2], ...)
"! https://support.microsoft.com/en-us/office/concatenate-function-8f8ae884-2ca8-4f7a-b093-75d702bea31d
CLASS zcl_xlom__ex_fu_concatenate DEFINITION
  PUBLIC FINAL
  INHERITING FROM zcl_xlom__ex_fu
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    TYPES tt_text TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS create
      IMPORTING text1         TYPE REF TO zif_xlom__ex
                texts         TYPE tt_text OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_concatenate.

    METHODs zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.
ENDCLASS.


CLASS zcl_xlom__ex_fu_concatenate IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-concatenate.
    zif_xlom__ex~parameters = VALUE #( ( name = 'TEXT' )
                                       ( name = 'TEXTS' variadic = abap_true ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_concatenate( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( text1 )
                                                          ( LINES OF texts ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(string) = VALUE string( ).
    LOOP AT arguments INTO DATA(argument).
      string = string && zcl_xlom__va=>to_string( argument )->get_string( ).
    ENDLOOP.
    result = zcl_xlom__va_string=>get( string ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
