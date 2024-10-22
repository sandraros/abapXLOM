class ZCL_XLOM__EX_EL_STRING definition
  public
  final
  create private

  global friends ZIF_XLOM__UT_ALL_FRIENDS .

public section.

  interfaces ZIF_XLOM__EX .

  class-methods CREATE
    importing
      !TEXT type CSEQUENCE
    returning
      value(RESULT) type ref to ZCL_XLOM__EX_EL_STRING .
  PRIVATE SECTION.
    DATA string TYPE string.
ENDCLASS.



CLASS ZCL_XLOM__EX_EL_STRING IMPLEMENTATION.


  METHOD create.
    result = NEW zcl_xlom__ex_el_string( ).
    result->string            = text.
    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-string.
  ENDMETHOD.


  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_string=>get( string ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
