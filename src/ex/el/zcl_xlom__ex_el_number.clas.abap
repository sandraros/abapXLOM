class ZCL_XLOM__EX_EL_NUMBER definition
  public
  final
  create private .

public section.

  interfaces ZIF_XLOM__EX .

  class-methods CREATE
    importing
      !NUMBER type F
    returning
      value(RESULT) type ref to ZCL_XLOM__EX_EL_NUMBER .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_buffer_line,
        number TYPE f,
        object TYPE REF TO zcl_xlom__ex_el_number,
      END OF ts_buffer_line.
    TYPES tt_buffer TYPE HASHED TABLE OF ts_buffer_line WITH UNIQUE KEY number.

    DATA number TYPE f.

    CLASS-DATA buffer TYPE tt_buffer.
ENDCLASS.



CLASS ZCL_XLOM__EX_EL_NUMBER IMPLEMENTATION.


  METHOD create.
    result = VALUE #( buffer[ number = number ]-object OPTIONAL ).
    IF result IS NOT BOUND.
      result = NEW zcl_xlom__ex_el_number( ).
      result->number            = number.
      result->zif_xlom__ex~type = zif_xlom__ex=>c_type-number.
      INSERT VALUE #( number = number
                      object = result )
             INTO TABLE buffer.
    ENDIF.
  ENDMETHOD.


  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_number=>create( number ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
