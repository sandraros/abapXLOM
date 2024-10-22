class ZCL_XLOM__VA_NUMBER definition
  public
  final
  create private .

public section.

  interfaces ZIF_XLOM__VA .

  class-methods CREATE
    importing
      !NUMBER type F
    returning
      value(RESULT) type ref to ZCL_XLOM__VA_NUMBER .
  class-methods GET
    importing
      !NUMBER type F
    returning
      value(RESULT) type ref to ZCL_XLOM__VA_NUMBER .
  methods GET_INTEGER
    returning
      value(RESULT) type I .
  methods GET_NUMBER
    returning
      value(RESULT) type F .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_buffer_line,
        number TYPE f,
        object TYPE REF TO zcl_xlom__va_number,
      END OF ts_buffer_line.
    TYPES tt_buffer TYPE SORTED TABLE OF ts_buffer_line WITH UNIQUE KEY number.

    CLASS-DATA buffer TYPE tt_buffer.

    DATA number TYPE f.
ENDCLASS.



CLASS ZCL_XLOM__VA_NUMBER IMPLEMENTATION.


  METHOD create.
    result = NEW zcl_xlom__va_number( ).
    result->zif_xlom__va~type = zif_xlom__va=>c_type-number.
    result->number            = number.
  ENDMETHOD.


  METHOD get.
    DATA(buffer_line) = REF #( buffer[ number = number ] OPTIONAL ).
    IF buffer_line IS NOT BOUND.
      result = create( number ).
      INSERT VALUE #( number = number
                      object = result )
             INTO TABLE buffer
             REFERENCE INTO buffer_line.
    ENDIF.
    result = buffer_line->object.
  ENDMETHOD.


  METHOD get_integer.
    " Excel rounding (1.99 -> 1, -1.99 -> -1)
    result = floor( number ).
  ENDMETHOD.


  METHOD get_number.
    result = number.
  ENDMETHOD.


  METHOD zif_xlom__va~get_value.
    result = REF #( number ).
  ENDMETHOD.


  METHOD zif_xlom__va~is_array.
    result = abap_false.
  ENDMETHOD.


  METHOD zif_xlom__va~is_boolean.
    result = abap_false.
  ENDMETHOD.


  METHOD zif_xlom__va~is_equal.
    IF input_result->type = zif_xlom__va=>c_type-number.
      DATA(input_number) = CAST zcl_xlom__va_number( input_result ).
      IF number = input_number->number.
        result = abap_true.
      ELSE.
        result = abap_false.
      ENDIF.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_xlom__va~is_error.
    result = abap_false.
  ENDMETHOD.


  METHOD zif_xlom__va~is_number.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_xlom__va~is_string.
    result = abap_false.
  ENDMETHOD.
ENDCLASS.
