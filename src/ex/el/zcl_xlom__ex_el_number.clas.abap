CLASS zcl_xlom__ex_el_number DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex_el.

    DATA number TYPE f READ-ONLY.

    CLASS-METHODS create
      IMPORTING !number       TYPE f
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_number.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_buffer_line,
        number TYPE f,
        object TYPE REF TO zcl_xlom__ex_el_number,
      END OF ts_buffer_line.
    TYPES tt_buffer TYPE HASHED TABLE OF ts_buffer_line WITH UNIQUE KEY number.

    CLASS-DATA buffer TYPE tt_buffer.
ENDCLASS.


CLASS zcl_xlom__ex_el_number IMPLEMENTATION.
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

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__ex_el~render.
    result = |{ number }|.
  ENDMETHOD.
ENDCLASS.
