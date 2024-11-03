CLASS zcl_xlom__ex_el_string DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex_el.

    DATA string TYPE string READ-ONLY.

    CLASS-METHODS create
      IMPORTING !text         TYPE csequence
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_buffer_line,
        string TYPE string,
        object TYPE REF TO zcl_xlom__ex_el_string,
      END OF ts_buffer_line.
    TYPES tt_buffer TYPE HASHED TABLE OF ts_buffer_line WITH UNIQUE KEY string.

    CLASS-DATA buffer TYPE tt_buffer.
ENDCLASS.


CLASS zcl_xlom__ex_el_string IMPLEMENTATION.
  METHOD create.
    result = VALUE #( buffer[ string = text ]-object OPTIONAL ).
    IF result IS NOT BOUND.
      result = NEW zcl_xlom__ex_el_string( ).
      result->string            = text.
      result->zif_xlom__ex~type = zif_xlom__ex=>c_type-string.
      INSERT VALUE #( string = text
                      object = result )
             INTO TABLE buffer.
    ENDIF.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_string=>get( string ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__ex_el~render.
    result = string.
  ENDMETHOD.
ENDCLASS.
