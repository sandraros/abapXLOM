CLASS zcx_xlom__va DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA result_error TYPE REF TO zcl_xlom__va_error READ-ONLY.

    METHODS constructor
      IMPORTING result_error TYPE REF TO zcl_xlom__va_error
                !text        TYPE csequence OPTIONAL
                long_text    TYPE csequence OPTIONAL.

  PRIVATE SECTION.
    DATA text      TYPE string.
    DATA long_text TYPE string.
ENDCLASS.


CLASS zcx_xlom__va IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->result_error = result_error.
    me->text         = text.
    me->long_text    = long_text.
  ENDMETHOD.
ENDCLASS.
