CLASS zcx_xlom_todo DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING !text TYPE csequence OPTIONAL.

    METHODS get_longtext REDEFINITION.

    METHODS get_text     REDEFINITION.

  PRIVATE SECTION.
    DATA text TYPE string.
ENDCLASS.


CLASS zcx_xlom_todo IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->text = text.
  ENDMETHOD.

  METHOD get_longtext.
    result = text.
  ENDMETHOD.

  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.
