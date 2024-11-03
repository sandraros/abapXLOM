CLASS zcl_xlom__ex_ut_renderer DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS render
      IMPORTING expression    TYPE REF TO zif_xlom__ex
                level         TYPE i DEFAULT 0
      RETURNING VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_xlom__ex_ut_renderer IMPLEMENTATION.
  METHOD render.
    result = expression->name.
    IF expression->type BETWEEN 1 AND 99.
      result = CAST zif_xlom__ex_el( expression )->render( ).
    ELSEIF expression->type BETWEEN 100 AND 9999.
      DATA(parameters) = expression->get_parameters( ).
      LOOP AT expression->arguments_or_operands INTO DATA(argument_or_operand).
        IF sy-tabix <= lines( parameters ).
          DATA(parameter) = parameters[ sy-tabix ].
        ENDIF.
        result = result && |\n{ repeat( val = `  `
                                        occ = level ) }{
                            to_lower( parameter-name ) }: {
                            render( expression = argument_or_operand
                                    level      = level + 1 ) }|.
      ENDLOOP.
    ELSE.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
