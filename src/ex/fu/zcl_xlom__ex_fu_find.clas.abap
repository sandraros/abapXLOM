"! FIND(find_text, within_text, [start_num])
"! https://support.microsoft.com/en-us/office/find-findb-functions-c7912941-af2a-4bdf-a553-d0d89b0a0628
CLASS zcl_xlom__ex_fu_find DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'FIND'
                                        type = zif_xlom__ex=>c_type-function-find.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING find_text     TYPE REF TO zif_xlom__ex
                within_text   TYPE REF TO zif_xlom__ex
                start_num     TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_find.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        find_text   TYPE i VALUE 1,
        within_text TYPE i VALUE 2,
        start_num   TYPE i VALUE 3,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_find IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'FIND_TEXT  ' )
                          ( name = 'WITHIN_TEXT' )
                          ( name = 'START_NUM  ' default = zcl_xlom__ex_el_number=>create( 1 ) ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_find( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( find_text   )
                                                          ( within_text )
                                                          ( start_num   ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(find_text) = zcl_xlom__va=>to_string( arguments[ c_arg-find_text ] )->get_string( ).
        DATA(within_text) = zcl_xlom__va=>to_string( arguments[ c_arg-within_text ] )->get_string( ).
        DATA(start_num) = zcl_xlom__va=>to_number( arguments[ c_arg-start_num ] )->get_number( ).

        IF find_text IS INITIAL
            AND start_num <= strlen( within_text ) + 1.
          result = zcl_xlom__va_number=>create( start_num ).
        ELSE.
          DATA(start_offset) = start_num - 1.
          IF start_offset >= strlen( within_text ).
            result = zcl_xlom__va_error=>value_cannot_be_calculated.
          ELSE.
            DATA(result_offset) = find( val = within_text
                                        sub = find_text
                                        off = start_offset ).
            IF result_offset = -1.
              result = zcl_xlom__va_error=>value_cannot_be_calculated.
            ELSE.
              result = zcl_xlom__va_number=>create( result_offset + 1 ).
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
