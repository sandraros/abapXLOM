"! RIGHT(text,[num_chars])
"! A1=RIGHT({"hello","world"},{2,3}) -> A1="lo", B1="rld"
"! https://support.microsoft.com/en-us/office/right-rightb-functions-240267ee-9afa-4639-a02b-f19e1786cf2f
CLASS zcl_xlom__ex_fu_right DEFINITION
  PUBLIC FINAL
  INHERITING FROM zcl_xlom__ex_fu
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'RIGHT'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !text         TYPE REF TO zif_xlom__ex
                num_chars     TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_right.

*    METHODs zif_xlom__ex~evaluate REDEFINITION.
*    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        text      TYPE i VALUE 1,
        num_chars TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_right IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'TEXT' )
                          ( name = 'NUM_CHARS' default = zcl_xlom__ex_el_number=>create( 1 ) ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-right.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_right( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( text )
                                                          ( num_chars ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA right TYPE string.

    TRY.
        DATA(text) = zcl_xlom__va=>to_string( arguments[ c_arg-TEXT ] )->get_string( ).
        DATA(result_num_chars) = arguments[ c_arg-NUM_CHARS ].
        DATA(number_num_chars) = COND #( WHEN result_num_chars       IS BOUND
                                          AND result_num_chars->type <> result_num_chars->c_type-empty THEN
                                           zcl_xlom__va=>to_number( result_num_chars )->get_number( )
                                         WHEN result_num_chars IS NOT BOUND THEN
                                           1 ).
        IF number_num_chars < 0.
          result = zcl_xlom__va_error=>value_cannot_be_calculated.
        ELSE.
          IF text = ''.
            right = ``.
          ELSE.
            DATA(off) = COND i( WHEN number_num_chars > strlen( text )
                                THEN 0
                                ELSE strlen( text ) - number_num_chars ).
            right = substring( val = text
                               off = off ).
          ENDIF.
          result = zcl_xlom__va_string=>create( right ).
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
