"! MID(text, start_num, num_chars)
"! https://support.microsoft.com/en-us/office/mid-midb-functions-d5f9e25c-d7d6-472e-b568-4ecb12433028
CLASS zcl_xlom__ex_fu_mid DEFINITION
  PUBLIC FINAL
  INHERITING FROM zcl_xlom__ex_fu
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !text         TYPE REF TO zif_xlom__ex
                start_num     TYPE REF TO zif_xlom__ex
                num_chars     TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_mid.

    METHODs zif_xlom__ex~evaluate REDEFINITION.
    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        text      TYPE i VALUE 1,
        start_num TYPE i VALUE 2,
        num_chars TYPE i VALUE 3,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_mid IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'TEXT     ' )
                          ( name = 'START_NUM' )
                          ( name = 'NUM_CHARS' ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-mid.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_mid( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( text      )
                                                          ( start_num )
                                                          ( num_chars ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA right TYPE string.

    TRY.
        DATA(text) = zcl_xlom__va=>to_string( arguments[ c_arg-text ] )->get_string( ).
        DATA(start_num) = zcl_xlom__va=>to_number( arguments[ c_arg-start_num ] )->get_integer( ).
        DATA(num_chars) = zcl_xlom__va=>to_number( arguments[ c_arg-num_chars ] )->get_integer( ).

        IF    start_num <= 0
           OR num_chars <= 0.
          result = zcl_xlom__va_error=>value_cannot_be_calculated.
        ELSE.
          result = zcl_xlom__va_string=>get( COND #( WHEN start_num                 > strlen( text )
                                                       OR start_num + num_chars - 1 > strlen( text )
                                                     THEN ``
                                                     ELSE substring( val = text
                                                                     off = start_num - 1
                                                                     len = num_chars ) ) ).
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
