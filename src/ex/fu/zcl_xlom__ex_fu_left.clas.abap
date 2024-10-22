"! LEFT(text,[num_chars])
"! A1=LEFT({"hello","world"},{2,3}) -> A1="he", B1="wor"
"! https://support.microsoft.com/en-us/office/left-leftb-functions-9203d2d2-7960-479b-84c6-1ea52b99640c
CLASS zcl_xlom__ex_fu_left DEFINITION
  PUBLIC FINAL
  INHERITING FROM zcl_xlom__ex_fu
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING !text         TYPE REF TO zif_xlom__ex
                num_chars     TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_left.

    METHODs zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        text      TYPE i VALUE 1,
        num_chars TYPE i VALUE 2,
      END OF c_arg.
ENDCLASS.


CLASS zcl_xlom__ex_fu_left IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-left.
    zif_xlom__ex~parameters = VALUE #( ( name = 'TEXT' )
                                       ( name = 'NUM_CHARS' default = zcl_xlom__ex_el_number=>create( 1 ) ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_left( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( text )
                                                          ( num_chars ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA left TYPE string.

    TRY.
        DATA(text) = zcl_xlom__va=>to_string( arguments[ c_arg-text ] )->get_string( ).
        DATA(result_num_chars) = arguments[ c_arg-num_chars ].
        DATA(number_num_chars) = COND #( WHEN result_num_chars       IS BOUND
                                          AND result_num_chars->type <> result_num_chars->c_type-empty THEN
                                           zcl_xlom__va=>to_number( result_num_chars )->get_number( )
                                         WHEN result_num_chars IS NOT BOUND THEN
                                           1 ).
        IF number_num_chars < 0.
          result = zcl_xlom__va_error=>value_cannot_be_calculated.
        ELSE.
          IF text = ''.
            left = ``.
          ELSE.
            DATA(len) = COND i( WHEN number_num_chars > strlen( text )
                                THEN strlen( text )
                                ELSE number_num_chars ).
            left = substring( val = text
                              off = 0
                              len = len ).
          ENDIF.
          result = zcl_xlom__va_string=>create( left ).
        ENDIF.
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
