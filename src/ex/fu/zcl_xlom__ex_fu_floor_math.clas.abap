"! FLOOR.MATH(number, [significance], [mode])
"! https://support.microsoft.com/en-us/office/floor-math-function-c302b599-fbdb-4177-ba19-2c2b1249a2f5
CLASS zcl_xlom__ex_fu_floor_math DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'FLOOR.MATH'
                                        type = zif_xlom__ex=>c_type-function-floor_math.

    CLASS-METHODS class_constructor.

    "! Examples:
    "! <ul>
    "! <li>FLOOR.MATH(6.7) -> 6</li>
    "! <li>FLOOR.MATH(-5.5,2,TRUE) -> -4</li>
    "! <li>FLOOR.MATH(-5.5,2,FALSE) -> 6</li>
    "! <li></li>
    "! </ul>
    "!
    "! @parameter number | Number    Required. The number to be rounded down.
    "! @parameter significance | Optional. The multiple to which you want to round. The default is 1. Currently not supported.
    "! @parameter mode | Optional. The direction (toward or away from 0) to round negative numbers. Currently not supported.
    "!                   <ul>
    "!                   <li>TRUE or not zero : rounds toward 0</li>
    "!                   <li>FALSE or zero or omitted : rounds away from 0</li>
    "!                   </ul>
    CLASS-METHODS create
      IMPORTING !number       TYPE REF TO zif_xlom__ex
                significance  TYPE REF TO zif_xlom__ex OPTIONAL
                !mode         TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_floor_math.

*    METHODS zif_xlom__ex~evaluate REDEFINITION.
*    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        number       TYPE i VALUE 1,
        significance TYPE i VALUE 2,
        mode         TYPE i VALUE 3,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_floor_math IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'NUMBER      ' )
                          ( name = 'SIGNIFICANCE' default = zcl_xlom__ex_el_number=>create( 1 ) )
                          ( name = 'MODE        ' default = zcl_xlom__ex_el_boolean=>false ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-len.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_floor_math( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( number       )
                                                          ( significance )
                                                          ( mode         ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(number) = zcl_xlom__va=>to_number( arguments[ c_arg-number ] )->get_number( ).
        DATA(significance) = zcl_xlom__va=>to_number( arguments[ c_arg-significance ] )->get_number( ).
        DATA(mode) = zcl_xlom__va=>to_boolean( arguments[ c_arg-mode ] )->boolean_value.

        IF significance <> 1.
          RAISE EXCEPTION TYPE zcx_xlom_todo EXPORTING text = 'The "Significance" argument of FLOOR.MATH is not supported (yet)'.
        ENDIF.
        IF mode <> abap_false.
          RAISE EXCEPTION TYPE zcx_xlom_todo EXPORTING text = 'The "Mode" argument of FLOOR.MATH is not supported (yet)'.
        ENDIF.

        result = zcl_xlom__va_number=>create(
                     floor( zcl_xlom__va=>to_number( arguments[ c_arg-number ] )->get_number( ) ) ).

      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
