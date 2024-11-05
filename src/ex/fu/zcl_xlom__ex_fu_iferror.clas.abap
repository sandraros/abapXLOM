"! IFERROR(value, value_if_error)
"! IFERROR(#N/A,"1") returns "1"
"! https://support.microsoft.com/en-us/office/iferror-function-c526fd07-caeb-47b8-8bb6-63f3e417f611
CLASS zcl_xlom__ex_fu_iferror DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'IFERROR'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !value         TYPE REF TO zif_xlom__ex
                value_if_error TYPE REF TO zif_xlom__ex
      RETURNING VALUE(result)  TYPE REF TO zcl_xlom__ex_fu_iferror.

*    METHODS zif_xlom__ex~evaluate REDEFINITION.
*    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        value          TYPE i VALUE 1,
        value_if_error TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_iferror IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'VALUE         ' not_part_of_result_array = abap_true   error_accepted = abap_true )
                          ( name = 'VALUE_IF_ERROR' not_part_of_result_array = abap_true ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-iferror.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_iferror( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( value          )
                                                          ( value_if_error ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    result = COND #( LET value_result = arguments[ c_arg-value ]
                                                  IN
                     WHEN value_result->type = zif_xlom__va=>c_type-error
                     THEN arguments[ c_arg-value_if_error ]
                     ELSE value_result ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
