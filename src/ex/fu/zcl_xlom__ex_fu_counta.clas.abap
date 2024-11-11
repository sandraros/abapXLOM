"! TODO COUNTA(value1, [value2], ...)
"! https://support.microsoft.com/en-us/office/counta-function-7dc98875-d5c1-46f1-9a82-53f3219e2509
CLASS zcl_xlom__ex_fu_counta DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'COUNTA'
                                        type = zif_xlom__ex=>c_type-function-counta.

    TYPES tt_value TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING value1        TYPE REF TO zif_xlom__ex
                values        TYPE tt_value OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_counta.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        value1 TYPE i VALUE 1,
        values TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_counta IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'VALUE1' )
                          ( name = 'VALUES'   variadic = abap_true    optional = abap_true ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_counta( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( value1 )
                                                          ( LINES OF values ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
