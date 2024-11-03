"! IFS(logical_test1, value_if_true1, ...)
"! https://support.microsoft.com/en-us/office/ifs-function-36329a26-37b2-467c-972b-4a39bd951d45
CLASS zcl_xlom__ex_fu_ifs DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'IFS'.

    TYPES tt_logical_test_and_value TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING logical_test1            TYPE REF TO zif_xlom__ex
                value_if_true1           TYPE REF TO zif_xlom__ex
                logical_tests_and_values TYPE tt_logical_test_and_value
      RETURNING VALUE(result)            TYPE REF TO zcl_xlom__ex_fu_ifs.

*    METHODS zif_xlom__ex~evaluate REDEFINITION.
*    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        logical_test1            TYPE i VALUE 1,
        value_if_true1           TYPE i VALUE 2,
        logical_tests_and_values TYPE i VALUE 3,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_ifs IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'LOGICAL_TEST1           ' )
                          ( name = 'VALUE_IF_TRUE1          ' )
                          ( name = 'LOGICAL_TESTS_AND_VALUES' variadic = abap_true ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-ifs.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_ifs( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( logical_test1 )
                                                          ( value_if_true1 )
                                                          ( LINES OF logical_tests_and_values ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(number_of_pairs) = CONV f( lines( arguments ) / 2 ).
    IF frac( number_of_pairs ) <> 0.
      RAISE EXCEPTION TYPE zcx_xlom_todo
        EXPORTING
          text = 'The function IFS must be passed an even number of arguments (test1, value1, test2, value2, etc.)'.
    ENDIF.
    DATA(argument_number) = 1.
    DO number_of_pairs TIMES.
      DATA(logical_test) = zcl_xlom__va=>to_boolean( arguments[ argument_number ] )->boolean_value.
      DATA(value_if_true) = arguments[ argument_number + 1 ].
      IF logical_test = abap_true.
        result = value_if_true.
        EXIT.
      ENDIF.
      argument_number = argument_number + 2.
    ENDDO.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
