"! TODO
"! SUMPRODUCT(array1, [array2], [array3], ...)<br/>
"! https://support.microsoft.com/en-us/office/sumproduct-function-16753e75-9f68-4874-94ac-4d2145a2fd2e
CLASS zcl_xlom__ex_fu_sumproduct DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'SUMPRODUCT'
                                        type = zif_xlom__ex=>c_type-function-sumproduct.

    TYPES tt_array TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING array1        TYPE REF TO zif_xlom__ex
                arrays        TYPE tt_array OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_sumproduct.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        array1 TYPE i VALUE 1,
        arrays TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_sumproduct IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( not_part_of_result_array = abap_true
                          ( name = 'ARRAY1' )
                          ( name = 'ARRAYS'   optional = abap_true   variadic = abap_true ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_sumproduct( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( array1 )
                                                          ( LINES OF arrays ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
