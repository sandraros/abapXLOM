"! The character &commat;, known as the "implicit intersection operator", can be used in front of either a column name
"! or a function name. Implicit intersection logic reduces many values to a single value.
"! For column names, it's mandatory inside the table and intersects with the current row eg. [@Column],
"! outside a table it's optional eg. Table1[Column] (the whole column) or Table1[@Column] (the value in the current row,
"! it returns #VALUE! if the formula is not located in a row of Table1)
"! When used in front of a function name, it's internally stored as the function _xlfn.SINGLE.
"! The character &commat; is well-suited for functions that return ranges or arrays; it's inoperative for the other functions.
"! More information:
"! <ul>
"! <li>https://support.microsoft.com/en-us/office/implicit-intersection-operator-ce3be07b-0101-4450-a24e-c1c999be2b34</li>
"! <li>https://support.microsoft.com/en-us/office/excel-functions-that-return-ranges-or-arrays-7d1970e2-cbaa-4279-b59c-b9dd3900fc69</li>
"! </ul>
CLASS zcl_xlom__ex_fu_single DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'SINGLE'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !function     TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_single.

*    METHODS zif_xlom__ex~evaluate REDEFINITION.
*    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        !function TYPE i VALUE 1,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_single IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'ANONYMOUS' not_part_of_result_array = abap_true ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-single.
  ENDMETHOD.

  METHOD create.
    " Faster than calling ZCL_XLOM__EX_UT=>CHECK_ARGUMENTS_OR_OPERANDS.
    IF function IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.
    result = NEW zcl_xlom__ex_fu_single( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( function ) ).
*    zcl_xlom__ex_ut=>check_arguments_or_operands(
*      EXPORTING expression            = result
*      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(function) = arguments[ c_arg-function ].
    CASE function->type.
      WHEN function->c_type-array
          OR function->c_type-range.
        result = CAST zif_xlom__va_array( function )->get_cell_value( column = 1
                                                                      row    = 1 ).
      WHEN OTHERS.
        result = function.
    ENDCASE.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
