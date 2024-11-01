"! ROW([reference])
"! https://support.microsoft.com/en-us/office/row-function-3a63b74a-c4d0-4093-b49a-e76eb49a6d8d
CLASS zcl_xlom__ex_fu_row DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !reference    TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_row.

    METHODS zif_xlom__ex~evaluate REDEFINITION.
    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        reference TYPE i VALUE 1,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_row IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'REFERENCE' default = zcl_xlom__ex_el_empty_argument=>singleton ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-row.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_row( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( reference ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(reference) = arguments[ c_arg-reference ].
    IF reference->type = reference->c_type-empty.
      result = zcl_xlom__va_number=>create( EXACT #( context->containing_cell-row ) ).
    ELSE.
      result = zcl_xlom__va_number=>create( ( CAST zcl_xlom_range( reference )->row ) ).
    ENDIF.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
