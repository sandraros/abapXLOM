"! COLUMN([reference])
"! https://support.microsoft.com/en-us/office/column-function-44e8c754-711c-4df3-9da4-47a55042554b
CLASS zcl_xlom__ex_fu_column DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING !reference    TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_column.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        reference TYPE i VALUE 1,
      END OF c_arg.
ENDCLASS.


CLASS zcl_xlom__ex_fu_column IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-column.
    zif_xlom__ex~parameters = VALUE #( ( name = 'REFERENCE' default = zcl_xlom__ex_el_empty_argument=>singleton ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_column( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( reference ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(reference) = arguments[ c_arg-reference ].
    IF reference->type = reference->c_type-empty.
      result = zcl_xlom__va_number=>create( EXACT #( context->containing_cell-column ) ).
    ELSE.
      result = zcl_xlom__va_number=>create( ( CAST zcl_xlom_range( reference )->column ) ).
    ENDIF.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
