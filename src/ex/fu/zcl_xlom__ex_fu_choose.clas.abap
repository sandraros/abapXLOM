"! CHOOSE(index_num, value1, [value2], ...)
"! https://support.microsoft.com/en-us/office/choose-function-fc5c184f-cb62-4ec7-a46e-38653b98f5bc
CLASS zcl_xlom__ex_fu_choose DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    TYPES tt_value TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS create
      IMPORTING index_num     TYPE REF TO zif_xlom__ex
                value1        TYPE REF TO zif_xlom__ex
                !values       TYPE tt_value OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_choose.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        index_num TYPE i VALUE 1,
        value1    TYPE i VALUE 2,
      END OF c_arg.
ENDCLASS.


CLASS zcl_xlom__ex_fu_choose IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-choose.
    zif_xlom__ex~parameters = VALUE #( ( name = 'INDEX_NUM' )
                                       ( name = 'VALUE1   ' )
                                       ( name = 'VALUES   ' variadic = abap_true ) ).
*    zif_xlom__ex~endless_number_of_parameters = abap_true.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_choose( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( index_num )
                                                          ( value1    )
                                                          ( LINES OF values ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(index_num) = zcl_xlom__va=>to_number( arguments[ c_arg-index_num ] )->get_integer( ).
    IF index_num + 1 BETWEEN 2 AND lines( arguments ).
      result = arguments[ index_num + 1 ].
    ELSE.
      result = zcl_xlom__va_error=>value_cannot_be_calculated.
    ENDIF.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
