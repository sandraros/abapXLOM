"! INDEX(array, row_num, [column_num])
"! If row_num is omitted, column_num is required.
"! If column_num is omitted, row_num is required.
"! row_num = 0 is interpreted the same way as row_num = 1. Same remark for column_num.
"! row_num < 0 or column_num < 0 lead to #VALUE!
"! row_num and column_num with values outside the array lead to #REF!
"! https://support.microsoft.com/en-us/office/index-function-a5dcf0dd-996d-40a4-a822-b56b061328bd
CLASS zcl_xlom__ex_fu_index DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING array         TYPE REF TO zif_xlom__ex
                row_num       TYPE REF TO zif_xlom__ex OPTIONAL
                column_num    TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_index.

    METHODS zif_xlom__ex~evaluate REDEFINITION.
    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        array      TYPE i VALUE 1,
        row_num    TYPE i VALUE 2,
        column_num TYPE i VALUE 3,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_index IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'ARRAY     ' not_part_of_result_array = abap_true )
                          ( name = 'ROW_NUM   ' )
                          ( name = 'COLUMN_NUM' ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-index.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_index( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( array      )
                                                          ( row_num    )
                                                          ( column_num ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(array_or_range) = zcl_xlom__va=>to_array( arguments[ c_arg-array ] ).
        DATA(row) = zcl_xlom__va=>to_number( arguments[ c_arg-row_num ] )->get_number( ).
        DATA(column) = zcl_xlom__va=>to_number( arguments[ c_arg-column_num ] )->get_number( ).
        result = array_or_range->get_cell_value( column = EXACT #( column )
                                                 row    = EXACT #( row ) ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
