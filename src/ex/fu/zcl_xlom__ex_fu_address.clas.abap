"! ADDRESS(row_num, column_num, [abs_num], [a1], [sheet_text])
"! https://support.microsoft.com/en-us/office/address-function-d0c26c0d-3991-446b-8de4-ab46431d4f89
CLASS zcl_xlom__ex_fu_address DEFINITION
  PUBLIC FINAL
  INHERITING FROM zcl_xlom__ex_fu
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.

    CLASS-METHODS class_constructor.

    "! @parameter row_num    | Required. A numeric value that specifies the row number to use in the cell reference.
    "! @parameter column_num    | Required. A numeric value that specifies the column number to use in the cell reference.
    "! @parameter abs_num    | Optional. A numeric value that specifies the type of reference to return.
    "!                       <ul>
    "!                       <li>1 or omitted: Absolute          </li>
    "!                       <li>2: Absolute row; relative column</li>
    "!                       <li>3: Relative row; absolute column</li>
    "!                       <li>4: Relative                     </li>
    "!                       </ul>
    "! @parameter A1    | Optional. A logical value that specifies the A1 or R1C1 reference style. In A1 style, columns
    "!                    are labeled alphabetically, and rows are labeled numerically. In R1C1 reference style, both
    "!                    columns and rows are labeled numerically.
    "!                       <ul>
    "!                       <li>TRUE or omitted: the ADDRESS function returns an A1-style reference</li>
    "!                       <li>FALSE: the ADDRESS function returns an R1C1-style reference</li>
    "!                       </ul>
    "!                    Note: To change the reference style that Excel uses, click the File tab, click Options, and then click
    "!                          Formulas. Under Working with formulas, select or clear the R1C1 reference style check box.
    "! @parameter sheet_text    | Optional. A text value that specifies the name of the worksheet to be used as the external
    "!                            reference. For example, the formula =ADDRESS(1,1,,,"Sheet2") returns Sheet2!$A$1. If the
    "!                            sheet_text argument is omitted, no sheet name is used, and the address returned by the
    "!                            function refers to a cell on the current sheet.
    CLASS-METHODS create
      IMPORTING row_num       TYPE REF TO zif_xlom__ex
                column_num    TYPE REF TO zif_xlom__ex
                abs_num       TYPE REF TO zif_xlom__ex OPTIONAL
                a1            TYPE REF TO zif_xlom__ex OPTIONAL
                sheet_text    TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_address.

    METHODS zif_xlom__ex~evaluate REDEFINITION.
    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        row_num    TYPE i VALUE 1,
        column_num TYPE i VALUE 2,
        abs_num    TYPE i VALUE 3,
        a1         TYPE i VALUE 4,
        sheet_text TYPE i VALUE 5,
      END OF c_arg.

    CONSTANTS:
      BEGIN OF c_abs,
        absolute                     TYPE i VALUE 1,
        absolute_row_relative_column TYPE i VALUE 2,
        relative_row_absolute_column TYPE i VALUE 3,
        relative                     TYPE i VALUE 4,
      END OF c_abs.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_address IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'ROW_NUM   ' )
                          ( name = 'COLUMN_NUM' )
                          ( name = 'ABS_NUM   ' default = zcl_xlom__ex_el_number=>create( 1 ) )
                          ( name = 'A1        ' default = zcl_xlom__ex_el_boolean=>true )
                          ( name = 'SHEET_TEXT' default = zcl_xlom__ex_el_string=>create( '' ) ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-address.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_address( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( row_num    )
                                                          ( column_num )
                                                          ( abs_num    )
                                                          ( a1         )
                                                          ( sheet_text ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(row_num) = zcl_xlom__va=>to_number( arguments[ c_arg-row_num ] )->get_integer( ).
        DATA(column_num) = zcl_xlom__va=>to_number( arguments[ c_arg-column_num ] )->get_integer( ).
        DATA(abs_num) = zcl_xlom__va=>to_number( arguments[ c_arg-abs_num ] )->get_integer( ).
        DATA(a1) = zcl_xlom__va=>to_boolean( arguments[ c_arg-a1 ] )->boolean_value.
        DATA(sheet_text) = zcl_xlom__va=>to_string( arguments[ c_arg-sheet_text ] )->get_string( ).

        " Only A1 reference style is currently supported
        IF a1 <> abap_true.
          RAISE EXCEPTION TYPE zcx_xlom_todo EXPORTING text = 'The function ADDRESS currently only supports the A1 reference style'.
        ENDIF.

        IF    row_num    NOT BETWEEN 1 AND zcl_xlom__ext_worksheet=>max_rows
           OR column_num NOT BETWEEN 1 AND zcl_xlom__ext_worksheet=>max_columns
           OR abs_num    NOT BETWEEN c_abs-absolute AND c_abs-relative.
          result = zcl_xlom__va_error=>value_cannot_be_calculated.
          RETURN.
        ENDIF.

        DATA(sheet_and_excl_mark) = COND string( WHEN sheet_text IS NOT INITIAL THEN |{ sheet_text }!| ).
        DATA(column_prefix) = SWITCH string( abs_num
                                             WHEN c_abs-absolute OR c_abs-relative_row_absolute_column
                                             THEN '$' ).
        DATA(row_prefix) = SWITCH string( abs_num
                                          WHEN c_abs-absolute OR c_abs-absolute_row_relative_column
                                          THEN '$' ).

        result = zcl_xlom__va_string=>get(
            |{ sheet_and_excl_mark }{ column_prefix }{ zcl_xlom_range=>convert_column_number_to_a_xfd( column_num ) }{ row_prefix }{ row_num }| ).

      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
