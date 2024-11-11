"! CELL(info_type, [reference])
"! CELL("filename",A1) => https://myoffice.accenture.com/personal/xxxxxxxxxxxxxxxxxxxxx/Documents/[activity log.xlsx]Log
"! In cell B1, formula =CELL("address",A1:A6) is the same result as =CELL("address",A1), which is $A$1 in cell B1;
"! the cells B2 to B6 are not initialized with $A$2, $A$3, etc.
"! https://support.microsoft.com/en-us/office/cell-function-51bd39a5-f338-4dbe-a33f-955d67c2b2cf
CLASS zcl_xlom__ex_fu_cell DEFINITION
  PUBLIC FINAL
  INHERITING FROM zcl_xlom__ex_fu
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'CELL'
                                        type = zif_xlom__ex=>c_type-function-cell.

    CLASS-METHODS class_constructor.

    "! @parameter info_type | <ul>
    "!                        <li>"address": Reference of the first cell in reference, as text. </li>
    "!                        <li>"col": Column number of the cell in reference.</li>
    "!                        <li>"color": The value 1 if the cell is formatted in color for negative values; otherwise returns 0 (zero).
    "!                                     Note: This value is not supported in Excel for the web, Excel Mobile, and Excel Starter.</li>
    "!                        <li>"contents": Value of the upper-left cell in reference; not a formula.</li>
    "!                        <li>"filename": Filename (including full path) of the file that contains reference, as text. Returns empty text ("")
    "!                                        if the worksheet that contains reference has not yet been saved. Note: This value is not supported in Excel
    "!                                        for the web, Excel Mobile, and Excel Starter.</li>
    "!                        <li>"format": Text value corresponding to the number format of the cell. The text values for the various formats are
    "!                                      shown in the following table. Returns "-" at the end of the text value if the cell is formatted in color
    "!                                      for negative values. Returns "()" at the end of the text value if the cell is formatted with parentheses
    "!                                      for positive or all values. Note: This value is not supported in Excel for the web, Excel Mobile, and Excel Starter.</li>
    "!                        <li>"parentheses": The value 1 if the cell is formatted with parentheses for positive or all values; otherwise returns 0.
    "!                                           Note: This value is not supported in Excel for the web, Excel Mobile, and Excel Starter.</li>
    "!                        <li>"prefix": Text value corresponding to the "label prefix" of the cell. Returns single quotation mark (') if the cell
    "!                                      contains left-aligned text, double quotation mark (") if the cell contains right-aligned text, caret (^)
    "!                                      if the cell contains centered text, backslash (\) if the cell contains fill-aligned text, and empty text ("")
    "!                                      if the cell contains anything else. Note: This value is not supported in Excel for the web, Excel Mobile, and Excel Starter.</li>
    "!                        <li>"protect": The value 0 if the cell is not locked; otherwise returns 1 if the cell is locked. Note: This value is
    "!                                       not supported in Excel for the web, Excel Mobile, and Excel Starter.</li>
    "!                        <li>"row": Row number of the cell in reference.</li>
    "!                        <li>"type": Text value corresponding to the type of data in the cell. Returns "b" for blank if the cell is empty,
    "!                                    "l" for label if the cell contains a text constant, and "v" for value if the cell contains anything else.</li>
    "!                        <li>"width": Returns an array with 2 items. The 1st item in the array is the column width of the cell, rounded off to
    "!                                     an integer. Each unit of column width is equal to the width of one character in the default font size. The
    "!                                     2nd item in the array is a Boolean value, the value is TRUE if the column width is the default or FALSE if the width
    "!                                     has been explicitly set by the user. Note: This value is not supported in Excel for the web, Excel Mobile, and Excel Starter.</li>
    "!                        </ul>
    "! @parameter reference | The parameter is technically OPTIONAL but should be passed, as explained at
    "!                        https://support.microsoft.com/en-us/office/cell-function-51bd39a5-f338-4dbe-a33f-955d67c2b2cf:
    "!                        "<em>Important: Although technically reference is <strong>optional</strong>, including it in your formula is encouraged,
    "!                        unless you understand the effect its absence has on your formula result and want that effect in place.
    "!                        Omitting the reference argument does not reliably produce information about a specific cell, for the following reasons:</em>"
    "!                        <ul>
    "!                        <li>"<em>In automatic calculation mode, when a cell is modified by a user the calculation may be triggered
    "!                            before or after the selection has progressed, depending on the platform you're using for Excel.
    "!                            For example, Excel for Windows currently triggers calculation before selection changes, but Excel
    "!                            for the web triggers it afterward.</em></li>
    "!                        <li>"<em>When Co-Authoring with another user who makes an edit, this function will report your active cell rather than the editor's.</em>"</li>
    "!                        <li>"<em>Any recalculation, for instance pressing F9, will cause the function to return a new result even though no cell edit has occurred.</em>"</li>
    "!                        </ul>
    CLASS-METHODS create
      IMPORTING info_type     TYPE REF TO zcl_xlom__ex_el_string
                !reference    TYPE REF TO zcl_xlom__ex_el_range OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_cell.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        info_type  TYPE i VALUE 1,
        !reference TYPE i VALUE 2,
      END OF c_arg.

    CONSTANTS:
      BEGIN OF c_info_type,
        address  TYPE string VALUE 'address',
        filename TYPE string VALUE 'filename',
      END OF c_info_type.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_cell IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'INFO_TYPE' )
                          ( name = 'REFERENCE' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_cell( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( INFO_TYPE )
                                                          ( REFERENCE ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(info_type) = zcl_xlom__va=>to_string( arguments[ c_arg-info_type ] )->get_string( ).
        DATA(reference) = CAST zcl_xlom_range( arguments[ c_arg-reference ] ).

        CASE info_type.
          WHEN c_info_type-address.
            " Examples:
            " - In the same worksheet                 : =CELL("address",A1)        -> $A$1
            " - Another worksheet in the same workbook: =CELL("address",Sheet2!A1) -> [Book1]Sheet2!$A$1
            " In cell B1, formula =CELL("address",A1:A6) is the same result as =CELL("address",A1), which is $A$1 in cell B1;
            " the cells B2 to B6 are not initialized with $A$2, $A$3, etc.
            IF reference->parent <> context->worksheet.
              RAISE EXCEPTION TYPE zcx_xlom_todo.
            ENDIF.
            DATA(reference_address) = zcl_xlom__ext_range=>get_address( reference ).
            result = zcl_xlom__va_string=>get(
                |${ zcl_xlom__ext_range=>convert_column_number_to_a_xfd( reference_address-top_left-column ) }${ reference_address-top_left-row }| ).

          WHEN c_info_type-filename.
            " =CELL("filename",A1) -> "C:\temp\[Book1.xlsx]Sheet1"
            result = zcl_xlom__va_string=>create( context->worksheet->parent->path
                                                        && |\\[{ context->worksheet->parent->name }]|
                                                        && context->worksheet->name ).
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDCASE.
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
