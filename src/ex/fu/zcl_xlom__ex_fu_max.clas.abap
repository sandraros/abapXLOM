"! MAX(MAX(number1, [number2], ...)
"! https://support.microsoft.com/en-us/office/max-function-e0012414-9ac8-4b34-9a47-73e662c08098
CLASS zcl_xlom__ex_fu_max DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'MAX'
                                        type = zif_xlom__ex=>c_type-function-max.

    TYPES tt_number TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    "! MAX(MAX(number1, [number2], ...)
    "! https://support.microsoft.com/en-us/office/max-function-e0012414-9ac8-4b34-9a47-73e662c08098
    "! @parameter number1       | First number
    "! @parameter numbers       | Other numbers
    "! @parameter result       | <p class="shorttext synchronized" lang="en">RESULT</p>
    CLASS-METHODS create
      IMPORTING number1       TYPE REF TO zif_xlom__ex
                numbers       TYPE tt_number OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_max.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        number1 TYPE i VALUE 1,
        numbers TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_max IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'NUMBER1' not_part_of_result_array = abap_true )
                          ( name = 'NUMBERS' not_part_of_result_array = abap_true   optional = abap_true   variadic = abap_true ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_max( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( number1 )
                                                          ( LINES OF numbers ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    DATA(number) = VALUE f( ).
    DATA(max) = VALUE f( ).
    TRY.
        LOOP AT arguments INTO DATA(argument)
             WHERE table_line <> zcl_xlom__va_none_argument=>singleton.
          CASE argument->type.
            WHEN argument->c_type-array
                OR argument->c_type-range.
              DATA(array) = CAST zif_xlom__va_array( argument ).
              DATA(row_number) = 1.
              WHILE row_number <= array->row_count.
                DATA(column_number) = 1.
                WHILE column_number <= array->column_count.
                  DATA(cell) = array->get_cell_value( column = column_number
                                                      row    = row_number ).
                  IF cell->type = cell->c_type-number.
                    number = zcl_xlom__va=>to_number( cell )->get_number( ).
                    max = nmax( val1 = max
                                val2 = number ).
                  ENDIF.
                  column_number = column_number + 1.
                ENDWHILE.
                row_number = row_number + 1.
              ENDWHILE.
            WHEN OTHERS.
              number = zcl_xlom__va=>to_number( argument )->get_number( ).
              max = nmax( val1 = max
                          val2 = number ).
          ENDCASE.
        ENDLOOP.
        result = zcl_xlom__va_number=>get( max ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
