"! COUNTIF(range, criteria)
"! https://support.microsoft.com/en-us/office/countif-function-e0de10c6-f885-4e71-abb4-1f464816df34
"!
"! Escape character ~ for *, ? and ~ (source: https://stackoverflow.com/questions/39726996/excel-countif-how-to-count-characters-like)
"! * is for counting the cells containing a non-empty text
"! a* is for counting the cells containing "a" or "a" followed by any text
"! ~* is for counting the cells containing the exact text *
"! ~~~* is for counting the cells containing the exact text ~*
"!
"! Comparison is either numeric or text, not both (source: https://stackoverflow.com/questions/33358534/unexpected-countif-behavior-when-mixing-number-and-text-excel)
"! When comparing different data types (e.g. numbers with texts or booleans), the result is always false.
"!
"! If the criteria is an array, COUNTIF returns an array.
"!
"! COUNTIFS can be used to indicate several criteria grouped by AND, e.g. to count the cells whose values are between two values (COUNTIF(A:A,">=5",A:A,"<6")).
CLASS zcl_xlom__ex_fu_countif DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'COUNTIF'
                                        type = zif_xlom__ex=>c_type-function-countif.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !range        TYPE REF TO zif_xlom__ex
                criteria      TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_countif.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        range    TYPE i VALUE 1,
        criteria TYPE i VALUE 2,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_countif IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'RANGE   ' not_part_of_result_array = abap_true )
                          ( name = 'CRITERIA' ) ).
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_countif( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( range    )
                                                          ( criteria ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TYPES ty_character TYPE c LENGTH 1.

    DATA(substring) = ``.
    DATA(operator) = ``.
    DATA(operand) = ``.
    TRY.
        DATA(result_of_range) = CAST zif_xlom__va_array( arguments[ c_arg-range ] ).
        DATA(result_of_criteria) = zcl_xlom__va=>to_string( arguments[ c_arg-criteria ] )->get_string( ).
        " criteria may start with the comparison operator =, <, <=, >, >=, <>.
        " It's followed by the string to compare.
        " Anything else will be equivalent to the same string prefixed with the = comparison operator.
        IF result_of_criteria = ''.
          operator = 'CP'.
          operand = ''.
        ELSE.
          IF strlen( result_of_criteria ) >= 2.
            substring = substring( val = result_of_criteria
                                   len = 2 ).
          ENDIF.
          CASE substring.
            WHEN '<>'.
              operator = 'NP'.
              operand = substring( val = result_of_criteria
                                   off = 2 ).
            WHEN '>='
                OR '<='.
              operator = substring.
              operand = substring( val = result_of_criteria
                                   off = 2 ).
            WHEN OTHERS.
              substring = substring( val = result_of_criteria
                                     len = 1 ).
              CASE substring.
                WHEN '='.
                  operator = 'CP'.
                  operand = substring( val = result_of_criteria
                                       off = 1 ).
                WHEN '<' OR '>'.
                  operator = substring.
                  operand = substring( val = result_of_criteria
                                       off = 1 ).
                WHEN OTHERS.
                  operator = 'CP'.
                  operand = result_of_criteria.
              ENDCASE.
          ENDCASE.
        ENDIF.

        CASE operator.
          WHEN 'CP' OR 'NP'.
            " Transform the COUNTIF logical expression into an equivalent expression for the ABAP "CP" or "NP" operator.
            " *  -> *
            " #  -> ##
            " ?  -> +
            " ~? -> ? ou #?
            " +  -> #+
            " ~* -> #*
            " ~~ -> ~ ou #~
            " ~E -> ~E (any character after ~ which is different from ?, * and ~)
            DATA(offset) = 0.
            DATA(max_offset) = strlen( operand ).
            DATA(current_character) = VALUE ty_character( ).
            DATA(previous_character) = VALUE ty_character( ).
            DATA(new_operand) = VALUE string( ).
            WHILE offset < max_offset.
              current_character = substring( val = operand
                                             off = offset
                                             len = 1 ).
              IF previous_character = '~'.
                IF current_character = '*'.
                  new_operand = new_operand && '#' && current_character.
                ELSEIF current_character CA '?*~'.
                  new_operand = new_operand && current_character.
                ELSE.
                  new_operand = new_operand && '~' && current_character.
                ENDIF.
              ELSEIF current_character = '?'.
                new_operand = new_operand && '+'.
              ELSEIF current_character CA '#+'.
                new_operand = new_operand && '#' && current_character.
              ELSE.
                new_operand = new_operand && current_character.
              ENDIF.
              previous_character = current_character.
              offset = offset + 1.
            ENDWHILE.
            IF previous_character = '~'.
              new_operand = new_operand && '~'.
            ENDIF.
            operand = new_operand.
        ENDCASE.

        DATA(intersect_result) = zcl_xlom__ut=>intersect_used_range( result_of_range ).
        DATA(inside_used_range) = intersect_result-inside_used_range.

        DATA(count_cells) = 0.
        DATA(row_number) = inside_used_range-top_left-row.
        WHILE row_number <= inside_used_range-bottom_right-row.
          DATA(column_number) = inside_used_range-top_left-column.
          WHILE column_number <= inside_used_range-bottom_right-column.
            DATA(cell_value_string) = zcl_xlom__va=>to_string( result_of_range->get_cell_value(
                                                                   column = column_number
                                                                   row    = row_number ) )->get_string( ).
            IF    ( operator = 'CP' AND cell_value_string CP operand )
               OR ( operator = 'NP' AND cell_value_string NP operand )
               OR ( operator = '>'  AND cell_value_string  > operand )
               OR ( operator = '>=' AND cell_value_string >= operand )
               OR ( operator = '<'  AND cell_value_string  < operand )
               OR ( operator = '<=' AND cell_value_string <= operand ).
              count_cells = count_cells + 1.
            ENDIF.
            column_number = column_number + 1.
          ENDWHILE.
          row_number = row_number + 1.
        ENDWHILE.

        IF intersect_result-count_cells_outside_used_range >= 1.
          cell_value_string = ''.
          IF    ( operator = 'CP' AND cell_value_string CP operand )
             OR ( operator = 'NP' AND cell_value_string NP operand )
             OR ( operator = '>'  AND cell_value_string  > operand )
             OR ( operator = '>=' AND cell_value_string >= operand )
             OR ( operator = '<'  AND cell_value_string  < operand )
             OR ( operator = '<=' AND cell_value_string <= operand ).
            count_cells = count_cells + intersect_result-count_cells_outside_used_range.
          ENDIF.
        ENDIF.

        result = zcl_xlom__va_number=>create( CONV #( count_cells ) ).

      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
