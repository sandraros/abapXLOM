CLASS zcl_xlom__ex_fu DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    METHODS adjust_evaluated_operands
      CHANGING evaluated_operands TYPE zif_xlom__ex=>tt_operand_result.

    CLASS-METHODS create_dynamic
      IMPORTING function_name TYPE csequence
                arguments     TYPE zif_xlom__ex=>tt_argument_or_operand
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.

    METHODS evaluate
      IMPORTING context       TYPE REF TO zcl_xlom__ex_ut_eval_context OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zif_xlom__va.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_xlom__ex_fu IMPLEMENTATION.
  METHOD adjust_evaluated_operands.
    " TO BE REDEFINED IN FUNCTIONS IF NEEDED (e.g. VLOOKUP)
  ENDMETHOD.

  METHOD create_dynamic.
    DATA function TYPE REF TO zcl_xlom__ex_fu.

    "    _xlfn.FILTER
    " OR _xlfn._xlws.FILTER
    DATA(function_name_2) = EXACT string( function_name ).
    WHILE function_name_2 CP '_xl++.*'.
      function_name_2 = substring( val = function_name_2
                                   off = 6 ).
    ENDWHILE.

    " Please indicate all functions statically. The goal is only to have an up-to-date where-used list.
    CASE function_name_2.
      WHEN 'ADDRESS'.     result = NEW zcl_xlom__ex_fu_address( ).
      WHEN 'AND'.         result = NEW zcl_xlom__ex_fu_and( ).
      WHEN 'CELL'.        result = NEW zcl_xlom__ex_fu_cell( ).
      WHEN 'CHOOSE'.      result = NEW zcl_xlom__ex_fu_choose( ).
      WHEN 'COLUMN'.      result = NEW zcl_xlom__ex_fu_column( ).
      WHEN 'CONCATENATE'. result = NEW zcl_xlom__ex_fu_concatenate( ).
      WHEN 'COUNTIF'.     result = NEW zcl_xlom__ex_fu_countif( ).
      WHEN 'FILTER'.      result = NEW zcl_xlom__ex_fu_filter( ).
      WHEN 'FIND'.        result = NEW zcl_xlom__ex_fu_find( ).
      WHEN 'FLOOR.MATH'.  result = NEW zcl_xlom__ex_fu_floor_math( ).
      WHEN 'IF'.          result = NEW zcl_xlom__ex_fu_if( ).
      WHEN 'IFERROR'.     result = NEW zcl_xlom__ex_fu_iferror( ).
      WHEN 'IFS'.         result = NEW zcl_xlom__ex_fu_ifs( ).
      WHEN 'INDEX'.       result = NEW zcl_xlom__ex_fu_index( ).
      WHEN 'INDIRECT'.    result = NEW zcl_xlom__ex_fu_indirect( ).
      WHEN 'LEFT'.        result = NEW zcl_xlom__ex_fu_left( ).
      WHEN 'LEN'.         result = NEW zcl_xlom__ex_fu_len( ).
      WHEN 'MATCH'.       result = NEW zcl_xlom__ex_fu_match( ).
      WHEN 'MID'.         result = NEW zcl_xlom__ex_fu_mid( ).
      WHEN 'MOD'.         result = NEW zcl_xlom__ex_fu_mod( ).
      WHEN 'OFFSET'.      result = NEW zcl_xlom__ex_fu_offset( ).
      WHEN 'RIGHT'.       result = NEW zcl_xlom__ex_fu_right( ).
      WHEN 'ROW'.         result = NEW zcl_xlom__ex_fu_row( ).
      WHEN 'T'.           result = NEW zcl_xlom__ex_fu_t( ).
      WHEN 'VLOOKUP'.     result = NEW zcl_xlom__ex_fu_vlookup( ).
      WHEN OTHERS.
        TRY.
            DATA(function_class_name) = |ZCL_XLOM__EX_FU_{ function_name_2 }|.
            CREATE OBJECT result TYPE (function_class_name).
          CATCH cx_root.
            RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDTRY.
    ENDCASE.

    function ?= result.
    function->zif_xlom__ex~arguments_or_operands = arguments.

    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = function->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD evaluate.
    result = zcl_xlom__ex_ut_eval=>evaluate_array_operands( expression = me
                                                            context    = context ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    RAISE EXCEPTION TYPE zcx_xlom_todo.
  ENDMETHOD.
ENDCLASS.
