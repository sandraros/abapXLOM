CLASS zcl_xlom__pv_range_calculate DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xlom_worksheet
                 zcl_xlom_range.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        formula       TYPE REF TO zif_xlom__ex
        context       TYPE REF TO zcl_xlom__ex_ut_eval_context
      RETURNING
        value(result) TYPE REF TO zif_xlom__va.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom__pv_range_calculate IMPLEMENTATION.
  METHOD run.
    TRY.
        DATA(cell_value) = zcl_xlom__ex_ut_eval=>evaluate_array_operands( expression = formula
                                                                          context    = context ).
        IF cell_value IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_xlom_todo.
        ENDIF.
        result = SWITCH #( cell_value->type
                           WHEN cell_value->c_type-array THEN
                             CAST zif_xlom__va_array( cell_value )->get_cell_value( column = 1
                                                                                    row    = 1 )
                           WHEN cell_value->c_type-range THEN
                             CAST zcl_xlom_range( cell_value )->value( )
                           ELSE
                             cell_value ).
      CATCH zcx_xlom_todo INTO DATA(todo) ##NEEDED.
*        IF 0 = 1.
*          DATA(formula_string) = zcl_xlom__ex_ut_renderer=>render( expression = formula ) ##NEEDED.
*        ENDIF.
        RAISE EXCEPTION todo.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
