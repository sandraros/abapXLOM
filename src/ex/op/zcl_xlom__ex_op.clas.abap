CLASS zcl_xlom__ex_op DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING operator      TYPE string
                operands      TYPE zif_xlom__ex=>tt_argument_or_operand
      RETURNING VALUE(result) TYPE REF TO zif_xlom__ex.
ENDCLASS.


CLASS zcl_xlom__ex_op IMPLEMENTATION.
  METHOD create.
    DATA(unary) = xsdbool( lines( operands ) = 1 ).
    CASE operator.
      WHEN '+'.
        result = COND #( WHEN unary = abap_false
                         THEN NEW zcl_xlom__ex_op_plus( )
                         ELSE THROW zcx_xlom_todo( ) ).
      WHEN '-'.
        result = COND #( WHEN unary = abap_false
                         THEN NEW zcl_xlom__ex_op_minus( )
                         ELSE THROW zcx_xlom_todo( ) ).
      WHEN '*'.
        result = NEW zcl_xlom__ex_op_mult( ).
      WHEN '/'.
        result = NEW zcl_xlom__ex_op_divide( ).
      WHEN '='.
        result = NEW zcl_xlom__ex_op_equal( ).
      WHEN '<'.
        result = NEW zcl_xlom__ex_op_lower( ).
      WHEN '<>'.
        result = NEW zcl_xlom__ex_op_not_equal( ).
      WHEN '&'.
        result = NEW zcl_xlom__ex_op_ampersand( ).
      WHEN ':'.
        result = NEW zcl_xlom__ex_op_colon( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
