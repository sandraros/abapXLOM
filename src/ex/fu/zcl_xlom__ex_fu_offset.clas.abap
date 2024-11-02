"! OFFSET(reference, rows, cols, [height], [width])
"! OFFSET($A$1,0,0,5,0) is equivalent to $A$1:$A$5
"! https://support.microsoft.com/en-us/office/offset-function-c8de19ae-dd79-4b9b-a14e-b4d906d11b66
CLASS zcl_xlom__ex_fu_offset DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING !reference    TYPE REF TO zif_xlom__ex
                !rows         TYPE REF TO zif_xlom__ex
                cols          TYPE REF TO zif_xlom__ex
                height        TYPE REF TO zif_xlom__ex OPTIONAL
                !width        TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_offset.

    METHODS zif_xlom__ex~evaluate REDEFINITION.
    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_arg,
        reference TYPE i VALUE 1,
        rows      TYPE i VALUE 2,
        cols      TYPE i VALUE 3,
        height    TYPE i VALUE 4,
        width     TYPE i VALUE 5,
      END OF c_arg.

    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_offset IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'REFERENCE' not_part_of_result_array = abap_true )
                          ( name = 'ROWS     ' )
                          ( name = 'COLS     ' default = zcl_xlom__ex_el_number=>create( 1 ) )
                          ( name = 'HEIGHT   ' optional = abap_true )
                          ( name = 'WIDTH    ' optional = abap_true ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-offset.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_offset( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( reference )
                                                          ( rows      )
                                                          ( cols      )
                                                          ( height    )
                                                          ( width     ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    TRY.
        DATA(reference) = CAST zcl_xlom_range( arguments[ c_arg-reference ] ).
        DATA(rows) = zcl_xlom__va=>to_number( arguments[ c_arg-rows ] )->get_integer( ).
        DATA(cols) = zcl_xlom__va=>to_number( arguments[ c_arg-cols ] )->get_integer( ).
        DATA(height) = arguments[ c_arg-height ].
        DATA(width) = arguments[ c_arg-width ].

        DATA(adjusted_height) = COND #( WHEN height <> zcl_xlom__va_none_argument=>singleton
                                       and height <> zcl_xlom__va_empty=>get_singleton( )
                                      THEN zcl_xlom__va=>to_number( arguments[ c_arg-height ] )->get_integer( )
                                      ELSE reference->zif_xlom__va_array~row_count ).
        DATA(adjusted_width) = COND #( WHEN width <> zcl_xlom__va_none_argument=>singleton
                                      and width <> zcl_xlom__va_empty=>get_singleton( )
                                     THEN zcl_xlom__va=>to_number( arguments[ c_arg-width ] )->get_integer( )
                                     ELSE reference->zif_xlom__va_array~column_count ).

        IF    adjusted_height = 0
           OR adjusted_width  = 0.
          result = zcl_xlom__va_error=>ref.
        ELSE.
          result = reference->offset( row_offset    = rows
                                      column_offset = cols
                                           )->resize( row_size    = adjusted_height
                                                      column_size = adjusted_width ).
        ENDIF.

      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
