"! OFFSET(reference, rows, cols, [height], [width])
"! OFFSET($A$1,0,0,5,0) is equivalent to $A$1:$A$5
"! https://support.microsoft.com/en-us/office/offset-function-c8de19ae-dd79-4b9b-a14e-b4d906d11b66
CLASS zcl_xlom__ex_fu_offset DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING !reference    TYPE REF TO zif_xlom__ex
                !rows         TYPE REF TO zif_xlom__ex
                cols          TYPE REF TO zif_xlom__ex
                height        TYPE REF TO zif_xlom__ex OPTIONAL
                !width        TYPE REF TO zif_xlom__ex OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_offset.

    METHODS zif_xlom__ex~evaluate REDEFINITION.

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
ENDCLASS.


CLASS zcl_xlom__ex_fu_offset IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-offset.
    zif_xlom__ex~parameters = VALUE #( ( name = 'REFERENCE' not_part_of_result_array = abap_true )
                                       ( name = 'ROWS     ' )
                                       ( name = 'COLS     ' default = zcl_xlom__ex_el_number=>create( 1 ) )
                                       ( name = 'HEIGHT   ' default = zcl_xlom__ex_el_empty_arg=>singleton )
                                       ( name = 'WIDTH    ' default = zcl_xlom__ex_el_empty_arg=>singleton ) ).
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
        DATA(rows_result) = zcl_xlom__va=>to_number( arguments[ c_arg-rows ] )->get_integer( ).
        DATA(cols_result) = zcl_xlom__va=>to_number( arguments[ c_arg-cols ] )->get_integer( ).
        DATA(reference_result) = CAST zcl_xlom_range( arguments[ c_arg-reference ] ).
        DATA(height) = arguments[ c_arg-height ].
        DATA(width) = arguments[ c_arg-width ].
        DATA(height_result) = COND #( WHEN height       IS BOUND
                                       AND height->type <> height->c_type-empty
                                      THEN zcl_xlom__va=>to_number( arguments[ c_arg-height ] )->get_integer( )
                                      ELSE reference_result->rows( )->count( ) ).
        DATA(width_result) = COND #( WHEN width       IS BOUND
                                      AND width->type <> width->c_type-empty
                                     THEN zcl_xlom__va=>to_number( arguments[ c_arg-width ] )->get_integer( )
                                     ELSE reference_result->columns( )->count( ) ).
        result = reference_result->offset( row_offset    = rows_result
                                           column_offset = cols_result
                                         )->resize( row_size    = height_result
                                                    column_size = width_result ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
