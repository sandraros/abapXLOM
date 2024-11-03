"! AND(logical1,logical2,...)
"! https://support.microsoft.com/en-us/office/and-function-5f19b2e8-e1df-4408-897a-ce285a19e9d9
CLASS zcl_xlom__ex_fu_and DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom__ex_fu FINAL
  GLOBAL FRIENDS zcl_xlom__ex_fu.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex DATA VALUES name = 'AND'.

    TYPES tt_logical TYPE STANDARD TABLE OF REF TO zif_xlom__ex WITH EMPTY KEY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      IMPORTING logical1      TYPE REF TO zif_xlom__ex
                logical2      TYPE REF TO zif_xlom__ex
                logicals      TYPE tt_logical OPTIONAL
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_fu_and.

*    METHODS zif_xlom__ex~evaluate REDEFINITION.
*    METHODS zif_xlom__ex~get_parameters REDEFINITION.

  PROTECTED SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    CLASS-DATA parameters TYPE zif_xlom__ex=>tt_parameter.
ENDCLASS.


CLASS zcl_xlom__ex_fu_and IMPLEMENTATION.
  METHOD class_constructor.
    parameters = VALUE #( ( name = 'LOGICAL1' )
                          ( name = 'LOGICAL2' )
                          ( name = 'LOGICALS' variadic = abap_true optional = abap_true ) ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).
    zif_xlom__ex~type = zif_xlom__ex=>c_type-function-and.
  ENDMETHOD.

  METHOD create.
    result = NEW zcl_xlom__ex_fu_and( ).
    result->zif_xlom__ex~arguments_or_operands = VALUE #( ( logical1 )
                                                          ( logical2 )
                                                          ( LINES OF logicals ) ).
    zcl_xlom__ex_ut=>check_arguments_or_operands(
      EXPORTING expression            = result
      CHANGING  arguments_or_operands = result->zif_xlom__ex~arguments_or_operands ).
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_boolean=>true.
    LOOP AT arguments INTO DATA(argument)
        WHERE table_line <> zcl_xlom__va_none_argument=>singleton.
      IF zcl_xlom__va=>to_boolean( argument ) = zcl_xlom__va_boolean=>false.
        result = zcl_xlom__va_boolean=>false.
        EXIT.
      ENDIF.
    ENDLOOP.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    result = parameters.
  ENDMETHOD.
ENDCLASS.
