CLASS zcl_xlom__ex_el_empty_arg DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    CLASS-DATA singleton TYPE REF TO zcl_xlom__ex_el_empty_arg READ-ONLY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS create
      RETURNING VALUE(result) TYPE REF TO zcl_xlom__ex_el_empty_arg.
ENDCLASS.


CLASS zcl_xlom__ex_el_empty_arg IMPLEMENTATION.
  METHOD class_constructor.
    singleton = NEW zcl_xlom__ex_el_empty_arg( ).
    singleton->zif_xlom__ex~type = zif_xlom__ex=>c_type-empty_argument.
  ENDMETHOD.

  METHOD create.
    result = singleton.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_empty=>get_singleton( ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
