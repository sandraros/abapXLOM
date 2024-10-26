CLASS zcl_xlom__ex_el_empty_argument DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    CLASS-DATA singleton TYPE REF TO zcl_xlom__ex_el_empty_argument READ-ONLY.

    CLASS-METHODS class_constructor.
ENDCLASS.


CLASS zcl_xlom__ex_el_empty_argument IMPLEMENTATION.
  METHOD class_constructor.
    singleton = NEW zcl_xlom__ex_el_empty_argument( ).
    singleton->zif_xlom__ex~type = zif_xlom__ex=>c_type-empty_argument.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_empty=>get_singleton( ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
