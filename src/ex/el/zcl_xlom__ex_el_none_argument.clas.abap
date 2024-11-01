"! <ul>
"! <li>OFFSET(range,rows,cols) has the height and width arguments of type "none" because they have no default value.</li>
"! <li>OFFSET(range,rows,cols,,) has the height and width arguments of type "empty" because the commas are indicated and because they have no default value.</li>
"! <li>RIGHT(text,) has the num_chars argument of type "empty", it will be interpreted as the number 0.</li>
"! <li>RIGHT(text) has the num_chars argument of type "number" because its default value is the number 1.</li>
"! </ul>
CLASS zcl_xlom__ex_el_none_argument DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ex.

    CLASS-DATA singleton TYPE REF TO zcl_xlom__ex_el_none_argument READ-ONLY.

    CLASS-METHODS class_constructor.
ENDCLASS.


CLASS zcl_xlom__ex_el_none_argument IMPLEMENTATION.
  METHOD class_constructor.
    singleton = NEW zcl_xlom__ex_el_none_argument( ).
    singleton->zif_xlom__ex~type = zif_xlom__ex=>c_type-no_argument.
  ENDMETHOD.

  METHOD zif_xlom__ex~evaluate.
    " The "no-argument" is evaluated as an empty argument.
    result = zcl_xlom__va_none_argument=>singleton.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.

  METHOD zif_xlom__ex~get_parameters.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.
ENDCLASS.
