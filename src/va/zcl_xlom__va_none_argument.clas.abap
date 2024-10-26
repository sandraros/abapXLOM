"! The "none" argument is useful to
"! <ol>
"! <li>simplify the handling of the arguments in the functions as all the possible arguments
"! are filled with one object eg. OFFSET has 5 parameters but the two last ones are optional, the method EVALUATE of OFFSET
"! will always receive a table of 5 arguments even if only 3 are passed OFFSET(A1,1,0).</li>
"! <li>differentiate cases like RIGHT(text,) and RIGHT(text) which mean respectively RIGHT(text,0) and RIGHT(text,1),
"! the second argument being respectively the empty and none argument.</li>
"! </ol>
"! Examples:
"! <ul>
"! <li>OFFSET(range,rows,cols) has the height and width arguments of type "none" because they have no default value.</li>
"! <li>OFFSET(range,rows,cols,,) has the height and width arguments of type "empty" because the commas are indicated and because they have no default value.</li>
"! <li>RIGHT(text,) has the num_chars argument of type "empty", it will be interpreted as the number 0.</li>
"! <li>RIGHT(text) has the num_chars argument of type "number" because its default value is the number 1.</li>
"! </ul>
CLASS zcl_xlom__va_none_argument DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_xlom__va.

    CLASS-DATA singleton TYPE REF TO zcl_xlom__va_none_argument READ-ONLY.

    CLASS-METHODS class_constructor.
ENDCLASS.


CLASS zcl_xlom__va_none_argument IMPLEMENTATION.
  METHOD class_constructor.
    singleton = NEW zcl_xlom__va_none_argument( ).
    singleton->zif_xlom__va~type = zif_xlom__va=>c_type-none_argument.
  ENDMETHOD.

  METHOD zif_xlom__va~get_value.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__va~is_array.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__va~is_boolean.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__va~is_equal.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__va~is_error.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__va~is_number.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.

  METHOD zif_xlom__va~is_string.
    RAISE EXCEPTION TYPE zcx_xlom_unexpected.
  ENDMETHOD.
ENDCLASS.
