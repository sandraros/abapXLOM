class ZCL_XLOM__EX_EL_RANGE definition
  public
  final
  create private

  global friends ZIF_XLOM__UT_ALL_FRIENDS .

public section.

  interfaces ZIF_XLOM__UT_ALL_FRIENDS .
  interfaces ZIF_XLOM__EX .
  interfaces ZIF_XLOM__EX_ARRAY .

  class-methods CREATE
    importing
      !ADDRESS_OR_NAME type STRING
    returning
      value(RESULT) type ref to ZCL_XLOM__EX_EL_RANGE .
  PRIVATE SECTION.
    DATA _address_or_name TYPE string.
ENDCLASS.



CLASS ZCL_XLOM__EX_EL_RANGE IMPLEMENTATION.


  METHOD create.
    result = NEW zcl_xlom__ex_el_range( ).
    result->_address_or_name  = address_or_name.
    result->zif_xlom__ex~type = zif_xlom__ex=>c_type-range.
  ENDMETHOD.


  METHOD zif_xlom__ex~evaluate.
    TRY.
        result = zcl_xlom_range=>create_from_address_or_name( address     = _address_or_name
                                                              relative_to = context->worksheet ).
      CATCH zcx_xlom__va INTO DATA(error).
        result = error->result_error.
    ENDTRY.
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
