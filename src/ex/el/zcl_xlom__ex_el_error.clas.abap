class ZCL_XLOM__EX_EL_ERROR definition
  public
  final
  create private .

public section.

  interfaces ZIF_XLOM__EX .

  types TY_ERROR_NUMBER type I .

  class-data BLOCKED type ref to ZCL_XLOM__EX_EL_ERROR read-only .
  class-data CALC type ref to ZCL_XLOM__EX_EL_ERROR read-only .
  class-data CONNECT type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #DIV/0! Is produced by =1/0
  class-data DIVISION_BY_ZERO type ref to ZCL_XLOM__EX_EL_ERROR read-only .
  class-data FIELD type ref to ZCL_XLOM__EX_EL_ERROR read-only .
  class-data GETTING_DATA type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #N/A. Is produced by =ERROR.TYPE(1) or if C1 contains =A1:A2+B1:B3 -> C3=#N/A.
  class-data NA_NOT_APPLICABLE type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #NAME! Is produced by =XXXX if XXXX is not an existing range name.
  class-data NAME type ref to ZCL_XLOM__EX_EL_ERROR read-only .
  class-data NULL type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #NUM! Is produced by =1E+240*1E+240
  class-data NUM type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! TODO #PYTHON! internal error number is not 2222, what is it?
  class-data PYTHON type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #REF! Is produced by =INDEX(A1,2,1)
  class-data REF type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #SPILL! Is produced by A1 containing ={1,2} and B1 containing a value -> A1=#SPILL!
  class-data SPILL type ref to ZCL_XLOM__EX_EL_ERROR read-only .
  class-data UNKNOWN type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! #VALUE! Is produced by =1+"a". #VALUE! in English, #VALEUR! in French.
  class-data VALUE_CANNOT_BE_CALCULATED type ref to ZCL_XLOM__EX_EL_ERROR read-only .
    "! English error name (different in other languages e.g. #VALUE! is #VALEUR! in French)
  data ENGLISH_ERROR_NAME type STRING read-only .
    "! Value of enumeration xlCVError e.g. xlErrValue = 2015 (#VALUE!)
  data INTERNAL_ERROR_NUMBER type TY_ERROR_NUMBER read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_FROM_ERROR_NAME
    importing
      !ERROR_NAME type CSEQUENCE
    returning
      value(RESULT) type ref to ZIF_XLOM__EX .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_error,
        "! English error name (different in other languages e.g. #VALUE! is #VALEUR! in French)
        english_error_name    TYPE string,
        "! Value of enumeration xlCVError e.g. xlErrValue = 2015 (#VALUE!)
        internal_error_number TYPE ty_error_number,
        object                TYPE REF TO zcl_xlom__ex_el_error,
      END OF ts_error.
    TYPES tt_error TYPE STANDARD TABLE OF ts_error WITH EMPTY KEY.

    CLASS-DATA errors TYPE tt_error.

    CLASS-METHODS create
      IMPORTING english_error_name    TYPE ts_error-english_error_name
                internal_error_number TYPE ts_error-internal_error_number
      RETURNING VALUE(result)         TYPE REF TO zcl_xlom__ex_el_error.
ENDCLASS.



CLASS ZCL_XLOM__EX_EL_ERROR IMPLEMENTATION.


  METHOD class_constructor.
    blocked                    = zcl_xlom__ex_el_error=>create( english_error_name    = '#BLOCKED!     '
                                                                internal_error_number = 2047 ).
    calc                       = zcl_xlom__ex_el_error=>create( english_error_name    = '#CALC!        '
                                                                internal_error_number = 2050 ).
    connect                    = zcl_xlom__ex_el_error=>create( english_error_name    = '#CONNECT!     '
                                                                internal_error_number = 2046 ).
    division_by_zero           = zcl_xlom__ex_el_error=>create( english_error_name    = '#DIV/0!       '
                                                                internal_error_number = 2007 ).
    field                      = zcl_xlom__ex_el_error=>create( english_error_name    = '#FIELD!       '
                                                                internal_error_number = 2049 ).
    getting_data               = zcl_xlom__ex_el_error=>create( english_error_name    = '#GETTING_DATA!'
                                                                internal_error_number = 2043 ).
    na_not_applicable          = zcl_xlom__ex_el_error=>create( english_error_name    = '#N/A          '
                                                                internal_error_number = 2042 ).
    name                       = zcl_xlom__ex_el_error=>create( english_error_name    = '#NAME?        '
                                                                internal_error_number = 2029 ).
    null                       = zcl_xlom__ex_el_error=>create( english_error_name    = '#NULL!        '
                                                                internal_error_number = 2000 ).
    num                        = zcl_xlom__ex_el_error=>create( english_error_name    = '#NUM!         '
                                                                internal_error_number = 2036 ).
    python                     = zcl_xlom__ex_el_error=>create( english_error_name    = '#PYTHON!      '
                                                                internal_error_number = 2222 ).
    ref                        = zcl_xlom__ex_el_error=>create( english_error_name    = '#REF!         '
                                                                internal_error_number = 2023 ).
    spill                      = zcl_xlom__ex_el_error=>create( english_error_name    = '#SPILL!       '
                                                                internal_error_number = 2045 ).
    unknown                    = zcl_xlom__ex_el_error=>create( english_error_name    = '#UNKNOWN!     '
                                                                internal_error_number = 2048 ).
    value_cannot_be_calculated = zcl_xlom__ex_el_error=>create( english_error_name    = '#VALUE!       '
                                                                internal_error_number = 2015 ).
  ENDMETHOD.


  METHOD create.
    result = NEW zcl_xlom__ex_el_error( ).
    result->zif_xlom__ex~type     = zif_xlom__ex=>c_type-error.
    result->english_error_name    = english_error_name.
    result->internal_error_number = internal_error_number.
    INSERT VALUE #( english_error_name    = english_error_name
                    internal_error_number = internal_error_number
                    object                = result )
           INTO TABLE errors.
  ENDMETHOD.


  METHOD get_from_error_name.
    result = errors[ english_error_name = error_name ]-object.
  ENDMETHOD.


  METHOD zif_xlom__ex~evaluate.
    result = zcl_xlom__va_error=>get_by_error_number( internal_error_number ).
    zif_xlom__ex~result_of_evaluation = result.
  ENDMETHOD.
ENDCLASS.
