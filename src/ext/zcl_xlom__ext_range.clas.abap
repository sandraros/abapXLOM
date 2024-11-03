CLASS zcl_xlom__ext_range DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS convert_column_a_xfd_to_number
      IMPORTING roman_letters TYPE csequence
      RETURNING VALUE(result) TYPE i.

    CLASS-METHODS convert_column_number_to_a_xfd
      IMPORTING !number       TYPE i
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_address
      IMPORTING !range        TYPE REF TO zcl_xlom_range
      RETURNING VALUE(result) TYPE zcl_xlom=>ts_range_address.
ENDCLASS.


CLASS zcl_xlom__ext_range IMPLEMENTATION.
  METHOD convert_column_a_xfd_to_number.
    DATA(offset) = 0.
    WHILE offset < strlen( roman_letters ).
      FIND roman_letters+offset(1) IN sy-abcde MATCH OFFSET DATA(offset_a_to_z).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      ENDIF.
      result = ( result * 26 ) + offset_a_to_z + 1.
      IF result > zcl_xlom__ext_worksheet=>max_columns.
        RAISE EXCEPTION TYPE zcx_xlom_todo.
      ENDIF.
      offset = offset + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD convert_column_number_to_a_xfd.
    IF number NOT BETWEEN 1 AND zcl_xlom__ext_worksheet=>max_columns.
      RAISE EXCEPTION TYPE zcx_xlom_todo.
    ENDIF.
    DATA(work_number) = number.
    DO.
      DATA(lv_mod) = ( work_number - 1 ) MOD 26.
      DATA(lv_div) = ( work_number - 1 ) DIV 26.
      work_number = lv_div.
      result = sy-abcde+lv_mod(1) && result.
      IF work_number <= 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD get_address.
    result = range->_address.
  ENDMETHOD.
ENDCLASS.
