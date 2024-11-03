CLASS zcl_xlom_columns_obs DEFINITION
  PUBLIC
  INHERITING FROM zcl_xlom_range FINAL
  CREATE PRIVATE
*  GLOBAL FRIENDS
.

  PUBLIC SECTION.
    METHODS count REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom_columns_obs IMPLEMENTATION.
  METHOD count.
    result = zif_xlom__va_array~column_count.
  ENDMETHOD.
ENDCLASS.
