CLASS zcl_xlom DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_xlom__ut_all_friends.

    "! xlApplicationInternational enumeration
    TYPES ty_application_international TYPE i.
    "! xlCalculation enumeration
    TYPES ty_calculation               TYPE i.
    "! not an Excel constant (Windows)
    TYPES ty_country                   TYPE i.
    "! xlListObjectSourceType enumeration
    TYPES ty_list_object_source_type   TYPE i.
    "! xlReferenceType enumeration
    TYPES ty_reference_style           TYPE i.
    "! XlYesNoGuess enumeration
    TYPES ty_yes_no_guess              TYPE i.

    TYPES:
      BEGIN OF ts_range_address_one_cell,
        column TYPE i,
        row    TYPE i,
      END OF ts_range_address_one_cell.

    TYPES:
      BEGIN OF ts_range_address,
        top_left     TYPE ts_range_address_one_cell,
        bottom_right TYPE ts_range_address_one_cell,
      END OF ts_range_address.

    CONSTANTS:
      BEGIN OF c_application_international,
        country_code TYPE ty_application_international VALUE 1,
      END OF c_application_international.

    CONSTANTS:
      BEGIN OF c_calculation,
        automatic     TYPE ty_calculation VALUE -4105,
        manual        TYPE ty_calculation VALUE -4135,
        semiautomatic TYPE ty_calculation VALUE 2,
      END OF c_calculation.

    CONSTANTS:
      BEGIN OF c_country,
        brazil         TYPE ty_country VALUE 55,
        czech_republic TYPE ty_country VALUE 420,
        denmark        TYPE ty_country VALUE 45,
        estonia        TYPE ty_country VALUE 372,
        finland        TYPE ty_country VALUE 358,
        france         TYPE ty_country VALUE 33,
        germany        TYPE ty_country VALUE 49,
        greece         TYPE ty_country VALUE 30,
        hungary        TYPE ty_country VALUE 36,
        indonesia      TYPE ty_country VALUE 62,
        italy          TYPE ty_country VALUE 39,
        japan          TYPE ty_country VALUE 81,
        malaysia       TYPE ty_country VALUE 60,
        netherlands    TYPE ty_country VALUE 31,
        norway         TYPE ty_country VALUE 47,
        poland         TYPE ty_country VALUE 48,
        portugal       TYPE ty_country VALUE 351,
        russia         TYPE ty_country VALUE 7,
        slovenia       TYPE ty_country VALUE 386,
        spain          TYPE ty_country VALUE 34,
        sweden         TYPE ty_country VALUE 46,
        turkey         TYPE ty_country VALUE 90,
        ukraine        TYPE ty_country VALUE 380,
        usa            TYPE ty_country VALUE 1,
      END OF c_country.

    CONSTANTS:
      "! XlListObjectSourceType enumeration
      "! https://learn.microsoft.com/en-us/office/vba/api/excel.xllistobjectsourcetype
      BEGIN OF c_list_object_source_type,
        "! xlSrcExternal    0   External data source (Microsoft SharePoint Foundation site).
        external TYPE ty_list_object_source_type VALUE 0,
        "! xlSrcModel       4   PowerPivot Model
        model    TYPE ty_list_object_source_type VALUE 4,
        "! xlSrcQuery       3   Query
        query    TYPE ty_list_object_source_type VALUE 3,
        "! xlSrcRange       1   Range
        range    TYPE ty_list_object_source_type VALUE 1,
        "! xlSrcXml         2   XML
        xml      TYPE ty_list_object_source_type VALUE 2,
      END OF c_list_object_source_type.

    CONSTANTS:
      BEGIN OF c_reference_style,
        a1    TYPE ty_reference_style VALUE 1,
        r1_c1 TYPE ty_reference_style VALUE -4150,
      END OF c_reference_style.

    CONSTANTS:
      "! XlYesNoGuess enumeration
      "! https://learn.microsoft.com/en-us/office/vba/api/excel.xlyesnoguess
      BEGIN OF c_yes_no_guess,
        "! xlGuess  0   Excel determines whether there is a header, and where it is, if there is one.
        guess TYPE ty_yes_no_guess VALUE 0,
        "! xlNo     2   Default. The entire range should be sorted.
        no    TYPE ty_yes_no_guess VALUE 2,
        "! xlYes    1   The entire range should not be sorted.
        yes   TYPE ty_yes_no_guess VALUE 1,
      END OF c_yes_no_guess.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_xlom IMPLEMENTATION.
ENDCLASS.
