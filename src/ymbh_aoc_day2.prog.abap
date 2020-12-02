REPORT ymbh_aoc_day2.

INTERFACE lif_password_scanner.
  TYPES: BEGIN OF s_password_structure,
           appearance          TYPE RANGE OF i,
           mandatory_character TYPE char1,
           password_string     TYPE char100,
           password_valid      TYPE abap_bool,
         END OF s_password_structure.
  TYPES t_password_structure  TYPE STANDARD TABLE OF s_password_structure WITH DEFAULT KEY.
  TYPES t_number_number_range TYPE RANGE OF i.
  TYPES t_char_occurences     TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDINTERFACE.

CLASS lcl_password_scanner DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        i_input TYPE stringtab..

    METHODS investigate_pwds
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_password_structure.

    METHODS investigate_official_pwds
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_password_structure.

    METHODS valid_passwords
      IMPORTING
        i_investigated_passwords TYPE lif_password_scanner=>t_password_structure
      RETURNING
        VALUE(r_result)          TYPE i.

  PRIVATE SECTION.
    DATA pwd_data TYPE lif_password_scanner=>t_password_structure.

    METHODS split_pwd_string
      IMPORTING
        i_input         TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_password_structure.

    METHODS split_line_in_fields
      IMPORTING
        i_line          TYPE string
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>s_password_structure.

    METHODS extract_numbers_to_range
      IMPORTING
        i_numbers             TYPE string
      RETURNING
        VALUE(r_number_range) TYPE lif_password_scanner=>t_number_number_range.

    METHODS count_char_in_string
      IMPORTING
        i_line          TYPE lif_password_scanner=>s_password_structure
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS find_occurences_of_mand_char
      IMPORTING
        i_line          TYPE  lif_password_scanner=>s_password_structure
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_char_occurences.

    METHODS check_char_occurences
      IMPORTING
        i_occurences    TYPE lif_password_scanner=>t_char_occurences
        i_table_line    TYPE lif_password_scanner=>s_password_structure
      RETURNING
        VALUE(r_result) TYPE abap_bool.


ENDCLASS.

CLASS lcl_password_scanner IMPLEMENTATION.

  METHOD constructor.
    pwd_data = split_pwd_string( i_input ).
  ENDMETHOD.

  METHOD investigate_pwds.
    r_result = VALUE #( FOR <line> IN pwd_data
                        LET char_count = count_char_in_string( <line> )
                            pwd_valid  = xsdbool( char_count IN <line>-appearance )
                        IN ( appearance          = <line>-appearance
                             mandatory_character = <line>-mandatory_character
                             password_string     = <line>-password_string
                             password_valid      = pwd_valid ) ).
  ENDMETHOD.

  METHOD investigate_official_pwds.
    r_result = VALUE #( FOR <line> IN pwd_data
                        LET occurences = find_occurences_of_mand_char( <line> )
                            correct_occurence = check_char_occurences( i_occurences = occurences
                                                                       i_table_line = <line> )
                            IN ( appearance          = <line>-appearance
                                 mandatory_character = <line>-mandatory_character
                                 password_string     = <line>-password_string
                                 password_valid      = correct_occurence ) ).
  ENDMETHOD.

  METHOD valid_passwords.
    r_result = REDUCE #( INIT sum = 0
                         FOR <line> IN i_investigated_passwords
                         LET value = COND #( WHEN <line>-password_valid EQ abap_true THEN 1
                                             ELSE 0 )
                         IN
                         NEXT sum = sum + value ).
  ENDMETHOD.


  METHOD split_pwd_string.
    r_result = VALUE #( FOR <line> IN i_input
                        LET line = split_line_in_fields( <line> )
                        IN ( line ) ).
  ENDMETHOD.

  METHOD split_line_in_fields.
    SPLIT i_line AT space INTO DATA(numbers) r_result-mandatory_character r_result-password_string.
    r_result-appearance = extract_numbers_to_range( numbers ).
  ENDMETHOD.

  METHOD extract_numbers_to_range.
    SPLIT i_numbers AT '-' INTO DATA(lower_number) DATA(upper_number).
    r_number_range = VALUE #( ( sign   = |I|
                                option = |BT|
                                low    = lower_number
                                high   = upper_number ) ).
  ENDMETHOD.

  METHOD count_char_in_string.
    FIND ALL OCCURRENCES OF i_line-mandatory_character IN i_line-password_string MATCH COUNT r_result.
  ENDMETHOD.

  METHOD find_occurences_of_mand_char.
    FIND ALL OCCURRENCES OF i_line-mandatory_character IN i_line-password_string RESULTS DATA(result).
    r_result = VALUE #( FOR <line> IN result
                           LET position = <line>-offset + <line>-length
                           IN ( position ) ).
  ENDMETHOD.

  METHOD check_char_occurences.
    r_result = xsdbool(  (     line_exists( i_occurences[ table_line = i_table_line-appearance[ 1 ]-low ]  )   AND
                           NOT line_exists( i_occurences[ table_line = i_table_line-appearance[ 1 ]-high ] ) )
                         OR
                         ( NOT line_exists( i_occurences[ table_line = i_table_line-appearance[ 1 ]-low ]  )   AND
                               line_exists( i_occurences[ table_line = i_table_line-appearance[ 1 ]-high ] ) ) ) .
  ENDMETHOD.

ENDCLASS.

CLASS ltc_password_policy DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut         TYPE REF TO lcl_password_scanner.
    DATA input_table TYPE stringtab.

    METHODS setup.


    METHODS count_valid_passwords          FOR TESTING.
    METHODS count_valid_official_passwords FOR TESTING.

ENDCLASS.


CLASS ltc_password_policy IMPLEMENTATION.

  METHOD setup.
    input_table = VALUE stringtab( ( |1-3 a: abcde| )
                                   ( |1-3 b: cdefg| )
                                   ( |2-9 c: ccccccccc| ) ).
    cut = NEW #( input_table ).
  ENDMETHOD.

  METHOD count_valid_passwords.
    DATA(investigated_passwords) = cut->investigate_pwds( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->valid_passwords( investigated_passwords )
        msg = |The test set should include 2 valid passwords.| ).
  ENDMETHOD.

  METHOD count_valid_official_passwords.
    DATA(investigated_passwords) = cut->investigate_official_pwds( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = cut->valid_passwords( investigated_passwords )
        msg = |The test set should include 2 valid passwords.| ).
  ENDMETHOD.

ENDCLASS.


DATA input TYPE char100.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
  DATA(lo_pwd)       = NEW lcl_password_scanner( input_values ).

  WRITE / |Result - Part 1: { lo_pwd->valid_passwords( lo_pwd->investigate_pwds( ) ) }|.
  WRITE / |Result - Part 2: { lo_pwd->valid_passwords( lo_pwd->investigate_official_pwds( ) ) }|.
