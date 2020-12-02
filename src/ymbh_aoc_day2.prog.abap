REPORT ymbh_aoc_day2.

INTERFACE lif_password_scanner.
  TYPES: BEGIN OF s_password_structure,
           appearance          TYPE RANGE OF i,
           mandatory_character TYPE char1,
           password_string     TYPE char100,
           password_valid      TYPE abap_bool,
         END OF s_password_structure.
  TYPES: t_password_structure TYPE STANDARD TABLE OF s_password_structure WITH DEFAULT KEY.
ENDINTERFACE.

CLASS lcl_password_scanner DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS split_pwd_string
      IMPORTING
        i_input         TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_password_structure.

    METHODS investigate_pwds
      IMPORTING
        input_curated   TYPE lif_password_scanner=>t_password_structure
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_password_structure.

    METHODS investigate_official_pwds
      IMPORTING
        input_curated   TYPE lif_password_scanner=>t_password_structure
      RETURNING
        VALUE(r_result) TYPE lif_password_scanner=>t_password_structure.

    METHODS valid_passwords
      IMPORTING
        i_investigated_passwords TYPE lif_password_scanner=>t_password_structure
      RETURNING
        VALUE(r_result)          TYPE i.

ENDCLASS.

CLASS lcl_password_scanner IMPLEMENTATION.

  METHOD split_pwd_string.
    DATA: ls_pwd_line TYPE lif_password_scanner=>s_password_structure.

    LOOP AT i_input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT space INTO DATA(numbers) ls_pwd_line-mandatory_character ls_pwd_line-password_string.
      SPLIT numbers AT '-' INTO DATA(lv_low) DATA(lv_high).
      ls_pwd_line-appearance = VALUE #( ( sign = 'I'
                                          option = 'BT'
                                          low = lv_low
                                          high = lv_high ) ).
      APPEND ls_pwd_line TO r_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD investigate_pwds.
    LOOP AT input_curated ASSIGNING FIELD-SYMBOL(<input>).
      FIND ALL OCCURRENCES OF <input>-mandatory_character IN <input>-password_string MATCH COUNT DATA(char_count).
      IF char_count IN <input>-appearance.
        r_result = VALUE #( BASE r_result ( appearance = <input>-appearance
                                            mandatory_character = <input>-mandatory_character
                                            password_string = <input>-password_string
                                            password_valid = abap_true ) ).
      ELSE.
        r_result = VALUE #( BASE r_result ( <input> ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD investigate_official_pwds.
    LOOP AT input_curated ASSIGNING FIELD-SYMBOL(<input>).
      FIND ALL OCCURRENCES OF <input>-mandatory_character IN <input>-password_string RESULTS DATA(result).


      IF line_exists( result[ offset = <input>-appearance[ 1 ]-low - 1 ] ) AND
         NOT line_exists( result[ offset = <input>-appearance[ 1 ]-high - 1 ] ).
        r_result = VALUE #( BASE r_result ( appearance = <input>-appearance
                                            mandatory_character = <input>-mandatory_character
                                            password_string = <input>-password_string
                                            password_valid = abap_true ) ).
      ELSEIF  NOT line_exists( result[ offset = <input>-appearance[ 1 ]-low - 1 ] ) AND
         line_exists( result[ offset = <input>-appearance[ 1 ]-high - 1 ] ).
        r_result = VALUE #( BASE r_result ( appearance = <input>-appearance
                                            mandatory_character = <input>-mandatory_character
                                            password_string = <input>-password_string
                                            password_valid = abap_true ) )..

      ELSE.
        r_result = VALUE #( BASE r_result ( <input> ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD valid_passwords.
    r_result = REDUCE #( INIT sum = 0
                         FOR <line> IN i_investigated_passwords
                         LET value = COND #( WHEN <line>-password_valid EQ abap_true THEN 1
                                             ELSE 0 )
                         IN
                         NEXT sum = sum + value ).
  ENDMETHOD.


ENDCLASS.

CLASS ltc_password_policy DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut         TYPE REF TO lcl_password_scanner.
    DATA input_table TYPE stringtab.

    METHODS setup.
    METHODS split_input_in_pwd_and_policy FOR TESTING.
    METHODS investigate_passwords         FOR TESTING.
    METHODS count_valid_passwords         FOR TESTING.
    METHODS investigate_offical_passwords FOR TESTING.
ENDCLASS.


CLASS ltc_password_policy IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_table = VALUE stringtab( ( |1-3 a: abcde| )
                                   ( |1-3 b: cdefg| )
                                   ( |2-9 c: ccccccccc| ) ).
  ENDMETHOD.

  METHOD split_input_in_pwd_and_policy.


    DATA(lt_password_structure) =
         VALUE lif_password_scanner=>t_password_structure( ( appearance          = VALUE #( ( sign   = |I|
                                                                                              option = |BT|
                                                                                              low    = 1
                                                                                              high   = 3 ) )
                                                             mandatory_character = |a:|
                                                             password_string     = |abcde| )

                                                           ( appearance          = VALUE #( ( sign   = |I|
                                                                                              option = |BT|
                                                                                              low    = 1
                                                                                              high   = 3 ) )
                                                             mandatory_character = |b:|
                                                             password_string     = |cdefg| )

                                                           ( appearance          = VALUE #( ( sign   = |I|
                                                                                              option = |BT|
                                                                                              low    = 2
                                                                                              high   = 9 ) )
                                                             mandatory_character = |c:|
                                                             password_string     = |ccccccccc| ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_password_structure
        act = cut->split_pwd_string( input_table )
        msg = |The passwords should be broken down to the structure| ).
  ENDMETHOD.



  METHOD investigate_passwords.
    DATA(pwds_investigated) = VALUE lif_password_scanner=>t_password_structure( ( appearance          = VALUE #( ( sign   = |I|
                                                                                                                   option = |BT|
                                                                                                                   low    = 1
                                                                                                                   high   = 3 ) )
                                                                                  mandatory_character = |a|
                                                                                  password_string     = |abcde|
                                                                                  password_valid      = abap_true )

                                                                                ( appearance          = VALUE #( ( sign   = |I|
                                                                                                                   option = |BT|
                                                                                                                   low    = 1
                                                                                                                   high   = 3 ) )
                                                                                  mandatory_character = |b|
                                                                                  password_string     = |cdefg|
                                                                                  password_valid      = abap_false )

                                                                                ( appearance          = VALUE #( ( sign   = |I|
                                                                                                                   option = |BT|
                                                                                                                   low    = 2
                                                                                                                   high   = 9 ) )
                                                                                  mandatory_character = |c|
                                                                                  password_string     = |ccccccccc|
                                                                                  password_valid      = abap_true )

                                                                                 ).
    DATA(input_curated) = cut->split_pwd_string( input_table ).
    cl_abap_unit_assert=>assert_equals(
        exp = pwds_investigated
        act = cut->investigate_pwds( input_curated )
        msg = |The passwords should be investigated like expected!| ).
  ENDMETHOD.

  METHOD investigate_offical_passwords.
    DATA(pwds_investigated) = VALUE lif_password_scanner=>t_password_structure( ( appearance          = VALUE #( ( sign   = |I|
                                                                                                                   option = |BT|
                                                                                                                   low    = 1
                                                                                                                   high   = 3 ) )
                                                                                  mandatory_character = |a|
                                                                                  password_string     = |abcde|
                                                                                  password_valid      = abap_true )

                                                                                ( appearance          = VALUE #( ( sign   = |I|
                                                                                                                   option = |BT|
                                                                                                                   low    = 1
                                                                                                                   high   = 3 ) )
                                                                                  mandatory_character = |b|
                                                                                  password_string     = |cdefg|
                                                                                  password_valid      = abap_false )

                                                                                ( appearance          = VALUE #( ( sign   = |I|
                                                                                                                   option = |BT|
                                                                                                                   low    = 2
                                                                                                                   high   = 9 ) )
                                                                                  mandatory_character = |c|
                                                                                  password_string     = |ccccccccc|
                                                                                  password_valid      = abap_false )

                                                                                 ).
    DATA(input_curated) = cut->split_pwd_string( input_table ).
    cl_abap_unit_assert=>assert_equals(
        exp = pwds_investigated
        act = cut->investigate_official_pwds( input_curated )
        msg = |The passwords should be investigated like expected!| ).
  ENDMETHOD.

  METHOD count_valid_passwords.
    DATA(input_curated) = cut->split_pwd_string( input_table ).
    DATA(investigated_passwords) = cut->investigate_pwds( input_curated ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->valid_passwords( investigated_passwords )
        msg = |The test set should include 2 valid passwords.| ).
  ENDMETHOD.

ENDCLASS.





DATA input TYPE char100.
SELECT-OPTIONS: so_input FOR input.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
  DATA(lo_pwd)          = NEW lcl_password_scanner( ).
  DATA(curated_pwds)    = lo_pwd->split_pwd_string( input_values ).
  DATA(investigated_passwords) = lo_pwd->investigate_pwds( curated_pwds ).

  WRITE / |Result - Part 1: { lo_pwd->valid_passwords( investigated_passwords ) }|.

  DATA(investigated_off_passwords) = lo_pwd->investigate_official_pwds( curated_pwds ).

  WRITE / |Result - Part 2: { lo_pwd->valid_passwords( investigated_off_passwords ) }|.
