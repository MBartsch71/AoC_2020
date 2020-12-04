REPORT ymbh_aoc_day4.

INTERFACE passport_scanner.
  TYPES: BEGIN OF s_passport_structure,
           byr           TYPE char4,
           iyr           TYPE char4,
           eyr           TYPE char4,
           hgt           TYPE char5,
           hcl           TYPE char7,
           ecl           TYPE char3,
           pid           TYPE char30,
           cid           TYPE i,
           filled_fields TYPE i,
           valid         TYPE abap_bool,
         END OF s_passport_structure.
  TYPES t_passports TYPE STANDARD TABLE OF s_passport_structure WITH DEFAULT KEY.
ENDINTERFACE.


CLASS lcl_input_processor DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS scan_input_data
      IMPORTING
        i_input                TYPE stringtab
      RETURNING
        VALUE(r_passport_data) TYPE passport_scanner=>t_passports.

    METHODS read_file
      IMPORTING
        i_filename      TYPE text255
      RETURNING
        VALUE(r_result) TYPE stringtab.

  PRIVATE SECTION.
    DATA passport_data TYPE passport_scanner=>t_passports.

    METHODS detect_input_pairs_in_row
      IMPORTING
        i_input              TYPE any
      RETURNING
        VALUE(r_input_pairs) TYPE stringtab.

    METHODS transfer_input_2_passport_line
      IMPORTING
        i_input_pairs          TYPE stringtab
        i_passport_line        TYPE passport_scanner=>s_passport_structure
      RETURNING
        VALUE(r_passport_line) TYPE passport_scanner=>s_passport_structure.

ENDCLASS.

CLASS lcl_input_processor IMPLEMENTATION.

  METHOD scan_input_data.
    DATA s_passport TYPE passport_scanner=>s_passport_structure.

    LOOP AT i_input ASSIGNING FIELD-SYMBOL(<input>).
      IF <input> IS INITIAL.
        APPEND s_passport TO r_passport_data.
        CLEAR s_passport.
        CONTINUE.
      ENDIF.

      DATA(input_pairs) = detect_input_pairs_in_row( <input> ).
      s_passport = transfer_input_2_passport_line( i_input_pairs = input_pairs
                                                   i_passport_line = s_passport ).
    ENDLOOP.

    APPEND s_passport TO r_passport_data.
  ENDMETHOD.

  METHOD detect_input_pairs_in_row.
    SPLIT i_input AT space INTO TABLE      r_input_pairs .
  ENDMETHOD.

  METHOD transfer_input_2_passport_line.
    r_passport_line = i_passport_line.
    LOOP AT i_input_pairs ASSIGNING FIELD-SYMBOL(<pair>).
      SPLIT <pair> AT ':' INTO DATA(key) DATA(value).
      ASSIGN COMPONENT key OF STRUCTURE r_passport_line TO FIELD-SYMBOL(<keyval>).
      IF <keyval> IS ASSIGNED.
        <keyval> = value.
        IF value IS NOT INITIAL.
          r_passport_line-filled_fields = r_passport_line-filled_fields + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_file.
    DATA line TYPE string.

    OPEN DATASET i_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    WHILE ( sy-subrc EQ 0 ).
      READ DATASET i_filename INTO line.
      APPEND line TO r_result.
      CLEAR line.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_passport_validator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS mark_valid
      IMPORTING
        i_passports                 TYPE passport_scanner=>t_passports
      RETURNING
        VALUE(r_validate_passports) TYPE passport_scanner=>t_passports.

    METHODS mark_extended_valid
      IMPORTING
        i_passports                 TYPE passport_scanner=>t_passports
      RETURNING
        VALUE(r_validate_passports) TYPE passport_scanner=>t_passports.

    METHODS count
      IMPORTING
        i_validate_passports TYPE passport_scanner=>t_passports
      RETURNING
        VALUE(r_valid_count) TYPE i.

  PRIVATE SECTION.

    METHODS check_validity_of_line
      IMPORTING
        i_line         TYPE passport_scanner=>s_passport_structure
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS check_extnd_validity_of_line
      IMPORTING
        i_line         TYPE passport_scanner=>s_passport_structure
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_birthdate
      IMPORTING
        i_date         TYPE char4
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_issued_year
      IMPORTING
        i_date         TYPE char4
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_expiration_year
      IMPORTING
        i_date         TYPE char4
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_height_value
      IMPORTING
        i_date         TYPE char5
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_eye_color
      IMPORTING
        i_date         TYPE char3
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_hair_color
      IMPORTING
        i_date         TYPE char7
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

    METHODS validate_pid
      IMPORTING
        i_date         TYPE char30
      RETURNING
        VALUE(r_valid) TYPE abap_bool.
    METHODS validate_filled_fields
      IMPORTING
        i_line         TYPE passport_scanner=>s_passport_structure
      RETURNING
        VALUE(r_valid) TYPE abap_bool.

ENDCLASS.

CLASS lcl_passport_validator IMPLEMENTATION.

  METHOD mark_valid.
    r_validate_passports = VALUE #( FOR <line> IN i_passports
                                        LET is_valid = check_validity_of_line( <line> )
                                        IN ( byr = <line>-byr
                                             cid = <line>-cid
                                             ecl = <line>-ecl
                                             eyr = <line>-eyr
                                             filled_fields = <line>-filled_fields
                                             hcl = <line>-hcl
                                             hgt = <line>-hgt
                                             iyr = <line>-iyr
                                             pid = <line>-pid
                                             valid = is_valid ) ).
  ENDMETHOD.

  METHOD mark_extended_valid.
    r_validate_passports = VALUE #( FOR <line> IN i_passports
                                        LET is_valid = check_extnd_validity_of_line( <line> )
                                        IN ( byr = <line>-byr
                                             cid = <line>-cid
                                             ecl = <line>-ecl
                                             eyr = <line>-eyr
                                             filled_fields = <line>-filled_fields
                                             hcl = <line>-hcl
                                             hgt = <line>-hgt
                                             iyr = <line>-iyr
                                             pid = <line>-pid
                                             valid = is_valid ) ).
  ENDMETHOD.

  METHOD check_validity_of_line.
    r_valid = xsdbool( i_line-filled_fields = 8 OR
                       ( i_line-filled_fields = 7 AND
                         i_line-cid IS INITIAL ) ).
  ENDMETHOD.

  METHOD check_extnd_validity_of_line.
    r_valid = xsdbool(   validate_filled_fields( i_line ) AND
                         validate_birthdate( i_line-byr ) AND
                         validate_expiration_year( i_line-eyr ) AND
                         validate_issued_year( i_line-iyr ) AND
                         validate_eye_color( i_line-ecl ) AND
                         validate_hair_color( i_line-hcl ) AND
                         validate_height_value( i_line-hgt ) AND
                         validate_pid( i_line-pid ) ).
  ENDMETHOD.

  METHOD count.
    r_valid_count = REDUCE #( INIT count = 0
                              FOR <line> IN i_validate_passports
                              NEXT count = COND #( WHEN <line>-valid EQ abap_true THEN count + 1
                                                   ELSE count ) ).
  ENDMETHOD.

  METHOD validate_filled_fields.
    r_valid = COND #( WHEN i_line-filled_fields = 7 AND i_line-cid IS INITIAL THEN abap_true
                      WHEN i_line-filled_fields = 8 THEN abap_true
                      ELSE abap_false ) .
  ENDMETHOD.

  METHOD validate_birthdate.
    r_valid = xsdbool( CONV i( i_date ) BETWEEN 1920 AND 2002 ).
  ENDMETHOD.

  METHOD validate_issued_year.
    r_valid = xsdbool( CONV i( i_date ) BETWEEN 2010 AND 2020 ).
  ENDMETHOD.

  METHOD validate_expiration_year.
    r_valid = xsdbool( CONV i( i_date ) BETWEEN 2020 AND 2030 ).
  ENDMETHOD.

  METHOD validate_height_value.
    DATA(in) = find( val = i_date sub = 'in' ).
    DATA(cm) = find( val = i_date sub = 'cm' ).

    DATA(length) = COND #( WHEN in > 0 THEN in
                           WHEN cm > 0 THEN cm ).

    DATA(height) = substring( val = i_date len = length ).

    r_valid = COND #( WHEN in > 0 THEN xsdbool( height BETWEEN 59 AND 76 )
                      WHEN cm > 0 THEN xsdbool( height BETWEEN 150 AND 193 ) ).
  ENDMETHOD.

  METHOD validate_eye_color.
    DATA eye_color_check TYPE RANGE OF char3.
    eye_color_check = VALUE #( ( sign = 'I' option = 'EQ' low = 'amb' )
                               ( sign = 'I' option = 'EQ' low = 'blu' )
                               ( sign = 'I' option = 'EQ' low = 'brn' )
                               ( sign = 'I' option = 'EQ' low = 'brn' )
                               ( sign = 'I' option = 'EQ' low = 'gry' )
                               ( sign = 'I' option = 'EQ' low = 'grn' )
                               ( sign = 'I' option = 'EQ' low = 'hzl' )
                               ( sign = 'I' option = 'EQ' low = 'oth' ) ).
    r_valid = xsdbool( i_date IN eye_color_check ).
  ENDMETHOD.

  METHOD validate_hair_color.
    DATA(regex) = NEW cl_abap_regex( pattern  = '^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$' ).
    DATA(matcher) = regex->create_matcher( text = i_date ).
    r_valid = xsdbool( matcher->match( ) IS NOT INITIAL ).
  ENDMETHOD.

  METHOD validate_pid.
    DATA(regex) = NEW cl_abap_regex( pattern  = '^([0-9]{9})$' ).
    DATA(matcher) = regex->create_matcher( text = i_date ).
    r_valid = xsdbool( matcher->match( ) IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_input_processor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_input_processor.
    DATA input_values TYPE stringtab.

    METHODS setup.

    METHODS read_file_in_table          FOR TESTING.
    METHODS transfer_input_to_passports FOR TESTING.

ENDCLASS.

CLASS ltc_input_processor IMPLEMENTATION.

  METHOD setup.
    cut = NEW #(  ).
    input_values = VALUE #( ( |ecl:gry pid:860033327 eyr:2020 hcl:#fffffd| )
                            ( |byr:1937 iyr:2017 cid:147 hgt:183cm| )
                            ( || )
                            ( |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884| )
                            ( |hcl:#cfa07d byr:1929| )
                            ( || )
                            ( |hcl:#ae17e1 iyr:2013| )
                            ( |eyr:2024| )
                            ( |ecl:brn pid:760753108 byr:1931| )
                            ( |hgt:179cm| )
                            ( || )
                            ( |hcl:#cfa07d eyr:2025 pid:166559648| )
                            ( |iyr:2011 ecl:brn hgt:59in| ) ).
  ENDMETHOD.

  METHOD read_file_in_table.
    DATA(expected_values) = VALUE stringtab( ( |ecl:gry pid:860033327 eyr:2020 hcl:#fffffd| )
                                             ( |byr:1937 iyr:2017 cid:147 hgt:183cm| )
                                             ( || )
                                             ( |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884| )
                                             ( |hcl:#cfa07d byr:1929| )
                                             ( || )
                                             ( |hcl:#ae17e1 iyr:2013| )
                                             ( |eyr:2024| )
                                             ( |ecl:brn pid:760753108 byr:1931| )
                                             ( |hgt:179cm| )
                                             ( || )
                                             ( |hcl:#cfa07d eyr:2025 pid:166559648| )
                                             ( |iyr:2011 ecl:brn hgt:59in| )
                                             ( || ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->read_file( |/usr/sap/tmp/input/Testinput.txt| )
        msg = |The imported data should be in a table like expected| ).
  ENDMETHOD.

  METHOD transfer_input_to_passports.

    DATA(lt_expected_values) = VALUE passport_scanner=>t_passports( ( byr           = |1937|
                                                                      iyr           = |2017|
                                                                      eyr           = |2020|
                                                                      hgt           = |183cm|
                                                                      hcl           = |#fffffd|
                                                                      ecl           = |gry|
                                                                      pid           = |860033327|
                                                                      cid           = 147
                                                                      filled_fields = 8 )

                                                                    ( byr           = |1929|
                                                                      iyr           = |2013|
                                                                      eyr           = |2023|
                                                                      hgt           = ||
                                                                      hcl           = |#cfa07d|
                                                                      ecl           = |amb|
                                                                      pid           = |028048884|
                                                                      cid           = 350
                                                                      filled_fields = 7 )

                                                                    ( byr           = |1931|
                                                                      iyr           = |2013|
                                                                      eyr           = |2024|
                                                                      hgt           = |179cm|
                                                                      hcl           = |#ae17e1|
                                                                      ecl           = |brn|
                                                                      pid           = |760753108|
                                                                      cid           = 0
                                                                      filled_fields = 7 )

                                                                    ( byr           = ||
                                                                      iyr           = |2011|
                                                                      eyr           = |2025|
                                                                      hgt           = |59in|
                                                                      hcl           = |#cfa07d|
                                                                      ecl           = |brn|
                                                                      pid           = |166559648|
                                                                      cid           = 0
                                                                      filled_fields = 6 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = lt_expected_values
        act = cut->scan_input_data( input_values )
        msg = |The passorts table should look like expected.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_passport_validator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut               TYPE REF TO lcl_passport_validator.
    DATA passports         TYPE passport_scanner=>t_passports.
    DATA valid_passports   TYPE passport_scanner=>t_passports.
    DATA invalid_passports TYPE passport_scanner=>t_passports.

    METHODS setup.

    METHODS mark_2_valid_passports       FOR TESTING.
    METHODS count_2_valid_passports      FOR TESTING.

    METHODS detect_all_invalid_passports FOR TESTING.
    METHODS detect_all_valid_passports   FOR TESTING.

ENDCLASS.


CLASS ltc_passport_validator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    passports = VALUE #( ( byr           = |1937|
                           iyr           = |2017|
                           eyr           = |2020|
                           hgt           = |183cm|
                           hcl           = |#fffffd|
                           ecl           = |gry|
                           pid           = |860033327|
                           cid           = 147
                           filled_fields = 8 )

                         ( byr           = |1929|
                           iyr           = |2013|
                           eyr           = |2023|
                           hgt           = ||
                           hcl           = |#cfa07d|
                           ecl           = |amb|
                           pid           = |028048884|
                           cid           = 350
                           filled_fields = 7 )

                         ( byr           = |1931|
                           iyr           = |2013|
                           eyr           = |2024|
                           hgt           = |179cm|
                           hcl           = |#ae17e1|
                           ecl           = |brn|
                           pid           = |760753108|
                           cid           = 0
                           filled_fields = 7 )

                         ( byr           = ||
                           iyr           = |2011|
                           eyr           = |2025|
                           hgt           = |59in|
                           hcl           = |#cfa07d|
                           ecl           = |brn|
                           pid           = |166559648|
                           cid           = 0
                           filled_fields = 6 ) ).

  ENDMETHOD.

  METHOD mark_2_valid_passports.

    DATA(expected_values) = VALUE passport_scanner=>t_passports( ( byr           = |1937|
                                                                   iyr           = |2017|
                                                                   eyr           = |2020|
                                                                   hgt           = |183cm|
                                                                   hcl           = |#fffffd|
                                                                   ecl           = |gry|
                                                                   pid           = |860033327|
                                                                   cid           = 147
                                                                   filled_fields = 8
                                                                   valid         = |X| )

                                                                 ( byr           = |1929|
                                                                   iyr           = |2013|
                                                                   eyr           = |2023|
                                                                   hgt           = ||
                                                                   hcl           = |#cfa07d|
                                                                   ecl           = |amb|
                                                                   pid           = |028048884|
                                                                   cid           = 350
                                                                   filled_fields = 7
                                                                   valid         = || )

                                                                 ( byr           = |1931|
                                                                   iyr           = |2013|
                                                                   eyr           = |2024|
                                                                   hgt           = |179cm|
                                                                   hcl           = |#ae17e1|
                                                                   ecl           = |brn|
                                                                   pid           = |760753108|
                                                                   cid           = 0
                                                                   filled_fields = 7
                                                                   valid         = |X| )

                                                                 ( byr           = ||
                                                                   iyr           = |2011|
                                                                   eyr           = |2025|
                                                                   hgt           = |59in|
                                                                   hcl           = |#cfa07d|
                                                                   ecl           = |brn|
                                                                   pid           = |166559648|
                                                                   cid           = 0
                                                                   filled_fields = 6
                                                                   valid         = || ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->mark_valid( passports )
        msg = 'msg' ).
  ENDMETHOD.

  METHOD count_2_valid_passports.
    DATA(validated_passports) = cut->mark_valid( passports ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->count( validated_passports )
        msg = 'msg' ).
  ENDMETHOD.

  METHOD detect_all_invalid_passports.
    DATA(raw_input) = VALUE stringtab( ( |eyr:1972 cid:100| )
                                       ( |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926| )
                                       ( || )
                                       ( |iyr:2019| )
                                       ( |hcl:#602927 eyr:1967 hgt:170cm| )
                                       ( |ecl:grn pid:012533040 byr:1946| )
                                       ( || )
                                       ( |hcl:dab227 iyr:2012| )
                                       ( |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277| )
                                       ( || )
                                       ( |hgt:59cm ecl:zzz| )
                                       ( |eyr:2038 hcl:74454a iyr:2023| )
                                       ( |pid:3556412378 byr:2007| ) ).

    DATA(passports)           = NEW lcl_input_processor( )->scan_input_data( raw_input ).
    DATA(validated_passports) = cut->mark_extended_valid( passports ).

    cl_abap_unit_assert=>assert_equals(
        exp = 0
        act = cut->count( validated_passports )
        msg = |There should be no valid passports.| ).

  ENDMETHOD.

  METHOD detect_all_valid_passports.
    DATA(raw_input) = VALUE stringtab( ( |pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980| )
                                       ( |hcl:#623a2f| )
                                       ( || )
                                       ( |eyr:2029 ecl:blu cid:129 byr:1989| )
                                       ( |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm| )
                                       ( || )
                                       ( |hcl:#888785| )
                                       ( |hgt:164cm byr:2001 iyr:2015 cid:88| )
                                       ( |pid:545766238 ecl:hzl| )
                                       ( |eyr:2022| )
                                       ( || )
                                       ( |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719| ) ).

    DATA(passports)     = NEW lcl_input_processor( )->scan_input_data( raw_input ).
    DATA(validated_passports) = cut->mark_extended_valid( passports ).
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->count( validated_passports )
        msg = |There should be no valid passports.| ).
  ENDMETHOD.

ENDCLASS.


PARAMETERS: filename TYPE text255.

START-OF-SELECTION.

  DATA(input_processor)     = NEW lcl_input_processor( ).
  DATA(input_values)        = input_processor->read_file( filename ).
  DATA(passports)           = input_processor->scan_input_data( input_values ).

  DATA(lo_validator)        = NEW lcl_passport_validator( ).

  DATA(validated_passports) = lo_validator->mark_valid( passports ).
  WRITE / |The result for part 1 is: { lo_validator->count( validated_passports ) }|.

  DATA(ext_valid_passports) = lo_validator->mark_extended_valid( passports ).
  WRITE / |The result for part 2 is: { lo_validator->count( ext_valid_passports ) }|.
