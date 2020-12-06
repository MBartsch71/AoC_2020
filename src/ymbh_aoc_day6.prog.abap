REPORT ymbh_aoc_day6.

INTERFACE if_answers.
  TYPES: BEGIN OF s_answer,
           answer TYPE char1,
         END OF s_answer.
  TYPES t_answers TYPE SORTED TABLE OF s_answer WITH NON-UNIQUE KEY answer.
  TYPES t_answer_count TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDINTERFACE.

CLASS answer_scanner DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS convert_input
      IMPORTING
        i_input_values   TYPE stringtab
      RETURNING
        VALUE(r_answers) TYPE if_answers=>t_answer_count.
    METHODS sum_up
      IMPORTING
        i_answers       TYPE if_answers=>t_answer_count
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS convert_input_alternative
      IMPORTING
        i_input_values2 TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE if_answers=>t_answer_count.

  PRIVATE SECTION.
    DATA chars TYPE if_answers=>t_answers.
    DATA curated_chars TYPE if_answers=>t_answers.
    METHODS count_single_chars
      IMPORTING
        i_string TYPE string.
    METHODS count_same_answers
      IMPORTING
        i_string TYPE string.
    METHODS clean_up_answers
      IMPORTING
        i_people TYPE i.
ENDCLASS.

CLASS answer_scanner IMPLEMENTATION.

  METHOD convert_input.
    LOOP AT i_input_values INTO DATA(line).

      IF line IS INITIAL.
        APPEND lines( chars ) TO r_answers.
        CLEAR chars.
      ELSE.
        count_single_chars( line ).
      ENDIF.
    ENDLOOP.
    APPEND lines( chars ) TO r_answers.
  ENDMETHOD.


  METHOD count_single_chars.
    chars = VALUE #( BASE chars FOR i = 0 THEN i + 1 UNTIL i = strlen( i_string )
                                         ( substring( val = i_string
                                                      off = i
                                                      len = 1 ) ) ).
    DELETE ADJACENT DUPLICATES FROM chars.
  ENDMETHOD.

  METHOD sum_up.
    r_result = REDUCE #( INIT sum = 0
                        FOR <line> IN i_answers
                        NEXT sum = sum + <line> ).
  ENDMETHOD.


  METHOD convert_input_alternative.
    DATA people  TYPE i.
    LOOP AT i_input_values2 INTO DATA(line).

      IF line IS INITIAL.
        clean_up_answers( people ).
        APPEND lines( curated_chars ) TO r_result.
        CLEAR people.
        CLEAR chars.
        CLEAR curated_chars.
      ELSE.
        count_same_answers( line ).
        people = people + 1.
      ENDIF.

    ENDLOOP.
    APPEND lines( curated_chars ) TO r_result.
  ENDMETHOD.


  METHOD count_same_answers.
    chars = VALUE #( BASE chars FOR i = 0 THEN i + 1 UNTIL i = strlen( i_string )
                                        ( substring( val = i_string
                                                     off = i
                                                     len = 1 ) ) ).
  ENDMETHOD.

  METHOD clean_up_answers.
    DATA old_value TYPE string.
    DATA count TYPE i VALUE 1.
    LOOP AT chars INTO DATA(line).
      IF old_value <> line.

        IF count = i_people.
          APPEND line TO curated_chars.
        ENDIF.
        count = 1.
        old_value = line.
      ELSE.
        count = count + 1.
      ENDIF.
    ENDLOOP.
    IF count = i_people.
      APPEND line TO curated_chars.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_answer_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut           TYPE REF TO answer_scanner.
    DATA input_values  TYPE stringtab.
    DATA input_values2 TYPE stringtab.

    METHODS setup.

    METHODS put_some_answers_in_table     FOR TESTING.
    METHODS put_answers_from_more_people  FOR TESTING.
    METHODS cut_same_answer_from_anyone   FOR TESTING.
    METHODS get_summary_count_of_answers  FOR TESTING.
ENDCLASS.


CLASS ltc_answer_scanner IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_values = VALUE #( ( |abcx| )
                            ( |abcy| )
                            ( |abcz| ) ).
    input_values2 = VALUE #( ( |abc| )
                             ( || )
                             ( |a| )
                             ( |b| )
                             ( |c| )
                             ( || )
                             ( |ab| )
                             ( |ac| )
                             ( || )
                             ( |a| )
                             ( |a| )
                             ( |a| )
                             ( |a| )
                             ( || )
                             ( |b| )  ).
  ENDMETHOD.

  METHOD put_some_answers_in_table.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE if_answers=>t_answer_count( ( 6 ) )
        act = cut->convert_input( input_values )
        msg = |The input values should be converted properly.| ).
  ENDMETHOD.

  METHOD put_answers_from_more_people.
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE if_answers=>t_answer_count( ( 3 )
                                                ( 3 )
                                                ( 3 )
                                                ( 1 )
                                                ( 1 ) )
        act = cut->convert_input( input_values2 )
        msg = |The input values should be converted properly.| ).
  ENDMETHOD.

  METHOD get_summary_count_of_answers.
    DATA(answers) = cut->convert_input( input_values2 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 11
        act = cut->sum_up( answers )
        msg = 'msg' ).
  ENDMETHOD.

  METHOD cut_same_answer_from_anyone.
    DATA(answers) = cut->convert_input_alternative( input_values2 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = cut->sum_up( answers )
        msg = 'msg' ).
  ENDMETHOD.

ENDCLASS.

CLASS file_loader DEFINITION.
  PUBLIC SECTION.
    METHODS load_file
      IMPORTING
        i_filename      TYPE char255
      RETURNING
        VALUE(r_result) TYPE stringtab.
ENDCLASS.

CLASS file_loader IMPLEMENTATION.

  METHOD load_file.
    DATA line TYPE string.

    OPEN DATASET i_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    WHILE ( sy-subrc EQ 0 ).
      READ DATASET i_filename INTO line.
      APPEND line TO r_result.
      CLEAR line.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

PARAMETERS: filename TYPE text255.

START-OF-SELECTION.

  DATA(input_values) = NEW file_loader( )->load_file( filename ).
  DATA(answer_scanner) = NEW answer_scanner( ).
  DATA(answers) = answer_scanner->convert_input( input_values ).

  WRITE / |There are { answer_scanner->sum_up( answers ) }|.

  DATA(alt_answers) = answer_scanner->convert_input_alternative( input_values ).

  WRITE / |Answer part 2: { answer_scanner->sum_up( alt_answers ) }|.
