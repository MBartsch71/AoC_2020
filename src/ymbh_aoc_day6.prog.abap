REPORT ymbh_aoc_day6.

INTERFACE if_answers.

  TYPES t_answer_count TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

  TYPES: BEGIN OF custom_declaration,
           people TYPE i,
           answer TYPE string,
         END OF custom_declaration.
  TYPES custom_declarations TYPE STANDARD TABLE OF custom_declaration WITH DEFAULT KEY.
ENDINTERFACE.

CLASS file_loader DEFINITION.
  PUBLIC SECTION.
    METHODS load_file
      IMPORTING
        i_filename      TYPE char255
      RETURNING
        VALUE(r_result) TYPE if_answers=>custom_declarations.
ENDCLASS.

CLASS file_loader IMPLEMENTATION.

  METHOD load_file.
    DATA line      TYPE string.
    DATA sumstring TYPE if_answers=>custom_declaration.

    OPEN DATASET i_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    WHILE ( sy-subrc EQ 0 ).
      READ DATASET i_filename INTO line.
      IF line IS INITIAL.
        APPEND sumstring TO r_result.
        CLEAR sumstring.
      ELSE.
        sumstring-answer = |{ sumstring-answer }{ line }|.
        sumstring-people = sumstring-people + 1.
      ENDIF.
      CLEAR line.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

CLASS answer_scanner DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS convert_input
      IMPORTING
        i_input_values   TYPE if_answers=>custom_declarations
      RETURNING
        VALUE(r_answers) TYPE if_answers=>t_answer_count.

    METHODS convert_input_alternative
      IMPORTING
        i_input_values2 TYPE if_answers=>custom_declarations
      RETURNING
        VALUE(r_result) TYPE if_answers=>t_answer_count.

    METHODS sum_up
      IMPORTING
        i_answers       TYPE if_answers=>t_answer_count
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.

    METHODS count_distinct_chars
      IMPORTING
        i_single_chars  TYPE if_answers=>custom_declarations
      RETURNING
        VALUE(r_result) TYPE i.
    METHODS count_same_answers
      IMPORTING
        i_single_chars  TYPE if_answers=>custom_declarations
      RETURNING
        VALUE(r_result) TYPE i.

ENDCLASS.

CLASS answer_scanner IMPLEMENTATION.

  METHOD convert_input.
    r_answers = VALUE #( FOR <line> IN i_input_values
                            LET single_chars = VALUE if_answers=>custom_declarations( FOR i = 0 THEN i + 1 UNTIL i = strlen( <line>-answer )
                                                                            ( answer = substring( val = <line>-answer
                                                                                                  off = i
                                                                                                  len = 1 ) ) )
                                distinct = count_distinct_chars( single_chars )
                            IN ( distinct ) ).
  ENDMETHOD.

  METHOD count_distinct_chars.
    DATA(single_chars) = i_single_chars.
    SORT single_chars BY answer ASCENDING.
    DELETE ADJACENT DUPLICATES FROM single_chars.
    r_result = lines( single_chars ).
  ENDMETHOD.

  METHOD convert_input_alternative.
    r_result = VALUE #(  FOR <line> IN i_input_values2
                       LET single_chars = VALUE if_answers=>custom_declarations( FOR i = 0 THEN i + 1 UNTIL i = strlen( <line>-answer )
                                                                          ( answer = substring( val = <line>-answer
                                                                                                off = i
                                                                                                len = 1 )
                                                                            people = <line>-people ) )
                              concurring_answers = count_same_answers( single_chars )
                         IN ( concurring_answers ) ).
  ENDMETHOD.

  METHOD count_same_answers.
    DATA concurring_answers TYPE stringtab.
    DATA(single_chars) = i_single_chars.

    SORT single_chars BY answer ASCENDING.
    LOOP AT single_chars INTO DATA(line) GROUP BY ( answer = line-answer
                                                    people = line-people
                                                    size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<group>).
      IF <group>-people = <group>-size.
        concurring_answers = VALUE #( BASE concurring_answers ( <group>-answer ) ).
      ENDIF.
    ENDLOOP.
    r_result = lines( concurring_answers ).
  ENDMETHOD.

  METHOD sum_up.
    r_result = REDUCE #( INIT sum = 0
                        FOR <line> IN i_answers
                        NEXT sum = sum + <line> ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_file_loader DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO file_loader.
    METHODS load_file_in_stringtable FOR TESTING.

ENDCLASS.

CLASS ltc_file_loader IMPLEMENTATION.

  METHOD load_file_in_stringtable.
    cut = NEW #( ).
    cl_abap_unit_assert=>assert_equals(
        exp = VALUE if_answers=>custom_declarations( ( people = 1 answer = |abc|  )
                                                     ( people = 3 answer = |abc|  )
                                                     ( people = 2 answer = |abac| )
                                                     ( people = 4 answer = |aaaa| )
                                                     ( people = 1 answer = |b|    ) )
        act = cut->load_file( '/usr/sap/tmp/input/Testinput Day 6.txt' )
        msg = |The file loader should concatenate all lines which belongs together.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_answer_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut          TYPE REF TO answer_scanner.
    DATA input_values TYPE if_answers=>custom_declarations.

    METHODS setup.

    METHODS cut_same_answer_from_anyone   FOR TESTING.
    METHODS get_summary_count_of_answers  FOR TESTING.

ENDCLASS.

CLASS ltc_answer_scanner IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_values = VALUE if_answers=>custom_declarations( ( answer = |abc|  people = 1 )
                                                          ( answer = |abc|  people = 3 )
                                                          ( answer = |abac| people = 2 )
                                                          ( answer = |aaaa| people = 4 )
                                                          ( answer = |b|    people = 1 ) ).
  ENDMETHOD.

  METHOD get_summary_count_of_answers.
    DATA(answers) = cut->convert_input( input_values ).
    cl_abap_unit_assert=>assert_equals(
        exp = 11
        act = cut->sum_up( answers ) ).
  ENDMETHOD.

  METHOD cut_same_answer_from_anyone.
    DATA(answers) = cut->convert_input_alternative( input_values ).
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = cut->sum_up( answers ) ).
  ENDMETHOD.

ENDCLASS.


PARAMETERS: filename TYPE text255.

START-OF-SELECTION.

  DATA(input_values)   = NEW file_loader( )->load_file( filename ).
  DATA(answer_scanner) = NEW answer_scanner( ).

  DATA(answers)        = answer_scanner->convert_input( input_values ).
  WRITE / |Answer part 1: { answer_scanner->sum_up( answers ) }|.

  DATA(alt_answers) = answer_scanner->convert_input_alternative( input_values ).
  WRITE / |Answer part 2: { answer_scanner->sum_up( alt_answers ) }|.
