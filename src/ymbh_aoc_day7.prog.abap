REPORT ymbh_aoc_day7.

INTERFACE lif_textscanner.
  TYPES: BEGIN OF bag_count,
           color    TYPE string,
           count    TYPE i,
           subcount TYPE i,
           total    TYPE i,
         END OF bag_count.
  TYPES t_bag_count TYPE STANDARD TABLE OF bag_count WITH DEFAULT KEY .
ENDINTERFACE.

CLASS textscanner DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS get_occurences
      RETURNING
        VALUE(r_occurences) TYPE i.

    METHODS constructor.
    METHODS search_string
      IMPORTING
        i_search_string TYPE string
        i_text_table    TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE stringtab.

    METHODS search_string_2
      IMPORTING
        i_search_string TYPE string
        i_text_table    TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS get_result_2nd_part
      RETURNING
        VALUE(r_bag_count) TYPE i.


  PRIVATE SECTION.
    DATA input_values TYPE stringtab.
    DATA occurences   TYPE i.
    DATA visited      TYPE RANGE OF i.
    DATA result_end   TYPE lif_textscanner=>t_bag_count.

    METHODS search_term
      IMPORTING
        i_string        TYPE string
        i_search_term   TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS search_term_2
      IMPORTING
        i_string        TYPE string
        i_search_term   TYPE string
      RETURNING
        VALUE(r_result) TYPE lif_textscanner=>t_bag_count.

    METHODS increase_occurences_by_one.
    METHODS add_line_to_visited
      IMPORTING
        i_line_nr TYPE i.

ENDCLASS.

CLASS textscanner IMPLEMENTATION.

  METHOD constructor.
    visited = VALUE #( ( sign = 'I' option = 'EQ' low = 0 ) ).
  ENDMETHOD.

  METHOD search_string.

    DATA res_table TYPE stringtab.
    LOOP AT i_text_table INTO DATA(line).
      DATA(line_idx)  = sy-tabix.
      DATA(result) = search_term( i_string      = line
                                  i_search_term = i_search_string ).
      IF result IS NOT INITIAL AND line_idx NOT IN visited.
        add_line_to_visited( line_idx ).
        increase_occurences_by_one( ).
        APPEND result TO res_table.
      ENDIF.
    ENDLOOP.

    IF res_table IS NOT INITIAL.
      LOOP AT res_table INTO DATA(res_line).
        search_string( i_search_string = res_line
                       i_text_table = i_text_table ).
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD search_string_2.
    DATA res_table TYPE lif_textscanner=>t_bag_count.
    LOOP AT i_text_table INTO DATA(line).
      DATA(result) = search_term_2( i_string      = line
                                    i_search_term = i_search_string ).
      IF result IS NOT INITIAL.
        APPEND LINES OF result TO res_table.
      ENDIF.
    ENDLOOP.
    IF res_table IS NOT INITIAL.
      LOOP AT res_table ASSIGNING FIELD-SYMBOL(<res_line>).
        <res_line>-subcount = search_string_2(  i_search_string = <res_line>-color
                                                i_text_table    = i_text_table ).
        <res_line>-total = <res_line>-total + ( <res_line>-count + ( <res_line>-count * <res_line>-subcount ) ).
      ENDLOOP.
      r_result = REDUCE #( INIT sum = 0
                     FOR <line> IN res_table
                     NEXT sum = sum + <line>-total ).
      result_end = res_table.
    ENDIF.

  ENDMETHOD.

  METHOD search_term.
    DATA(l_string) = i_string.
    FIND ALL OCCURRENCES OF i_search_term IN l_string RESULTS DATA(match_result).
    ASSIGN match_result[ 1 ] TO FIELD-SYMBOL(<match>).
    IF sy-subrc = 0.
      IF <match>-offset = 0.
      ELSE.
        DATA(tempstring) = substring( val = i_string off = 0 len = <match>-offset - 1 ).
        FIND FIRST OCCURRENCE OF 'bag' IN tempstring MATCH OFFSET DATA(offset).
        r_result = substring( val = tempstring off = 0 len = offset - 1 ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD search_term_2.
    DATA(l_string) = i_string.
    FIND ALL OCCURRENCES OF i_search_term IN l_string RESULTS DATA(match_result).
    ASSIGN match_result[ 1 ] TO FIELD-SYMBOL(<match>).
    IF sy-subrc = 0.
      IF <match>-offset = 0.
        REPLACE ALL OCCURRENCES OF |1| IN l_string WITH |-1+|.
        REPLACE ALL OCCURRENCES OF |2| IN l_string WITH |-2+|.
        REPLACE ALL OCCURRENCES OF |3| IN l_string WITH |-3+|.
        REPLACE ALL OCCURRENCES OF |4| IN l_string WITH |-4+|.
        REPLACE ALL OCCURRENCES OF |5| IN l_string WITH |-5+|.
        REPLACE ALL OCCURRENCES OF |6| IN l_string WITH |-6+|.
        REPLACE ALL OCCURRENCES OF |7| IN l_string WITH |-7+|.
        REPLACE ALL OCCURRENCES OF |8| IN l_string WITH |-8+|.
        REPLACE ALL OCCURRENCES OF |9| IN l_string WITH |-9+|.

        SPLIT l_string AT '-' INTO TABLE DATA(substrings).
        DELETE substrings INDEX 1.
        LOOP AT substrings INTO DATA(line).
          SPLIT line AT '+' INTO DATA(count) DATA(color).
          FIND FIRST OCCURRENCE OF 'bag' IN color MATCH OFFSET DATA(offset).
          DATA(col_string) = substring( val = color off = 0 len = offset - 1 ).
          CONDENSE col_string.
          r_result = VALUE #( BASE r_result ( color = col_string
                                              count = count ) ).
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD increase_occurences_by_one.
    occurences = occurences + 1.
  ENDMETHOD.

  METHOD get_occurences.
    r_occurences = occurences.
  ENDMETHOD.

  METHOD add_line_to_visited.
    visited = VALUE #( BASE visited ( sign = |I| option = |EQ| low = i_line_nr ) ).
  ENDMETHOD.

  METHOD get_result_2nd_part.
    r_bag_count = REDUCE #( INIT sum = 0
                            FOR <line> IN result_end
                            NEXT sum = sum + <line>-total ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_text_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA input_values TYPE stringtab.
    DATA cut          TYPE REF TO textscanner.

    METHODS setup.

    METHODS find_first_order_texts FOR TESTING.
    METHODS find_successor_bags    FOR TESTING.
    METHODS second_successor       FOR TESTING.


ENDCLASS.

CLASS ltc_text_scanner IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |light red bags contain 1 bright white bag, 2 muted yellow bags.| )
                            ( |dark orange bags contain 3 bright white bags, 4 muted yellow bags.| )
                            ( |bright white bags contain 1 shiny gold bag.| )
                            ( |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.| )
                            ( |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.| )
                            ( |dark olive bags contain 3 faded blue bags, 4 dotted black bags.| )
                            ( |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.| )
                            ( |faded blue bags contain no other bags.| )
                            ( |dotted black bags contain no other bags.| ) ).
    cut = NEW #( ).
  ENDMETHOD.

  METHOD find_first_order_texts.
    cut->search_string( i_search_string = |shiny gold|
                        i_text_table    = input_values ).
    cl_abap_unit_assert=>assert_equals(
            exp = 4
            act = cut->get_occurences( )
            msg = |The result should be 4| ).
  ENDMETHOD.

  METHOD find_successor_bags.
    DATA(count_table) = cut->search_string_2( i_search_string = |shiny gold|
                                              i_text_table    = input_values ).
    cl_abap_unit_assert=>assert_equals(
        exp = 32
        act = cut->get_result_2nd_part( )
        msg = |The bag count should be 32.| ).
  ENDMETHOD.

  METHOD second_successor.
    input_values = VALUE #( ( |shiny gold bags contain 2 dark red bags.| )
                            ( |dark red bags contain 2 dark orange bags.| )
                            ( |dark orange bags contain 2 dark yellow bags.| )
                            ( |dark yellow bags contain 2 dark green bags.| )
                            ( |dark green bags contain 2 dark blue bags.| )
                            ( |dark blue bags contain 2 dark violet bags.| )
                            ( |dark violet bags contain no other bags.| ) ).
    DATA(count_table) = cut->search_string_2( i_search_string = |shiny gold|
                                              i_text_table    = input_values ).
    cl_abap_unit_assert=>assert_equals(
        exp = 126
        act = cut->get_result_2nd_part( )
        msg = |The bag count should be 126.| ).
  ENDMETHOD.

ENDCLASS.


DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(lo_text_scanner) = NEW textscanner( ).

  lo_text_scanner->search_string(  i_search_string = |shiny gold| i_text_table = input_values ).
  WRITE / |The reault of part 1 is: { lo_text_scanner->get_occurences( ) }|.

  lo_text_scanner->search_string_2( i_search_string = |shiny gold| i_text_table = input_values ).
  WRITE / |The result of part2 is: { lo_text_scanner->get_result_2nd_part( ) }|.
