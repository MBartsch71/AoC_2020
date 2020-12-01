REPORT ymbh_aoc_day1.

CLASS lcx_store DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_input_items DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

    METHODS get_next_item
      RETURNING
        VALUE(r_item) TYPE i
      RAISING
        lcx_store.

    METHODS reset_store_index.

  PRIVATE SECTION.
    DATA input_values  TYPE stringtab.
    DATA current_index TYPE i VALUE 1.

    METHODS increase_index_by_one.

ENDCLASS.

CLASS lcl_input_items IMPLEMENTATION.

  METHOD constructor.
    input_values = i_input_values.
  ENDMETHOD.

  METHOD get_next_item.
    TRY.
        r_item = input_values[ current_index ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_store.
    ENDTRY.
    increase_index_by_one( ).
  ENDMETHOD.

  METHOD increase_index_by_one.
    current_index = current_index + 1 .
  ENDMETHOD.

  METHOD reset_store_index.
    current_index = 1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_matching_items DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_matching_items TYPE stringtab.

    METHODS set_target_value
      IMPORTING
        i_target_value TYPE i.

    METHODS get_match_for
      IMPORTING
        i_number       TYPE i
      RETURNING
        VALUE(r_match) TYPE i.

  PRIVATE SECTION.
    DATA matching_items TYPE stringtab.
    DATA target_value TYPE i.

ENDCLASS.

CLASS lcl_matching_items IMPLEMENTATION.

  METHOD constructor.
    me->matching_items = i_matching_items.
  ENDMETHOD.

  METHOD set_target_value.
    target_value = i_target_value.
  ENDMETHOD.

  METHOD get_match_for.
    r_match = REDUCE #( INIT result = 0
                        FOR <number> IN matching_items
                        LET sum = i_number + CONV i( <number> )
                        IN
                        NEXT result = COND #( WHEN sum = target_value THEN <number>
                                              ELSE result ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_aoc_2020_day_1 DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS calculate_expense_total_part1
      IMPORTING
        i_input_data    TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS calculate_expense_total_part2
      IMPORTING
        i_input_data    TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    CONSTANTS c_target_value TYPE i VALUE 2020.

ENDCLASS.

CLASS lcl_aoc_2020_day_1 IMPLEMENTATION.

  METHOD calculate_expense_total_part1.
    DATA(value_store) = NEW lcl_input_items( i_input_data ).
    DATA(matcher)     = NEW lcl_matching_items( i_input_data ).
    matcher->set_target_value( c_target_value ).
    WHILE r_result IS INITIAL.
      TRY.
          DATA(item) = value_store->get_next_item( ).
        CATCH lcx_store.
          EXIT.
      ENDTRY.
      DATA(match) = matcher->get_match_for( item ).
      IF match IS NOT INITIAL.
        r_result = item * match.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD calculate_expense_total_part2.
    DATA(value_store)   = NEW lcl_input_items( i_input_data ).
    DATA(value_store_2) = NEW lcl_input_items( i_input_data ).
    DATA(matcher)       = NEW lcl_matching_items( i_input_data ).
    matcher->set_target_value( c_target_value ).

    WHILE r_result IS INITIAL.
      DATA(item1) = value_store->get_next_item( ).
      TRY.
          WHILE r_result IS INITIAL.
            DATA(item2) = value_store_2->get_next_item( ).
            DATA(sum) = item1 + item2.
            DATA(match) = matcher->get_match_for( sum ).
            IF match IS NOT INITIAL.
              r_result = item1 * item2 * match.
            ENDIF.
          ENDWHILE.
        CATCH lcx_store.
          value_store_2->reset_store_index( ).
      ENDTRY.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.



CLASS ltc_input_items DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_input_items.
    METHODS setup.

    METHODS get_first_item_from_store    FOR TESTING.
    METHODS get_third_item_from_store    FOR TESTING.
    METHODS reset_store_index            FOR TESTING.
    METHODS exception_at_end_of_store    FOR TESTING.

ENDCLASS.


CLASS ltc_input_items IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( VALUE #( ( |1721| )
                             ( |979|  )
                             ( |366|  )
                             ( |299|  )
                             ( |675|  )
                             ( |1456| ) ) ).
  ENDMETHOD.

  METHOD get_first_item_from_store.
    TRY.
        cl_abap_unit_assert=>assert_equals(
            exp = 1721
            act = mo_cut->get_next_item( )
            msg = |The first item should be 1721| ).
      CATCH lcx_store.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_third_item_from_store.
    TRY.
        mo_cut->get_next_item( ).
        mo_cut->get_next_item( ).
        cl_abap_unit_assert=>assert_equals(
            exp = 366
            act = mo_cut->get_next_item( )
            msg = |The third item should be 366| ).
      CATCH lcx_store.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.

  ENDMETHOD.

  METHOD reset_store_index.
    TRY.
        mo_cut->get_next_item( ).
        mo_cut->get_next_item( ).
        mo_cut->reset_store_index( ).
        cl_abap_unit_assert=>assert_equals(
            exp = 1721
            act = mo_cut->get_next_item( )
            msg = |After reset the first item should be returned again.| ).
      CATCH lcx_store.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD exception_at_end_of_store.
    TRY.
        DO 7 TIMES.
          mo_cut->get_next_item( ).
        ENDDO.
      CATCH lcx_store INTO DATA(lx_error).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
        act = lx_error
        msg = |Object should be bound.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_matching_items DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_matching_items.

    METHODS setup.

    METHODS get_matching_value_for_input FOR TESTING.

ENDCLASS.

CLASS ltc_matching_items IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( VALUE #( ( |1721| )
                             ( |979|  )
                             ( |366|  )
                             ( |299|  )
                             ( |675|  )
                             ( |1456| ) ) ).
  ENDMETHOD.

  METHOD get_matching_value_for_input.
    mo_cut->set_target_value( 2020 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 299
        act = mo_cut->get_match_for( 1721 )
        msg = |The result should be the match for the target value| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_aoc_day_1 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut       TYPE REF TO lcl_aoc_2020_day_1.
    DATA input_values TYPE stringtab.

    METHODS setup.
    METHODS get_result_for_testset_part1 FOR TESTING.
    METHODS get_result_for_testset_part2 FOR TESTING.
ENDCLASS.

CLASS ltc_aoc_day_1 IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
    input_values = VALUE #( ( |1721| )
                            ( |979|  )
                            ( |366|  )
                            ( |299|  )
                            ( |675|  )
                            ( |1456| ) ).
  ENDMETHOD.

  METHOD get_result_for_testset_part1.

    cl_abap_unit_assert=>assert_equals(
        exp = 514579
        act = mo_cut->calculate_expense_total_part1( input_values )
        msg = |The total should bne the expected value| ).
  ENDMETHOD.

  METHOD get_result_for_testset_part2.
    cl_abap_unit_assert=>assert_equals(
        exp = 241861950
        act = mo_cut->calculate_expense_total_part2( input_values )
        msg = |The total should be the expected value| ).
  ENDMETHOD.

ENDCLASS.

DATA input_value TYPE char200.
SELECT-OPTIONS so_input FOR input_value NO INTERVALS .

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  WRITE / |Result Part1: { NEW lcl_aoc_2020_day_1( )->calculate_expense_total_part1( input_values ) }|.
  WRITE / |Result Part2: { NEW lcl_aoc_2020_day_1( )->calculate_expense_total_part2( input_values ) }|.
