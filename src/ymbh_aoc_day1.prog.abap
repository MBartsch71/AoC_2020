REPORT ymbh_aoc_day1.

CLASS lcl_input_items DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

    METHODS get_input_values
      RETURNING
        VALUE(r_input_value) TYPE stringtab.

    METHODS get_next_item
      RETURNING
        VALUE(r_item) TYPE i.

  PRIVATE SECTION.
    DATA input_values TYPE stringtab.
    DATA current_index TYPE i VALUE 1.
    METHODS increase_index_by_one.

ENDCLASS.

CLASS lcl_input_items IMPLEMENTATION.

  METHOD constructor.
    input_values = i_input_values.
  ENDMETHOD.

  METHOD get_input_values.
    r_input_value = input_values.
  ENDMETHOD.

  METHOD get_next_item.
    r_item = input_values[ current_index ].
    increase_index_by_one( ).
  ENDMETHOD.

  METHOD increase_index_by_one.
    current_index = current_index + 1 .
  ENDMETHOD.

ENDCLASS.

CLASS ltc_input_items DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_input_items.
    METHODS setup.

    METHODS build_input_value_object     FOR TESTING.
    METHODS insert_input_values_in_store FOR TESTING.
    METHODS get_first_item_from_store    FOR TESTING.
    METHODS get_third_item_from_store    FOR TESTING.
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

  METHOD build_input_value_object.
    cl_abap_unit_assert=>assert_bound(
        act = mo_cut
        msg = |Object should be bound.| ).
  ENDMETHOD.

  METHOD insert_input_values_in_store.
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = lines( mo_cut->get_input_values( ) )
        msg = |The store should have 6 lines| ).
  ENDMETHOD.

  METHOD get_first_item_from_store.
    cl_abap_unit_assert=>assert_equals(
        exp = 1721
        act = mo_cut->get_next_item( )
        msg = |The first item should be 1721| ).
  ENDMETHOD.

  METHOD get_third_item_from_store.
    mo_cut->get_next_item( ).
    mo_cut->get_next_item( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 366
        act = mo_cut->get_next_item( )
        msg = |The third item should be 366| ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_matching_items DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_matching_items TYPE stringtab.

    METHODS get_matching_items
      RETURNING
        VALUE(r_matching_items) TYPE stringtab.

    METHODS set_target_value
      IMPORTING
        i_target_value TYPE i.

    METHODS get_target_value
      RETURNING
        VALUE(r_target_value) TYPE i.
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

  METHOD get_matching_items.
    r_matching_items = matching_items.
  ENDMETHOD.


  METHOD set_target_value.
    target_value = i_target_value.
  ENDMETHOD.

  METHOD get_target_value.
    r_target_value = target_value.
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


CLASS ltc_matching_items DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_matching_items.

    METHODS setup.
    METHODS build_matching_items_store   FOR TESTING.
    METHODS set_target_value_for_match   FOR TESTING.
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

  METHOD build_matching_items_store.
    cl_abap_unit_assert=>assert_equals(
        exp = 6
        act = lines( mo_cut->get_matching_items( ) )
        msg = |The matching items store should have 6 lines| ).
  ENDMETHOD.

  METHOD set_target_value_for_match.
    mo_cut->set_target_value( 2020 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2020
        act = mo_cut->get_target_value( )
        msg = |The target value should be 2020| ).
  ENDMETHOD.

  METHOD get_matching_value_for_input.
    mo_cut->set_target_value( 2020 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 299
        act = mo_cut->get_match_for( 1721 )
        msg = |The result should be the match for the target value| ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_aoc_2020_day_1 DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        input_data TYPE stringtab.
    METHODS calculate_expense_total
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA value_store     TYPE REF TO lcl_input_items.
    DATA matching_values TYPE REF TO lcl_matching_items.

ENDCLASS.

CLASS lcl_aoc_2020_day_1 IMPLEMENTATION.

  METHOD constructor.
    value_store = NEW lcl_input_items( input_data ).
    matching_values = NEW lcl_matching_items( input_data ).
    matching_values->set_target_value( 2020 ).
  ENDMETHOD.


  METHOD calculate_expense_total.
    WHILE r_result IS INITIAL.
      DATA(item) = value_store->get_next_item( ).
      DATA(match) = matching_values->get_match_for( item ).
      IF match IS NOT INITIAL.
        r_result = item * match.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_aoc_day_1 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_aoc_2020_day_1.

    METHODS setup.
    METHODS get_result_for_testset FOR TESTING.
ENDCLASS.


CLASS ltc_aoc_day_1 IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( VALUE #( ( |1721| )
                             ( |979|  )
                             ( |366|  )
                             ( |299|  )
                             ( |675|  )
                             ( |1456| ) ) ).
  ENDMETHOD.

  METHOD get_result_for_testset.

    cl_abap_unit_assert=>assert_equals(
        exp = 514579
        act = mo_cut->calculate_expense_total( )
        msg = 'msg' ).
  ENDMETHOD.

ENDCLASS.

DATA input_value TYPE char200.
SELECT-OPTIONS so_input FOR input_value .

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input
                                          ( CONV #( <line>-low ) ) ).
  DATA(lo_aoc) = NEW lcl_aoc_2020_day_1( input_values ).

  WRITE |Result: { lo_aoc->calculate_expense_total( ) }|.
