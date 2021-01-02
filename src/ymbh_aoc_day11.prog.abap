REPORT ymbh_aoc_day11.

INTERFACE if_seat.

  CONSTANTS c_state_empty    TYPE char1 VALUE 'L'.
  CONSTANTS c_state_occupied TYPE char1 VALUE '#'.
  CONSTANTS c_state_floor    TYPE char1 VALUE '.'.

  METHODS get_row
    RETURNING
      VALUE(r_row) TYPE i.

  METHODS get_col
    RETURNING
      VALUE(r_col) TYPE i.

  METHODS get_current_state
    RETURNING
      VALUE(r_current_state) TYPE char1.

  METHODS set_new_state
    IMPORTING
      i_new_state TYPE char1.

  METHODS status_change.

ENDINTERFACE.

INTERFACE if_seat_collection.
  TYPES t_seats TYPE STANDARD TABLE OF REF TO if_seat WITH DEFAULT KEY.

  METHODS build_from_string
    IMPORTING
      i_seats TYPE string.

  METHODS get_neighbours
    IMPORTING
      i_row           TYPE i
      i_col           TYPE i
    RETURNING
      VALUE(r_result) TYPE if_seat_collection=>t_seats.

  METHODS get_occupied_neighbour_count
    IMPORTING
      i_col          TYPE i
      i_row          TYPE i
    RETURNING
      VALUE(r_count) TYPE i.

ENDINTERFACE.


CLASS seat DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_seat.

    METHODS constructor
      IMPORTING
        i_row   TYPE i
        i_col   TYPE i
        i_state TYPE char1 OPTIONAL.

  PRIVATE SECTION.
    DATA row           TYPE i.
    DATA col           TYPE i.
    DATA current_state TYPE char1.
    DATA new_state     TYPE char1.

ENDCLASS.

CLASS seat IMPLEMENTATION.

  METHOD constructor.
    row = i_row.
    col = i_col.
    current_state = COND #( WHEN i_state IS INITIAL THEN if_seat=>c_state_empty
                            ELSE i_state ).
  ENDMETHOD.

  METHOD if_seat~get_row.
    r_row = row.
  ENDMETHOD.

  METHOD if_seat~get_col.
    r_col = col.
  ENDMETHOD.

  METHOD if_seat~get_current_state.
    r_current_state = me->current_state.
  ENDMETHOD.

  METHOD if_seat~set_new_state.
    new_state = i_new_state.
  ENDMETHOD.

  METHOD if_seat~status_change.
    IF new_state IS NOT INITIAL.
      current_state = new_state.
      CLEAR new_state.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS seat_collection DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_seat_collection.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

  PRIVATE SECTION.
    DATA seats TYPE if_seat_collection=>t_seats.
    DATA current_row TYPE i.

    METHODS increase_value_by_one
      IMPORTING
        i_value         TYPE i
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS locate_seat
      IMPORTING
        i_row           TYPE i
        i_col           TYPE i
      RETURNING
        VALUE(r_result) TYPE REF TO if_seat.

    METHODS verify_seat_row
      IMPORTING
        i_row          TYPE i
        i_seat         TYPE REF TO if_seat
      RETURNING
        VALUE(r_seats) TYPE if_seat_collection=>t_seats.

    METHODS locate_correct_row
      IMPORTING
        i_row             TYPE i
      RETURNING
        VALUE(r_seat_row) TYPE if_seat_collection=>t_seats.

    METHODS locate_seat_in_row
      IMPORTING
        i_col           TYPE i
        i_seat_row      TYPE if_seat_collection=>t_seats
      RETURNING
        VALUE(r_result) TYPE REF TO if_seat.

ENDCLASS.

CLASS seat_collection IMPLEMENTATION.

  METHOD constructor.
    LOOP AT i_input_values INTO DATA(line).
      if_seat_collection~build_from_string( line ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_seat_collection~build_from_string.
    current_row = increase_value_by_one( current_row ).
    seats = VALUE #( BASE seats
                     FOR i = 0 THEN i + 1 UNTIL i = strlen( i_seats )
                     LET col = increase_value_by_one( col )
                     IN
                     ( NEW seat( i_row   = current_row
                                 i_col   = col
                                 i_state = substring( val = i_seats off = i len = 1 ) ) ) ).
  ENDMETHOD.

  METHOD if_seat_collection~get_neighbours.
    DATA empty_seat TYPE REF TO if_seat.
    r_result = VALUE #( FOR i = i_row - 1 THEN i + 1 UNTIL i = i_row + 2
                        FOR j = i_col - 1 THEN j + 1 UNTIL j = i_col + 2
                        LET seat = locate_seat( i_row = i i_col = j )
                        IN
                        ( COND #( WHEN seat IS INITIAL THEN empty_seat
                                  WHEN seat->get_col( ) <> i_col OR
                                       seat->get_row( ) <> i_row THEN seat ) ) ).
    DELETE r_result WHERE table_line IS INITIAL.
  ENDMETHOD.

  METHOD increase_value_by_one.
    r_result = i_value + 1.
  ENDMETHOD.

  METHOD locate_seat.
    r_result = locate_seat_in_row( i_col      = i_col
                                   i_seat_row = locate_correct_row( i_row ) ).
  ENDMETHOD.

  METHOD locate_seat_in_row.
    DATA initial_seat TYPE REF TO if_seat.
    r_result = REDUCE #( INIT res = initial_seat
                         FOR single_seat IN i_seat_row
                         LET returning_seat = COND #( WHEN single_seat->get_col( ) = i_col THEN single_seat )
                         IN
                         NEXT res = COND #( WHEN returning_seat IS NOT INITIAL THEN returning_seat
                                            ELSE res ) ).
  ENDMETHOD.

  METHOD locate_correct_row.
    r_seat_row  = VALUE #( FOR seat IN seats
                           ( LINES OF verify_seat_row( i_row = i_row i_seat = seat ) ) ).
  ENDMETHOD.

  METHOD verify_seat_row.
    IF i_row = i_seat->get_row( ).
      r_seats = VALUE #( ( i_seat ) ).
    ENDIF.
  ENDMETHOD.

  METHOD if_seat_collection~get_occupied_neighbour_count.
    DATA(neighbours) = if_seat_collection~get_neighbours( i_row = i_row i_col = i_col ).
    r_count = REDUCE #( INIT sum = 0
                        FOR seat IN neighbours
                        NEXT sum = COND #( WHEN seat->get_current_state( ) = if_seat=>c_state_occupied THEN sum + 1
                                           ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_seat DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO seat.

    METHODS setup.

    METHODS current_state_empty FOR TESTING.
    METHODS new_state_occupied  FOR TESTING.

ENDCLASS.


CLASS ltc_seat IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( i_row = 2 i_col = 4 ).
  ENDMETHOD.

  METHOD current_state_empty.
    cl_abap_unit_assert=>assert_equals(
        msg = |The current state of the seat should be EMPTY.|
        exp = |L|
        act = cut->if_seat~get_current_state( ) ).
  ENDMETHOD.

  METHOD new_state_occupied.
    cut->if_seat~set_new_state( |#| ).
    cut->if_seat~status_change( ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The state should be OCCUPIED.|
        exp = |#|
        act = cut->if_seat~get_current_state( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_seat_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO seat_collection.
    DATA input_values TYPE stringtab.

    METHODS setup.

    METHODS get_occupied_neighbours  FOR TESTING.

ENDCLASS.

CLASS ltc_seat_collection IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |#.LL.L#.##| )
                            ( |#LLLLLL.L#| )
                            ( |L.L.L..L..| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD get_occupied_neighbours.
    cl_abap_unit_assert=>assert_equals(
        msg = |There should be the expected amount of occupied neighbour seats.|
        exp = 3
        act = cut->if_seat_collection~get_occupied_neighbour_count( i_col = 9 i_row = 2 ) ).
  ENDMETHOD.

ENDCLASS.



DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
