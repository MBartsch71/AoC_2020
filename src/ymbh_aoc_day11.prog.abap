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

  METHODS get_seats
    RETURNING
      VALUE(r_seats) TYPE t_seats.
  METHODS get_seat
    IMPORTING
      i_row           TYPE i
      i_col           TYPE i
    RETURNING
      VALUE(r_result) TYPE REF TO if_seat.
  METHODS get_neighbours
    IMPORTING
      i_row           TYPE i
      i_col           TYPE i
    RETURNING
      VALUE(r_result) TYPE if_seat_collection=>t_seats.

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

    METHODS check_seat_row
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

  METHOD if_seat_collection~get_seats.
    r_seats = seats.
  ENDMETHOD.

  METHOD increase_value_by_one.
    r_result = i_value + 1.
  ENDMETHOD.

  METHOD if_seat_collection~get_seat.
    r_result = locate_seat( i_row = i_row i_col = i_col ).
  ENDMETHOD.

  METHOD locate_seat.
    r_result = locate_seat_in_row( i_col      = i_col
                                   i_seat_row = locate_correct_row( i_row ) ).
  ENDMETHOD.

  METHOD locate_seat_in_row.
    DATA temp_seat TYPE REF TO if_seat.
    r_result = REDUCE #( INIT res = temp_seat
                         FOR single_seat IN i_seat_row
                         LET ret_seat = COND #( WHEN single_seat->get_col( ) = i_col THEN single_seat )
                         IN
                         NEXT res = COND #( WHEN ret_seat IS NOT INITIAL THEN ret_seat
                                            ELSE res ) ).
  ENDMETHOD.

  METHOD locate_correct_row.
    r_seat_row  = VALUE if_seat_collection=>t_seats( FOR seat IN seats
                                  ( LINES OF check_seat_row( i_row = i_row i_seat = seat ) ) ).
  ENDMETHOD.

  METHOD check_seat_row.
    IF i_row = i_seat->get_row( ).
      r_seats = VALUE #( ( i_seat ) ).
    ENDIF.
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

ENDCLASS.

CLASS ltc_seat DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO seat.

    METHODS setup.
    METHODS check_seat_position FOR TESTING.
    METHODS current_state_empty FOR TESTING.
    METHODS new_state_occupied  FOR TESTING.

ENDCLASS.


CLASS ltc_seat IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( i_row = 2 i_col = 4 ).
  ENDMETHOD.

  METHOD check_seat_position.
    cl_abap_unit_assert=>assert_equals(
    	msg = |The seat row should be like expected.|
    	exp = 2
    	act = cut->if_seat~get_row( ) ).

    cl_abap_unit_assert=>assert_equals(
     	msg = |The seat col should be like expected.|
     	exp = 4
     	act = cut->if_seat~get_col( ) ).
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

    METHODS setup.
    METHODS all_neighbours_correct
      IMPORTING
        i_neighbours    TYPE if_seat_collection=>t_seats
        i_expected      TYPE string
      RETURNING
        VALUE(r_result) TYPE abap_bool.
    METHODS build_seat_collection    FOR TESTING.
    METHODS get_seats_by_row_and_col FOR TESTING.
    METHODS get_neighbours_from_cell FOR TESTING.
    METHODS get_neighbours_from_edge FOR TESTING.

ENDCLASS.

CLASS ltc_seat_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->if_seat_collection~build_from_string( |L.LL.LL.LL| ).
  ENDMETHOD.

  METHOD build_seat_collection.
    cl_abap_unit_assert=>assert_equals(
        msg = |The collection should have 10 objects.|
        exp = 10
        act = lines( cut->if_seat_collection~get_seats( ) ) ).
  ENDMETHOD.

  METHOD get_seats_by_row_and_col.
    cut->if_seat_collection~build_from_string( |LLLLLLL.LL| ).
    DATA(seat) = cut->if_seat_collection~get_seat( i_row = 2 i_col = 4 ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The returned seat should have the correct row.|
        exp = 2
        act = seat->get_row( ) ).

    cl_abap_unit_assert=>assert_equals(
        msg = |The returned seat should have the correct col.|
        exp = 4
        act = seat->get_col( ) ).
  ENDMETHOD.

  METHOD get_neighbours_from_cell.
    cut->if_seat_collection~build_from_string( |LLLLLLL.LL| ).
    cut->if_seat_collection~build_from_string( |L.L.L..L..| ).
    DATA(neighbours) = cut->if_seat_collection~get_neighbours( i_row = 2 i_col = 4 ).
    cl_abap_unit_assert=>assert_true(
        act =  all_neighbours_correct( i_neighbours = neighbours
                                       i_expected   = |LL.LLL.L| )
        msg =  |The neighbours should be correctly_detected.| ).
  ENDMETHOD.

  METHOD get_neighbours_from_edge.
    cut->if_seat_collection~build_from_string( |LLLLLLL.LL| ).
    cut->if_seat_collection~build_from_string( |L.L.L..L..| ).
    DATA(neighbours) = cut->if_seat_collection~get_neighbours( i_row = 2 i_col = 1 ).
    cl_abap_unit_assert=>assert_true(
        act =  all_neighbours_correct( i_neighbours = neighbours
                                       i_expected   = |L.LL.| )
        msg =  |The neighbours should be correctly_detected.| ).
  ENDMETHOD.

  METHOD all_neighbours_correct.
    DATA(actual_values)   = REDUCE string( INIT res = ``
                                           FOR lines IN i_neighbours
                                           NEXT res = res && lines->get_current_state( ) ).
    r_result = xsdbool( i_expected = actual_values ).
  ENDMETHOD.

ENDCLASS.



DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
