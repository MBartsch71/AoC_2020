REPORT ymbh_aoc_day11.

INTERFACE if_seat.
  CONSTANTS c_state_empty    TYPE char1 VALUE 'L'.
  CONSTANTS c_state_floor    TYPE char1 VALUE '.'.
  CONSTANTS c_state_occupied TYPE char1 VALUE '#'.

  TYPES: BEGIN OF s_position,
           col TYPE i,
           row TYPE i,
         END OF s_position.
  TYPES state TYPE char1.

  METHODS get_instance_by_position
    IMPORTING
      i_position    TYPE s_position
    RETURNING
      VALUE(r_seat) TYPE REF TO if_seat.

  METHODS get_current_state
    RETURNING
      VALUE(r_state) TYPE if_seat=>state.

  METHODS determine_new_state.

  METHODS change_state.

ENDINTERFACE.

INTERFACE if_seat_collection.
  TYPES t_seats TYPE STANDARD TABLE OF REF TO if_seat WITH DEFAULT KEY.

  METHODS get_seats
    RETURNING
      VALUE(r_result) TYPE if_seat_collection=>t_seats.

ENDINTERFACE.

CLASS seat DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_seat.
    METHODS constructor
      IMPORTING
        i_position   TYPE if_seat=>s_position
        i_init_state TYPE if_seat=>state.

  PRIVATE SECTION.
    DATA col           TYPE if_seat=>s_position-col.
    DATA row           TYPE if_seat=>s_position-row.
    DATA current_state TYPE if_seat=>state.
    DATA new_state     TYPE if_seat=>state.

ENDCLASS.

CLASS seat IMPLEMENTATION.

  METHOD constructor.
    col           = i_position-col.
    row           = i_position-row.
    current_state = i_init_state.
  ENDMETHOD.

  METHOD if_seat~get_instance_by_position.
    IF i_position-col = col AND
       i_position-row = row.
      r_seat = me.
    ENDIF.
  ENDMETHOD.

  METHOD if_seat~get_current_state.
    r_state = current_state.
  ENDMETHOD.

  METHOD if_seat~change_state.
    current_state = COND #( WHEN current_state = if_seat=>c_state_floor THEN current_state
                            WHEN new_state IS INITIAL THEN current_state
                            ELSE new_state ).
    CLEAR new_state.
  ENDMETHOD.

  METHOD if_seat~determine_new_state.
    new_state = if_seat=>c_state_occupied.
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
    METHODS build_seats_from_string
      IMPORTING
        i_line         TYPE string
        i_row_index    TYPE i
      RETURNING
        VALUE(r_seats) TYPE if_seat_collection=>t_seats.

ENDCLASS.

CLASS seat_collection IMPLEMENTATION.

  METHOD constructor.
    seats = VALUE #( FOR line IN i_input_values
                        INDEX INTO row_index
                        ( LINES OF build_seats_from_string( i_line      = line
                                                            i_row_index = row_index ) ) ).
  ENDMETHOD.

  METHOD if_seat_collection~get_seats.
    r_result = seats.
  ENDMETHOD.

  METHOD build_seats_from_string.
    r_seats = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = strlen( i_line )
                            LET col = i + 1
                            IN
                            ( NEW seat( i_position   = VALUE #( col = col row = i_row_index )
                                        i_init_state = substring( val = i_line off = i len =  1 ) ) ) ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_seat DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO seat.

    METHODS setup.
    METHODS find_seat_by_coordinates     FOR TESTING.
    METHODS no_seat_from_coordinates     FOR TESTING.
    METHODS status_of_seat_not_changed_1 FOR TESTING.
    METHODS status_of_seat_not_changed_2 FOR TESTING.
ENDCLASS.


CLASS ltc_seat IMPLEMENTATION.

  METHOD find_seat_by_coordinates.
    DATA(seat) = cut->if_seat~get_instance_by_position( VALUE if_seat=>s_position( col = 2 row = 1 ) ).
    cl_abap_unit_assert=>assert_bound(
        msg = |The seat object should be found.|
        act = seat ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( i_position   = VALUE #( col = 2 row = 1 )
                 i_init_state = if_seat=>c_state_empty ).
  ENDMETHOD.

  METHOD no_seat_from_coordinates.
    DATA(seat) = cut->if_seat~get_instance_by_position( VALUE #( col = 3 row = 1 ) ).
    cl_abap_unit_assert=>assert_not_bound(
        msg = |The seat object should not be found.|
        act = seat ).
  ENDMETHOD.

  METHOD status_of_seat_not_changed_1.
    cut->if_seat~change_state( ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The status should not be changed.|
        exp = |L|
        act = cut->if_seat~get_current_state( ) ).
  ENDMETHOD.

  METHOD status_of_seat_not_changed_2.
    cut = NEW #( i_position   = VALUE #( col = 5 row = 2 )
                 i_init_state = if_seat=>c_state_floor ).
    cut->if_seat~determine_new_state( ).
    cut->if_seat~change_state( ).
    cl_abap_unit_assert=>assert_equals(
        msg = |The state of the seat should not be changed.|
        exp = |.|
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
    METHODS build_collection_from_table FOR TESTING.
ENDCLASS.


CLASS ltc_seat_collection IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |L.LL.LL.LL| )
                            ( |LLLLLLL.LL| )
                            ( |L.L.L..L..| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD build_collection_from_table.
    cl_abap_unit_assert=>assert_equals(
        msg = |The collection should have the expected amount of lines.|
        exp = 30
        act = lines( cut->if_seat_collection~get_seats( ) ) ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
