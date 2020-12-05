REPORT ymbh_aoc_day5.

INTERFACE if_plane_seatings.
  TYPES: BEGIN OF s_line,
           line_no TYPE i,
         END OF s_line.
  TYPES t_lines TYPE SORTED TABLE OF s_line WITH UNIQUE KEY line_no.
  TYPES seat_ids TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDINTERFACE.

INTERFACE if_table_extractor.
  METHODS upper_part
    IMPORTING
      i_table        TYPE if_plane_seatings=>t_lines
    RETURNING
      VALUE(r_table) TYPE if_plane_seatings=>t_lines.
  METHODS lower_part
    IMPORTING
      i_table        TYPE if_plane_seatings=>t_lines
    RETURNING
      VALUE(r_table) TYPE if_plane_seatings=>t_lines.
ENDINTERFACE.

CLASS table_extractor DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_table_extractor.

ENDCLASS.

CLASS table_extractor IMPLEMENTATION.

  METHOD if_table_extractor~lower_part.
    DATA(end) = ( lines( i_table ) / 2 ).
    r_table   = VALUE #( FOR i = 1 THEN i + 1 UNTIL i > end
                         ( i_table[ i ] ) ).
  ENDMETHOD.

  METHOD if_table_extractor~upper_part.
    DATA(start) = lines( i_table ) / 2 + 1.
    DATA(end)   = lines( i_table ).
    r_table  = VALUE #( FOR i = start THEN i + 1 UNTIL i > end
                        ( i_table[ i ] ) ).
  ENDMETHOD.

ENDCLASS.

CLASS rows DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get
      RETURNING
        VALUE(r_table) TYPE if_plane_seatings=>t_lines.

  PRIVATE SECTION.
    DATA plane_rows TYPE if_plane_seatings=>t_lines.

ENDCLASS.

CLASS rows IMPLEMENTATION.

  METHOD constructor.
    plane_rows = VALUE #( FOR i = 0 THEN i + 1 UNTIL i > 127 ( line_no = i ) ).
  ENDMETHOD.

  METHOD get.
    r_table = plane_rows.
  ENDMETHOD.

ENDCLASS.

CLASS cols DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS get
      RETURNING
        VALUE(r_table) TYPE if_plane_seatings=>t_lines.

  PRIVATE SECTION.
    DATA plane_cols TYPE if_plane_seatings=>t_lines.

ENDCLASS.

CLASS cols IMPLEMENTATION.

  METHOD constructor.
    plane_cols = VALUE #( FOR i = 0 THEN i + 1 UNTIL i > 7 ( line_no = i ) ).
  ENDMETHOD.

  METHOD get.
    r_table = plane_cols.
  ENDMETHOD.

ENDCLASS.

CLASS airplane_seat DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_row_from_sequence
      IMPORTING
        i_sequence   TYPE char7
      RETURNING
        VALUE(r_row) TYPE i.

    METHODS get_col_from_sequence
      IMPORTING
        i_sequence   TYPE char3
      RETURNING
        VALUE(r_col) TYPE i.

    METHODS get_seat_id
      IMPORTING
        row_no         TYPE i
        col_no         TYPE i
      RETURNING
        VALUE(seat_id) TYPE i.

  PRIVATE SECTION.
    DATA plane_rows   TYPE if_plane_seatings=>t_lines.
    DATA plane_cols   TYPE if_plane_seatings=>t_lines.
    DATA detector TYPE REF TO table_extractor.

ENDCLASS.

CLASS airplane_seat IMPLEMENTATION.

  METHOD constructor.
    plane_rows = NEW rows( )->get( ).
    plane_cols = NEW cols( )->get( ).
    detector   = NEW table_extractor( ).
  ENDMETHOD.

  METHOD get_row_from_sequence.
    DATA(sequence) = VALUE stringtab( FOR i = 0 THEN i + 1 UNTIL i > 6
                                         ( substring( val = i_sequence
                                                      off = i
                                                      len = 1 ) ) ).

    LOOP AT sequence INTO DATA(seq).
      plane_rows = SWITCH #( seq WHEN 'F' THEN detector->if_table_extractor~lower_part( plane_rows )
                                 WHEN 'B' THEN detector->if_table_extractor~upper_part( plane_rows ) ).
    ENDLOOP.
    r_row = plane_rows[ 1 ]-line_no.
  ENDMETHOD.

  METHOD get_col_from_sequence.
    DATA(sequence) = VALUE stringtab( FOR i = 0 THEN i + 1 UNTIL i > 2
                                        ( substring( val = i_sequence
                                                     off = i
                                                     len = 1 ) ) ).

    LOOP AT sequence INTO DATA(seq).
      plane_cols = SWITCH #( seq WHEN 'L' THEN detector->if_table_extractor~lower_part( plane_cols )
                                 WHEN 'R' THEN detector->if_table_extractor~upper_part( plane_cols ) ).
    ENDLOOP.
    r_col = plane_cols[ 1 ]-line_no.
  ENDMETHOD.

  METHOD get_seat_id.
    seat_id = row_no * 8 + col_no.
  ENDMETHOD.

ENDCLASS.

CLASS seat_detector DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS get_seats
      IMPORTING
        i_scans           TYPE stringtab
      RETURNING
        VALUE(r_seat_ids) TYPE if_plane_seatings=>seat_ids.

  PRIVATE SECTION.
    DATA airplane_seat TYPE REF TO airplane_seat.
ENDCLASS.

CLASS seat_detector IMPLEMENTATION.

  METHOD get_seats.
    r_seat_ids = VALUE #( FOR <line> IN i_scans
                             LET airplane_seats = NEW airplane_seat( )
                                 seat_id = airplane_seats->get_seat_id(
                                             row_no = airplane_seats->get_row_from_sequence( CONV #( <line>(7) ) )
                                             col_no = airplane_seats->get_col_from_sequence( CONV #( <line>+7(3) ) )
                                           )
                              IN ( seat_id ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_table_extractor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO table_extractor.
    DATA test_values TYPE if_plane_seatings=>t_lines.

    METHODS setup.
    METHODS extract_upper_half_of_a_table FOR TESTING.
    METHODS extract_lower_half_of_a_table FOR TESTING.
ENDCLASS.


CLASS ltc_table_extractor IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    test_values = VALUE #( ( line_no = 1 )
                           ( line_no = 2 )
                           ( line_no = 3 )
                           ( line_no = 4 )
                           ( line_no = 5 )
                           ( line_no = 6 ) ).
  ENDMETHOD.

  METHOD extract_upper_half_of_a_table.
    DATA(excpected_values) = VALUE if_plane_seatings=>t_lines( ( line_no = 4 )
                                                               ( line_no = 5 )
                                                               ( line_no = 6 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = excpected_values
        act = cut->if_table_extractor~upper_part( test_values )
        msg = |The expected value should be delivered back.| ).
  ENDMETHOD.

  METHOD extract_lower_half_of_a_table.
    DATA(excpected_values) = VALUE if_plane_seatings=>t_lines( ( line_no = 1 )
                                                               ( line_no = 2 )
                                                               ( line_no = 3 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = excpected_values
        act = cut->if_table_extractor~lower_part( test_values )
        msg = |The expected value should be delivered back.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_row_builder DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO rows.

    METHODS setup.
    METHODS initial_row_map_built  FOR TESTING.

ENDCLASS.

CLASS ltc_row_builder IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD initial_row_map_built.
    cl_abap_unit_assert=>assert_equals(
        exp = 128
        act = lines( cut->get( ) )
        msg = |The whole table should be delivered| ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_col_builder DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO cols.

    METHODS setup.
    METHODS initial_col_map_built  FOR TESTING.

ENDCLASS.

CLASS ltc_col_builder IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD initial_col_map_built.
    cl_abap_unit_assert=>assert_equals(
        exp = 8
        act = lines( cut->get( ) )
        msg = |The whole table should be delivered| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_seat_detector DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO seat_detector.

    METHODS setup.
    METHODS get_seats_from_scanned_list FOR TESTING.
ENDCLASS.

CLASS ltc_airplane_rows DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO airplane_seat.

    METHODS setup.
    METHODS get_correct_row_from_sequence FOR TESTING.
    METHODS get_correct_col_from_sequence FOR TESTING.
    METHODS get_correct_seat_id           FOR TESTING.

ENDCLASS.


CLASS ltc_airplane_rows IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_correct_row_from_sequence.
    cl_abap_unit_assert=>assert_equals(
        exp = 44
        act = cut->get_row_from_sequence( |FBFBBFF| )
        msg = |The correct row - 44 should be detected.| ).
  ENDMETHOD.

  METHOD get_correct_col_from_sequence.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = cut->get_col_from_sequence( |RLR| )
        msg = |The correct col - 5 should be detected| ).
  ENDMETHOD.

  METHOD get_correct_seat_id.
    cl_abap_unit_assert=>assert_equals(
        exp = 357
        act = cut->get_seat_id( row_no = 44
                                col_no = 5 )
        msg = |The seat ID should be 357| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_seat_detector IMPLEMENTATION.

  METHOD get_seats_from_scanned_list.
    DATA expected_values TYPE TABLE OF i.
    expected_values = VALUE #( ( 567 )
                               ( 119 )
                               ( 820 ) ).
    cl_abap_unit_assert=>assert_equals(
       exp = expected_values
       act = cut->get_seats(  VALUE stringtab( ( |BFFFBBFRRR| )
                                               ( |FFFBBBFRRR| )
                                               ( |BBFFBBFRLL| ) ) )
       msg = |The expected values should be delivered back| ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

ENDCLASS.


DATA input  TYPE char100.

SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(seats) = NEW seat_detector( )->get_seats( input_values ).
  SORT seats DESCENDING BY table_line.

  WRITE / |Answer Part1: The highest seat number is: { seats[ 1 ] }|.

  LOOP AT seats INTO DATA(seat).
    IF NOT line_exists( seats[ table_line = seat - 1 ] ).
      WRITE / |Answer Part2: Missing seat id: { seat - 1 }|.
      EXIT.
    ENDIF.
  ENDLOOP.
