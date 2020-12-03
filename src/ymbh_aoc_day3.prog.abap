REPORT ymbh_aoc_day3.

INTERFACE iterator.
  METHODS initialize
    IMPORTING
      start_value    TYPE i DEFAULT 1
      iteration_step TYPE i
      value_reset_at TYPE i.

  METHODS get_next
    RETURNING
      VALUE(r_next_value) TYPE i.
ENDINTERFACE.

CLASS lcl_col DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES iterator.

    METHODS constructor
      IMPORTING
        i_value        TYPE i
        i_step_width   TYPE i
        i_col_reset_at TYPE i.

  PRIVATE SECTION.
    DATA value        TYPE i.
    DATA step_width   TYPE i.
    DATA col_reset_at TYPE i.

ENDCLASS.

CLASS lcl_col IMPLEMENTATION.

  METHOD constructor.
    iterator~initialize( start_value    = i_value
                         iteration_step = i_step_width
                         value_reset_at = i_col_reset_at ).
  ENDMETHOD.

  METHOD iterator~initialize.
    value        = start_value.
    step_width   = iteration_step.
    col_reset_at = value_reset_at.
  ENDMETHOD.

  METHOD iterator~get_next.
    value = value + step_width.
    value = COND #( WHEN value > col_reset_at THEN value - col_reset_at
                    ELSE value ).
    r_next_value = value.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_row DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES iterator.
    METHODS constructor
      IMPORTING
        i_value      TYPE i
        i_step_width TYPE i.

  PRIVATE SECTION.
    DATA value      TYPE i.
    DATA step_width TYPE i.

ENDCLASS.

CLASS lcl_row IMPLEMENTATION.

  METHOD constructor.
    iterator~initialize( start_value    = i_value
                         iteration_step = i_step_width
                         value_reset_at = 0 ).
  ENDMETHOD.

  METHOD iterator~get_next.
    value = value + step_width.
    r_next_value = value.
  ENDMETHOD.

  METHOD iterator~initialize.
    value      = start_value.
    step_width = iteration_step.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_position DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_start_col_pos  TYPE i
        i_start_row_pos  TYPE i
        i_col_step_width TYPE i
        i_row_step_width TYPE i
        i_col_reset_at   TYPE i.

    METHODS get_col
      RETURNING
        VALUE(r_col) TYPE i.

    METHODS get_row
      RETURNING
        VALUE(r_row) TYPE i.

  PRIVATE SECTION.
    DATA row TYPE REF TO lcl_row.
    DATA col TYPE REF TO lcl_col.

ENDCLASS.

CLASS lcl_position IMPLEMENTATION.

  METHOD constructor.
    col = NEW #( i_value        = i_start_col_pos
                 i_step_width   = i_col_step_width
                 i_col_reset_at = i_col_reset_at ).
    row = NEW #( i_value        = i_start_row_pos
                 i_step_width   = i_row_step_width ).
  ENDMETHOD.

  METHOD get_col.
    r_col = col->iterator~get_next( ).
  ENDMETHOD.

  METHOD get_row.
    r_row = row->iterator~get_next( ).
  ENDMETHOD.

ENDCLASS.



CLASS lcl_toboggan DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_map            TYPE stringtab
        i_col_step_width TYPE i
        i_row_step_width TYPE i.

    METHODS go
      IMPORTING
        i_steps TYPE i OPTIONAL.

    METHODS get_trees
      RETURNING
        VALUE(r_trees) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA map   TYPE stringtab.
    DATA trees TYPE i.

    DATA position TYPE REF TO lcl_position.

ENDCLASS.

CLASS lcl_toboggan IMPLEMENTATION.

  METHOD constructor.
    map = i_map.
    position = NEW #( i_col_step_width = i_col_step_width
                      i_row_step_width = i_row_step_width
                      i_start_col_pos  = 1
                      i_start_row_pos  = 1
                      i_col_reset_at   = 31 ).
  ENDMETHOD.

  METHOD go.
    DO i_steps TIMES.
      DATA(col) = position->get_col( ).
      DATA(row) = position->get_row( ).
      trees = trees + count( val = map[ row ]
                             off = col - 1
                             len = 1
                             sub = |#| ).
    ENDDO.
  ENDMETHOD.

  METHOD get_trees.
    r_trees = trees.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_toboggan DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_toboggan.
    METHODS setup.

    METHODS detect_one_tree_after_2_steps FOR TESTING.
    METHODS detect_7_trees_after_all_steps FOR TESTING.
ENDCLASS.

CLASS ltc_col DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_col.

    METHODS setup.

    METHODS get_next_col               FOR TESTING.
    METHODS get_col_after_right_border FOR TESTING.
ENDCLASS.


CLASS ltc_col IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( i_value        = 1
                 i_step_width   = 3
                 i_col_reset_at = 31 ).
  ENDMETHOD.

  METHOD get_next_col.
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->iterator~get_next( )
        msg = |The next col öposition should be 4.| ).
  ENDMETHOD.

  METHOD get_col_after_right_border.
    DO 10 TIMES.
      cut->iterator~get_next( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->iterator~get_next( )
        msg = |After 10 steps the col should switch back to expected value| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_row DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_row.
    METHODS setup.

    METHODS get_next_col FOR TESTING.
ENDCLASS.


CLASS ltc_row IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( i_value      = 1
                 i_step_width = 1 ).
  ENDMETHOD.

  METHOD get_next_col.
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->iterator~get_next( )
        msg = |The next after initialization should be 2.| ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_toboggan IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( i_map = VALUE #( ( |..##.........##.........##.........##.........##.........##....... | )
                                ( |#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#.. | )
                                ( |.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#. | )
                                ( |..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.# | )
                                ( |.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#. | )
                                ( |..#.##.......#.##.......#.##.......#.##.......#.##.......#.##..... | )
                                ( |.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....# | )
                                ( |.#........#.#........#.#........#.#........#.#........#.#........# | )
                                ( |#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#... | )
                                ( |#...##....##...##....##...##....##...##....##...##....##...##....# | )
                                ( |.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.# | ) )
                  i_col_step_width = 3
                  i_row_step_width = 1 ).
  ENDMETHOD.

  METHOD detect_one_tree_after_2_steps.
    cut->go( 2 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = cut->get_trees( )
        msg = |After 2 steps one tree should be detected.| ).
  ENDMETHOD.

  METHOD detect_7_trees_after_all_steps.
    cut->go( 10 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 7
        act = cut->get_trees( )
        msg = |After 10 steps seven trees should be detected.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_position.
    METHODS setup.

    METHODS get_first_position FOR TESTING.
    METHODS get_position_after_10_rounds FOR TESTING.

ENDCLASS.

CLASS ltc_position IMPLEMENTATION.

  METHOD get_first_position.
    cl_abap_unit_assert=>assert_equals(
        exp = 4
        act = cut->get_col( )
        msg = |The first col position should be 4.| ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->get_row( )
        msg = |The first row position should be 2.| ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( i_start_col_pos  = 1
                 i_start_row_pos  = 1
                 i_col_step_width = 3
                 i_row_step_width = 1
                 i_col_reset_at   = 67 ).
  ENDMETHOD.

  METHOD get_position_after_10_rounds.
    DO 10 TIMES.
      cut->get_col( ).
      cut->get_row( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals(
        exp = 34
        act = cut->get_col( )
        msg = |After 10 times the col position should switch to 3.| ).
    cl_abap_unit_assert=>assert_equals(
        exp = 12
        act = cut->get_row( )
        msg = |Avfter 10 times the row position should be at 11.| ).
  ENDMETHOD.

ENDCLASS.

DATA input TYPE char100.
DATA result TYPE p.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(lo_toboggan) = NEW lcl_toboggan( i_map = input_values
                                        i_col_step_width = 1
                                        i_row_step_width = 1 ).
  lo_toboggan->go( lines( input_values ) - 1 ).
  DATA(result1) = lo_toboggan->get_trees( ).
  WRITE / |The result from test is { result1 }|.

  DATA(lo_toboggan2) = NEW lcl_toboggan( i_map = input_values
                                        i_col_step_width = 3
                                        i_row_step_width = 1 ).
  lo_toboggan2->go( lines( input_values ) - 1 ).
  DATA(result2) = lo_toboggan2->get_trees( ).
  WRITE / |The result from test is { result2 }|.

  DATA(lo_toboggan3) = NEW lcl_toboggan( i_map = input_values
                                        i_col_step_width = 5
                                        i_row_step_width = 1 ).
  lo_toboggan3->go( lines( input_values ) - 1 ).
  DATA(result3) = lo_toboggan3->get_trees( ).
  WRITE / |The result from test is { result3 }|.

  DATA(lo_toboggan4) = NEW lcl_toboggan( i_map = input_values
                                        i_col_step_width = 7
                                        i_row_step_width = 1 ).
  lo_toboggan4->go( lines( input_values ) - 1 ).
  DATA(result4) = lo_toboggan4->get_trees( ).
  WRITE / |The result from test is { result4 }|.

  DATA(lo_toboggan5) = NEW lcl_toboggan( i_map = input_values
                                        i_col_step_width = 1
                                        i_row_step_width = 2 ).
  lo_toboggan5->go( lines( input_values ) / 2 - 1 ).
  DATA(result5) = lo_toboggan5->get_trees( ).
  WRITE / |The result from test is { result5 }|.

  result = result1 * result2 * result3 * result4 * result5.
  write / |Total: { result }.|.
