REPORT ymbh_aoc_day13.

INTERFACE if_bus_search.
  TYPES: BEGIN OF s_bus_schedules,
           id            TYPE decfloat34,
           current_count TYPE i,
           difference    TYPE i,
           future_id     TYPE i,
           expected_id   TYPE i,
         END OF s_bus_schedules.
  TYPES t_bus_schedule      TYPE SORTED TABLE OF s_bus_schedules WITH NON-UNIQUE KEY primary_key COMPONENTS difference.
  TYPES t_bus_diff_schedule TYPE STANDARD TABLE OF s_bus_schedules WITH DEFAULT KEY.
ENDINTERFACE.

CLASS bus_search DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

    METHODS calc_minutes.

    METHODS calculate_result
      RETURNING
        VALUE(r_result) TYPE i.
    METHODS find_solution
      IMPORTING
        i_1             TYPE i
        i_2             TYPE i
        i_diff          TYPE i
      RETURNING
        VALUE(r_result) TYPE i.



  PRIVATE SECTION.
    DATA id             TYPE i.
    DATA schedules      TYPE if_bus_search=>t_bus_schedule.
    DATA diff_schedules TYPE if_bus_search=>t_bus_diff_schedule.

ENDCLASS.

CLASS bus_search IMPLEMENTATION.

  METHOD constructor.
    id = i_input_values[ 1 ].
    SPLIT i_input_values[ 2 ] AT ',' INTO TABLE DATA(temp_schedules).

    schedules = VALUE if_bus_search=>t_bus_schedule( FOR line IN temp_schedules
                            WHERE ( table_line <> 'x' )
                            ( id = CONV #( line ) ) ).

    diff_schedules = VALUE if_bus_search=>t_bus_diff_schedule( FOR line IN temp_schedules
                                    INDEX INTO line_index
                                    WHERE ( table_line <> 'x' )
                                    ( id = CONV #( line )
                                      difference = line_index - 1 ) ).
  ENDMETHOD.

  METHOD calc_minutes.
    DATA(working_values) = VALUE if_bus_search=>t_bus_schedule( FOR line IN schedules
                              LET result    = id / line-id
                                  rounded   = round( val = result dec = 0 mode = cl_abap_math=>round_ceiling )
                                  future_id = rounded * line-id
                                  diff      = abs( future_id - id )
                              IN
                                ( id         = line-id
                                  future_id  = future_id
                                  difference = diff ) ).
    schedules = working_values.
  ENDMETHOD.

  METHOD calculate_result.
    r_result = schedules[ 1 ]-id * schedules[ 1 ]-difference.
  ENDMETHOD.


  METHOD find_solution.
    DATA counter TYPE i VALUE 1.
    DO.
      counter = counter + 1.
      DATA(operand1) = i_1 * counter + i_diff.
      IF operand1 MOD i_2 = 0.
        r_result = counter.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_bus_search DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO bus_search.
    DATA input_values TYPE stringtab.

    METHODS setup.

    METHODS calculate_result_part_1   FOR TESTING.
    METHODS find_solution_for_7_and_13 FOR TESTING.
    METHODS find_solution_for_13_and_59 FOR TESTING.
    METHODS find_solution_for_59_and_31 FOR TESTING.
    METHODS find_solution_for_31_and_19 FOR TESTING.



ENDCLASS.


CLASS ltc_bus_search IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |939| )
                            ( |7,13,x,x,59,x,31,19| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD calculate_result_part_1.
    cut->calc_minutes( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 295
        act = cut->calculate_result( )
        msg = |The expected result should be 295.| ).
  ENDMETHOD.

  METHOD find_solution_for_7_and_13.
    cl_abap_unit_assert=>assert_equals(
            exp = 11
            act = cut->find_solution( i_1 = 7 i_2 = 13 i_diff = 1 )
            msg = 'msg' ).
  ENDMETHOD.

  METHOD find_solution_for_13_and_59.
    cl_abap_unit_assert=>assert_equals(
        exp = 27
        act = cut->find_solution( i_1 = 13 i_2 = 59 i_diff = 3 )
        msg = 'msg' ).
  ENDMETHOD.

  METHOD find_solution_for_59_and_31.
    cl_abap_unit_assert=>assert_equals(
           exp = 11
           act = cut->find_solution( i_1 = 59 i_2 = 31 i_diff = 2 )
           msg = 'msg' ).
  ENDMETHOD.

  METHOD find_solution_for_31_and_19.
    cl_abap_unit_assert=>assert_equals(
        exp = 11
        act = cut->find_solution( i_1 = 31 i_2 = 19 i_diff = 1 )
        msg = 'msg' ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(bus_schedule) = NEW bus_search( input_values ).
  bus_schedule->calc_minutes( ).

  WRITE / |The result for part 1 is: { bus_schedule->calculate_result( ) }|.
