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

    METHODS get_earliest_id
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
    DATA(temp_sched) = VALUE if_bus_search=>t_bus_schedule( FOR line IN schedules
                        LET result  = id / line-id
                            rounde = round( val = result dec = 0 mode = cl_abap_math=>round_ceiling )
                            future_id = rounde * line-id
                            diff = abs( future_id - id )
                        IN
                        ( id         = line-id
                          future_id  = future_id
                          difference = diff ) ).
    schedules = temp_sched.
  ENDMETHOD.

  METHOD calculate_result.
    r_result = schedules[ 1 ]-id * schedules[ 1 ]-difference.
  ENDMETHOD.

  METHOD get_earliest_id.
    DATA loop_count TYPE i VALUE 0.
    DATA(ids) = VALUE if_bus_search=>t_bus_schedule( FOR line IN diff_schedules
                                                   ( id            = line-id
                                                     difference    = line-difference
                                                     current_count = 1 ) ).
    DATA(base) = ids[ 1 ]-id.
    ids[ 1 ]-current_count = 0.
    DATA(factor) = 0.
    DO.
      loop_count = loop_count + 1.
      IF loop_count MOD base = 0.
        factor = factor + 1.
      ENDIF.
      DATA(temp_ids) = VALUE if_bus_search=>t_bus_schedule(
                         FOR lin IN ids
                            LET curr_count = COND i( WHEN loop_count MOD lin-id = 0 THEN lin-current_count + 1
                                                     ELSE lin-current_count )
                                exp_id = base * factor + lin-difference
                                fut_id = lin-id * curr_count
                                IN
                         ( id = lin-id
                           current_count = curr_count
                           difference    = lin-difference
                           expected_id   = exp_id
                           future_id     = fut_id ) ) .
      ids = temp_ids.

      DATA(match) = abap_false.
      LOOP AT ids INTO DATA(single_id).
        IF single_id-expected_id = single_id-future_id.
          match = abap_true.
        ELSE.
          match = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF match = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    r_result = ids[ 1 ]-future_id.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_bus_search DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO bus_search.
    DATA input_values TYPE stringtab.

    METHODS setup.

    METHODS calculate_result   FOR TESTING.

    METHODS get_earliest_id_2  FOR TESTING.
    METHODS get_earliest_id_3  FOR TESTING.


ENDCLASS.


CLASS ltc_bus_search IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |939| )
                            ( |7,13,x,x,59,x,31,19| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.


  METHOD calculate_result.
    cut->calc_minutes( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 295
        act = cut->calculate_result( )
        msg = |The expected result should be 295.| ).
  ENDMETHOD.

  METHOD get_earliest_id_2.
    input_values = VALUE #( ( |007| )
                            ( |17,x,13,19| ) ).
    cut = NEW #( input_values ).
    cl_abap_unit_assert=>assert_equals(
        exp = 3417
        act = cut->get_earliest_id( )
        msg = |The earliest ID should be 3417.| ).
  ENDMETHOD.

  METHOD get_earliest_id_3.
    input_values = VALUE #( ( |007| )
                            ( |67,7,59,61| ) ).
    cut = NEW #( input_values ).
    cl_abap_unit_assert=>assert_equals(
        exp = 754018
        act = cut->get_earliest_id( )
        msg = |The earliest ID should be 754018.| ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(bus_schedule) = NEW bus_search( input_values ).
  bus_schedule->calc_minutes( ).

  WRITE / |The result for part 1 is: { bus_schedule->calculate_result( ) }|.
  WRITE / |The result for part 2 is: { bus_schedule->get_earliest_id( ) }|.
