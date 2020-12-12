REPORT ymbh_aoc_day9.

INTERFACE if_preamble.
  TYPES t_preamble TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
ENDINTERFACE.

CLASS preamble DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_preamble_lines TYPE i.

    METHODS preamble_can_build_sum
      IMPORTING
        i_sum           TYPE i
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS find_not_calculable_number
      IMPORTING
        i_input_values  TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS slice_input
      IMPORTING
        i_input_values  TYPE stringtab
        i_current_index TYPE i.

  PRIVATE SECTION.
    DATA preamble_lines  TYPE i.
    DATA preamble_values TYPE if_preamble=>t_preamble.

ENDCLASS.

CLASS preamble IMPLEMENTATION.

  METHOD constructor.
    preamble_lines = i_preamble_lines.
  ENDMETHOD.

  METHOD preamble_can_build_sum.
    LOOP AT preamble_values INTO DATA(line).
      DATA(difference) = i_sum - line.
      IF line_exists( preamble_values[ table_line = difference ] ).
        r_result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_not_calculable_number.
    LOOP AT i_input_values INTO DATA(line).
      DATA(line_index) = sy-tabix.
      IF line_index <= preamble_lines.
        CONTINUE.
      ENDIF.
      slice_input( i_input_values  = i_input_values
                   i_current_index = line_index ).
      IF preamble_can_build_sum( CONV i( line ) ) = abap_false.
        r_result = line.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD slice_input.
    preamble_values = VALUE #( FOR i = i_current_index - preamble_lines  THEN i + 1 UNTIL i = i_current_index
                                ( i_input_values[ i ] ) ).
  ENDMETHOD.

ENDCLASS.


CLASS sequence DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.
    METHODS build_sequence
      IMPORTING
        i_last_number   TYPE string
      RETURNING
        VALUE(r_result) TYPE if_preamble=>t_preamble.
    METHODS find_parts_for_sum
      IMPORTING
        i_sum           TYPE i
      RETURNING
        VALUE(r_result) TYPE if_preamble=>t_preamble.
    METHODS build_weakness_result
      IMPORTING
        i_parts_of_sum  TYPE if_preamble=>t_preamble
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA input_values TYPE stringtab.
    DATA sequence     TYPE if_preamble=>t_preamble.
ENDCLASS.

CLASS sequence IMPLEMENTATION.

  METHOD constructor.
    input_values = i_input_values.
  ENDMETHOD.

  METHOD build_sequence.
    DATA(search_term)  = condense( i_last_number ).
    DATA(line_index) = line_index( input_values[ table_line = search_term ] ).
    sequence = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = line_index
                           ( input_values[ i ] ) ).
  ENDMETHOD.

  METHOD find_parts_for_sum.
    DATA(temp_diff) = 0.
    DATA(counter) = 1.
    WHILE counter <= lines( sequence ).
      LOOP AT sequence INTO DATA(line) FROM counter.
        r_result = VALUE #( BASE r_result ( line ) ).
        temp_diff = temp_diff + line.
        DATA(difference) = i_sum - temp_diff.
        IF temp_diff > i_sum.
          CLEAR r_result.
          counter = counter + 1.
          temp_diff = 0.
          EXIT.
        ENDIF.
        IF difference = 0.
          RETURN.
        ENDIF.

      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.

  METHOD build_weakness_result.
    DATA(working_values) = i_parts_of_sum.
    SORT working_values.
    r_result = working_values[ 1 ] + working_values[ lines( working_values ) ].
  ENDMETHOD.

ENDCLASS.

CLASS ltc_preamble DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO preamble.
    DATA input_values TYPE stringtab.

    METHODS setup.

    METHODS success_finding_operands_4_40  FOR TESTING.
    METHODS find_not_calculable_number     FOR TESTING.

ENDCLASS.

CLASS ltc_preamble IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( 5 ).
    input_values = VALUE #( ( |35|  )
                            ( |20|  )
                            ( |15|  )
                            ( |25|  )
                            ( |47|  )
                            ( |40|  )
                            ( |62|  )
                            ( |55|  )
                            ( |65|  )
                            ( |95|  )
                            ( |102| )
                            ( |117| )
                            ( |150| )
                            ( |182| )
                            ( |127| )
                            ( |219| )
                            ( |299| )
                            ( |277| )
                            ( |309| )
                            ( |576| ) ).
  ENDMETHOD.

  METHOD success_finding_operands_4_40.
    cut->slice_input( i_input_values  = input_values
                      i_current_index = 6 ).

    cl_abap_unit_assert=>assert_true(
            act = cut->preamble_can_build_sum( 40 )
            msg = |The value 40 should be calculable.| ).
  ENDMETHOD.

  METHOD find_not_calculable_number.
    cl_abap_unit_assert=>assert_equals(
        exp = 127
        act = cut->find_not_calculable_number( input_values )
        msg = |The first not calculable number should be 127.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_seqeuence DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO sequence.

    METHODS setup.

    METHODS find_parts_for_sum   FOR TESTING.
    METHODS build_result_for_weakness FOR TESTING.

ENDCLASS.


CLASS ltc_seqeuence IMPLEMENTATION.

  METHOD setup.
    DATA(input_values) = VALUE stringtab(  ( |35|  )
                                           ( |20|  )
                                           ( |15|  )
                                           ( |25|  )
                                           ( |47|  )
                                           ( |40|  )
                                           ( |62|  )
                                           ( |55|  )
                                           ( |65|  )
                                           ( |95|  )
                                           ( |102| )
                                           ( |117| )
                                           ( |150| )
                                           ( |182| )
                                           ( |127| )
                                           ( |219| )
                                           ( |299| )
                                           ( |277| )
                                           ( |309| )
                                           ( |576| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD find_parts_for_sum.
    DATA(expected_values) = VALUE if_preamble=>t_preamble( ( |15| )
                                                           ( |25| )
                                                           ( |47| )
                                                           ( |40| ) ).
    cut->build_sequence( CONV #( 127 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->find_parts_for_sum( 127 )
        msg = |The expected sequence should be found.| ).
  ENDMETHOD.

  METHOD build_result_for_weakness.
    cut->build_sequence( CONV #( 127 ) ).
    DATA(parts_of_sum) = cut->find_parts_for_sum( 127 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 62
        act = cut->build_weakness_result( parts_of_sum )
        msg = |The expected result should be calculated.| ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
PARAMETERS: preamble TYPE i DEFAULT 25.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(input_processor) = NEW preamble( i_preamble_lines = preamble ).
  DATA(not_calculable_number) = input_processor->find_not_calculable_number( input_values ).
  WRITE / |The value for the first part is { not_calculable_number }|.

  DATA(sequence) = NEW sequence( input_values ).
  sequence->build_sequence( CONV #( not_calculable_number ) ).
  DATA(parts_of_sum) = sequence->find_parts_for_sum( not_calculable_number ).

  WRITE / |The encryption weakness ( Part 2 ) is: { sequence->build_weakness_result( parts_of_sum ) }|.
