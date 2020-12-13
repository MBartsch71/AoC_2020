REPORT ymbh_aoc_day10.

INTERFACE if_array_populator.
  TYPES: BEGIN OF s_adapter_array,
           adapter_1 TYPE i,
           adapter_2 TYPE i,
           adapter_3 TYPE i,
           jolts_1   TYPE i,
           jolts_3   TYPE i,
           binary    TYPE char3,
         END OF s_adapter_array.
  TYPES t_adapter_array TYPE STANDARD TABLE OF s_adapter_array WITH DEFAULT KEY.
  TYPES t_integer       TYPE STANDARD TABLE OF i               WITH DEFAULT KEY.
ENDINTERFACE.

CLASS array_populator DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS build_array
      IMPORTING
        i_input_values         TYPE any
      RETURNING
        VALUE(r_adapter_array) TYPE if_array_populator=>t_adapter_array.

  PRIVATE SECTION.
    DATA adapter_array TYPE if_array_populator=>t_adapter_array.
    DATA input_values TYPE stringtab.

    METHODS determine_value_for_field
      IMPORTING
        i_offset       TYPE i
      RETURNING
        VALUE(r_value) TYPE i.

    METHODS check_input
      IMPORTING
        i_value         TYPE string
      RETURNING
        VALUE(r_exists) TYPE abap_bool.
ENDCLASS.

CLASS array_populator IMPLEMENTATION.

  METHOD build_array.
    DATA(row) = 3.
    input_values = i_input_values.

    DO.
      DATA(adapter_row) = VALUE if_array_populator=>s_adapter_array(
           adapter_1 = determine_value_for_field( row - 2 )
           adapter_2 = determine_value_for_field( row - 1 )
           adapter_3 = determine_value_for_field( row ) ).

      IF adapter_row IS NOT INITIAL.
        r_adapter_array = VALUE #( BASE r_adapter_array ( adapter_row ) ).
        row = row + 3.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD determine_value_for_field.
    DATA(value) = condense( CONV string( i_offset ) ).
    r_value = SWITCH #( check_input( i_value = value )
                            WHEN abap_true THEN value
                            ELSE 0 ).
  ENDMETHOD.

  METHOD check_input.
    r_exists = xsdbool( line_exists( input_values[ table_line = i_value ] ) ).
  ENDMETHOD.

ENDCLASS.


CLASS array_analyzer DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_adapter_array TYPE if_array_populator=>t_adapter_array.

    METHODS populate_1_jolts
      RETURNING
        VALUE(r_result) TYPE if_array_populator=>t_adapter_array.
    METHODS populate_3_jolts
      RETURNING
        VALUE(r_result) TYPE if_array_populator=>t_adapter_array.
    METHODS calculate_jolts_product
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA adapter_array TYPE if_array_populator=>t_adapter_array.
    DATA bitcode_compare TYPE RANGE OF char6.
    DATA jolts_1 TYPE i.
    DATA jolts_3 TYPE i.

    METHODS analyze_1_jolts
      IMPORTING
        i_line          TYPE if_array_populator=>s_adapter_array
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS sum_jolts_1
      IMPORTING
        i_line_adapter_1 TYPE i
      RETURNING
        VALUE(r_result)  TYPE i.
    METHODS sum_jolts_3
      IMPORTING
        i_jolts         TYPE i
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS build_binary
      IMPORTING
        i_line          TYPE if_array_populator=>s_adapter_array
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS is_in_bitmap
      IMPORTING
        i_temp          TYPE char6
      RETURNING
        VALUE(r_result) TYPE abap_bool.


ENDCLASS.

CLASS array_analyzer IMPLEMENTATION.

  METHOD constructor.
    adapter_array = i_adapter_array.
    bitcode_compare = VALUE #( ( sign = |I| option = |EQ| low = |100111| )
                               ( sign = |I| option = |EQ| low = |110010| )
                               ( sign = |I| option = |EQ| low = |010011| )
                               ( sign = |I| option = |EQ| low = |110011| )
                               ( sign = |I| option = |EQ| low = |100100| )
                               ( sign = |I| option = |EQ| low = |001001| )
                               ( sign = |I| option = |EQ| low = |001111| )
                               ( sign = |I| option = |EQ| low = |100110| )
                               ( sign = |I| option = |EQ| low = |001110| )
                               ( sign = |I| option = |EQ| low = |001100| ) ).
  ENDMETHOD.

  METHOD populate_1_jolts.
    LOOP AT adapter_array ASSIGNING FIELD-SYMBOL(<line>).
      <line>-jolts_1 = analyze_1_jolts( <line> ).
    ENDLOOP.
    r_result = adapter_array.
  ENDMETHOD.


  METHOD analyze_1_jolts.
    DATA(line_index) = line_index( adapter_array[ adapter_1 = i_line-adapter_1 ] ).

    DATA(line_tab) = VALUE if_array_populator=>t_integer( ( i_line-adapter_1 )
                                                          ( i_line-adapter_2 )
                                                          ( i_line-adapter_3 ) ).
    r_result = REDUCE #( INIT sum = 0
                         FOR line IN line_tab
                         NEXT sum = COND #( WHEN line = 0 THEN sum
                                            ELSE sum + 1  ) ).
    r_result = COND #( WHEN r_result > 0 THEN r_result - 1 ).
    r_result = COND #( WHEN line_index = 1 AND i_line-adapter_1 IS NOT INITIAL THEN r_result + 1
                       ELSE r_result ).
    r_result = COND #( WHEN line_index > 1 AND adapter_array[ line_index - 1 ]-adapter_3 IS NOT INITIAL
                                           AND i_line-adapter_1 IS NOT INITIAL
                            THEN r_result + 1
                            ELSE r_result ).
  ENDMETHOD.

  METHOD populate_3_jolts.
    DATA(empty_cells) = 0.

    LOOP AT adapter_array ASSIGNING FIELD-SYMBOL(<line>).
      <line>-binary = build_binary( <line> ).
    ENDLOOP.

    LOOP AT adapter_array ASSIGNING <line> FROM 2 TO lines( adapter_array ) - 1.
      DATA(line_index) = sy-tabix.
      DATA(temp) = |{ <line>-binary }{ adapter_array[ line_index + 1 ]-binary }|.
      IF temp IN bitcode_compare.
        <line>-jolts_3 = 1.
      ENDIF.
    ENDLOOP.

    adapter_array[ lines( adapter_array ) ]-jolts_3 = 1.
    r_result = adapter_array.
  ENDMETHOD.

  METHOD is_in_bitmap.
    r_result = COND #( WHEN i_temp IN bitcode_compare THEN abap_true
                       ELSE abap_false ).
  ENDMETHOD.

  METHOD build_binary.
    DATA(adpater_tab) = VALUE if_array_populator=>t_integer( ( i_line-adapter_1 )
                                                             ( i_line-adapter_2 )
                                                             ( i_line-adapter_3 ) ).
    LOOP AT adpater_tab INTO DATA(line).
      IF line IS INITIAL.
        r_result = |{ r_result }{ |0| }|.
      ELSE.
        r_result = |{ r_result }{ |1| }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_jolts_product.
    r_result = REDUCE #( INIT product = 0
                         FOR line IN adapter_array
                         LET sum_jolt1 = sum_jolts_1( line-jolts_1 )
                             sum_jolt3 = sum_jolts_3( line-jolts_3 )
                         IN
                         NEXT product = sum_jolt1 * sum_jolt3 ).
  ENDMETHOD.

  METHOD sum_jolts_1.
    jolts_1 = jolts_1 + i_line_adapter_1.
    r_result = jolts_1.
  ENDMETHOD.

  METHOD sum_jolts_3.
    jolts_3 = jolts_3 + i_jolts.
    r_result = jolts_3.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_array_populator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO array_populator.
    DATA input_values   TYPE stringtab.
    DATA input_values_2 TYPE stringtab.

    METHODS setup.
    METHODS build_small_array  FOR TESTING.
    METHODS check_bigger_array FOR TESTING.
ENDCLASS.


CLASS ltc_array_populator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_values = VALUE #( ( |16| )
                            ( |10| )
                            ( |15| )
                            ( |5|  )
                            ( |1|  )
                            ( |11| )
                            ( |7|  )
                            ( |19| )
                            ( |6|  )
                            ( |12| )
                            ( |4| ) ).

    input_values_2 = VALUE #( ( |28| )
                              ( |33| )
                              ( |18| )
                              ( |42| )
                              ( |31| )
                              ( |14| )
                              ( |46| )
                              ( |20| )
                              ( |48| )
                              ( |47| )
                              ( |24| )
                              ( |23| )
                              ( |49| )
                              ( |45| )
                              ( |19| )
                              ( |38| )
                              ( |39| )
                              ( |11| )
                              ( |1|  )
                              ( |32| )
                              ( |25| )
                              ( |35| )
                              ( |8|  )
                              ( |17| )
                              ( |7|  )
                              ( |9|  )
                              ( |4|  )
                              ( |2|  )
                              ( |34| )
                              ( |10| )
                              ( |3|  ) ).

  ENDMETHOD.

  METHOD build_small_array.
    DATA(expected_array) = VALUE if_array_populator=>t_adapter_array(
                                   ( adapter_1 = 1  adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 4  adapter_2 = 5  adapter_3 = 6  )
                                   ( adapter_1 = 7  adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 10 adapter_2 = 11 adapter_3 = 12 )
                                   ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 15 )
                                   ( adapter_1 = 16 adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 19 adapter_2 = 0  adapter_3 = 0  ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_array
        act = cut->build_array( input_values )
        msg = |The built array should match the expected one.| ).
  ENDMETHOD.

  METHOD check_bigger_array.
    DATA(expected_array) = VALUE if_array_populator=>t_adapter_array(
                                   ( adapter_1 = 1  adapter_2 = 2  adapter_3 = 3  )
                                   ( adapter_1 = 4  adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 7  adapter_2 = 8  adapter_3 = 9  )
                                   ( adapter_1 = 10 adapter_2 = 11 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 14 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 17 adapter_3 = 18 )
                                   ( adapter_1 = 19 adapter_2 = 20 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 23 adapter_3 = 24 )
                                   ( adapter_1 = 25 adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 28 adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 31 adapter_2 = 32 adapter_3 = 33 )
                                   ( adapter_1 = 34 adapter_2 = 35 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 38 adapter_3 = 39 )
                                   ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 42 )
                                   ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 45 )
                                   ( adapter_1 = 46 adapter_2 = 47 adapter_3 = 48 )
                                   ( adapter_1 = 49 adapter_2 = 0  adapter_3 = 0  ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_array
        act = cut->build_array( input_values_2 )
        msg = |The built array should match the expected one.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_array_analyzer DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO array_analyzer.

    METHODS setup.
    METHODS populate_1_jolts_column FOR TESTING.
    METHODS populate_3_jolts_column FOR TESTING.
    METHODS calculate_jolts_product FOR TESTING.
ENDCLASS.


CLASS ltc_array_analyzer IMPLEMENTATION.

  METHOD setup.
    DATA(adapter_array) = VALUE if_array_populator=>t_adapter_array(
                                   ( adapter_1 = 1  adapter_2 = 2  adapter_3 = 3  )
                                   ( adapter_1 = 4  adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 7  adapter_2 = 8  adapter_3 = 9  )
                                   ( adapter_1 = 10 adapter_2 = 11 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 14 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 17 adapter_3 = 18 )
                                   ( adapter_1 = 19 adapter_2 = 20 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 23 adapter_3 = 24 )
                                   ( adapter_1 = 25 adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 28 adapter_2 = 0  adapter_3 = 0  )
                                   ( adapter_1 = 31 adapter_2 = 32 adapter_3 = 33 )
                                   ( adapter_1 = 34 adapter_2 = 35 adapter_3 = 0  )
                                   ( adapter_1 = 0  adapter_2 = 38 adapter_3 = 39 )
                                   ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 42 )
                                   ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 45 )
                                   ( adapter_1 = 46 adapter_2 = 47 adapter_3 = 48 )
                                   ( adapter_1 = 49 adapter_2 = 0  adapter_3 = 0  ) ).
    cut = NEW #( adapter_array ).
  ENDMETHOD.

  METHOD populate_1_jolts_column.
    DATA(expected_array) = VALUE if_array_populator=>t_adapter_array(
                                  ( adapter_1 = 1  adapter_2 = 2  adapter_3 = 3  jolts_1 = 3 )
                                  ( adapter_1 = 4  adapter_2 = 0  adapter_3 = 0  jolts_1 = 1 )
                                  ( adapter_1 = 7  adapter_2 = 8  adapter_3 = 9  jolts_1 = 2 )
                                  ( adapter_1 = 10 adapter_2 = 11 adapter_3 = 0  jolts_1 = 2 )
                                  ( adapter_1 = 0  adapter_2 = 14 adapter_3 = 0  jolts_1 = 0 )
                                  ( adapter_1 = 0  adapter_2 = 17 adapter_3 = 18 jolts_1 = 1 )
                                  ( adapter_1 = 19 adapter_2 = 20 adapter_3 = 0  jolts_1 = 2 )
                                  ( adapter_1 = 0  adapter_2 = 23 adapter_3 = 24 jolts_1 = 1 )
                                  ( adapter_1 = 25 adapter_2 = 0  adapter_3 = 0  jolts_1 = 1 )
                                  ( adapter_1 = 28 adapter_2 = 0  adapter_3 = 0  jolts_1 = 0 )
                                  ( adapter_1 = 31 adapter_2 = 32 adapter_3 = 33 jolts_1 = 2 )
                                  ( adapter_1 = 34 adapter_2 = 35 adapter_3 = 0  jolts_1 = 2 )
                                  ( adapter_1 = 0  adapter_2 = 38 adapter_3 = 39 jolts_1 = 1 )
                                  ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 42 jolts_1 = 0 )
                                  ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 45 jolts_1 = 0 )
                                  ( adapter_1 = 46 adapter_2 = 47 adapter_3 = 48 jolts_1 = 3 )
                                  ( adapter_1 = 49 adapter_2 = 0  adapter_3 = 0  jolts_1 = 1 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_array
        act = cut->populate_1_jolts(  )
        msg = |The expected array should match the actual one.| ).
  ENDMETHOD.

  METHOD populate_3_jolts_column.
    DATA(expected_array) = VALUE if_array_populator=>t_adapter_array(
                                  ( adapter_1 = 1  adapter_2 = 2  adapter_3 = 3  jolts_3 = 0 binary = |111| )
                                  ( adapter_1 = 4  adapter_2 = 0  adapter_3 = 0  jolts_3 = 1 binary = |100| )
                                  ( adapter_1 = 7  adapter_2 = 8  adapter_3 = 9  jolts_3 = 0 binary = |111| )
                                  ( adapter_1 = 10 adapter_2 = 11 adapter_3 = 0  jolts_3 = 1 binary = |110| )
                                  ( adapter_1 = 0  adapter_2 = 14 adapter_3 = 0  jolts_3 = 1 binary = |010| )
                                  ( adapter_1 = 0  adapter_2 = 17 adapter_3 = 18 jolts_3 = 0 binary = |011| )
                                  ( adapter_1 = 19 adapter_2 = 20 adapter_3 = 0  jolts_3 = 1 binary = |110| )
                                  ( adapter_1 = 0  adapter_2 = 23 adapter_3 = 24 jolts_3 = 0 binary = |011| )
                                  ( adapter_1 = 25 adapter_2 = 0  adapter_3 = 0  jolts_3 = 1 binary = |100| )
                                  ( adapter_1 = 28 adapter_2 = 0  adapter_3 = 0  jolts_3 = 1 binary = |100| )
                                  ( adapter_1 = 31 adapter_2 = 32 adapter_3 = 33 jolts_3 = 0 binary = |111| )
                                  ( adapter_1 = 34 adapter_2 = 35 adapter_3 = 0  jolts_3 = 1 binary = |110| )
                                  ( adapter_1 = 0  adapter_2 = 38 adapter_3 = 39 jolts_3 = 0 binary = |011| )
                                  ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 42 jolts_3 = 1 binary = |001| )
                                  ( adapter_1 = 0  adapter_2 = 0  adapter_3 = 45 jolts_3 = 1 binary = |001| )
                                  ( adapter_1 = 46 adapter_2 = 47 adapter_3 = 48 jolts_3 = 0 binary = |111| )
                                  ( adapter_1 = 49 adapter_2 = 0  adapter_3 = 0  jolts_3 = 1 binary = |100| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_array
        act = cut->populate_3_jolts(  )
        msg = |The expected array should match the actual one.| ).
  ENDMETHOD.

  METHOD calculate_jolts_product.
    cut->populate_1_jolts( ).
    cut->populate_3_jolts( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 220
        act = cut->calculate_jolts_product( )
        msg = |The product of jolts should be 220.| ).
  ENDMETHOD.

ENDCLASS.


DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(array_populator) = NEW array_populator( ).
  DATA(adapter_array) = array_populator->build_array( input_values ).
  DATA(array_analyzer)  = NEW array_analyzer( adapter_array ).
  array_analyzer->populate_1_jolts( ).
  array_analyzer->populate_3_jolts( ).
  WRITE / |The result of part 1 is: { array_analyzer->calculate_jolts_product( ) }|.
