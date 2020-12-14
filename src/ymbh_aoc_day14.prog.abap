REPORT ymbh_aoc_day14.

CLASS binary_tool DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES ty_bit_mask TYPE c LENGTH 36.

    METHODS convert_int_2_bin
      IMPORTING
        i_int           TYPE i
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS convert_bin_2_int
      IMPORTING
        i_binary        TYPE string
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS convert_value_with_mask
      IMPORTING
        i_input         TYPE ty_bit_mask
      RETURNING
        VALUE(r_result) TYPE ty_bit_mask.

    METHODS set_bit_mask
      IMPORTING i_bit_mask TYPE ty_bit_mask.

  PRIVATE SECTION.
    DATA bit_mask TYPE c LENGTH 36.

ENDCLASS.

CLASS binary_tool IMPLEMENTATION.

  METHOD convert_int_2_bin.
    DATA(div) = i_int DIV 2.
    DATA(mod) = i_int MOD 2.
    IF i_int > 1.
      r_result = convert_int_2_bin( div ).
    ENDIF.
    r_result = |{ r_result }{ mod }|.
  ENDMETHOD.

  METHOD convert_bin_2_int.
    r_result = /ui2/cl_number=>base_converter( number = i_binary from = 2 to = 10 ).
  ENDMETHOD.

  METHOD set_bit_mask.
    me->bit_mask = i_bit_mask.
  ENDMETHOD.

  METHOD convert_value_with_mask.
    r_result = REDUCE #( INIT result = ``
                         FOR i = 0 THEN i + 1 UNTIL i = 36
                         LET input_char = substring( val = i_input  off = i len = 1 )
                             mask_value = substring( val = bit_mask off = i len = 1 )
                             conv_char  = SWITCH #( mask_value WHEN 'X' THEN input_char
                                                               ELSE mask_value )
                             IN
                             NEXT result = result && conv_char ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_dec_to_bin DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO binary_tool.

    METHODS setup.

    METHODS encode_9_as_binary       FOR TESTING.
    METHODS encode_101_as_binary     FOR TESTING.
    METHODS decode_73_bin_2_dec      FOR TESTING.

    METHODS conv_11_bin_using_mask   FOR TESTING.
    METHODS conv_0_bin_using_mask    FOR TESTING.
ENDCLASS.


CLASS ltc_dec_to_bin IMPLEMENTATION.

  METHOD setup.
    cut = NEW #(  ).
  ENDMETHOD.

  METHOD encode_9_as_binary.
    cl_abap_unit_assert=>assert_equals(
        exp = |1001|
        act = cut->convert_int_2_bin( 9 )
        msg = |The binary code should be like expected.| ).
  ENDMETHOD.

  METHOD encode_101_as_binary.
    cl_abap_unit_assert=>assert_equals(
        exp = |1100101|
        act = cut->convert_int_2_bin( 101 )
        msg = |The binary code should be like expected.| ).
  ENDMETHOD.

  METHOD decode_73_bin_2_dec.
    cl_abap_unit_assert=>assert_equals(
       exp = 73
       act = cut->convert_bin_2_int( |000000000000000000000000000001001001| )
       msg = |The binary number should be decoded as 73.| ).
  ENDMETHOD.

  METHOD conv_11_bin_using_mask.
    cut->set_bit_mask( 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X' ).
    cl_abap_unit_assert=>assert_equals(
        exp = |000000000000000000000000000001001001|
        act = cut->convert_value_with_mask( |000000000000000000000000000000001011|  )
        msg = |The expected value should be delivered.| ).
  ENDMETHOD.

  METHOD conv_0_bin_using_mask.
    cut->set_bit_mask( 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X' ).
    cl_abap_unit_assert=>assert_equals(
        exp = |000000000000000000000000000001000000|
        act = cut->convert_value_with_mask( |000000000000000000000000000000000000|  )
        msg = |The expected value should be delivered.| ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
