REPORT ymbh_aoc_day14.

INTERFACE if_input_scanner.
  TYPES ty_p_value TYPE p LENGTH 16 DECIMALS 0.
  TYPES ty_bit_mask TYPE c LENGTH 36.
  TYPES: BEGIN OF s_bit_array,
           line          TYPE ty_p_value,
           value_binary  TYPE c LENGTH 36,
           value_decimal TYPE ty_p_value,
           bit_mask      TYPE ty_bit_mask,
         END OF s_bit_array.
  TYPES t_bit_array      TYPE STANDARD TABLE OF s_bit_array WITH DEFAULT KEY.
  TYPES ty_mem_addresses TYPE SORTED TABLE OF ty_p_value WITH UNIQUE KEY table_line.
  TYPES t_bitmask_tab    TYPE STANDARD TABLE OF ty_bit_mask WITH DEFAULT KEY.

ENDINTERFACE.

CLASS binary_tool DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES ty_bit_mask TYPE c LENGTH 36.


    METHODS convert_int_2_bin
      IMPORTING
        i_int           TYPE if_input_scanner=>ty_p_value
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS convert_bin_2_int
      IMPORTING
        i_binary        TYPE string
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>ty_p_value.

    METHODS convert_value_with_mask
      IMPORTING
        i_input         TYPE if_input_scanner=>ty_bit_mask
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>ty_bit_mask.

    METHODS convert_value_with_mask_a
      IMPORTING
        i_input         TYPE if_input_scanner=>ty_bit_mask
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>ty_bit_mask.

    METHODS set_bit_mask
      IMPORTING i_bit_mask TYPE if_input_scanner=>ty_bit_mask.

    METHODS get_bit_mask
      RETURNING
        VALUE(r_result) TYPE ty_bit_mask.

  PRIVATE SECTION.
    DATA bit_mask TYPE c LENGTH 36.

ENDCLASS.

CLASS binary_tool IMPLEMENTATION.

  METHOD convert_int_2_bin.
    DATA(div) = i_int DIV 2.
    DATA(mod) = i_int MOD 2.
    IF i_int > 1.
      r_result = convert_int_2_bin( CONV #( div ) ).
    ENDIF.
    r_result = |{ r_result }{ mod }|.
  ENDMETHOD.

  METHOD convert_bin_2_int.
    r_result = /ui2/cl_number=>base_converter( number = i_binary from = 2 to = 10 ).
  ENDMETHOD.

  METHOD set_bit_mask.
    bit_mask = i_bit_mask.
  ENDMETHOD.

  METHOD get_bit_mask.
    r_result = bit_mask.
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

  METHOD convert_value_with_mask_a.
    r_result = REDUCE #( INIT result = ``
                            FOR i = 0 THEN i + 1 UNTIL i = 36
                            LET input_char = substring( val = i_input  off = i len = 1 )
                                mask_value = substring( val = bit_mask off = i len = 1 )
                                conv_char  = COND #( WHEN mask_value = 'X' THEN 'X'
                                                     WHEN mask_value = '1' THEN '1'
                                                     WHEN mask_value = '0' THEN input_char )
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

CLASS input_scanner DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

    METHODS process_input.

    METHODS summarize_value
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>ty_p_value.

    METHODS get_memory_block
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>t_bit_array.
    METHODS process_input_alternative.
    METHODS generate_all_mem_addresses
      IMPORTING
        i_mem_address   TYPE i
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>ty_mem_addresses.

  PRIVATE SECTION.
    DATA input_values TYPE stringtab.
    DATA memory_block TYPE if_input_scanner=>t_bit_array.
    DATA bit_mask     TYPE REF TO binary_tool.

    METHODS determine_mem_address
      IMPORTING
        i_line               TYPE string
      RETURNING
        VALUE(r_mem_address) TYPE i.

    METHODS find_pattern
      IMPORTING
        i_line         TYPE string
        i_pattern      TYPE string
      RETURNING
        VALUE(r_found) TYPE abap_bool.

    METHODS determine_number
      IMPORTING
        i_line          TYPE string
      RETURNING
        VALUE(r_number) TYPE i.
    METHODS determine_mask
      IMPORTING
        i_line          TYPE string
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS replace_x_with_bin_number
      IMPORTING
        i_address       TYPE if_input_scanner=>ty_bit_mask
        i_offset        TYPE match_result-offset
        i_replacement   TYPE char1
      RETURNING
        VALUE(r_result) TYPE if_input_scanner=>ty_bit_mask.

ENDCLASS.

CLASS input_scanner IMPLEMENTATION.

  METHOD constructor.
    input_values = i_input_values.
  ENDMETHOD.

  METHOD process_input.
    bit_mask = NEW #( ).
    LOOP AT input_values INTO DATA(line).
      DATA(mask) = determine_mask( line ).
      IF mask IS NOT INITIAL.
        bit_mask->set_bit_mask( CONV #( mask ) ).
        CONTINUE.
      ENDIF.

      DATA(mem_address) = determine_mem_address( line ).
      IF mem_address IS NOT INITIAL.
        DATA(number) = determine_number( line ).
        DATA(input_binary) = |{ bit_mask->convert_int_2_bin( CONV #( number ) ) WIDTH = 36 PAD = '0' ALIGN = RIGHT }|.
        DATA(output_binary) = bit_mask->convert_value_with_mask( CONV #( input_binary ) ).
        ASSIGN memory_block[ line = mem_address ] TO FIELD-SYMBOL(<line>).
        IF sy-subrc = 0.
          <line>-value_binary  = output_binary.
          <line>-value_decimal = bit_mask->convert_bin_2_int( CONV #( output_binary ) ).
          <line>-bit_mask      = bit_mask->get_bit_mask( ).
        ELSE.
          memory_block = VALUE #( BASE memory_block ( line          = mem_address
                                                      value_binary  = output_binary
                                                      value_decimal = bit_mask->convert_bin_2_int( CONV #( output_binary ) )
                                                      bit_mask      = bit_mask->get_bit_mask( ) ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_input_alternative.
    bit_mask = NEW #( ).
    LOOP AT input_values INTO DATA(line).
      DATA(mask) = determine_mask( line ).
      IF mask IS NOT INITIAL.
        bit_mask->set_bit_mask( CONV #( mask ) ).
        CONTINUE.
      ENDIF.

      DATA(mem_address) = determine_mem_address( line ).
      DATA(mem_addresses) = generate_all_mem_addresses( mem_address ).

      LOOP AT mem_addresses INTO DATA(mem_single_address).

        IF mem_single_address IS NOT INITIAL.
          DATA(number) = determine_number( line ).
          DATA(input_binary) = |{ bit_mask->convert_int_2_bin( CONV #( number ) ) WIDTH = 36 PAD = '0' ALIGN = RIGHT }|.
          DATA(output_binary) = bit_mask->convert_value_with_mask( CONV #( input_binary ) ).
          ASSIGN memory_block[ line = mem_single_address ] TO FIELD-SYMBOL(<line>).
          IF sy-subrc = 0.
            <line>-value_binary  = output_binary.
            <line>-value_decimal = bit_mask->convert_bin_2_int( CONV #( output_binary ) ).
            <line>-bit_mask      = bit_mask->get_bit_mask( ).
          ELSE.
            memory_block = VALUE #( BASE memory_block ( line          = mem_single_address
                                                        value_binary  = output_binary
                                                        value_decimal = bit_mask->convert_bin_2_int( CONV #( output_binary ) )
                                                        bit_mask      = bit_mask->get_bit_mask( ) ) ).
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD summarize_value.
    LOOP AT memory_block INTO DATA(line).
      r_result = r_result + line-value_decimal.
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_number.
    SPLIT i_line AT '=' INTO DATA(key) DATA(value).
    CONDENSE value.
    r_number = value.
  ENDMETHOD.

  METHOD find_pattern.
    r_found = xsdbool( find( val = i_line off = 0 sub = i_pattern ) >= 0 ).
  ENDMETHOD.

  METHOD determine_mem_address.
    DATA mem_address TYPE string.
    DATA(temp_string) = substring( val = i_line off = 4 len = strlen( i_line ) - 4 ).
    SPLIT temp_string AT ']' INTO mem_address  DATA(rest).
    r_mem_address = condense( mem_address ).
  ENDMETHOD.

  METHOD determine_mask.
    IF find_pattern( i_line = i_line i_pattern = 'mask' ).
      SPLIT i_line AT '=' INTO DATA(key) r_result.
      CONDENSE r_result.
    ENDIF.
  ENDMETHOD.

  METHOD get_memory_block.
    r_result = me->memory_block.
  ENDMETHOD.

  METHOD generate_all_mem_addresses.
    DATA work_tab TYPE if_input_scanner=>t_bitmask_tab.

    FIND ALL OCCURRENCES OF 'X' IN bit_mask->get_bit_mask( ) RESULTS DATA(bitmask_xs).

    DATA(mem_address_bin) = |{ bit_mask->convert_int_2_bin( CONV #( i_mem_address ) ) WIDTH = 36 PAD = '0' ALIGN = RIGHT }|.

    DATA(mem_address_tab) = VALUE if_input_scanner=>t_bitmask_tab(
                    ( |{ bit_mask->convert_value_with_mask_a( CONV #( mem_address_bin ) ) }| ) ).


    LOOP AT bitmask_xs INTO DATA(bitmask).

      LOOP AT mem_address_tab INTO DATA(address).
        DATA(new_address_0) = replace_x_with_bin_number( i_address     = address
                                                         i_offset      = bitmask-offset
                                                         i_replacement = '0' ).
        APPEND new_address_0 TO work_tab.
        DATA(new_address_1) = replace_x_with_bin_number( i_address     = address
                                                         i_offset      = bitmask-offset
                                                         i_replacement = '1' ).
        APPEND new_address_1 TO work_tab.
      ENDLOOP.

      mem_address_tab = work_tab.
      REFRESH work_tab.

    ENDLOOP.

    r_result = VALUE #( FOR addresses IN mem_address_tab
                            ( bit_mask->convert_bin_2_int( CONV #( addresses ) ) ) ).

  ENDMETHOD.


  METHOD replace_x_with_bin_number.
    r_result = i_address.
    r_result+i_offset(1) = i_replacement.
  ENDMETHOD.

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


CLASS ltc_input_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut            TYPE REF TO input_scanner.
    DATA input_values   TYPE stringtab.
    DATA input_values_2 TYPE stringtab.

    METHODS setup.

    METHODS get_sum_from_memory_block     FOR TESTING.
    METHODS get_addresses_bitmask_located FOR TESTING.
    METHODS get_sum_of_new_decoder_logic  FOR TESTING.

ENDCLASS.


CLASS ltc_input_scanner IMPLEMENTATION.

  METHOD setup.
    input_values   = VALUE #( ( |mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X| )
                              ( |mem[8] = 11| )
                              ( |mem[7] = 101| )
                              ( |mem[8] = 0| ) ).

    input_values_2 = VALUE #( ( |mask = 000000000000000000000000000000X1001X| )
                              ( |mem[42] = 100| )
                              ( |mask = 00000000000000000000000000000000X0XX| )
                              ( |mem[26] = 1| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD get_sum_from_memory_block.
    cut->process_input( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 165
        act = cut->summarize_value( )
        msg = |The expected sum should be delivered.| ).
  ENDMETHOD.

  METHOD get_addresses_bitmask_located.
    DATA(expected_addresses) = VALUE if_input_scanner=>ty_mem_addresses( ( CONV #( 26 ) )
                                                                         ( CONV #( 27 ) )
                                                                         ( CONV #( 58 ) )
                                                                         ( CONV #( 59 ) ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_addresses
        act = cut->generate_all_mem_addresses( 42 )
        msg = 'msg' ).
  ENDMETHOD.

  METHOD get_sum_of_new_decoder_logic.
    cut = NEW #( input_values_2 ).
    cut->process_input_alternative( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 208
        act = cut->summarize_value( )
        msg = 'msg' ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(input_scanner) = NEW input_scanner( input_values ).
  input_scanner->process_input( ).

  WRITE / |The result for part 1 is: { input_scanner->summarize_value( ) }|.

  DATA(input_scanner2) = NEW input_scanner( input_values ).
  input_scanner2->process_input_alternative( ).
  WRITE / |The reuslt for part 2 is: { input_scanner2->summarize_value( ) }|.
