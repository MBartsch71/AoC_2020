REPORT ymbh_aoc_day25.

INTERFACE if_constants.
  CONSTANTS c_mod                    TYPE i VALUE 20201227.
  CONSTANTS c_initial_subject_number TYPE i VALUE 7.
ENDINTERFACE.

INTERFACE if_private_key.
  METHODS determine_key
    IMPORTING
      i_loop_size     TYPE i
    RETURNING
      VALUE(r_result) TYPE i.
ENDINTERFACE.

INTERFACE if_public_key.
  METHODS determine_loop_size
    IMPORTING
      i_public_key       TYPE i
    RETURNING
      VALUE(r_loop_size) TYPE i.
ENDINTERFACE.

CLASS private_key DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_private_key.
    METHODS constructor
      IMPORTING
        i_subject_number TYPE i.

  PRIVATE SECTION.
    DATA subject_number TYPE p LENGTH 16.

ENDCLASS.

CLASS private_key IMPLEMENTATION.

  METHOD constructor.
    subject_number = i_subject_number.
  ENDMETHOD.

  METHOD if_private_key~determine_key.
    DATA(number) = subject_number MOD if_constants=>c_mod.
    DO i_loop_size - 1 TIMES.
      number = number * subject_number.
      number = number MOD if_constants=>c_mod.
    ENDDO.
    r_result = number.
  ENDMETHOD.

ENDCLASS.

CLASS public_key DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_public_key.

ENDCLASS.

CLASS public_key IMPLEMENTATION.

  METHOD if_public_key~determine_loop_size.
    DATA loop_counter TYPE i VALUE 1.
    DATA(number) = if_constants=>c_initial_subject_number.
    WHILE number <> i_public_key.
      loop_counter = loop_counter + 1.
      number = number * if_constants=>c_initial_subject_number.
      number = number MOD if_constants=>c_mod.
    ENDWHILE.
    r_loop_size = loop_counter.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_public_key
 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO public_key.

    METHODS setup.
    METHODS determine_loop_size_8  FOR TESTING.
    METHODS determine_loop_size_11 FOR TESTING.
ENDCLASS.

CLASS ltc_public_key IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD determine_loop_size_8.
    cl_abap_unit_assert=>assert_equals(
        exp = 8
        act = cut->if_public_key~determine_loop_size( 5764801 )
        msg = |The expected loop size should be determined.| ).
  ENDMETHOD.

  METHOD determine_loop_size_11.
    cl_abap_unit_assert=>assert_equals(
        exp = 11
        act = cut->if_public_key~determine_loop_size( 17807724 )
        msg = |The expected loop size should be determined.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_private_key DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO private_key.

    METHODS get_private_key_of_17807724 FOR TESTING.
    METHODS get_private_key_of_5764801  FOR TESTING.
ENDCLASS.


CLASS ltc_private_key IMPLEMENTATION.

  METHOD get_private_key_of_17807724.
    cut = NEW #( 17807724 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 14897079
        act = cut->if_private_key~determine_key( 8 )
        msg = |The private key should be the expected one.| ).
  ENDMETHOD.

  METHOD get_private_key_of_5764801.
    cut = NEW #( 5764801 ).
    cl_abap_unit_assert=>assert_equals(
        exp = 14897079
        act = cut->if_private_key~determine_key( 11 )
        msg = |The private key should be the expected one.| ).
  ENDMETHOD.

ENDCLASS.


DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(door_pubk) = CONV i( input_values[ 1 ] ).
  DATA(card_pubk) = CONV i( input_values[ 2 ] ).

  DATA(door_loopsize) = NEW public_key( )->if_public_key~determine_loop_size( door_pubk ).
  DATA(card_loopsize) = NEW public_key( )->if_public_key~determine_loop_size( card_pubk ).

  DATA(door_handshake) = NEW private_key( card_pubk )->if_private_key~determine_key( door_loopsize ).
  DATA(card_handshake) = NEW private_key( door_pubk )->if_private_key~determine_key( card_loopsize ).

  WRITE / |The door handshake is: { door_handshake }|.
  WRITE / |The card handshake is: { card_handshake }|.
