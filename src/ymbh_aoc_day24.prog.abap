REPORT ymbh_aoc_day24.

INTERFACE if_directions.
  TYPES: BEGIN OF s_direction,
           direction TYPE char2,
           x_value   TYPE p LENGTH 3 DECIMALS 1,
           y_value   TYPE p LENGTH 3 DECIMALS 1,
         END OF s_direction.
  TYPES t_directions TYPE SORTED TABLE OF s_direction WITH UNIQUE KEY primary_key COMPONENTS direction.

  METHODS get_next_direction
    RETURNING
      VALUE(r_result) TYPE if_directions=>s_direction.

ENDINTERFACE.

INTERFACE if_tile.
  TYPES: BEGIN OF s_coordinates,
           x TYPE p LENGTH 3 DECIMALS 1,
           y TYPE p LENGTH 3 DECIMALS 1,
         END OF s_coordinates.

  METHODS get_coordinates
    RETURNING
      VALUE(r_coordinates) TYPE if_tile=>s_coordinates.

  METHODS set_coordinates
    IMPORTING
      i_coordinates TYPE if_tile=>s_coordinates.

  METHODS get_color
    RETURNING
      VALUE(r_color) TYPE string.

  METHODS toggle_color.

ENDINTERFACE.

INTERFACE if_tile_collection.
  TYPES t_tiles TYPE STANDARD TABLE OF REF TO if_tile WITH DEFAULT KEY.

  METHODS add_tile
    IMPORTING
      i_tile TYPE REF TO if_tile.

  METHODS get_tile_by_coordinates
    IMPORTING
      i_coordinates TYPE if_tile=>s_coordinates
    RETURNING
      VALUE(r_tile) TYPE REF TO if_tile.

  METHODS get_sum_of_color
    IMPORTING
      i_color         TYPE string
    RETURNING
      VALUE(r_result) TYPE i.
ENDINTERFACE.


CLASS directions DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_directions.

    METHODS constructor
      IMPORTING
        i_directions TYPE string.

  PRIVATE SECTION.
    DATA direction_string TYPE string.
    DATA directions       TYPE if_directions=>t_directions.

    METHODS build_directions_template
      RETURNING
        VALUE(r_directions) TYPE if_directions=>t_directions.

    METHODS extract_next_direction
      IMPORTING
        i_direction     TYPE char2
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS extract_part_from_directions
      IMPORTING
        i_direction     TYPE char2
        i_offset        TYPE i
      RETURNING
        VALUE(r_result) TYPE string.

ENDCLASS.

CLASS directions IMPLEMENTATION.

  METHOD constructor.
    direction_string = i_directions.
    directions       = build_directions_template( ).
  ENDMETHOD.

  METHOD build_directions_template.
    r_directions = VALUE #( ( direction = |e|  x_value = 2  y_value = 0    )
                            ( direction = |ne| x_value = 1  y_value = '+1.5' )
                            ( direction = |nw| x_value = -1 y_value = '+1.5' )
                            ( direction = |se| x_value = 1  y_value = '-1.5' )
                            ( direction = |sw| x_value = -1 y_value = '-1.5' )
                            ( direction = |w|  x_value = -2 y_value = 0    ) ).
  ENDMETHOD.

  METHOD if_directions~get_next_direction.
    LOOP AT directions INTO DATA(line).
      IF extract_next_direction( i_direction = line-direction ) IS NOT INITIAL.
        r_result = line.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_next_direction.
    FIND FIRST OCCURRENCE OF i_direction IN direction_string RESULTS DATA(result).
    IF sy-subrc = 0.
      r_result = extract_part_from_directions( i_direction = i_direction
                                               i_offset    = result-offset ).
    ENDIF.
  ENDMETHOD.

  METHOD extract_part_from_directions.
    IF i_offset = 0.
      DATA(len) = strlen( i_direction ).
      r_result = direction_string(len).
      SHIFT direction_string LEFT BY len PLACES.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS tile DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_tile.
    METHODS constructor.

  PRIVATE SECTION.
    DATA coordinates TYPE if_tile=>s_coordinates.
    DATA color TYPE string.

ENDCLASS.

CLASS tile IMPLEMENTATION.

  METHOD constructor.
    color = |White|.
  ENDMETHOD.

  METHOD if_tile~get_coordinates.
    r_coordinates = coordinates.
  ENDMETHOD.

  METHOD if_tile~set_coordinates.
    coordinates = i_coordinates.
  ENDMETHOD.

  METHOD if_tile~get_color.
    r_color = color.
  ENDMETHOD.

  METHOD if_tile~toggle_color.
    color = SWITCH #( color WHEN 'White' THEN |Black|
                            WHEN 'Black' THEN |White| ).
  ENDMETHOD.

ENDCLASS.

CLASS tile_collection DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_tile_collection.

  PRIVATE SECTION.
    DATA tiles TYPE if_tile_collection=>t_tiles.

ENDCLASS.

CLASS tile_collection IMPLEMENTATION.

  METHOD if_tile_collection~get_tile_by_coordinates.
    LOOP AT tiles INTO DATA(tile).
      IF tile->get_coordinates( ) = i_coordinates.
        r_tile = tile.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_tile_collection~add_tile.
    IF if_tile_collection~get_tile_by_coordinates( i_tile->get_coordinates( ) ) IS NOT BOUND.
      APPEND i_tile TO tiles.
    ENDIF.
  ENDMETHOD.

  METHOD if_tile_collection~get_sum_of_color.
    r_result = REDUCE #( INIT sum = 0
                         FOR tile IN tiles
                         NEXT sum = COND #( WHEN tile->get_color( ) = i_color THEN sum + 1
                                            ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.


CLASS locator DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS determine_location
      IMPORTING
        i_instruction        TYPE string
      RETURNING
        VALUE(r_coordinates) TYPE if_tile=>s_coordinates.


  PRIVATE SECTION.
    DATA coordinates TYPE if_tile=>s_coordinates.

    METHODS process_new_coordinates
      IMPORTING
        i_direction TYPE REF TO directions.

ENDCLASS.

CLASS locator IMPLEMENTATION.

  METHOD constructor.
    coordinates = VALUE #( x = 0 y = 0 ).
  ENDMETHOD.

  METHOD determine_location.
    DATA(direction) = NEW directions( i_instruction ).
    process_new_coordinates( direction ).
    r_coordinates = coordinates.
  ENDMETHOD.

  METHOD process_new_coordinates.
    DATA(next_coordinates) = i_direction->if_directions~get_next_direction( ).
    IF next_coordinates IS NOT INITIAL.
      process_new_coordinates( i_direction ).
    ENDIF.
    coordinates-x = coordinates-x + next_coordinates-x_value.
    coordinates-y = coordinates-y + next_coordinates-y_value.
  ENDMETHOD.

ENDCLASS.

CLASS processor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

    METHODS process_values.

    METHODS get_black_tiles
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA input_values TYPE stringtab.
    DATA tiles TYPE REF TO if_tile_collection.

ENDCLASS.

CLASS processor IMPLEMENTATION.

  METHOD constructor.
    input_values = i_input_values.
    tiles = NEW tile_collection(  ).
  ENDMETHOD.

  METHOD get_black_tiles.
    r_result = tiles->get_sum_of_color( |Black| ).
  ENDMETHOD.

  METHOD process_values.
    LOOP AT input_values INTO DATA(value).
      DATA(locator) = NEW locator( ).
      DATA(coordinates) = locator->determine_location( value ).
      DATA(existing_tile) = tiles->get_tile_by_coordinates( coordinates ).
      IF existing_tile IS INITIAL.
        DATA(tile) = NEW tile( ).
        tile->if_tile~set_coordinates( coordinates ).
        tile->if_tile~toggle_color( ).
        tiles->add_tile( tile ).
      ELSE.
        existing_tile->toggle_color( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_directions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO directions.

    METHODS setup.

    METHODS get_fully_first_direction FOR TESTING.
    METHODS get_fully_3rd_direction   FOR TESTING.
ENDCLASS.


CLASS ltc_directions IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |sesenwnenenewseeswwswswwnenewsewsw| ).
  ENDMETHOD.

  METHOD get_fully_3rd_direction.
    DATA(expected_direction) = VALUE if_directions=>s_direction( direction = |nw| x_value = -1 y_value = '+1.5' ).

    cut->if_directions~get_next_direction( ).
    cut->if_directions~get_next_direction( ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_direction
        act = cut->if_directions~get_next_direction( )
        msg = |The method should deliver the direction and the repsective values for x and y.| ).
  ENDMETHOD.

  METHOD get_fully_first_direction.
    DATA(expected_direction) = VALUE if_directions=>s_direction( direction = |se| x_value = 1 y_value = '-1.5' ).

    cl_abap_unit_assert=>assert_equals(
        exp = expected_direction
        act = cut->if_directions~get_next_direction(  )
        msg = |The method should deliver the direction and the repsective values for x and y.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_tile DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO tile.

    METHODS setup.
    METHODS get_coordinates_from_tile FOR TESTING.
    METHODS get_initial_color_of_tile FOR TESTING.
    METHODS get_color_after_toggle    FOR TESTING.
ENDCLASS.


CLASS ltc_tile IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_coordinates_from_tile.
    DATA(expected_coordinates) = VALUE if_tile=>s_coordinates( x = 1 y = '-1.5' ).
    cut->if_tile~set_coordinates( VALUE #( x = '1.0' y = '-1.5' ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_coordinates
        act = cut->if_tile~get_coordinates( )
        msg = |The epxected coordinates should be delivered.| ).
  ENDMETHOD.

  METHOD get_initial_color_of_tile.
    cl_abap_unit_assert=>assert_equals(
        exp = |White|
        act = cut->if_tile~get_color( )
        msg = |The initial color of the tile should be white.| ).
  ENDMETHOD.

  METHOD get_color_after_toggle.
    cut->if_tile~toggle_color( ).
    cl_abap_unit_assert=>assert_equals(
        exp = |Black|
        act = cut->if_tile~get_color( )
        msg = |After toggling the color should be Black.| ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_tile_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO tile_collection.

    METHODS setup.
    METHODS add_tile_to_collection
      IMPORTING
        i_coordinates TYPE if_tile=>s_coordinates.

    METHODS find_coordinates_of_tile   FOR TESTING.
    METHODS change_color_of_found_tile FOR TESTING.

ENDCLASS.


CLASS ltc_tile_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    add_tile_to_collection( VALUE #( x = 1  y = -1    ) ).
    add_tile_to_collection( VALUE #( x = -2 y = '1.5' ) ).
  ENDMETHOD.

  METHOD add_tile_to_collection.
    DATA(tile)  = NEW tile( ).
    tile->if_tile~set_coordinates( i_coordinates ).
    tile->if_tile~toggle_color( ).
    cut->if_tile_collection~add_tile( tile ).
  ENDMETHOD.

  METHOD find_coordinates_of_tile.
    DATA(expected_coordinates) = VALUE if_tile=>s_coordinates( x = -2 y = '1.5' ).
    DATA(coordinates)          = VALUE if_tile=>s_coordinates( x = -2 y = '1.5' ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_coordinates
        act = cut->if_tile_collection~get_tile_by_coordinates( coordinates )->get_coordinates( )
        msg = |The found tile should have the right coordinates.| ).
  ENDMETHOD.

  METHOD change_color_of_found_tile.
    DATA(coordinates) = VALUE if_tile=>s_coordinates( x = 1 y = -1 ).
    DATA(tile) = cut->if_tile_collection~get_tile_by_coordinates( coordinates ).
    IF tile IS BOUND.
      tile->toggle_color( ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
        exp = |White|
        act = cut->if_tile_collection~get_tile_by_coordinates( coordinates )->get_color( )
        msg = |The color of the found tile should be as expected.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_locator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO locator.

    METHODS setup.

    METHODS coordinates_from_full_instrctn FOR TESTING.
    METHODS coordinates_from_full_instrc_2 FOR TESTING.

ENDCLASS.


CLASS ltc_locator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD coordinates_from_full_instrctn.
    DATA(expected_coordinates) = VALUE if_tile=>s_coordinates( x = 1 y = '-1.5' ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_coordinates
        act = cut->determine_location( |esew| )
        msg = |The coordinates should be the expected ones.| ).
  ENDMETHOD.

  METHOD coordinates_from_full_instrc_2.
    DATA(expected_coordinates) = VALUE if_tile=>s_coordinates( x = 0 y = 0 ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_coordinates
        act = cut->determine_location( |nwwswee| )
        msg = |The coordinates should be the expected ones.| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_processor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO processor.

    METHODS setup.
    METHODS process_1_tile  FOR TESTING.
    METHODS process_2_tiles FOR TESTING.
    METHODS process_3_tiles FOR TESTING.
ENDCLASS.


CLASS ltc_processor IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( VALUE #( ( |esew| ) ) ).
  ENDMETHOD.

  METHOD process_1_tile.
    cut->process_values( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = cut->get_black_tiles( )
        msg = |There should be 1 black tile.| ).
  ENDMETHOD.

  METHOD process_2_tiles.
    cut = NEW #( VALUE #( ( |esew| )
                          ( |nwwswee| ) ) ).
    cut->process_values( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 2
        act = cut->get_black_tiles( )
        msg = |There should be 2 black tiles.| ).
  ENDMETHOD.

  METHOD process_3_tiles.
    cut = NEW #( VALUE #( ( |esew| )
                          ( |nwwswee| )
                          ( |neeseswswwnwne| ) ) ).
    cut->process_values( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 1
        act = cut->get_black_tiles( )
        msg = |There should be 2 black tiles.| ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(processor) = NEW processor( input_values ).
  processor->process_values( ).

  WRITE / |The answer to part 1 is: { processor->get_black_tiles( ) }|.
