REPORT ymbh_aoc_day22.

CLASS cx_player DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

INTERFACE if_combat.

  TYPES: BEGIN OF s_game,
           count  TYPE i,
           player TYPE i,
         END OF s_game.
  TYPES t_game TYPE STANDARD TABLE OF s_game WITH DEFAULT KEY.

  TYPES: BEGIN OF s_result,
           count         TYPE i,
           multiplicator TYPE i,
           sum           TYPE i,
         END OF s_result.
  TYPES t_result TYPE STANDARD TABLE OF s_result WITH DEFAULT KEY.
ENDINTERFACE.

INTERFACE if_player.
  TYPES: BEGIN OF s_player_deck,
           count TYPE i,
         END OF s_player_deck.
  TYPES t_player_deck TYPE STANDARD TABLE OF s_player_deck WITH DEFAULT KEY.

  CONSTANTS: BEGIN OF player_names,
               player1 TYPE c LENGTH 9 VALUE 'Player 1:',
               player2 TYPE c LENGTH 9 VALUE 'Player 2:',
             END OF player_names.

  METHODS get_players_deck
    RETURNING
      VALUE(r_deck) TYPE t_player_deck.

  METHODS add_winning_cards
    IMPORTING
      i_cards TYPE if_combat=>t_game.

  METHODS put_back_not_used_card
    IMPORTING
      i_cards TYPE if_combat=>t_game.

  METHODS play_card
    RETURNING
      VALUE(r_played) TYPE if_combat=>s_game
    RAISING
      cx_player.

ENDINTERFACE.


CLASS result DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS calculate_result
      IMPORTING
        i_player_values TYPE if_player=>t_player_deck
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA result_tab TYPE if_combat=>t_result.
    METHODS init
      IMPORTING
        i_lines TYPE i.

ENDCLASS.

CLASS result IMPLEMENTATION.

  METHOD init.
    result_tab = VALUE #( FOR i = i_lines THEN i - 1 UNTIL i = 0
                            ( multiplicator = i ) ).
  ENDMETHOD.

  METHOD calculate_result.
    init( lines( i_player_values ) ).
    LOOP AT i_player_values ASSIGNING FIELD-SYMBOL(<line>).
      ASSIGN result_tab[ sy-tabix ] TO FIELD-SYMBOL(<result>).
      IF sy-subrc = 0.
        <result>-count = <line>-count.
        <result>-sum   = <line>-count * <result>-multiplicator.
      ENDIF.
    ENDLOOP.

    r_result = REDUCE #( INIT sum = 0
                        FOR line IN result_tab
                        NEXT sum = sum + line-sum ).
  ENDMETHOD.

ENDCLASS.

CLASS player_1 DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_player.

    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

  PRIVATE SECTION.
    DATA player_number TYPE i VALUE 1.
    DATA deck TYPE if_player=>t_player_deck.

ENDCLASS.

CLASS player_1 IMPLEMENTATION.

  METHOD constructor.
    FIND if_player=>player_names-player1 IN TABLE i_input_values RESULTS DATA(result).
    DATA(start_line) = result-line + 1.
    FIND if_player=>player_names-player2 IN TABLE i_input_values RESULTS result.
    DATA(end_line) = result-line - 1.

    deck = VALUE #( FOR value IN i_input_values FROM start_line TO end_line
                        ( count = value ) ).

  ENDMETHOD.

  METHOD if_player~get_players_deck.
    r_deck = deck.
  ENDMETHOD.

  METHOD if_player~add_winning_cards.
    TRY.
        IF i_cards[ 1 ]-player = player_number.
          deck = VALUE #( BASE deck FOR line IN i_cards
                               ( count = line-count ) ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD if_player~play_card.
    TRY.
        r_played = VALUE #( count = deck[ 1 ]-count player = player_number ).
        DELETE deck INDEX 1.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE cx_player.
    ENDTRY.
  ENDMETHOD.

  METHOD if_player~put_back_not_used_card.
    TRY.
        IF i_cards[ 1 ]-player = player_number.
          INSERT VALUE if_combat=>s_game( count = i_cards[ 1 ]-count ) INTO deck INDEX 1.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS player_2 DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_player.

    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.

  PRIVATE SECTION.
    DATA player_number TYPE i VALUE 2.
    DATA deck TYPE if_player=>t_player_deck.

ENDCLASS.

CLASS player_2 IMPLEMENTATION.

  METHOD constructor.
    FIND if_player=>player_names-player2 IN TABLE i_input_values RESULTS DATA(result).
    DATA(start_line) = result-line + 1.
    DATA(end_line) = lines( i_input_values ).

    deck = VALUE #( FOR value IN i_input_values FROM start_line TO end_line
                        ( count = value ) ).

  ENDMETHOD.

  METHOD if_player~get_players_deck.
    r_deck = deck.
  ENDMETHOD.

  METHOD if_player~add_winning_cards.
    TRY.
        IF i_cards[ 1 ]-player = player_number.
          deck = VALUE #( BASE deck FOR line IN i_cards
                               ( count = line-count ) ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD if_player~play_card.
    TRY.
        r_played = VALUE #( count = deck[ 1 ]-count player = player_number ).
        DELETE deck INDEX 1.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE cx_player.
    ENDTRY.
  ENDMETHOD.

  METHOD if_player~put_back_not_used_card.
    TRY.
        IF i_cards[ 1 ]-player = player_number.
          INSERT VALUE if_combat=>s_game( count = i_cards[ 1 ]-count ) INTO deck INDEX 1.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


CLASS game DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.



    METHODS start_game
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA player_1 TYPE REF TO if_player.
    DATA player_2 TYPE REF TO if_player.
    DATA game_deck TYPE if_combat=>t_game.

    METHODS play_round
      RAISING
        cx_player.
    METHODS add_cards_to_players.

ENDCLASS.

CLASS game IMPLEMENTATION.

  METHOD constructor.
    player_1 = NEW player_1( i_input_values ).
    player_2 = NEW player_2( i_input_values ).
  ENDMETHOD.

  METHOD play_round.
    game_deck = VALUE #( ( player_1->play_card(  ) )
                         ( player_2->play_card(  ) ) ).
    add_cards_to_players( ).
  ENDMETHOD.

  METHOD add_cards_to_players.
    SORT game_deck DESCENDING BY count.
    player_1->add_winning_cards( game_deck ).
    player_2->add_winning_cards( game_deck ).
  ENDMETHOD.

  METHOD start_game.
    TRY.
        DO.
          play_round( ).
        ENDDO.
      CATCH cx_player.
        player_1->put_back_not_used_card( game_deck ).
        player_2->put_back_not_used_card( game_deck ).
        DATA(player_1_deck) = player_1->get_players_deck( ).
        DATA(player_2_deck) = player_2->get_players_deck( ).
        r_result = COND #( WHEN player_1_deck IS NOT INITIAL THEN NEW result( )->calculate_result( player_1_deck )
                           WHEN player_2_deck IS NOT INITIAL THEN NEW result( )->calculate_result( player_2_deck )  ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_result DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO result.

    METHODS setup.

    METHODS calculate_result       FOR TESTING.

ENDCLASS.

CLASS ltc_result IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD calculate_result.
    DATA(player_values) = VALUE if_player=>t_player_deck( ( count = 3  )
                                                          ( count = 2  )
                                                          ( count = 10 )
                                                          ( count = 6  )
                                                          ( count = 8  )
                                                          ( count = 5  )
                                                          ( count = 9  )
                                                          ( count = 4  )
                                                          ( count = 7  )
                                                          ( count = 1  ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 306
        act = cut->calculate_result( player_values )
        msg = |The result should be 306.| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_play DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut_player1  TYPE REF TO player_1.
    DATA cut_player2  TYPE REF TO player_2.
    DATA input_values TYPE stringtab.

    METHODS setup.
    METHODS build_deck_for_player1       FOR TESTING.
    METHODS build_deck_for_player2       FOR TESTING.
    METHODS add_winning_cards_to_player1 FOR TESTING.
ENDCLASS.


CLASS ltc_play IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |Player 1:| )
                            ( |9| )
                            ( |2| )
                            ( |6| )
                            ( |3| )
                            ( |1| )
                            ( |Player 2:| )
                            ( |5| )
                            ( |8| )
                            ( |4| )
                            ( |7| )
                            ( |10| ) ).
    cut_player1 = NEW #( input_values ).
    cut_player2 = NEW #( input_values ).
  ENDMETHOD.

  METHOD build_deck_for_player1.
    DATA(expected_values) = VALUE if_player=>t_player_deck( ( count = 9 )
                                                            ( count = 2 )
                                                            ( count = 6 )
                                                            ( count = 3 )
                                                            ( count = 1 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut_player1->if_player~get_players_deck(  )
        msg = |The expected deck should be extracted.| ).
  ENDMETHOD.

  METHOD build_deck_for_player2.
    DATA(expected_values) = VALUE if_player=>t_player_deck( ( count = 5  )
                                                            ( count = 8  )
                                                            ( count = 4  )
                                                            ( count = 7  )
                                                            ( count = 10 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut_player2->if_player~get_players_deck(  )
        msg = |The expected deck should be extracted.| ).
  ENDMETHOD.

  METHOD add_winning_cards_to_player1.
    DATA(expected_values) = VALUE if_player=>t_player_deck( ( count = 9 )
                                                            ( count = 2 )
                                                            ( count = 6 )
                                                            ( count = 3 )
                                                            ( count = 1 )
                                                            ( count = 5 )
                                                            ( count = 3 ) ).
    cut_player1->if_player~add_winning_cards( VALUE #( ( count = 5 player = 1 )
                                                       ( count = 3 player = 2 ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut_player1->if_player~get_players_deck(  )
        msg = |The expected deck should be extracted.| ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_game DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO game.
    DATA input_values TYPE stringtab.

    METHODS setup.
    METHODS get_result_after_game    FOR TESTING.

ENDCLASS.


CLASS ltc_game IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |Player 1:| )
                            ( |9| )
                            ( |2| )
                            ( |6| )
                            ( |3| )
                            ( |1| )
                            ( |Player 2:| )
                            ( |5| )
                            ( |8| )
                            ( |4| )
                            ( |7| )
                            ( |10| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD get_result_after_game.
    cl_abap_unit_assert=>assert_equals(
        exp = 306
        act = cut->start_game( )
        msg = |The result of the game should be 306| ).
  ENDMETHOD.

ENDCLASS.


DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(game) = NEW game( input_values ).
  WRITE / |The result for round 1 is: { game->start_game( ) }|.
