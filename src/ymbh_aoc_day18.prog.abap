REPORT ymbh_aoc_day18.

CLASS formula_scanner DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS compute_expression
      IMPORTING
        i_line          TYPE string
      RETURNING
        VALUE(r_result) TYPE i.

    METHODS compute_chain
      IMPORTING
        i_expression    TYPE string
      RETURNING
        VALUE(r_result) TYPE string.


  PRIVATE SECTION.

ENDCLASS.

CLASS formula_scanner IMPLEMENTATION.

  METHOD compute_expression.
    DATA expression TYPE string.
    data(pattern) = new cl_abap_regex( pattern = '((\(.*\)))' ).
    data(matcher) = pattern->create_matcher( text = i_Line ).

    while matcher->find_next( ).
        data(match) = matcher->get_match( ).
        expression = substring( val = i_Line off = match-offset + 1 len = match-length - 1 ).
        r_result = compute_expression( expression ).
    endwhile.

    expression = i_line.
*    IF results IS NOT INITIAL.
*      expression = replace( val = i_line
*                          off = results-offset
*                          len = results-length
*                          with = CONV string( r_result ) ).
*    ENDIF.
*    r_result = compute_chain( expression ).
  ENDMETHOD.


  METHOD compute_chain.
    SPLIT i_expression AT space INTO TABLE DATA(expression).
    r_result = REDUCE #( INIT sum = expression[ 1 ]
                         FOR i = 2 THEN i + 2 UNTIL i > lines( expression )
                         LET operation = expression[ i ]
                         IN
                         NEXT sum = SWITCH #( operation WHEN '+' THEN sum + expression[ i + 1 ]
                                                        WHEN '-' THEN sum - expression[ i + 1 ]
                                                        WHEN '*' THEN sum * expression[ i + 1 ]
                                                        WHEN '/' THEN sum / expression[ i + 1 ] ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_formula_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO formula_scanner.

    METHODS setup.

    METHODS compute_paranth_expresssion FOR TESTING.
    METHODS compute_chain_expression    FOR TESTING.
    METHODS compute_fully_teststring_26 FOR TESTING.
    METHODS compute_teststring_simple   FOR TESTING.
    METHODS compute_fully_teststring    FOR TESTING.
ENDCLASS.


CLASS ltc_formula_scanner IMPLEMENTATION.

  METHOD setup.
    cut =  NEW #(  ).
  ENDMETHOD.

  METHOD compute_paranth_expresssion.
    DATA(line) = |1 + (2 * 3 + 1)|.
    cl_abap_unit_assert=>assert_equals(
        exp = 8
        act = cut->compute_expression( line )
        msg = |The result should be 8.| ).
  ENDMETHOD.

  METHOD compute_chain_expression.
    cl_abap_unit_assert=>assert_equals(
        exp = 7
        act = cut->compute_chain( |2 * 3 + 1| )
        msg = |The result should be 7.| ).
  ENDMETHOD.

  METHOD compute_fully_teststring_26.
    cl_abap_unit_assert=>assert_equals(
        exp = 34
        act = cut->compute_expression( |2 * 3 + (4 * (2 + 5)|  )
        msg = |The result should be 26.| ).
  ENDMETHOD.

  METHOD compute_teststring_simple.
    cl_abap_unit_assert=>assert_equals(
        exp = 28
        act = cut->compute_expression( |8 * 3 + 4| )
        msg = |The result should be 28.| ).
  ENDMETHOD.

  METHOD compute_fully_teststring.
    cl_abap_unit_assert=>assert_equals(
        exp = 13632
        act = cut->compute_expression( |((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2| )
        msg = |The result should be 13632.| ).
  ENDMETHOD.

ENDCLASS.


DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
