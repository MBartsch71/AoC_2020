REPORT ymbh_aoc_day16.

INTERFACE if_fields.
  TYPES: BEGIN OF s_range,
           field_name TYPE text100,
           range      TYPE RANGE OF i,
         END OF s_range.
  TYPES t_range TYPE STANDARD TABLE OF s_range WITH DEFAULT KEY.

  METHODS number_in_range
    IMPORTING
      i_value           TYPE i
    RETURNING
      VALUE(r_in_range) TYPE abap_bool.

  METHODS get_range
    RETURNING
      VALUE(r_ranges) TYPE if_fields=>t_range.

ENDINTERFACE.

INTERFACE if_ticket.
  TYPES t_numbers TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

  METHODS build_numbers
    IMPORTING
      i_input_values TYPE string.

  METHODS get_numbers
    RETURNING
      VALUE(r_numbers) TYPE t_numbers.

  METHODS get_class
    RETURNING
      VALUE(r_class) TYPE text20.

  METHODS valid
    RETURNING
      VALUE(r_valid) TYPE abap_bool.

  METHODS add_invalid_number
    IMPORTING
      i_number TYPE i.

  METHODS get_invalid_sum
    RETURNING
      VALUE(r_sum) TYPE i.

ENDINTERFACE.

INTERFACE if_ticket_collection.
  TYPES: BEGIN OF s_ticket,
           ticket TYPE REF TO if_ticket,
         END OF s_ticket.
  TYPES t_tickets TYPE STANDARD TABLE OF s_ticket WITH DEFAULT KEY.

  METHODS get_tickets
    RETURNING
      VALUE(r_result) TYPE if_ticket_collection=>t_tickets.
ENDINTERFACE.

CLASS fields DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_fields.

    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.


  PRIVATE SECTION.
    DATA ranges TYPE if_fields=>t_range.

ENDCLASS.

CLASS fields IMPLEMENTATION.

  METHOD constructor.
    LOOP AT i_input_values ASSIGNING FIELD-SYMBOL(<val>).
      IF <val> CS 'your ticket:'.
        EXIT.
      ENDIF.
      CHECK <val> IS NOT INITIAL.
      SPLIT <val> AT ':' INTO DATA(name) DATA(full_range).
      CONDENSE name.
      SPLIT full_range AT 'or' INTO DATA(range_1) DATA(range_2).
      SPLIT range_1 AT '-' INTO DATA(low_1) DATA(high_1).
      SPLIT range_2 AT '-' INTO DATA(low_2) DATA(high_2).
      CONDENSE: low_1, high_1, low_2, high_2.
      ranges = VALUE #( BASE ranges ( field_name = name
                                      range = VALUE #( ( sign = 'I' option = 'BT' low = low_1 high = high_1 )
                                                       ( sign = 'I' option = 'BT' low = low_2 high = high_2  ) ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD if_fields~get_range.
    r_ranges = ranges.
  ENDMETHOD.

  METHOD if_fields~number_in_range.
    LOOP AT ranges INTO DATA(line).
      DATA(range) = line-range.
      IF i_value IN line-range.
        r_in_range = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


CLASS ticket DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_ticket.
    METHODS constructor
      IMPORTING
        i_ticket_class TYPE text15.

  PRIVATE SECTION.
    DATA numbers      TYPE if_ticket=>t_numbers.
    DATA invalid_numbers TYPE if_ticket=>t_numbers.

    DATA ticket_class TYPE text20.
    DATA state        TYPE char10.
    DATA is_valid     TYPE abap_bool.


ENDCLASS.

CLASS ticket IMPLEMENTATION.

  METHOD constructor.
    ticket_class = i_ticket_class.
  ENDMETHOD.

  METHOD if_ticket~get_numbers.
    r_numbers = numbers.
  ENDMETHOD.

  METHOD if_ticket~valid.
    r_valid = is_valid.
  ENDMETHOD.

  METHOD if_ticket~build_numbers.
    SPLIT i_input_values AT ',' INTO TABLE DATA(raw_numbers).
    numbers = CONV #( raw_numbers ).
  ENDMETHOD.

  METHOD if_ticket~get_class.
    r_class = ticket_class.
  ENDMETHOD.

  METHOD if_ticket~add_invalid_number.
    invalid_numbers = VALUE #( BASE invalid_numbers ( i_number ) ).
  ENDMETHOD.

  METHOD if_ticket~get_invalid_sum.
    r_sum = REDUCE #( INIT invalids = 0
                      FOR line IN invalid_numbers
                      NEXT invalids = invalids + line ).
  ENDMETHOD.

ENDCLASS.

CLASS ticket_collection DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_ticket_collection.

    METHODS constructor
      IMPORTING
        i_input_values TYPE stringtab.
    METHODS check_ticket_validity.
    METHODS get_invalid_ticket_sum
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA tickets TYPE if_ticket_collection=>t_tickets.
    DATA state TYPE text15.
    DATA fields TYPE REF TO if_fields.

ENDCLASS.

CLASS ticket_collection IMPLEMENTATION.

  METHOD constructor.
    fields = NEW fields( i_input_values ).
    DATA ticket TYPE REF TO if_ticket.
    LOOP AT i_input_values INTO DATA(value).
      IF value CS 'your ticket'.
        state = 'Personal ticket'.
        CONTINUE.
      ELSEIF value CS 'nearby ticket'.
        state = 'Nearby tickets'.
        CONTINUE.
      ENDIF.

      IF value IS INITIAL AND state IS NOT INITIAL.
        CLEAR state.
        CONTINUE.
      ENDIF.

      IF state IS NOT INITIAL.
        ticket = NEW ticket( state ).
        ticket->build_numbers( value ).
        tickets = VALUE #( BASE tickets ( ticket = ticket ) ).
        FREE ticket.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD if_ticket_collection~get_tickets.
    r_result = me->tickets.
  ENDMETHOD.

  METHOD check_ticket_validity.
    LOOP AT tickets INTO DATA(ticket).
      IF ticket-ticket->get_class( ) = 'Personal ticket'.
        CONTINUE.
      ENDIF.
      DATA(numbers) = ticket-ticket->get_numbers( ).
      LOOP AT numbers INTO DATA(number).

        IF fields->number_in_range( number ) = abap_false.
          ticket-ticket->add_invalid_number( number ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_invalid_ticket_sum.
    r_result = REDUCE #( INIT invalids = 0
                         FOR ticket IN tickets
                         NEXT invalids = invalids + ticket-ticket->get_invalid_sum( ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_fields DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO fields.
    DATA input_values TYPE stringtab.

    METHODS setup.
    METHODS build_fields_ranges       FOR TESTING.
    METHODS get_valid_value_from_range FOR TESTING.
    METHODS get_invalid_value_of_range FOR TESTING.
ENDCLASS.


CLASS ltc_fields IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #(
                    ( |departure location: 45-535 or 550-961|  )
                    ( |departure station: 45-278 or 294-974|  )
                    ( |departure platform: 46-121 or 138-965|  )
                    ( |departure track: 38-149 or 173-949|  )
                    ( |departure date: 34-223 or 248-957|  )
                    ( |departure time: 32-64 or 79-952|  )
                    ( |arrival location: 49-879 or 905-968|  )
                    ( |arrival station: 47-306 or 323-973|  )
                    ( |arrival platform: 46-823 or 834-971|  )
                    ( |arrival track: 30-464 or 486-963|  )
                    ( |class: 40-350 or 372-965|  )
                    ( |duration: 47-414 or 423-950|  )
                    ( |price: 45-507 or 526-956|  )
                    ( |route: 42-779 or 799-970|  )
                    ( |row: 26-865 or 872-955|  )
                    ( |seat: 43-724 or 739-970|  )
                    ( |train: 25-914 or 926-958|  )
                    ( |type: 33-205 or 218-965|  )
                    ( |wagon: 43-101 or 118-951|  )
                    ( |zone: 45-844 or 858-970|  )
                    ( ||  )
                    ( |your ticket:|  )
                    ( |173,191,61,199,101,179,257,79,193,223,139,97,83,197,251,53,89,149,181,59|  )
                    ( ||  )
                    ( |nearby tickets:|  )
                    ( |949,764,551,379,767,144,556,835,638,591,653,872,198,825,690,527,260,396,873,333| )
                    ( |438,627,99,622,408,671,695,561,695,121,706,144,55,985,566,706,255,595,680,407|  )
                    ( |879,876,665,928,874,436,766,328,620,267,995,54,430,503,86,936,489,305,64,688|  )
                    ( |613,812,756,258,341,765,91,551,859,379,447,842,148,501,293,766,93,532,939,406| )
                    ( |349,340,670,248,813,557,249,949,506,656,100,19,204,409,944,659,777,843,712,801 | ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD build_fields_ranges.
    cl_abap_unit_assert=>assert_equals(
        exp = 20
        act = lines( cut->if_fields~get_range( ) )
        msg = |The range table shoudl have 20 lines.| ).
  ENDMETHOD.

  METHOD get_valid_value_from_range.
    cl_abap_unit_assert=>assert_true(
        act = cut->if_fields~number_in_range( 123 ) ).
  ENDMETHOD.

  METHOD get_invalid_value_of_range.
    cl_abap_unit_assert=>assert_false(
        act = cut->if_fields~number_in_range( 975 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_ticket DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO ticket.
    DATA input_values TYPE string.

    METHODS setup.
    METHODS build_ticket_object_w_entries FOR TESTING.
    METHODS check_number_is_valid         FOR TESTING.
    METHODS get_correct_invalid_numbers   FOR TESTING.
ENDCLASS.


CLASS ltc_ticket IMPLEMENTATION.

  METHOD setup.
    input_values = |173,191,61,199,101,179,257,79,193,223,139,97,83,197,251,53,89,149,181,59|.
    cut = NEW #( 'your ticket' ).
    cut->if_ticket~build_numbers( input_values ).
  ENDMETHOD.

  METHOD build_ticket_object_w_entries.
    cl_abap_unit_assert=>assert_equals(
        exp = 20
        act = lines( cut->if_ticket~get_numbers( ) )
        msg = |There should be 20 numbers in the ticket.| ).
  ENDMETHOD.

  METHOD check_number_is_valid.
    cl_abap_unit_assert=>assert_false(
        act = cut->if_ticket~valid( )
        msg = |Object should be bound.| ).
  ENDMETHOD.

  METHOD get_correct_invalid_numbers.
    cut->if_ticket~add_invalid_number( 20 ).
    cut->if_ticket~add_invalid_number( 10 ).
    cut->if_ticket~add_invalid_number( 23 ).

    cl_abap_unit_assert=>assert_equals(
        exp = 53
        act = cut->if_ticket~get_invalid_sum( )
        msg = |The sum of invalid numbers should be 53.| ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_ticket_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut          TYPE REF TO ticket_collection.
    DATA input_values TYPE stringtab.

    METHODS setup.
    METHODS build_tickets               FOR TESTING.
    METHODS sum_upinvalid_tickets_count FOR TESTING.
ENDCLASS.


CLASS ltc_ticket_collection IMPLEMENTATION.

  METHOD setup.
    input_values = VALUE #( ( |class: 1-3 or 5-7| )
                            ( |row: 6-11 or 33-44| )
                            ( |seat: 13-40 or 45-50| )
                            ( || )
                            ( |your ticket:| )
                            ( |7,1,14| )
                            ( || )
                            ( |nearby tickets:| )
                            ( |7,3,47| )
                            ( |40,4,50| )
                            ( |55,2,20| )
                            ( |38,6,12| ) ).
    cut = NEW #( input_values ).
  ENDMETHOD.

  METHOD build_tickets.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = lines( cut->if_ticket_collection~get_tickets( ) )
        msg = 'msg' ).
  ENDMETHOD.

  METHOD sum_upinvalid_tickets_count.
    cut->check_ticket_validity( ).
    cl_abap_unit_assert=>assert_equals(
        exp = 71
        act = cut->get_invalid_ticket_sum( )
        msg = |The invalid ticket count should be 71.| ).
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).

  DATA(ticket_collection) = NEW ticket_collection( input_values ).
  ticket_collection->check_ticket_validity( ).

  WRITE / |The result for part 1 is: { ticket_collection->get_invalid_ticket_sum( ) }|.
