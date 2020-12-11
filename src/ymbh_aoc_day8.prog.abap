REPORT ymbh_aoc_day8.

INTERFACE if_table_processor.
  TYPES: BEGIN OF s_procedure_table,
           instruction  TYPE char3,
           operator     TYPE char1,
           op_value     TYPE i,
           visited      TYPE abap_bool,
           value        TYPE i,
           repair_state TYPE char10,
         END OF s_procedure_table.
  TYPES t_procedure_table TYPE STANDARD TABLE OF s_procedure_table WITH DEFAULT KEY.
ENDINTERFACE.

CLASS cx_processor DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS table_processor DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS convert_raw_data
      IMPORTING
        i_input_values  TYPE stringtab
      RETURNING
        VALUE(r_result) TYPE if_table_processor=>t_procedure_table.

    METHODS process_instructions
      IMPORTING
        i_converted_values TYPE if_table_processor=>t_procedure_table
      RETURNING
        VALUE(r_result)    TYPE i
      RAISING
        cx_processor.

    METHODS process_instructions_repair
      IMPORTING
        i_converted_values TYPE if_table_processor=>t_procedure_table.

    METHODS get_current_value RETURNING VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA current_value TYPE i.
    DATA current_line  TYPE i VALUE 1.
    DATA repair_state  TYPE char10 VALUE 'idle'.

    METHODS fill_fields_from_input
      IMPORTING
        i_line          TYPE string
      RETURNING
        VALUE(r_fields) TYPE if_table_processor=>s_procedure_table.

    METHODS determine_next_line
      IMPORTING
        i_line TYPE if_table_processor=>s_procedure_table.

    METHODS update_accumulator_value
      IMPORTING
        i_line TYPE if_table_processor=>s_procedure_table.

    METHODS repair_table
      IMPORTING
        i_working_values TYPE if_table_processor=>t_procedure_table
      RETURNING
        VALUE(r_result)  TYPE if_table_processor=>t_procedure_table.

    METHODS switch_repair_state
      IMPORTING
        i_repair_state TYPE char10
      RETURNING
        VALUE(r_state) TYPE char10.

    METHODS reset_processor.

ENDCLASS.

CLASS table_processor IMPLEMENTATION.

  METHOD convert_raw_data.
    r_result = VALUE #( FOR line IN i_input_values
                        ( fill_fields_from_input( line ) ) ).
  ENDMETHOD.

  METHOD process_instructions.
    DATA(working_values) = i_converted_values.
    ASSIGN working_values[ current_line ] TO FIELD-SYMBOL(<line>).

    IF sy-subrc = 0.
      IF <line>-visited = abap_true.
        RAISE EXCEPTION TYPE cx_processor.
      ENDIF.

      update_accumulator_value( <line> ).
      determine_next_line( <line> ).
      <line> = VALUE #( BASE <line> visited = abap_true
                                    value   = current_value ).

      process_instructions( working_values ).
    ENDIF.

  ENDMETHOD.

  METHOD process_instructions_repair.
    DATA(working_values) = i_converted_values.
    TRY.
        process_instructions( working_values ).
      CATCH cx_processor.
        reset_processor( ).
        working_values = repair_table( working_values ).
        process_instructions_repair( working_values ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_current_value.
    r_result = current_value.
  ENDMETHOD.

  METHOD fill_fields_from_input.
    SPLIT i_line AT space INTO r_fields-instruction DATA(operator_and_value).
    r_fields-operator = operator_and_value(1).
    r_fields-op_value = operator_and_value+1.
  ENDMETHOD.

  METHOD update_accumulator_value.
    current_value = SWITCH #( i_line-instruction WHEN 'acc' THEN SWITCH #( i_line-operator WHEN '-' THEN current_value - i_line-op_value
                                                                                           ELSE current_value + i_line-op_value )
                                                 ELSE current_value ).
  ENDMETHOD.

  METHOD determine_next_line.
    current_line = SWITCH #( i_line-instruction WHEN 'jmp' THEN SWITCH #( i_line-operator WHEN '-' THEN current_line - i_line-op_value
                                                                                          ELSE current_line + i_line-op_value )
                                                ELSE current_line + 1 ).
  ENDMETHOD.

  METHOD repair_table.
    DATA(repairing_copy) = i_working_values.
    LOOP AT repairing_copy ASSIGNING FIELD-SYMBOL(<line>) WHERE ( instruction = 'jmp' OR instruction = 'nop') AND ( repair_state <> 'processed' ).
      <line> = VALUE #( BASE <line> instruction = COND #( WHEN <line>-instruction = 'jmp' THEN 'nop'
                                                       WHEN <line>-instruction = 'nop' THEN 'jmp'
                                                       ELSE <line>-instruction )
                                    repair_state = COND #( WHEN <line>-instruction = 'jmp' THEN switch_repair_state( <line>-repair_state )
                                                           WHEN <line>-instruction = 'nop' THEN switch_repair_state( <line>-repair_state )
                                                           ELSE repair_state ) ).
      EXIT.
    ENDLOOP.
    r_result = repairing_copy.
  ENDMETHOD.


  METHOD switch_repair_state.
    repair_state = SWITCH #( repair_state WHEN 'idle'      THEN 'line_try'
                                          WHEN 'line_try'  THEN 'processed'
                                          WHEN 'processed' THEN 'line_try' ).
    r_state = repair_state.
  ENDMETHOD.

  METHOD reset_processor.
    current_line = 1.
    current_value = 0.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_table_processor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO table_processor.
    DATA input_values TYPE stringtab.

    METHODS setup.
    METHODS fill_table_properly           FOR TESTING.
    METHODS value_is_5_after_input_values FOR TESTING.
    METHODS value_after_termination       FOR TESTING.
ENDCLASS.


CLASS ltc_table_processor IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_values = VALUE #( ( |nop +0|  )
                            ( |acc +1|  )
                            ( |jmp +4|  )
                            ( |acc +3|  )
                            ( |jmp -3|  )
                            ( |acc -99| )
                            ( |acc +1|  )
                            ( |jmp -4|  )
                            ( |acc +6|  ) ).
  ENDMETHOD.

  METHOD fill_table_properly.
    DATA(expected_values) = VALUE if_table_processor=>t_procedure_table( ( instruction = |nop| operator = |+| op_value = 0  )
                                                                         ( instruction = |acc| operator = |+| op_value = 1  )
                                                                         ( instruction = |jmp| operator = |+| op_value = 4  )
                                                                         ( instruction = |acc| operator = |+| op_value = 3  )
                                                                         ( instruction = |jmp| operator = |-| op_value = 3  )
                                                                         ( instruction = |acc| operator = |-| op_value = 99 )
                                                                         ( instruction = |acc| operator = |+| op_value = 1  )
                                                                         ( instruction = |jmp| operator = |-| op_value = 4  )
                                                                         ( instruction = |acc| operator = |+| op_value = 6  ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = cut->convert_raw_data( input_values )
        msg = |The resulting table should contain the expecte values.| ).

  ENDMETHOD.

  METHOD value_is_5_after_input_values.
    DATA(converted_values) = cut->convert_raw_data( input_values ).
    TRY.
        cut->process_instructions( converted_values ).
      CATCH cx_processor.
        cl_abap_unit_assert=>assert_equals(
           exp = 5
           act = cut->get_current_value( )
           msg = |The resulting table should contain the expecte values.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD value_after_termination.
    DATA(converted_values) = cut->convert_raw_data( input_values ).
    TRY.
        cut->process_instructions_repair( converted_values ).
        cl_abap_unit_assert=>assert_equals(
          exp = 8
          act = cut->get_current_value( )
          msg = |The resulting table should contain the expecte values.| ).
      CATCH cx_processor.
        cl_abap_unit_assert=>fail( msg = |'An exception should not occur.| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

DATA input  TYPE text1024.
SELECT-OPTIONS: so_input FOR input NO INTERVALS.

START-OF-SELECTION.
  DATA(input_values) = VALUE stringtab( FOR <line> IN so_input ( CONV #( <line>-low ) ) ).
  DATA(table_processor) = NEW table_processor( ).

  DATA(converted_values) = table_processor->convert_raw_data( input_values ).

  TRY.
      table_processor->process_instructions( converted_values ).
    CATCH cx_processor.
      WRITE / |The value at unexpected termination for part 1 is: { table_processor->get_current_value( ) }|.
  ENDTRY.

  TRY.
      table_processor->process_instructions_repair( converted_values ).
      WRITE / |The value at successful termination for part 2 is: { table_processor->get_current_value( ) }|.
    CATCH cx_processor.
  ENDTRY.
