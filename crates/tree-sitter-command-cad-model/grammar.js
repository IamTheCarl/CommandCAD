/**
 * @file Scripts for creating models in Command CAD
 * @author James Carl
 * @license AGPL
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  struct_member: 18,
  unit: 17,
  method_call: 16,
  function_call: 15,
  field: 14,
  unary: 12,
  exponential: 11,
  multiplicative: 10,
  additive: 9,
  shift: 8,
  bitand: 7,
  bitxor: 6,
  bitor: 5,
  comparative: 4,
  and: 3,
  or: 2,
  range: 1,
  assign: 0,
  closure: -1,
};

module.exports = grammar({
  name: "command_cad_model",
  extras: $ => [$.comment, $._whitespace],
  word: $ => $.identifier,
  rules: {
    source_file: $ => $.expression,

    comment: $ => token(seq('#', /.*/)),
    _whitespace: _ => /\s/,
    identifier: _ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    string: _ => /\"(\\\"|[^\"])*\"/,
    default: _ => 'default',
    void: _ => seq('(', ')'),

    base_ten: _ => /[0-9]+/,
    octal: _ => seq(token.immediate(/0o/), /[0-9]+/),
    hex: _ => seq(token.immediate(/0x/), /[0-9a-fA-F]+/),
    binary: _ => seq(token.immediate(/0b/), /[01]+/),
    integer: $ => choice($.base_ten, $.octal, $.hex, $.binary),
    signed_integer: $ => seq(field('value', $.integer), token.immediate(/[iI]/)),
    unsigned_integer: $ => seq(field('value', $.integer), token.immediate(/[uU]/)),

    number: _ => /[0-9]+/,
    unit_quote: _ => /'(\\'|[^'])*'/,

    _float: $ => choice(
      seq(field('whole', $.number), '.', field('fractional', $.number)),
      seq(field('whole', $.number)),
    ),

    _unit: $ => choice($.identifier, $.unit_quote),

    scalar: $ => prec.left(PREC.unit, seq($._float, field('unit', optional($._unit)))),

    true: $ => 'true',
    false: $ => 'false',
    boolean: $ => choice($.true, $.false),

    function_call: $ => seq(
      prec.left(PREC.function_call, seq(
        field('to_call', $.expression), field("argument", choice($.dictionary_construction, $.void)),
      ))
    ), 
    
    method_call: $ => seq(
      prec.left(PREC.method_call, seq(
        field('self_dictionary', $.expression), ':', field('to_call', $.identifier), field("argument", choice($.dictionary_construction, $.void)) 
      ))
    ), 

    expression: $ => choice(
      $.void,
      $.parenthesis,
      $.default,
      $.signed_integer,
      $.unsigned_integer,
      $.scalar,
      $.boolean,
      $.string,
      $.path,
      $.list,
      $.if,
      $.struct_definition,
      $.dictionary_construction,
      $.procedural_block,
      $.closure_definition,
      $.unary_expression,
      $.binary_expression,
      $.function_call,
      $.method_call,
    ),
    unary_expression: $ => prec(PREC.unary, choice(
      seq(field('op', '-'), $.expression),
      seq(field('op', '+'), $.expression),
      seq(field('op', '!'), $.expression),
    )),
    binary_expression: $ => {
      const table = [
        [PREC.exponential, '**'],

        [PREC.multiplicative, '*'],
        [PREC.multiplicative, '/'],
        [PREC.multiplicative, '//'],

        [PREC.additive, '+'],
        [PREC.additive, '-'],

        [PREC.shift, '<<'],
        [PREC.shift, '>>'],

        [PREC.bitand, '&'],
        [PREC.bitor, '|'],
        [PREC.bitxor, '^'],

        [PREC.comparative, '>'],
        [PREC.comparative, '>='],
        [PREC.comparative, '=='],
        [PREC.comparative, '<='],
        [PREC.comparative, '<'],
        [PREC.comparative, '!='],

        [PREC.and, '&&'],
        [PREC.or, '||'],

        [PREC.range, '..'],
        [PREC.range, '..='],
        
      ];

      return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
        field('a', $.expression),
        field('op', operator),
        field('b', $.expression),
      ))));
    },

    if: $ => seq('if', field('condition', $.expression), field('on_true', $.procedural_block), optional(seq('else', field('on_false', $.procedural_block)))),

    _variable_type: $ => field('type_path', $.path),
    path: $ => seq($.identifier, repeat(seq('.', $.identifier))),

    _declaration_type: $ => seq(':', $._variable_type),

    parenthesis: $ => seq('(', $.expression, ')'),
    list: $ => seq(
      '[',
      repeat(seq($.expression, ',')),
      optional(seq($.expression)),
      ']'
    ),

    varadic_dots: $ => '...',

    struct_member: $ => prec.left(PREC.struct_member, seq(field('name', $.identifier), $._declaration_type, optional(seq('=', field('default', $.expression))))),
    _struct_final_element: $ => choice(
      seq($.struct_member),
      seq($.varadic_dots, optional(','))
    ),
    struct_definition: $ => seq('(',
      choice(
        seq(
          field('members', repeat1(seq($.struct_member, ','))),
          field('final_element', optional($._struct_final_element)),
        ),
        field('final_element', $._struct_final_element)
      ),
      ')'),

    dictionary_member_assignment: $ => seq(field('name', $.identifier), '=', field('assignment', $.expression)),
    dictionary_construction: $ => seq('(',
      field('assignments',
        seq(
          repeat(seq($.dictionary_member_assignment, ',')),
          $.dictionary_member_assignment,
          optional(',')
        ),
      ),
      ')'
    ),

    procedural_block: $ => seq(
      '{',
      repeat($.statement),
      '}'
    ),

    closure_captured_variables: $ => seq(
      '[',
      seq(repeat(seq($.identifier, ',')), optional($.identifier)),
      ']'
    ),
    closure_definition: $ => prec.left(PREC.closure, seq(
      field('argument', choice($.struct_definition, $.path, $.void)),
      field('captured_variables', $.closure_captured_variables),
      '->',
      field('result', choice($.struct_definition, $.path, $.void)),
      field('expression', $.expression),
    )),

    closed_expression: $ => seq($.expression, ';'),

    statement: $ => choice(
      $.assign,
      $.let,
      $.for,
      $.expression,
      $.closed_expression,
    ),

    assignment_operator: $ => {
      let operators = [
        '=',
        '&=',
        '|=',
        '^=',
        '&&=',
        '||=',
        '^^=',
        '+=',
        '-=',
        '**=',
        '*=',
        '//=',
        '/=',
        '<<=',
        '>>='
      ];

      return choice(...operators.map((operator) => field('op', operator)));
    },

    let: $ => seq('let', field('to_assign', $.identifier), '=', field('value', $.expression), ';'),
    assign: $ => seq(field('to_assign', $.path), $.assignment_operator, field('value', $.expression), ';'),
    for: $ => seq('for', field('to_assign', $.identifier), 'in', field('to_iterate', $.expression), field('to_run', $.procedural_block)),
  }
});
