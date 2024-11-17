/**
 * @file Scripts for creating models in Command CAD
 * @author James Carl
 * @license AGPL
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  unit: 16,
  call: 15,
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

    signed_integer: _ => /[0-9]+[iI]/,
    unsigned_integer: _ => /[0-9]+[uU]/,

    number: _ => /[0-9]+/,
    unit_quote: _ => /'(\\'|[^'])*'/,

    _float: $ => choice(
      seq($.number, '.', $.number),
      seq($.number),
    ),

    _unit: $ => choice($.identifier, $.unit_quote),

    scalar: $ => prec.left(PREC.unit, seq($._float, optional($._unit))),

    boolean: $ => choice('true', 'false'),

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
    ),
    unary_expression: $ => prec(PREC.unary, choice(
      seq('-', $.expression),
      seq('+', $.expression),
      seq('!', $.expression),
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

    if: $ => seq('if', $.expression, $.procedural_block, optional(seq('else', $.procedural_block))),

    _variable_type: $ => $.path,
    path: $ => choice($.local_path, $.argument_path),
    argument_path: $ => seq('@', repeat(seq('.', $.identifier))),
    local_path: $ => seq($.identifier, repeat(seq('.', $.identifier))),

    _declaration_type: $ => seq(':', $._variable_type),

    parenthesis: $ => seq('(', $.expression, ')'),
    list: $ => seq(
      '[',
      repeat(seq($.expression, ',')),
      optional(seq($.expression)),
      ']'
    ),

    varadic_dots: $ => '...',

    struct_member: $ => seq(field('name', $.identifier), $._declaration_type, field('default', optional(seq('=', $.expression)))),
    _struct_final_element: $ => choice(
      seq($.struct_member),
      $.varadic_dots,
      seq($.varadic_dots, ',')
    ),
    struct_definition: $ => seq('(',
      choice(
        seq(
          repeat1(seq($.struct_member, ',')),
          optional($._struct_final_element),
        ),
        $._struct_final_element
      ),
      ')'),

    dictionary_member_assignment: $ => seq($.identifier, '=', $.expression),
    dictionary_construction: $ => seq('(',
      choice(
        seq(
          repeat1(seq($.dictionary_member_assignment, ',')),
          optional($.dictionary_member_assignment),
        ),
        $.dictionary_member_assignment
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
      choice($.struct_definition, $.path, $.void), $.closure_captured_variables, '->', choice($.struct_definition, $.path, $.void), $.expression,
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

      return choice(...operators);
    },

    let: $ => seq('let', $.identifier, $.assignment_operator, $.expression, ';'),
    assign: $ => seq($.path, $.assignment_operator, $.expression, ';'),
    for: $ => seq('for', $.identifier, 'in', $.expression, $.procedural_block),
  }
});
