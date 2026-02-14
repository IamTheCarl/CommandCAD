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
  member_access: 16,
  method_call: 15,
  function_call: 14,
  field: 13,
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
  xor: 1,
  assign: 0,
  closure: -1,
  struct_def: -2,
};

const unary_operator_table = ['-', '+', '!'];
const constraint_set_unary_operator_table = ['-', '+'];

function make_unary_expression(table, expression) {
  return choice(...table.map((operator) => seq(
    seq(field('op', operator), expression),
  )));
}

const binary_operator_table = [
  [PREC.exponential, '**'],

  [PREC.multiplicative, '*'],
  [PREC.multiplicative, '/'],

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
  [PREC.xor, '^^'],
];

const constraint_set_binary_operator_table = [
  [PREC.multiplicative, '*'],
  [PREC.multiplicative, '/'],

  [PREC.additive, '+'],
  [PREC.additive, '-'],
];

function make_binary_expression(table, expression) {
  return choice(...table.map(([precedence, operator]) => prec.left(precedence, seq(
    field('a', expression),
    field('op', operator),
    field('b', expression),
  ))));
}

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
    self: _ => 'self',

    base_ten: _ => /[0-9]+/,
    octal: _ => seq(token.immediate(/0o/), /[0-9]+/),
    hex: _ => seq(token.immediate(/0x/), /[0-9a-fA-F]+/),
    binary: _ => seq(token.immediate(/0b/), /[01]+/),
    integer: $ => choice($.base_ten, $.octal, $.hex, $.binary),
    signed_integer: $ => seq(field('value', $.integer), token.immediate(/[iI]/)),
    unsigned_integer: $ => seq(field('value', $.integer), token.immediate(/[uU]/)),

    number: _ => /[0-9]+/,
    unit_quote: _ => /'(\\'|[^'])*'/,

    _float: $ => prec.right(choice(
      seq(field('whole', $.number), '.', field('fractional', $.number)),
      seq(field('whole', $.number)),
    )),

    _unit: $ => choice($.identifier, $.unit_quote),

    scalar: $ => prec.left(PREC.unit, seq($._float, field('unit', optional($._unit)))),
    vector2: $ => seq('<(', field('x', $.expression), ',', field('y', $.expression), ')>'),
    vector3: $ => seq('<(', field('x', $.expression), ',', field('y', $.expression), ',', field('z', $.expression), ')>'),
    vector4: $ => seq('<(', field('x', $.expression), ',', field('y', $.expression), ',', field('z', $.expression), ',', field('w', $.expression), ')>'),

    true: $ => 'true',
    false: $ => 'false',
    boolean: $ => choice($.true, $.false),

    function_call: $ => seq(
      prec.left(PREC.function_call, seq(
        field('to_call', $.expression), field("argument", $.dictionary_construction),
      ))
    ), 
    
    method_call: $ => seq(
      prec.left(PREC.method_call, seq(
        field('self_dictionary', $.expression), '::', field('to_call', $.identifier), field("argument", $.dictionary_construction) 
      ))
    ), 

    expression: $ => choice(
      $.parenthesis,
      $.signed_integer,
      $.unsigned_integer,
      $.scalar,
      $.vector2,
      $.vector3,
      $.vector4,
      $.boolean,
      $.string,
      $.self,
      $.identifier,
      $.member_access,
      $.list,
      $.if,
      $.struct_definition,
      $.dictionary_construction,
      $.closure_definition,
      $.unary_expression,
      $.binary_expression,
      $.function_call,
      $.method_call,
      $.constraint_set,
      $.let_in
    ),
    unary_expression: $=> make_unary_expression(unary_operator_table, $.expression),
    binary_expression: $ => make_binary_expression(binary_operator_table, $.expression),

    if: $ => seq('if', field('condition', $.expression), 'then ', field('on_true', $.expression), 'else', field('on_false', $.expression)),

    let_in: $ => seq('let', field('assignment', repeat($.let_in_assignment)), 'in', field('expression', $.expression)),
    let_in_assignment: $ => seq(field('ident', $.identifier), '=', field('value', $.expression), ';'),

    member_access: $ => prec.left(PREC.member_access, seq(
      field('base', $.expression), seq('.', field('member', $.identifier)) 
    )),

    declaration_type: $ => seq(':', $.expression),

    parenthesis: $ => seq('(', $.expression, ')'),
    list: $ => seq(
      '[',
      repeat(seq($.expression, ',')),
      optional($.expression),
      ']'
    ),

    varadic_dots: $ => '...',

    struct_member: $ => prec.left(PREC.struct_member, seq(field('name', $.identifier), $.declaration_type, optional(seq('=', field('default', $.expression))))),
    _struct_final_element: $ => choice(
      seq($.struct_member),
      seq($.varadic_dots, optional(','))
    ),
    struct_definition: $ => prec.left(PREC.struct_def, seq('(',
      seq(
        optional(field('members', repeat1(seq($.struct_member, ',')))),
        field('final_element', optional($._struct_final_element)),
      ),
      ')')),

    dictionary_member_assignment: $ => seq(field('name', $.identifier), '=', field('assignment', $.expression)),
    dictionary_construction: $ => seq('(',
      field('assignments',
        optional(seq(
          $.dictionary_member_assignment,
          repeat(seq(',', $.dictionary_member_assignment)),
          optional(',')
        )),
      ),
      ')'
    ),

    closure_definition: $ => prec.left(PREC.closure, seq(
      field('argument', $.struct_definition),
      '->',
      field('result', $.expression),
      ':',
      field('expression', $.expression),
    )),
    
    _constraint_set_relation: $ => choice('>', '>=', '==', '<=', '<', '!='),
    constraint_set_fields: $ => seq(repeat(seq($.identifier, ',')), $.identifier),
    constraint_set: $ => seq(
      '<<<',
      field('variables', $.constraint_set_fields), ':',
      field('lhs', $.constraint_set_expression),
      field('relation', $._constraint_set_relation),
      field('rhs', $.constraint_set_expression),
      '>>>'),
    constraint_set_expression: $ => choice(
      $.constraint_set_parenthesis,
      $.scalar,
      $.identifier,
      $.constraint_set_unary_expression,
      $.constraint_set_binary_expression,
      $.constraint_set_method_call,
    ),
    constraint_set_parenthesis: $ => seq('(', $.constraint_set_expression, ')'),
    constraint_set_unary_expression: $ => make_unary_expression(constraint_set_unary_operator_table, $.constraint_set_expression),
    constraint_set_binary_expression: $ => make_binary_expression(constraint_set_binary_operator_table, $.constraint_set_expression),
    constraint_set_method_call: $ => seq(
      prec.left(PREC.method_call, seq(
        field('self_dictionary', $.constraint_set_expression), '::', field('to_call', $.identifier), '(', field("argument", $.constraint_set_expression), ')' 
      ))
    ), 
  }
});
