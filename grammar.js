const PREC = {
    primary: 7,
    unary: 6,
    multiplicative: 5,
    additive: 4,
    comparative: 3,
    and: 2,
    or: 1,
    composite_literal: -1,
  },
  multiplicative_operators = ['*', '/', '%', '<<', '>>', '&', '&^'],
  additive_operators = ['+', '-', '|', '^'],
  comparative_operators = ['==', '!=', '<', '<=', '>', '>='],
  assignment_operators = multiplicative_operators
    .concat(additive_operators)
    .map(operator => operator + '=')
    .concat('='),
  unicodeLetter = /\p{L}/,
  unicodeDigit = /[0-9]/,
  unicodeChar = /./,
  unicodeValue = unicodeChar,
  letter = choice(unicodeLetter, '_'),
  newline = '\n',
  terminator = choice(newline, ';'),
  hexDigit = /[0-9a-fA-F]/,
  octalDigit = /[0-7]/,
  decimalDigit = /[0-9]/,
  binaryDigit = /[01]/,
  hexDigits = seq(hexDigit, repeat(seq(optional('_'), hexDigit))),
  octalDigits = seq(octalDigit, repeat(seq(optional('_'), octalDigit))),
  decimalDigits = seq(decimalDigit, repeat(seq(optional('_'), decimalDigit))),
  binaryDigits = seq(binaryDigit, repeat(seq(optional('_'), binaryDigit))),
  hexLiteral = seq('0', choice('x', 'X'), optional('_'), hexDigits),
  octalLiteral = seq('0', optional(choice('o', 'O')), optional('_'), octalDigits),
  decimalLiteral = choice('0', seq(/[1-9]/, optional(seq(optional('_'), decimalDigits)))),
  binaryLiteral = seq('0', choice('b', 'B'), optional('_'), binaryDigits),
  intLiteral = choice(binaryLiteral, decimalLiteral, octalLiteral, hexLiteral),
  decimalExponent = seq(choice('e', 'E'), optional(choice('+', '-')), decimalDigits),
  decimalFloatLiteral = choice(
    seq(decimalDigits, '.', optional(decimalDigits), optional(decimalExponent)),
    seq(decimalDigits, decimalExponent),
    seq('.', decimalDigits, optional(decimalExponent))
  ),
  hexExponent = seq(choice('p', 'P'), optional(choice('+', '-')), decimalDigits),
  hexMantissa = choice(
    seq(optional('_'), hexDigits, '.', optional(hexDigits)),
    seq(optional('_'), hexDigits),
    seq('.', hexDigits)
  ),
  hexFloatLiteral = seq('0', choice('x', 'X'), hexMantissa, hexExponent),
  floatLiteral = choice(decimalFloatLiteral, hexFloatLiteral),
  imaginaryLiteral = seq(choice(decimalDigits, intLiteral, floatLiteral), 'i')

module.exports = grammar({
  name: 'go',

  extras: $ => [$.comment, /\s/],

  inline: $ => [$._type_identifier, $._field_identifier, $.top_level_declaration],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.simple_type, $._expression],
    [$.qualified_type, $._expression],
    [$.func_literal, $.function_type],
    [$.func_literal, $.definition_parameters],
    [$.function_type],
    [$.parameter, $.simple_type],
    [$.parameter, $.type_optional],
    [$.declaration_parameters, $.type_parameters],
    [$.declaration_parameters, $.receiver_argument_optional],
    [$.var_spec_with_assignment_list],
    [$.declaration_parameters],
    [$.return_value_name_list_optional],
    [$.type_parameter_list],
    [$.return_type_list, $.return_value_name_list_optional],
  ],

  supertypes: $ => [$._expression, $._simple_statement, $._statement],

  rules: {
    source_file: $ =>
      seq(
        optional_with_placeholder('package_optional', $.package),
        optional_with_placeholder('import_list', $._import_list),
        optional_with_placeholder('top_level_statement_list', $.top_level_statement_list)
      ),

    top_level_statement_list: $ =>
      repeat1(
        choice(seq($._statement, terminator), seq($.top_level_declaration, optional(terminator)))
      ),

    top_level_declaration: $ => choice($.function, alias($.method, $.function)),

    package: $ => seq('package', $.identifier),

    _import_list: $ => repeat1($.import),

    import: $ => seq('import', choice($.import_specifier, $.import_specifier_list)),

    import_specifier: $ =>
      seq(
        optional(field('name', choice($.dot, $.blank_identifier, $.identifier))),
        field('path', $.string)
      ),
    dot: $ => '.',
    blank_identifier: $ => '_',

    import_specifier_list: $ => seq('(', repeat(seq($.import_specifier, terminator)), ')'),

    _declaration: $ =>
      choice(
        alias($.const_declaration, $.declaration_statement),
        alias($.const_declaration_with_assignment, $.assignment_statement),
        $.type_declaration,
        alias($.var_declaration, $.declaration_statement),
        alias($.var_declaration_with_assignment, $.assignment_statement),
        $.interface,
        $.struct
      ),

    const_declaration: $ =>
      seq('const', choice($.var_spec, seq('(', repeat(seq($.var_spec, terminator)), ')'))),

    const_declaration_with_assignment: $ =>
      seq(
        'const',
        choice($.var_spec_with_assignment, seq('(', $.var_spec_with_assignment_list, ')'))
      ),

    var_declaration: $ =>
      seq('var', choice($.var_spec, seq('(', repeat(seq($.var_spec, terminator)), ')'))),

    var_declaration_with_assignment: $ =>
      seq(
        'var',
        choice($.var_spec_with_assignment, seq('(', $.var_spec_with_assignment_list, ')'))
      ),

    var_spec: $ =>
      seq(field('assignment_variable', commaSep1($.identifier)), field('type', $._type)),

    var_spec_with_assignment: $ =>
      seq(
        field('assignment_variable', commaSep1($.identifier)),
        optional_with_placeholder('type_optional', $.type),
        seq('=', field('assignment_value', $.expression_list))
      ),

    var_spec_with_assignment_list: $ =>
      seq(
        repeat(choice($.var_spec, $.var_spec_with_assignment)),
        $.var_spec_with_assignment,
        repeat(choice($.var_spec, $.var_spec_with_assignment))
      ),

    function: $ =>
      prec.right(
        1,
        seq(
          'func',
          optional_with_placeholder('receiver_argument_optional', blank()),
          $.identifier,
          $.declaration_parameters,
          choice(
            $.return_block,
            optional_with_placeholder('type_optional', alias($.simple_type, $.type))
          ),
          optional($.block)
        )
      ),

    method: $ =>
      prec.right(
        1,
        seq(
          'func',
          $.receiver_argument_optional,
          field('identifier', $._field_identifier),
          $.declaration_parameters,
          choice(
            $.return_block,
            optional_with_placeholder('type_optional', alias($.simple_type, $.type))
          ),
          optional($.block)
        )
      ),

    declaration_parameters: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'parameter_list',
          seq(
            optional(
              seq(
                commaSep1(
                  choice($.parameter, alias($.variadic_parameter_declaration, $.parameter))
                ),
                ','
              )
            ),
            choice(
              alias($.typed_parameter, $.parameter),
              alias($.variadic_parameter_declaration, $.parameter)
            ),
            optional(',')
          )
        ),
        ')'
      ),

    return_value_name_list_optional: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'return_value_name_list',
          seq(
            optional(
              seq(
                commaSep1(
                  choice(
                    alias($.parameter, $.return_value_name),
                    alias($.variadic_parameter_declaration, $.return_value_name)
                  )
                ),
                ','
              )
            ),
            choice(
              alias($.typed_parameter, $.return_value_name),
              alias($.variadic_parameter_declaration, $.return_value_name)
            ),
            optional(',')
          )
        ),
        ')'
      ),

    receiver_argument_optional: $ =>
      seq(
        '(',
        choice(
          alias($.parameter, $.receiver_argument),
          alias($.variadic_parameter_declaration, $.receiver_argument)
        ),
        ')'
      ),

    definition_parameters: $ => choice($.declaration_parameters, $.type_parameters),

    return_block: $ =>
      choice(
        $.return_value_name_list_optional,
        alias($.return_type_list, $.return_value_name_list_optional) // TODO: label this correctly
      ),

    type_parameters: $ =>
      seq('(', alias($.type_parameter_list, $.parameter_list), optional(','), ')'),

    type_parameter_list: $ => commaSep1(alias($.type_parameter, $.parameter)),

    type_parameter: $ => $.type_optional,

    return_type_list: $ =>
      seq(
        '(',
        optional_with_placeholder('return_value_name_list', commaSep1($.type)), // TODO: remove incorrect label when ast is updated
        optional(','),
        ')'
      ),

    parameter: $ => seq($.identifier, optional_with_placeholder('type_optional', $.type)),

    typed_parameter: $ => seq($.identifier, $.type_optional),

    type_optional: $ => $.type,

    variadic_parameter_declaration: $ => seq(optional($.identifier), '...', $.type),

    type_alias: $ =>
      seq(
        field('name', $._type_identifier),
        '=',
        field('type', choice($._simple_type_excluding, $.parenthesized_type))
      ),

    type_declaration: $ =>
      seq(
        'type',
        choice(
          $.type_spec,
          $.type_alias,
          seq('(', repeat(seq(choice($.type_spec, $.type_alias), terminator)), ')')
        )
      ),

    type_spec: $ =>
      seq(
        field('identifier', $._type_identifier),
        field('type', choice($._simple_type_excluding, $.parenthesized_type))
      ),

    field_name_list: $ => commaSep1($._field_identifier),

    expression_list: $ => commaSep1($._expression),

    type: $ => choice($.simple_type, $.parenthesized_type),

    _type: $ => choice($.simple_type, $.parenthesized_type),

    parenthesized_type: $ => seq('(', $._type, ')'),

    simple_type: $ =>
      choice(
        prec.dynamic(-1, $._type_identifier),
        $.qualified_type,
        $.pointer_type,
        $.struct_type,
        $.interface_type,
        $.array_type,
        $.slice_type,
        $.map_type,
        $.channel_type,
        $.function_type
      ),

    _simple_type_excluding: $ =>
      choice(
        prec.dynamic(-1, $._type_identifier),
        $.qualified_type,
        $.pointer_type,
        $.array_type,
        $.slice_type,
        $.map_type,
        $.channel_type,
        $.function_type
      ),

    pointer_type: $ => prec(PREC.unary, seq('*', $._type)),

    array_type: $ => seq('[', field('length', $._expression), ']', field('element', $._type)),

    implicit_length_array_type: $ => seq('[', '...', ']', field('element', $._type)),

    slice_type: $ => seq('[', ']', field('element', $._type)),

    struct: $ => seq('type', field('identifier', $._type_identifier), $.struct_type),

    struct_type: $ => seq('struct', alias($.struct_body, $.block)),

    struct_body: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'struct_member_list',
          seq($.property, repeat(seq(terminator, $.property)), optional(terminator))
        ),
        '}'
      ),

    property: $ =>
      seq(
        choice(
          seq(field('identifier', commaSep1($._field_identifier)), $.type),
          seq(
            optional_with_placeholder('identifier', '*'),
            field('type', choice($._type_identifier, $.qualified_type))
          )
        ),
        field('tag', optional($.string))
      ),

    interface: $ => seq('type', field('identifier', $._type_identifier), $.interface_type),

    interface_type: $ => seq('interface', alias($.interface_body, $.block)),

    interface_body: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'interface_member_list',
          seq(
            choice($._type_identifier, $.qualified_type, alias($.method_signature, $.method)),
            repeat(
              seq(
                terminator,
                choice($._type_identifier, $.qualified_type, alias($.method_signature, $.method))
              )
            ),
            optional(terminator)
          )
        ),
        '}'
      ),

    method_signature: $ =>
      seq(
        field('identifier', $._field_identifier),
        $.definition_parameters,
        choice(
          $.return_block,
          optional_with_placeholder('type_optional', alias($.simple_type, $.type))
        )
      ),

    map_type: $ => seq('map', '[', field('key_type', $._type), ']', field('value_type', $._type)),

    channel_type: $ =>
      choice(
        seq('chan', field('value', $._type)),
        seq('chan', '<-', field('value', $._type)),
        prec(PREC.unary, seq('<-', 'chan', field('value', $._type)))
      ),

    function_type: $ =>
      seq(
        'func',
        $.definition_parameters,
        choice(
          $.return_block,
          optional_with_placeholder('type_optional', alias($.simple_type, $.type))
        )
      ),

    // this goes away after spec update
    statement_or_block: $ => $.block,

    block: $ => seq('{', optional_with_placeholder('statement_list', $.statement_list), '}'),

    statement_list: $ =>
      choice(
        seq(
          $._statement,
          repeat(seq(terminator, $._statement)),
          optional(seq(terminator, optional(alias($.empty_labeled_statement, $.labeled_statement))))
        ),
        alias($.empty_labeled_statement, $.labeled_statement)
      ),

    _statement: $ =>
      choice(
        $._declaration,
        $._simple_statement,
        $.return,
        $.go_statement,
        $.defer,
        $.if,
        $.for_clause,
        $.enhanced_for_clause,
        alias($.expression_switch, $.switch),
        alias($.type_switch, $.switch),
        $.select_statement,
        $.labeled_statement,
        $.fallthrough_statement,
        $.break,
        $.continue,
        $.goto_statement,
        $.block
        // $.empty_statement
      ),

    // empty_statement: ($) => ";",

    _simple_statement: $ =>
      choice(
        $._expression,
        $.send_statement,
        $.inc_statement,
        $.dec_statement,
        $.assignment_statement,
        alias($.short_var_declaration, $.assignment_statement)
      ),

    send_statement: $ => seq(field('channel', $._expression), '<-', field('value', $._expression)),

    receive_statement: $ =>
      seq(
        optional(seq(field('left', $.expression_list), choice('=', ':='))),
        field('right', $._expression)
      ),

    inc_statement: $ => seq($._expression, '++'),

    dec_statement: $ => seq($._expression, '--'),

    assignment_statement: $ =>
      seq(
        field('assignment_variable', $.expression_list),
        field('operator', choice(...assignment_operators)),
        field('assignment_value', $.expression_list)
      ),

    short_var_declaration: $ =>
      seq(
        // TODO: this should really only allow identifier lists, but that causes
        // conflicts between identifiers as expressions vs identifiers here.
        field('assignment_variable', $.expression_list),
        ':=',
        field('assignment_value', $.expression_list)
      ),

    labeled_statement: $ =>
      seq(field('label', alias($.identifier, $.label_name)), ':', $._statement),

    empty_labeled_statement: $ => seq(field('label', alias($.identifier, $.label_name)), ':'),

    // This is a hack to prevent `fallthrough_statement` from being parsed as
    // a single token. For consistency with `break_statement` etc it should
    // be parsed as a parent node that *contains* a `fallthrough` token.
    fallthrough_statement: $ => prec.left('fallthrough'),

    break: $ => seq('break', optional(alias($.identifier, $.label_name))),

    continue: $ => seq('continue', optional(alias($.identifier, $.label_name))),

    goto_statement: $ => seq('goto', alias($.identifier, $.label_name)),

    return: $ =>
      seq(
        'return',
        optional_with_placeholder('return_value_optional', alias($.expression_list, $.return_value))
      ),

    go_statement: $ => seq('go', $._expression),

    defer: $ => seq('defer', $._expression),

    if: $ =>
      seq(
        $.if_clause,
        optional_with_placeholder('else_if_clause_list', repeat1($.else_if_clause)),
        optional_with_placeholder('else_clause_optional', $.else_clause)
      ),

    if_clause: $ =>
      seq(
        'if',
        optional_with_placeholder(
          'block_initializer_optional',
          seq(alias($._simple_statement, $.block_initializer), ';')
        ),
        field('condition', $._expression),
        $.statement_or_block
      ),

    else_if_clause: $ => seq('else', alias($.if_clause, $.if_clause_)),

    else_clause: $ => seq('else', $.statement_or_block),

    for_clause: $ =>
      seq(
        'for',
        choice(
          seq(
            optional_with_placeholder('block_initializer_optional', blank()),
            optional_with_placeholder('condition_optional', blank()),
            optional_with_placeholder('block_update_optional', blank())
          ),
          seq(
            optional_with_placeholder('block_initializer_optional', blank()),
            $.condition_optional,
            optional_with_placeholder('block_update_optional', blank())
          ),
          $.for_header
        ),
        $.statement_or_block
      ),

    enhanced_for_clause: $ => seq('for', $.range_condition, $.statement_or_block),

    for_header: $ =>
      seq(
        optional_with_placeholder(
          'block_initializer_optional',
          alias($._simple_statement, $.block_initializer)
        ),
        ';',
        optional_with_placeholder('condition_optional', alias($._expression, $.condition)),
        ';',
        optional_with_placeholder(
          'block_update_optional',
          alias($._simple_statement, $.block_update)
        )
      ),

    condition_optional: $ => alias($._expression, $.condition),

    range_condition: $ =>
      seq(
        choice(
          seq(field('block_iterator', $.expression_list), choice('=', ':=')),
          optional_with_placeholder('block_iterator', blank())
        ),
        'range',
        alias($._expression, $.block_collection)
      ),

    expression_switch: $ =>
      seq(
        'switch',
        optional(seq($._simple_statement, ';')),
        field('value', optional($._expression)),
        '{',
        repeat($.expression_case_or_default),
        '}'
      ),

    expression_case_or_default: $ =>
      choice(alias($.expression_case, $.case), alias($.default_case, $.case)),

    expression_case: $ =>
      seq(
        'case',
        field('value', $.expression_list),
        ':',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    default_case: $ =>
      seq('default', ':', optional_with_placeholder('statement_list', $.statement_list)),

    type_switch: $ =>
      seq('switch', $._type_switch_header, '{', repeat($.type_case_or_default), '}'),

    type_case_or_default: $ => choice(alias($.type_case, $.case), alias($.default_case, $.case)),

    _type_switch_header: $ =>
      seq(
        optional(seq($._simple_statement, ';')),
        optional(seq($.expression_list, ':=')),
        field('value', $._expression),
        '.',
        '(',
        'type',
        ')'
      ),

    type_case: $ =>
      seq(
        'case',
        field('type', commaSep1($._type)),
        ':',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    select_statement: $ => seq('select', '{', repeat($.communication_case_or_default), '}'),

    communication_case_or_default: $ =>
      choice(alias($.communication_case, $.case), alias($.default_case, $.case)),

    communication_case: $ =>
      seq(
        'case',
        field('communication', choice($.send_statement, $.receive_statement)),
        ':',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    _expression: $ =>
      choice(
        $.unary_expression,
        $.binary_expression,
        $.selector_expression,
        $.index_expression,
        $.slice_expression,
        $.call,
        $.type_assertion_expression,
        $.type_conversion_expression,
        $.identifier,
        alias(choice('new', 'make'), $.identifier),
        $.composite_literal,
        $.map_literal,
        alias($.func_literal, $.lambda),
        $.string,
        $.int_literal,
        $.float_literal,
        $.imaginary_literal,
        $.rune_literal,
        $.nil,
        $.true,
        $.false,
        $.parenthesized_expression
      ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    call: $ =>
      prec(
        PREC.primary,
        choice(
          seq(alias(choice('new', 'make'), $.identifier), alias($.special_arguments, $.arguments)),
          seq($._expression, $.arguments)
        )
      ),

    variadic_argument: $ => prec.right(seq($._expression, '...')),

    special_arguments: $ =>
      seq(
        '(',
        field('argument_list', seq($.type, repeat(seq(',', $._expression)), optional(','))),
        ')'
      ),

    arguments: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'argument_list',
          seq(
            commaSep(
              choice(alias($._expression, $.argument), alias($.variadic_argument, $.argument))
            ),
            optional(',')
          )
        ),
        ')'
      ),

    selector_expression: $ =>
      prec(
        PREC.primary,
        seq(field('operand', $._expression), '.', field('field', $._field_identifier))
      ),

    index_expression: $ =>
      prec(
        PREC.primary,
        seq(field('operand', $._expression), '[', field('index', $._expression), ']')
      ),

    slice_expression: $ =>
      prec(
        PREC.primary,
        seq(
          field('operand', $._expression),
          '[',
          choice(
            seq(
              field('start', optional($._expression)),
              ':',
              field('end', optional($._expression))
            ),
            seq(
              field('start', optional($._expression)),
              ':',
              field('end', $._expression),
              ':',
              field('capacity', $._expression)
            )
          ),
          ']'
        )
      ),

    type_assertion_expression: $ =>
      prec(
        PREC.primary,
        seq(field('operand', $._expression), '.', '(', field('type', $._type), ')')
      ),

    type_conversion_expression: $ =>
      prec.dynamic(
        -1,
        seq(field('type', $._type), '(', field('operand', $._expression), optional(','), ')')
      ),

    composite_literal: $ =>
      prec(
        PREC.composite_literal,
        seq(
          field(
            'type',
            choice(
              $.slice_type,
              $.array_type,
              $.implicit_length_array_type,
              $.struct_type,
              $._type_identifier,
              $.qualified_type
            )
          ),
          field('body', $.literal_value)
        )
      ),

    map_literal: $ =>
      prec(PREC.composite_literal, seq(field('type', $.map_type), field('body', $.map_value))),

    literal_value: $ =>
      seq(
        '{',
        optional(
          seq(
            choice($.element, $.key_value_pair),
            repeat($.list_element_with_seperator),
            optional(',')
          )
        ),
        '}'
      ),

    list_element_with_seperator: $ => seq(',', choice($.element, $.key_value_pair)),

    key_value_pair_with_seperator: $ => seq(',', $.key_value_pair),

    map_value: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'key_value_pair_list',
          seq($.key_value_pair, repeat($.key_value_pair_with_seperator), optional(','))
        ),
        '}'
      ),

    key_value_pair: $ =>
      seq(
        choice(
          seq(alias($._expression, $.key_value_pair_key), ':'),
          seq(alias($.literal_value, $.key_value_pair_key), ':'),
          prec(1, seq(alias($._field_identifier, $.key_value_pair_key), ':'))
        ),
        field('key_value_pair_value', choice($._expression, $.literal_value))
      ),

    element: $ => choice($._expression, $.literal_value),

    func_literal: $ =>
      seq(
        'func',
        $.declaration_parameters,
        choice(
          $.return_block,
          optional_with_placeholder('type_optional', alias($.simple_type, $.type))
        ),
        field('body', $.block)
      ),

    unary_expression: $ =>
      prec(
        PREC.unary,
        seq(
          field('operator', choice('+', '-', '!', '^', '*', '&', '<-')),
          field('operand', $._expression)
        )
      ),

    binary_expression: $ => {
      const table = [
        [PREC.multiplicative, choice(...multiplicative_operators)],
        [PREC.additive, choice(...additive_operators)],
        [PREC.comparative, choice(...comparative_operators)],
        [PREC.and, '&&'],
        [PREC.or, '||'],
      ]

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field('left', $._expression),
              field('operator', operator),
              field('right', $._expression)
            )
          )
        )
      )
    },

    qualified_type: $ =>
      seq(field('package', $.identifier), '.', field('name', $._type_identifier)),

    identifier: $ => token(seq(letter, repeat(choice(letter, unicodeDigit)))),

    _type_identifier: $ => alias($.identifier, $.type_identifier),
    _field_identifier: $ => alias($.identifier, $.field_identifier),

    string: $ => choice($._raw_string_literal, $._interpreted_string_literal),

    _raw_string_literal: $ =>
      token(seq('`', optional_with_placeholder('string_text', repeat1(/[^`]/)), '`')),

    // TODO: use externals like with js/python grammars. Will enable string_text field correctly
    _interpreted_string_literal: $ =>
      token(seq('"', field('string_text', repeat(choice(/[^\\"\n]/, /\\(.|\n)/))), '"')),

    escape_sequence: $ =>
      token.immediate(
        seq(
          '\\',
          choice(/[^xuU]/, /\d{2,3}/, /x[0-9a-fA-F]{2,}/, /u[0-9a-fA-F]{4}/, /U[0-9a-fA-F]{8}/)
        )
      ),

    int_literal: $ => token(intLiteral),

    float_literal: $ => token(floatLiteral),

    imaginary_literal: $ => token(imaginaryLiteral),

    rune_literal: $ =>
      token(
        seq(
          "'",
          choice(
            /[^'\\]/,
            seq(
              '\\',
              choice(
                seq('x', hexDigit, hexDigit),
                seq(octalDigit, octalDigit, octalDigit),
                seq('u', hexDigit, hexDigit, hexDigit, hexDigit),
                seq(
                  'U',
                  hexDigit,
                  hexDigit,
                  hexDigit,
                  hexDigit,
                  hexDigit,
                  hexDigit,
                  hexDigit,
                  hexDigit
                ),
                seq(choice('a', 'b', 'f', 'n', 'r', 't', 'v', '\\', "'", '"'))
              )
            )
          ),
          "'"
        )
      ),

    nil: $ => 'nil',
    true: $ => 'true',
    false: $ => 'false',

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    comment: $ => token(choice(seq('//', /.*/), seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'))),
  },
})

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)))
}

function commaSep(rule) {
  return optional(commaSep1(rule))
}

function optional_with_placeholder(field_name, rule) {
  return choice(field(field_name, rule), field(field_name, blank()))
}
