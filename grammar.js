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
  octalLiteral = seq(
    '0',
    optional(choice('o', 'O')),
    optional('_'),
    octalDigits
  ),
  decimalLiteral = choice(
    '0',
    seq(/[1-9]/, optional(seq(optional('_'), decimalDigits)))
  ),
  binaryLiteral = seq('0', choice('b', 'B'), optional('_'), binaryDigits),
  intLiteral = choice(binaryLiteral, decimalLiteral, octalLiteral, hexLiteral),
  decimalExponent = seq(
    choice('e', 'E'),
    optional(choice('+', '-')),
    decimalDigits
  ),
  decimalFloatLiteral = choice(
    seq(decimalDigits, '.', optional(decimalDigits), optional(decimalExponent)),
    seq(decimalDigits, decimalExponent),
    seq('.', decimalDigits, optional(decimalExponent))
  ),
  hexExponent = seq(
    choice('p', 'P'),
    optional(choice('+', '-')),
    decimalDigits
  ),
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

  inline: $ => [
    $._type_identifier,
    $._field_identifier,
    $.top_level_declaration,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.simple_type, $.expression_],
    [$.qualified_type, $.expression_],
    [$.func_literal, $.function_type],
    [$.function_type],
    [$.plain_parameter, $.simple_type],
    [$.parameters, $.receiver_argument_optional],
    [$.return_type_list, $.return_value_name_list_optional],
    [$.return_type_list, $.type_optional],
    [$.special_call_identifier, $.expression_],
    [$.block_initializer_optional, $._type_switch_header],
  ],

  supertypes: $ => [],

  rules: {
    program: $ =>
      seq(
        optional_with_placeholder('package_optional', $.package),
        optional_with_placeholder('import_list', $.import_list),
        optional_with_placeholder(
          'statement_list',
          repeat(alias($.top_level_statement, $.statement))
        )
      ),

    top_level_statement: $ =>
      choice(
        seq($.statement, terminator),
        seq($.top_level_declaration, optional(terminator))
      ),

    top_level_declaration: $ => field('function', choice($.function, $.method)),

    package: $ => seq('package', $.identifier),

    import_list: $ => repeat1($.import),

    import: $ =>
      seq('import', choice($.import_specifier, $.import_specifier_list)),

    import_specifier: $ =>
      seq(
        optional(
          field('name', choice($.dot, $.blank_identifier, $.identifier))
        ),
        field('path', $.string)
      ),
    dot: $ => '.',
    blank_identifier: $ => '_',

    import_specifier_list: $ =>
      seq('(', repeat(seq($.import_specifier, terminator)), ')'),

    _declaration: $ =>
      choice(
        $.type_declaration,
        $.declaration_statement,
        $.interface,
        $.struct
      ),

    declaration_statement: $ =>
      choice($.const_declaration, $.variable_declaration),

    const_declaration: $ =>
      seq(
        'const',
        choice(
          $.const_spec,
          seq(
            '(',
            optional_with_placeholder(
              'const_spec_list',
              repeat(seq($.const_spec, terminator))
            ),
            ')'
          )
        )
      ),

    variable_declaration: $ =>
      seq(
        'var',
        choice(
          field('assignment_list', $.assignment),
          $.variable_declaration_list
        )
      ),

    variable_declaration_list: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'assignment_list',
          repeat(seq($.assignment, terminator))
        ),
        ')'
      ),

    assignment_variable: $ => $.identifier,
    assignment: $ =>
      seq(
        field('assignment_variable_list', commaSep1($.assignment_variable)),
        optional_with_placeholder('type_optional', $.type),
        optional_with_placeholder(
          'assignment_value_list_optional',
          seq('=', alias($.expression_list, $.assignment_value))
        )
      ),

    const_spec: $ =>
      prec.left(
        seq(
          commaSep1($.assignment_variable),
          optional(
            seq(
              optional_with_placeholder('type_optional', $.type),
              '=',
              alias($.expression_list, $.assignment_value)
            )
          )
        )
      ),

    function: $ =>
      prec.right(
        1,
        seq(
          'func',
          optional_with_placeholder(
            'receiver_argument_optional',
            '$%$**NEVER#xuyuz#MATCH**$%$'
          ),
          $.identifier,
          $.parameters,
          choice(
            alias($.return_block, $.return_value_name_list_optional),
            optional_with_placeholder(
              'type_optional',
              alias($.simple_type, $.type)
            )
          ),
          $.enclosed_body
        )
      ),

    function_definition: $ =>
      prec.right(
        1,
        seq(
          'func',
          optional_with_placeholder(
            'receiver_argument_optional',
            '$%$**NEVER#xuyuz#MATCH**$%$'
          ),
          $.identifier,
          $.parameters,
          choice(
            alias($.return_block, $.return_value_name_list_optional),
            optional_with_placeholder(
              'type_optional',
              alias($.simple_type, $.type)
            )
          )
        )
      ),

    method: $ =>
      prec.right(
        1,
        seq(
          'func',
          $.receiver_argument_optional,
          field('identifier', $._field_identifier),
          $.parameters,
          choice(
            alias($.return_block, $.return_value_name_list_optional),
            optional_with_placeholder(
              'type_optional',
              alias($.simple_type, $.type)
            )
          ),
          optional($.enclosed_body)
        )
      ),

    parameters: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'parameter_list',
          seq(commaSep($.parameter), optional(','))
        ),
        ')'
      ),

    return_value_name_list_optional: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'return_value_name_list',
          seq(commaSep(alias($.parameter, $.return_value_name)), optional(','))
        ),
        ')'
      ),

    receiver_argument_optional: $ =>
      seq('(', alias($.parameter, $.receiver_argument), ')'),

    return_block: $ =>
      choice(
        $.return_value_name_list_optional,
        $.return_type_list // TODO: label this correctly
      ),

    return_type_list: $ =>
      seq(
        '(',
        optional_with_placeholder('return_value_name_list', commaSep1($.type)), // TODO: remove incorrect label when ast is updated
        optional(','),
        ')'
      ),

    parameter: $ =>
      choice($.plain_parameter, $.variadic_parameter, $.type_optional),

    plain_parameter: $ =>
      seq($.identifier, optional_with_placeholder('type_optional', $.type)),

    variadic_parameter: $ =>
      seq(optional($.identifier), '...', $.type_optional),

    type_optional: $ => $.type,

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
          seq(
            '(',
            repeat(seq(choice($.type_spec, $.type_alias), terminator)),
            ')'
          )
        )
      ),

    type_spec: $ =>
      seq(
        field('identifier', $._type_identifier),
        field('type', choice($._simple_type_excluding, $.parenthesized_type))
      ),

    field_name_list: $ => commaSep1($._field_identifier),

    expression_list: $ => commaSep1($.expression_),

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

    array_type: $ =>
      seq('[', field('length', $.expression_), ']', field('element', $._type)),

    implicit_length_array_type: $ =>
      seq('[', '...', ']', field('element', $._type)),

    slice_type: $ => seq('[', ']', field('element', $._type)),

    struct: $ =>
      seq('type', alias($._type_identifier, $.identifier), $.struct_type),

    struct_type: $ => seq('struct', alias($.struct_body, $.enclosed_body)),

    struct_body: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'struct_member_list',
          seq(
            $.struct_member,
            repeat(seq(terminator, $.struct_member)),
            optional(terminator)
          )
        ),
        '}'
      ),

    struct_member: $ => field('member', $.property),

    property_case_star: $ =>
      seq(
        optional_with_placeholder('identifier', '*'),
        field('type_optional', alias($.case_star_type, $.type))
      ),

    case_star_type: $ =>
      field('type', choice($._type_identifier, $.qualified_type)),

    property: $ =>
      seq(
        choice(
          seq(
            field('identifier', commaSep1($._field_identifier)),
            $.type_optional
          ),
          $.property_case_star
        ),
        optional_with_placeholder(
          'property_tag_optional',
          alias($.string, $.property_tag)
        ) // tag
      ),

    interface: $ =>
      seq('type', alias($._type_identifier, $.identifier), $.interface_type),

    interface_type: $ =>
      seq('interface', alias($.interface_body, $.enclosed_body)),

    interface_body: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'interface_member_list',
          seq(
            $.interface_member,
            repeat(seq(terminator, $.interface_member)),
            optional(terminator)
          )
        ),
        '}'
      ),

    interface_member: $ =>
      field(
        'member',
        choice(
          $._type_identifier,
          $.qualified_type,
          alias($.method_signature, $.method)
        )
      ),

    method_signature: $ =>
      seq(
        alias($._field_identifier, $.identifier),
        $.parameters,
        choice(
          alias($.return_block, $.return_value_name_list_optional),
          optional_with_placeholder(
            'type_optional',
            alias($.simple_type, $.type)
          )
        )
      ),

    map_type: $ => seq('map', '[', $.type, ']', $.type),

    channel_type: $ =>
      choice(
        seq('chan', field('value', $._type)),
        seq('chan', '<-', field('value', $._type)),
        prec(PREC.unary, seq('<-', 'chan', field('value', $._type)))
      ),

    function_type: $ =>
      seq(
        'func',
        $.parameters,
        choice(
          alias($.return_block, $.return_value_name_list_optional),
          optional_with_placeholder(
            'type_optional',
            alias($.simple_type, $.type)
          )
        )
      ),

    enclosed_body: $ =>
      seq(
        '{',
        optional_with_placeholder('statement_list', $.statement_list),
        '}'
      ),

    statement_list: $ =>
      choice(
        seq(
          $.statement,
          repeat(seq(terminator, $.statement)),
          optional(
            seq(
              terminator,
              optional(alias($.empty_labeled_statement, $.labeled_statement))
            )
          )
        ),
        alias($.empty_labeled_statement, $.labeled_statement)
      ),

    statement: $ =>
      choice(
        $._declaration,
        $._simple_statement,
        $.return,
        $.go_statement,
        $.defer,
        $.if,
        $.for,
        $.switch,
        $.type_switch,
        $.select_statement,
        $.labeled_statement,
        $.fallthrough_statement,
        $.break,
        $.continue,
        $.goto_statement,
        $.enclosed_body
        // $.empty_statement
      ),

    // empty_statement: ($) => ";",

    _simple_statement: $ =>
      choice(
        $.expression_,
        $.send_statement,
        $.inc_statement,
        $.dec_statement,
        alias($.assignment_statement_as_declaration, $.variable_declaration)
      ),

    assignment_statement_as_declaration: $ =>
      field('assignment_list', alias($.assignment_statement, $.assignment)),

    assignment_statement: $ =>
      choice($.plain_assignment_statement, $.short_var_declaration),

    send_statement: $ =>
      seq(field('channel', $.expression_), '<-', field('value', $.expression_)),

    receive_statement: $ =>
      seq(
        optional(seq(field('left', $.expression_list), choice('=', ':='))),
        field('right', $.expression_)
      ),

    inc_statement: $ => seq($.expression_, '++'),

    dec_statement: $ => seq($.expression_, '--'),

    plain_assignment_statement: $ =>
      seq(
        alias($.expression_list, $.assignment_variable),
        field('operator', choice(...assignment_operators)),
        alias($.expression_list, $.assignment_value)
      ),

    short_var_declaration: $ =>
      seq(
        // TODO: this should really only allow identifier lists, but that causes
        // conflicts between identifiers as expressions vs identifiers here.
        alias($.expression_list, $.assignment_variable),
        ':=',
        alias($.expression_list, $.assignment_value)
      ),

    labeled_statement: $ =>
      seq(field('label', alias($.identifier, $.label_name)), ':', $.statement),

    empty_labeled_statement: $ =>
      seq(field('label', alias($.identifier, $.label_name)), ':'),

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
        optional_with_placeholder(
          'return_value_optional',
          alias($.expression_list, $.return_value)
        )
      ),

    go_statement: $ => seq('go', $.expression_),

    defer: $ => seq('defer', $.expression_),

    if: $ =>
      seq(
        $.if_clause,
        optional_with_placeholder(
          'else_if_clause_list',
          repeat1($.else_if_clause)
        ),
        optional_with_placeholder('else_clause_optional', $.else_clause)
      ),

    if_clause: $ =>
      seq(
        'if',
        optional_with_placeholder(
          'block_initializer_optional',
          $.block_initializer_optional
        ),
        field('condition', $.expression_),
        $.enclosed_body
      ),

    block_initializer_optional: $ =>
      seq(alias($._simple_statement, $.block_initializer), ';'),

    else_if_clause: $ => seq('else', alias($.if_clause, $.if_clause_)),

    else_clause: $ => seq('else', $.enclosed_body),

    for: $ => choice($.for_clause, $.for_each_clause),

    for_clause: $ =>
      seq(
        'for',
        choice(
          // seq(
          //   optional_with_placeholder('block_initializer_optional', blank()),
          //   optional_with_placeholder('condition_optional', blank()),
          //   optional_with_placeholder('block_update_optional', blank())
          // ),
          seq(
            optional_with_placeholder('block_initializer_optional', blank()),
            optional_with_placeholder(
              'condition_optional',
              $.condition_optional
            ),
            optional_with_placeholder('block_update_optional', blank())
          ),
          $.for_header
        ),
        $.enclosed_body
      ),

    for_each_clause: $ => seq('for', $.range_condition, $.enclosed_body),

    for_header: $ =>
      seq(
        optional_with_placeholder(
          'block_initializer_optional',
          alias($._simple_statement, $.block_initializer)
        ),
        ';',
        optional_with_placeholder(
          'condition_optional',
          alias($.expression_, $.condition)
        ),
        ';',
        optional_with_placeholder(
          'block_update_optional',
          alias($._simple_statement, $.block_update)
        )
      ),

    condition_optional: $ => alias($.expression_, $.condition),

    range_condition: $ =>
      seq(
        optional_with_placeholder('block_iterator', seq($.expression_list)),
        field('for_each_separator', seq(choice('=', ':='), 'range')),
        alias($.expression_, $.block_collection)
      ),

    switch: $ =>
      seq(
        'switch',
        optional_with_placeholder(
          'block_initializer_optional',
          $.block_initializer_optional
        ),
        optional_with_placeholder(
          'condition_optional',
          alias($.expression_, $.condition)
        ), // value
        '{',
        optional_with_placeholder('switch_case_list', $.switch_case_list),
        '}'
      ),

    switch_case_list: $ => repeat1($.expression_case_or_default),

    expression_case_or_default: $ =>
      choice(alias($.expression_case, $.case), $.default_case),

    expression_case: $ =>
      seq(
        'case',
        alias($.expression_list, $.condition),
        ':',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    default_case: $ =>
      seq(
        'default',
        ':',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    type_switch: $ =>
      seq(
        'switch',
        $._type_switch_header,
        '{',
        repeat($.type_case_or_default),
        '}'
      ),

    type_case_or_default: $ => choice($.type_case, $.default_case),

    _type_switch_header: $ =>
      seq(
        optional(seq($._simple_statement, ';')),
        optional(seq($.expression_list, ':=')),
        field('value', $.expression_),
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

    select_statement: $ =>
      seq('select', '{', repeat($.communication_case_or_default), '}'),

    communication_case_or_default: $ =>
      choice(
        alias($.communication_case, $.case),
        alias($.default_case, $.case)
      ),

    communication_case: $ =>
      seq(
        'case',
        field('communication', choice($.send_statement, $.receive_statement)),
        ':',
        optional_with_placeholder('statement_list', $.statement_list)
      ),

    expression_: $ =>
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
        choice('new', 'make'),
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

    parenthesized_expression: $ => seq('(', $.expression_, ')'),

    call: $ => prec(PREC.primary, choice($.special_call, $.standard_call)),

    standard_call: $ => seq($.expression_, $.arguments),

    special_call: $ =>
      seq(
        alias($.special_call_identifier, $.identifier),
        alias($.special_arguments, $.arguments)
      ),

    special_call_identifier: $ => choice('new', 'make'),

    variadic_argument: $ => prec.right(seq($.expression_, '...')),

    special_arguments: $ =>
      seq(
        '(',
        field(
          'argument_list',
          seq($.type, repeat(seq(',', $.expression_)), optional(','))
        ),
        ')'
      ),

    argument: $ => choice($.expression_, $.variadic_argument),

    arguments: $ =>
      seq(
        '(',
        optional_with_placeholder(
          'argument_list',
          seq(
            commaSep(
              $.argument
              // choice(alias($.expression_, $.argument), alias($.variadic_argument, $.argument))
            ),
            optional(',')
          )
        ),
        ')'
      ),

    selector_expression: $ =>
      prec(
        PREC.primary,
        seq(
          field('operand', $.expression_),
          '.',
          field('field', $._field_identifier)
        )
      ),

    index_expression: $ =>
      prec(
        PREC.primary,
        seq(
          field('operand', $.expression_),
          '[',
          field('index', $.expression_),
          ']'
        )
      ),

    slice_expression: $ =>
      prec(
        PREC.primary,
        seq(
          field('operand', $.expression_),
          '[',
          choice(
            seq(
              optional($.expression_), // start
              ':',
              optional($.expression_) // end
            ),
            seq(
              optional($.expression_), // start
              ':',
              field('end', $.expression_),
              ':',
              field('capacity', $.expression_)
            )
          ),
          ']'
        )
      ),

    type_assertion_expression: $ =>
      prec(
        PREC.primary,
        seq(
          field('operand', $.expression_),
          '.',
          '(',
          field('type', $._type),
          ')'
        )
      ),

    type_conversion_expression: $ =>
      prec.dynamic(
        -1,
        seq(
          field('type', $._type),
          '(',
          field('operand', $.expression_),
          optional(','),
          ')'
        )
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
      prec(
        PREC.composite_literal,
        seq(field('type', $.map_type), field('enclosed_body', $.map_value))
      ),

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

    list_element_with_seperator: $ =>
      seq(',', choice($.element, $.key_value_pair)),

    key_value_pair_with_seperator: $ => seq(',', $.key_value_pair),

    map_value: $ =>
      seq(
        '{',
        optional_with_placeholder(
          'key_value_pair_list',
          seq(
            $.key_value_pair,
            repeat($.key_value_pair_with_seperator),
            optional(',')
          )
        ),
        '}'
      ),

    key_value_pair: $ =>
      seq(
        choice(
          seq(alias($.expression_, $.key_value_pair_key), ':'),
          seq(alias($.literal_value, $.key_value_pair_key), ':'),
          prec(1, seq(alias($._field_identifier, $.key_value_pair_key), ':'))
        ),
        $.key_value_pair_value
      ),

    key_value_pair_value: $ => choice($.expression_, $.literal_value),

    element: $ => choice($.expression_, $.literal_value),

    func_literal: $ =>
      seq(
        'func',
        $.parameters,
        choice(
          alias($.return_block, $.return_value_name_list_optional),
          optional_with_placeholder(
            'type_optional',
            alias($.simple_type, $.type)
          )
        ),
        $.enclosed_body
      ),

    unary_expression: $ =>
      prec(
        PREC.unary,
        seq(
          field('operator', choice('+', '-', '!', '^', '*', '&', '<-')),
          field('operand', $.expression_)
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
              field('left', $.expression_),
              field('operator', operator),
              field('right', $.expression_)
            )
          )
        )
      )
    },

    qualified_type: $ =>
      seq(
        field('package', $.identifier),
        '.',
        field('name', $._type_identifier)
      ),

    identifier: $ => token(seq(letter, repeat(choice(letter, unicodeDigit)))),

    _type_identifier: $ => alias($.identifier, $.type_identifier),
    _field_identifier: $ => alias($.identifier, $.field_identifier),

    string: $ => choice($._raw_string_literal, $._interpreted_string_literal),

    _raw_string_literal: $ =>
      token(
        seq('`', optional_with_placeholder('string_text', repeat1(/[^`]/)), '`')
      ),

    // TODO: use externals like with js/python grammars. Will enable string_text field correctly
    _interpreted_string_literal: $ =>
      token(
        seq(
          '"',
          field('string_text', repeat(choice(/[^\\"\n]/, /\\(.|\n)/))),
          '"'
        )
      ),

    escape_sequence: $ =>
      token.immediate(
        seq(
          '\\',
          choice(
            /[^xuU]/,
            /\d{2,3}/,
            /x[0-9a-fA-F]{2,}/,
            /u[0-9a-fA-F]{4}/,
            /U[0-9a-fA-F]{8}/
          )
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
    comment: $ =>
      token(
        choice(seq('//', /.*/), seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'))
      ),
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
