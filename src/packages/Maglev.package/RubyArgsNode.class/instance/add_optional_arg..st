*maglev-ast
add_optional_arg: node
  node class == RubyBlockNode ifFalse:[
    RubyParserM signalError: 'add_optional_arg bad arg kind'
  ].
  optArgs ifNotNil:[
    RubyParserM signalError: 'optional_arg already assigned'
  ].
  optArgs := node.
  ^ self

