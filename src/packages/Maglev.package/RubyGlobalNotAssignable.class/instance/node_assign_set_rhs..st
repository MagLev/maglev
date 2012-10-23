*maglev-ast
node_assign_set_rhs: rhs
  valueNode ifNil:[
    rhs is_void_result ifTrue:[
       RubyParserM signalError: 'void value expression'
    ].
    valueNode := rhs .
  ] ifNotNil:[
    RubyParserM signalError: ' value already assigned'
  ].
  ^ self       

