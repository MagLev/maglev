*maglev-ast
add_star_arg: symArg
  | sym |
  (sym := symArg) _isSymbol ifFalse:[
    sym := sym symval "args is an RpNameToken"
  ].
  restArgNode ifNil:[
    restArgNode := (RubyArgumentNode _basicNew identifier: sym) .
  ] ifNotNil:[
    RubyParserM signalError: 'RubyArgsNode - star arg already installed' .
  ].
  ^ self

