*maglev-ast
new_dsym:  val
  | v_cls |
  v_cls := val class .
  v_cls == RubyDStrNode ifTrue:[ ^ val asDSymbolNode ]   .
  v_cls == RubyStrNode ifTrue:[ | str |
    str := val strNodeValue .
    ^ RubySymbolNode _basicNew  name: ( self string_to_symbol: str )
  ].
  v_cls == RubyEvStrNode ifTrue:[
    ^ RubyDSymbolNode _basicNew list: { RubyStrNode _basicNew _value: '' . val }
  ].
  self signalError: 'internal_error, unimplemented dsym conversion'.
  ^ nil

