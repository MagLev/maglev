*maglev-ast
append_arg: v
  "returns receiver"
  | vcls |
  v ifNil:[ RubyParserM signalError:'internal error, append_arg, arg is nil' ].
  vcls := v class .
  vcls == RubyBlockPassNode ifTrue:[
    RubyParserM signalError:'internal error, append_arg, must use append_blk_arg'.
  ].
  v is_void_result ifTrue:[
    RubyParserM signalError:'void value expression'
  ].
  self _append: v .
  ^ self

