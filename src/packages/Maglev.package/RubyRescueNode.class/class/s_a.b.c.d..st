*maglev-ast
s_a: body b: rescuebody c: elsebody d: srcOfs
  | res |
  rescuebody class == RubyRescueBodyNode ifFalse:[
    RubyParserM signalError:'invalid rescuebody for RubyRescueNode' .
    ^ nil
  ].
  (res := self _basicNew)
     init_a: body b: rescuebody c: elsebody d: srcOfs .
  ^ res

