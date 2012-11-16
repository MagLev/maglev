*maglev-ast
s_a: argBlock b: srcOfs
  | res |
  (res := self _basicNew )
     ensureNode: (argBlock ifNil:[ RubyNilNode _basicNew]) ;
     position: srcOfs .
  ^ res

