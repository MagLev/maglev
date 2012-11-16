*maglev-ast
s_a: anArray 
  | res |
  anArray _isArray ifFalse:[
     ArgumentError signal:'RubyDStrNode s_a, arg not an Array'
  ].
  (res := self _basicNew) list: anArray .
  ^ res

