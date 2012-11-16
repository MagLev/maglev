*maglev-runtime
add_arg: name_tok
  "Returns total number of fixed args"
  | args nod |
  (nod := RubyArgumentNode _basicNew) identifier: name_tok symval .
  (args := arguments ) ifNil:[
    (arguments := RubyListNode _basicNew) list: { nod } .
    ^ 1
  ] ifNotNil:[
    args _append: nod .
    ^ args size
  ].

