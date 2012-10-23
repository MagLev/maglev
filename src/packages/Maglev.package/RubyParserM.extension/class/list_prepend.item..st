*maglev-ast
list_prepend: list item: item
  | arrayCls |
  arrayCls := RubyRpCallArgs .
  list class == arrayCls ifTrue:[
    list _prepend: item .
    ^ list
  ].
  ^  arrayCls s_a: item b: list

