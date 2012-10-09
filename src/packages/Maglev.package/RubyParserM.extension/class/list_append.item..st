*maglev-ast
list_append: list item: item
  | arrayCls lst |
  arrayCls := RubyRpCallArgs .
  (lst := list) ifNil:[
    ^ arrayCls _new: item 
  ].
  lst class == arrayCls ifFalse:[
    lst := arrayCls _new: lst
  ].
  lst _append: item .
  ^ lst

