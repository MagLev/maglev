*maglev-ast
logop: cls left: left_arg right: right
  | left |
  left := left_arg kbegin_value "remove_begin" .
  (left class == cls and:[ left paren == nil "not left.paren"]) ifTrue:[ 
    | node second |
    node := left .
    [  second := node secondNode .
       second class == cls and:[ second paren == nil ]
    ] whileTrue:[
       node := second
    ].
    node secondNode: ( cls s_a: second b: right ) .
    ^ left
  ].
  left is_void_result ifTrue:[
    self signalError: 'void value expression'
  ].
  ^ cls s_a: left b: right

