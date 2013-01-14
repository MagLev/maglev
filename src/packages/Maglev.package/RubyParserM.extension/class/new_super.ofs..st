*maglev-ast
new_super: args ofs: srcOfs
  | aryCls argsCls |
  aryCls := RubyRpCallArgs .
  args ifNil:[
    ^ RubySuperNode _basicNew argsNode: aryCls _new  ; position: srcOfs 
  ].
  (argsCls := args class) == RubyBlockPassNode ifTrue:[ 
    ^ RubySuperNode _basicNew iterNode: args ; position: srcOfs
  ].
  argsCls == aryCls ifTrue:[
    ^ RubySuperNode _basicNew argsNode: args ; position: srcOfs
  ].
  self signalError: 'internal_error: new_super, invalid args'.
  ^ nil

