*maglev-ast
new_yield: args ofs: srcOfs
  | cArgs |
  args ifNil:[ 
    cArgs := RubyRpCallArgs _new 
  ] ifNotNil:[ | args_cls |
    (args_cls := args class) == RubyRpCallArgs ifTrue:[
      cArgs := args as_yield_args
    ] ifFalse:[ 
      args_cls == RubyBlockPassNode ifTrue:[
        self signalError: 'Block argument should not be given.'
      ] ifFalse:[
        self signalError: 'internal_error new_yield invalid args' 
  ]]].
  ^ RubyYieldNode _basicNew argsNode: cArgs ; position: srcOfs

