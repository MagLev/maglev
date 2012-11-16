*maglev-ast
append_mrhs: val
  thirdNode ifNil:[ | v_cls |
    v_cls := val class .
    (v_cls == RubyRpCallArgs or:[ v_cls == RubySplatNode ]) ifTrue:[
      thirdNode := val
    ] ifFalse:[
      RubyParserM signalError: 'append_mrhs invalid arg' .
    ]
  ] ifNotNil:[
    RubyParserM signalError: 'append_mrhs rhs already present'.
  ].
  ^ self

