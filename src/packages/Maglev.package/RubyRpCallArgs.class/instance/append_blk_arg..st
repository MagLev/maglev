*maglev-ast
append_blk_arg: node
  "returns receiver"
  iterNode ifNil:[
    (node == nil or:[ node class == RubyBlockPassNode]) ifTrue:[
      iterNode := node
    ] ifFalse:[
      RubyParserM signalError:'append_blk_arg, invalid block argument'
    ].
  ] ifNotNil:[
    RubyParserM signalError:'append_blk_arg, block argument already present'
  ].
  ^ self

