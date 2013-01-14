*maglev-ast
new_fcall_braceBlock: selTok args: args blkArg: blkArg
  | res |
  res := self new_call: RubySelfNode _basicNew sel: selTok arg: args .
  blkArg ifNotNil:[
    res class == RubyBlockPassNode ifTrue:[
      self signalError: 'both block arg and actual block given'.
    ].
    blkArg callNode: res .
    res := blkArg
  ].
  ^ res

