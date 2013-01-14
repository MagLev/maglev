*maglev-ast
new_call_braceBlock: rcv sel: selTok args: args blkArg: blkArg
  | res |
  res := self new_call: rcv sel: selTok arg: args  .
  blkArg ifNotNil:[
    res class == RubyBlockPassNode ifTrue:[
      self signalError: 'both block arg and actual block given'.
    ].
    blkArg callNode: res .
    res := blkArg
  ].
  ^ res

