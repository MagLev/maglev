*maglev-runtime
_inspect
  | res |
  res := '[:args ' copy .
  arguments ifNotNil:[ res addAll: arguments _inspect_list ].
  optArgs ifNotNil:[ res addAll:', <optionalArgs> '; addAll: optArgs _inspect ].
  restArgNode ifNotNil:[ res addAll: ', :*' , restArgNode identifier  ].
  blockArgNode ifNotNil:[ :b | res addAll:', '; addAll: b _inspect ].
  res add: $]  .
  ^ res

