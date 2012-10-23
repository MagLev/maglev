*maglev-runtime
translateResult: res usingEnum: anEnum
  | r |
  r := anEnum @ruby1:__val_to_symbol: res  .
  r ifNil:[ ArgumentError signal: 'value not valid for specified result Enum' ].
  ^ r

