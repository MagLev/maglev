*maglev-runtime
_inspect
 | res |
  res := '
[:masgnRp bofs ', position _inspect, ', ', firstNode _inspect .
trailingComma ifTrue:[ res addAll: ', <,> ' ].
res addAll:( ', ', thirdNode _inspect , $] ).
^ res

