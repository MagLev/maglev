*maglev-runtime
_inspect
  | str sz |
  str := '[' copy .
  1 to: (sz := self size) do:[:j |
     str addAll: (self at: j) _inspect .
     j < sz ifTrue:[ str addAll: ', ' ].
  ].
  str addAll: ']' .
  ^ str

