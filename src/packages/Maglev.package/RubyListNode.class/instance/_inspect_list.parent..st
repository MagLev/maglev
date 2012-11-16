*maglev-runtime
_inspect_list: separator parent: aParent
  | res sz |
  res := String new .
  1 to: (sz := list size) do:[:j | | elem str |
     elem := list at: j . 
     str := aParent ifNotNil:[ 
         elem == aParent ifTrue:[ '<recursiveList>' ] ifFalse:[ elem _inspect]
     ] ifNil:[ 
         elem _inspect 
     ] .
     res addAll: str .
     j < sz ifTrue:[ res addAll: separator ].
  ].
  ^ res

