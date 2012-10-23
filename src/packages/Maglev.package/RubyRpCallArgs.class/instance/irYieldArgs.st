*maglev-runtime
irYieldArgs
      "ruby_selector_suffix dependent"
  | lst sz | 
  lst := list.
  sz := lst size .
  sz >= 1 ifTrue:[ 
    (lst at: sz)  isSplatNode ifTrue:[ "inline hasRestArg"
       ^ { #'call#0*_' .  self irNode }
    ].
    sz > 3 ifTrue:[  | ary | 
      ^ { #'call#1__' . self irNode  }
    ].
  ].
  ^ nil

