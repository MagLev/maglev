*maglev-runtime
moduleNesting
  "Result is the contents of rtModuleStack, excluding Object"
  | stk sz |
  stk := GsProcess _current _rubyThreadDataAt: 5 "rtModuleStack" .
  sz := stk size.
  sz > 1 ifTrue:[ | k res |
    k := sz - 1.
    res := Array new: k .
    2 to: sz do:[:j | res at: k put: (stk at: j) . k := k - 1 ].
    ^ res
  ] ifFalse:[  
    ^ { }
  ].

