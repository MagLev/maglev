*maglev-runtime
rtModuleLexPath
  "Result is in inner class to outer class order"
  | stk res sz |
  stk := rtModuleStack .
  sz := stk size .
  sz ~~ 1 ifTrue:[
    res := Array new: sz - 1 . 
    0 to: sz - 2 do:[:j | res _rubyAt: j put: (stk at: sz - j) ].
    ^ res
  ] ifFalse:[
    ^ { }
  ].

