*maglev-runtime
evalLineArg
  | pos lnum  |
  (pos := position) _isInteger ifTrue:[ | cst | 
     cst := RubyCompilerState current .
     cst topMethodDefOrNil ifNotNil:[:mth|  "use implementation in RubyRootNode"
       [ lnum := mth lineForOffset: pos
       ] on: ArgumentError do:[:ex | "ignore" ]
     ]
  ].
  ^ lnum ifNotNil:[ RubyFixnumNode newForInt: lnum ].

