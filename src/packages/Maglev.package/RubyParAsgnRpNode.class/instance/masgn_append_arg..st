*maglev-ast
masgn_append_arg: val
   thirdNode ifNil:[ 
     firstNode ifNil:[ " path probably never taken"
       RubyParserM signalError: 'masgn_append_arg lhs is nil' .
     ].
     thirdNode := val .
     toAry := true .
   ] ifNotNil:[
     RubyParserM signalError: ' masgn_append_arg rhs already present'.
   ].
   ^ self

