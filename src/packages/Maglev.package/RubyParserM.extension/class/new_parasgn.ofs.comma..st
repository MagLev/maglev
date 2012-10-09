*maglev-ast
new_parasgn: lhs ofs: srcOfs comma: trailingCommaBool
   lhs ifNil:[ 
     self signalError: 'lhs is nil in new_parasgn'
   ].
   ^ RubyParAsgnRpNode s_a: lhs ofs: srcOfs comma: trailingCommaBool

