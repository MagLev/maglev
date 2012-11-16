*maglev-ast
s_a: aRegexNode
  | node  str |
  ( str := RubyVcGlobalNode _basicNew) name: #'$_' .
  (node := RubyMatch2Node _basicNew) 
     receiverNode: str ;
     valueNode:  aRegexNode .
  node initImplicitDollarTildeForRp .
  ^ node 

