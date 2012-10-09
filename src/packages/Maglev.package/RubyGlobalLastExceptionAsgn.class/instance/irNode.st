*maglev-runtime
irNode
  | leaf node  |
   (leaf := self irLeaf)  ifNotNil:[ 
      node := (GsComAssignmentNode _basicNew
                  dest: leaf 
                 source:  (self typeCheckNodeFor: valueNode irEvaluatedBlockNode)) .
  ] ifNil:[ | gbl |
     "no enclosing rescue , assign to global $! "
     (gbl := RubyGlobalAsgnNode newForIr)
        name: #'$!' ;  valueNode: valueNode .
      node := gbl irNode .
  ].
   self ir: node .  
   ^ node 

