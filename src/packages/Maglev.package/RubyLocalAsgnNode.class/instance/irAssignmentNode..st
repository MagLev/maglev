*maglev-runtime
irAssignmentNode: srcIrNode
   | asgn loc locVinfo  dLeaf varRef | 
   isBlockArg ifTrue:[ self error:'irNode illegal for LocalAsgn as outgoing block arg'].
   dLeaf := (loc := location) leaf .
   dLeaf ifNotNil:[
     asgn := GsComAssignmentNode _basicNew dest: (dLeaf := (loc := location) leaf)
                        source:  srcIrNode .
     self ir: asgn .
     (locVinfo := loc varInfo) readCount > 0 ifTrue:[  | clr |
       "need to clear the tmp caching result of __to_proc"
       createsVar ifTrue:[ 
           self error:'RubyLocalAsgnNode:both createsVar and (toProc)readCount > 0'
       ].
       clr := GsComAssignmentNode _basicNew dest: locVinfo toProcInfo leaf 
                                        source: GsComLiteralNode newNil .     
        varRef := GsComVariableNode new leaf: dLeaf .  
       ^ GsComStatementsNode new list:{  asgn . clr . varRef  }. 
     ] ifFalse:[
       ^ asgn
     ]
   ] ifNil:[
     ^ loc irAssignmentNode: srcIrNode "store into var in eval binding, trac 829"
   ]

