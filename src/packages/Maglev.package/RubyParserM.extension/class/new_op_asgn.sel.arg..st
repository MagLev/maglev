*maglev-ast
new_op_asgn: lhs sel: asgnSel arg: argNode
   | nLhs lhsAcc asgn_op arg res aCall |
   lhsAcc := lhs as_accessor .
   arg := argNode  kbegin_value .
   asgn_op := asgnSel symval .
   asgn_op == #'||' ifTrue:[
     nLhs :=  lhs node_assign_set_rhs: arg .
     nLhs == lhs ifFalse:[  self error: 'become needed in new_op_asgn'].
     (res := RubyOpAsgnOrNode _basicNew )
          firstNode: lhsAcc; secondNode: nLhs ; position: asgnSel src_offset .
     ^ res
   ].
   asgn_op == #'&&' ifTrue:[
     nLhs :=  lhs node_assign_set_rhs: arg .
     nLhs == lhs ifFalse:[  self error: 'become needed in new_op_asgn'].
     (res := RubyOpAsgnAndNode _basicNew )
          firstNode: lhsAcc; secondNode: nLhs ; position: asgnSel src_offset .
     ^ res
   ].
   aCall := self new_call_1: lhsAcc sel: asgnSel arg:  arg .
   nLhs := lhs node_assign_set_rhs: aCall .
   nLhs == lhs ifFalse:[  self error: 'become needed in new_op_asgn'].
   ^ lhs 

