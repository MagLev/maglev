*maglev-ast
opt_rescue: exClasses var: exVar body: bodyArg rescue: rescueBody ofs: srcOfs
  | body |
  body := bodyArg .
  exVar ifNotNil:[ | rhs asgn |
    rhs := RubyGlobalVarNode s_a: #'$!' .
    asgn := self node_assign: exVar ofs: srcOfs rhs: rhs .
    body ifNil:[
      body := RubyBlockNode s_list: { asgn }
    ] ifNotNil:[
      body := body prepend_to_block: asgn 
    ].
  ].
  ^ RubyRescueBodyNode s_a: exClasses b: body c: rescueBody d: srcOfs 

