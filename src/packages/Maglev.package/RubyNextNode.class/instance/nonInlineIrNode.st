*maglev-runtime
nonInlineIrNode
   "next in a non-inline loop does a normal return from block "
  | node | 
  node := GsComSendNode new.
  (RubyCompilerState current topLoop) ifNil:[  "fix 861"
    node rcvr: (GsComLiteralNode newObject: CannotReturn) ;
      stSelector:  #signal:  ; 
      appendArgument: (GsComLiteralNode newString: 'unexpected next' ).
  ] ifNotNil:[
    node rcvr: self irGotoValueNode;
      stSelector: #_rubyNext:with: ;
      appendArgument: (GsComLiteralNode newInteger: 0 );
      appendArgument: GsComLiteralNode newNil .
  ].
  self ir: node .
  ^ node 

