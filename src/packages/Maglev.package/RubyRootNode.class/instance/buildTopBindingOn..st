*maglev-runtime
buildTopBindingOn: irMeth
      "ruby_selector_suffix dependent"
  | bnd put  |
  (bnd := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     rubySelector:  #'binding#1__'  ;
     appendArgument: (GsComLiteralNode newObject: (RubyLexicalPath withAll: { Object }) ) .
  (put := GsComSendNode new) 
    rcvr: (GsComLiteralNode newObject: RubyContext ) ;
    stSelector:  #installTopBinding:  ;
    appendArgument:  (self ir: bnd) .

  irMeth appendStatement: ( self ir: put  )  .

