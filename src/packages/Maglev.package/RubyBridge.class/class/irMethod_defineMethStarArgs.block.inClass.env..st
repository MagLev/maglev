*maglev-runtime
irMethod_defineMethStarArgs: selPrefix block: aBlock inClass: aClass  env: envId
   "aBlock must have been produced by _copyForRuby:1   "
      "ruby_selector_suffix dependent"
    | node sel sndSet sndCall argsLeaf blkLit srcStr rtn blkArgLeaf |
    sel := selPrefix _asSymbolWithRubySuffix: 16r3  "(prefix , '#0*&') asSymbol " .
    srcStr := '<a define_method >
' , aBlock _debugSourceForBlock .
    (node := GsComMethNode newRuby)
       environment: envId ; class: aClass ;  selector: sel  ;
       source: srcStr ; sourceOffset: 1 .
   argsLeaf := GsComVarLeaf new methodArg: #args argNumber: 1 .
   blkArgLeaf := GsComVarLeaf new methodArg: #block argNumber: 2 .
    node appendArg:  argsLeaf ; appendArg: blkArgLeaf .
    blkLit := GsComLiteralNode newObject: aBlock .  
    (sndSet := GsComSendNode new)
       rcvr: blkLit ;
       stSelector:  #setSelf:  ;
       appendArgument: ( GsComVariableNode newSelf ) ;
       sourceOffset: 1 . 
    (sndCall := GsComSendNode new)
       rcvr: sndSet ;
       stSelector:  #'_rubyCall:block:' ;
       appendArgument: (GsComVariableNode new leaf: argsLeaf) ;
       appendArgument: (GsComVariableNode new leaf: blkArgLeaf) ;
       sourceOffset: srcStr size .
    rtn := sndCall returnNode .
    rtn sourceOffset: srcStr size  - 5 .
    node  sourceOffset: 1 ;
      appendStatement: rtn ;
      endSourceOffset: srcStr size .
    ^ node

