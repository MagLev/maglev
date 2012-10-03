*maglev-runtime
irNode
      "ruby_selector_suffix dependent"
  | onceLit acc ifn array lst bld save |
  onceLit := GsComLiteralNode newObject: RubyDRegexpOnceLiteral _basicNew .

  array := GsComArrayBuilderNode new.
  lst := list .
  1 to: lst size do: [:n | array appendElement: (lst at: n) irNode].
  (bld := GsComSendNode new)
     rcvr: array;
     rubySelector:  #'__joinStringsWithRegexpOptions#1__' ;
     appendArgument: (GsComLiteralNode newInteger: self fixedOptions) .
  (save := GsComSendNode new)
     rcvr: onceLit ;
     stSelector:  #_atCachedIv:put: ;
     appendArgument: ( GsComLiteralNode newInteger: 1) ;
     appendArgument: bld  .  "bld is result of _atCachedIv:put: "

  (acc := GsComSendNode new)
    rcvr: onceLit ;
    stSelector: #regex .
  (ifn := GsComSendNode new)
    rcvr: acc ; 
    stSelector: #ifNil: ;
    appendArgument: ( self irInlineBlockIr: save  ) ;
    optimize . 

  self ir: acc ; ir: ifn ; ir: bld ; ir: save .
  ^ ifn

