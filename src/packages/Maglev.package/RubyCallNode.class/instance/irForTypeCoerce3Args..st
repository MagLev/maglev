*maglev-runtime
irForTypeCoerce3Args: fullSel 
      "ruby_selector_suffix dependent"
   | toCls coercSel tmpLeaf asgnNode isaSelector specSendNode ifNode coercSufix |
   "at this point self.irArgNodes should be IR for  anObj, aLitVar, aSymbol .
    If compiling bootstrap code we always optimize a call such as   
       Type.coerce_to(o , Integer, :to_int)
    to
       o _isInteger ifTrue:[ o ]
          ifFalse:[ Type.__coerce_to_Integer_to_int(o) ]
    If not compiling bootstrap code, we attempt the optimization.
   "
   fullSel == #'coerce_to#3__' ifTrue:[ coercSufix := '' ]
     ifFalse:[  fullSel == #'coerce_to_or_nil#3__' ifTrue:[ coercSufix := '_or_nil' ]
                       ifFalse:[ self error:'invalid coerce selector']].  
   toCls := (irArgNodes at: 2) litVarValueOrNil .
   toCls ifNil:[  
       self installingPrims ifTrue:[   GsFile gciLogServer:
          'warning 2nd arg dynamic constant deoptimizes Type.coerce_to , near ',     self sourcePositionAsString.
        ].
       ^ nil
    ].
   (coercSel  := (irArgNodes at:3) symbolLiteralValue) ifNil:[  
       self signalParseError:'3rd arg to coerce_to must be a Symbol' 
   ].
   isaSelector := GsComSelectorLeaf classToISAselector: toCls.
   isaSelector ifNil:[  
      self installingPrims ifTrue:[   GsFile gciLogServer:
         'warning Type.coerce_to not optimized for ', toCls name , ' near ' , self sourcePositionAsString
       ].
      ^ nil
   ].
  (asgnNode := GsComAssignmentNode _basicNew)     "tmp := firstIrArg "
     dest:  (tmpLeaf := evaluationTmpAssoc leaf )
     source:  (irArgNodes at:1)  .
  self ir: asgnNode .
  ( specSendNode := GsComSendNode new )        " example:   tmp _isInteger"
     rcvr: asgnNode ;
     stSelector: isaSelector .                
  ( ifNode := GsComSendNode new)
     rcvr: specSendNode ;
     stSelector: #ifTrue:ifFalse:   ;
     appendArgument: (self newInlineBlock:[ :blk | 
                blk appendStatement: (GsComVariableNode new leaf: tmpLeaf). blk ]) ;
     appendArgument: (self newInlineBlock:[ :blk | | slowNode slowSel  |
              (slowSel := '__coerce_to_' copy )  addAll: (GsComSelectorLeaf classToRubyClassName: toCls) ;
                  addAll: '_' ; addAll: coercSel ; addAll: coercSufix ; addAll: '#1__'    .
              slowSel := slowSel asSymbol .
              ( slowNode := GsComSendNode new)
                   rcvr: self irReceiverNode ;
                   rubySelector:  slowSel .
              slowNode appendArgument: (GsComVariableNode new leaf: tmpLeaf) .
              self ir: slowNode .
              blk appendStatement: slowNode .
              blk ] );
      optimize .
   self ir: ifNode .
   ^ ifNode 

