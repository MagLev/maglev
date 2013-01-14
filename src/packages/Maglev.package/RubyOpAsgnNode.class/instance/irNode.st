*maglev-runtime
irNode   "t :=rcvr . t setter: ( (t getter) op: arg  ) .     for all but ||= , &&=  
           t:= rcvr . (v := t getter) or:[ t setter: v ]     for   ||=   "
      "ruby_selector_suffix dependent"
    | tasgn tmpLeaf  get opName opSnd store blkSel getSel storeSel  |  
    tmpLeaf := rcvrTmp leaf . 
    opName := operatorCallName .
    opName == #'||' ifTrue: [ blkSel := #or: ] ifFalse:[ opName == #'&&' ifTrue:[ blkSel := #and: ]].
    getSel := variableCallName .
    storeSel := variableAsgnCallName _asSymbolWithRubySuffix: 16r4 " #1__ " .
    (tasgn := GsComAssignmentNode _basicNew) dest: tmpLeaf source: receiverNode irNode .  self ir: tasgn .
    (get := GsComSendNode new) 
       rubySelector: ( getSel _asSymbolWithRubySuffix: 16r0 "#0__" ). 
    self ir: get .
    (store := GsComSendNode new)  rubySelector: storeSel .      self ir: store .
    blkSel ifNil:[ 
      store rcvr: tasgn  .
      get  rcvr:  (GsComVariableNode new leaf: tmpLeaf) .
      (opSnd := GsComSendNode new)  rcvr: get ;
        rubySelector: (opName _asSymbolWithRubySuffix: 16r4 " #1__ ")  ;
        appendArgument: valueNode irNode .                  self ir: opSnd .
      store  appendArgument: opSnd .
      ^  store .
    ] ifNotNil:[
      get rcvr: tasgn .
      store rcvr: (GsComVariableNode new leaf: tmpLeaf) ;
            appendArgument: valueNode irNode .  
      (opSnd := GsComSendNode new)    rcvr: get ;
         stSelector:  blkSel ;
         appendArgument: ( self irInlineBlockIr: store ) ; optimize .
      ^ self ir: opSnd 
    ]

