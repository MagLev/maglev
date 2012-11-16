*maglev-runtime
irOpElAsgn: asgnNodes tmps: tmpLeafs mName: mName 
  "generated code of form    store: a val:( r := op: (get: a) with: v) . r   "
      "ruby_selector_suffix dependent"

  | getSend nArgs getSel storeSel storeSend opSend saveRes resLeaf getMask storeMask|
  getSel := #'[]' .    getMask := 0 .
  storeSel := #'[]=' . storeMask := 4 "one fixed arg" .
  nArgs := asgnNodes size - 2 .
  (storeSend := GsComSendNode new)  rcvr:  (asgnNodes at: 1 ) .
  1 to:  nArgs do:[ :k |  storeSend appendArgument: (asgnNodes at: k + 2) .
                          getMask := getMask + 4 "add one fixed arg" .
                          storeMask := storeMask + 4 "add one fixed arg" .
  ].
  storeSend  rubySelector: (storeSel _asSymbolWithRubySuffix: storeMask) .

  (getSend := GsComSendNode new) 
     rcvr: ( GsComVariableNode new leaf: (tmpLeafs at: 1)) ;
     rubySelector: (getSel _asSymbolWithRubySuffix: getMask) .
  1 to: nArgs do:[:m | 
      getSend appendArgument:( GsComVariableNode new leaf: (tmpLeafs at: m + 2)) 
  ].
  (opSend := GsComSendNode new)   
     rcvr: getSend ; 
     rubySelector: (mName _asSymbolWithRubySuffix: 16r4 "#1__") ;
     appendArgument: valueNode irNode . 
  saveRes := GsComAssignmentNode _basicNew dest: (resLeaf := tmpLeafs at: 2) source: opSend .
  storeSend appendArgument: saveRes .

  self ir: opSend ; ir: storeSend; ir: getSend .
  ^ GsComStatementsNode new list:{ storeSend . (GsComVariableNode new leaf: resLeaf) }. 

