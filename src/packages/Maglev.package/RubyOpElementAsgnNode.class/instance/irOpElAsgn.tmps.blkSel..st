*maglev-runtime
irOpElAsgn: asgnNodes tmps: tmpLeafs blkSel: blkSel
      "ruby_selector_suffix dependent"
  " generated code of form     (get: a) blkSel:[ store: a val: v ]   "

  | getSend nArgs getSel storeSel storeSend testSend saveRes blkBody resLeaf 
    getMask storeMask |
  getSel := #'[]'     . getMask := 0 .
  storeSel := #'[]='  . storeMask := 4 .  "one fixed arg"
  nArgs := asgnNodes size - 2 .
  (getSend := GsComSendNode new)  rcvr:  (asgnNodes at: 1 ) .
  1 to:  nArgs do:[ :k |  getSend appendArgument: (asgnNodes at: k + 2) .
                          getMask := getMask + 4 "add one fixed arg" .
                          storeMask := storeMask + 4 "add one fixed arg"
  ].
  getSend  rubySelector:  (getSel _asSymbolWithRubySuffix: getMask) .

  (storeSend := GsComSendNode new) 
     rcvr: ( GsComVariableNode new leaf: (tmpLeafs at: 1)) ;
     rubySelector: (storeSel _asSymbolWithRubySuffix: storeMask) .
  1 to: nArgs do:[:m | 
      storeSend appendArgument:( GsComVariableNode new leaf: (tmpLeafs at: m + 2 )) 
  ].
  saveRes := GsComAssignmentNode _basicNew dest:( resLeaf := tmpLeafs at: 2) 
                                           source: valueNode irNode .
  storeSend appendArgument: saveRes .
  blkBody := GsComStatementsNode new list:{ storeSend . (GsComVariableNode new leaf: resLeaf) }.
  (testSend := GsComSendNode new)   rcvr: getSend ; 
     stSelector: blkSel ;
     appendArgument: ( self irInlineBlockIr: blkBody ) ;     
     optimize .   "typically blkSel is #or:  or #and:  "
  self ir: testSend ; 
    ir: storeSend; 
    ir: getSend .
  ^ testSend 

