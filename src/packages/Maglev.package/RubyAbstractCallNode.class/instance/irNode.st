*maglev-runtime
irNode
      "ruby_selector_suffix dependent"
  | node fullSel  numIrArgs isStSend  rcv  firstCh |
  irArgNodes := self irArgNodes .
  numIrArgs := irArgNodes size .   
  isStSend := self isSmalltalkSend .
  fullSel := self fullSelector: isStSend .  
  firstCh := fullSel at: 1 .
  firstCh == $c ifTrue:[
    ((fullSel == #'coerce_to#3__' or:[ fullSel == #'coerce_to_or_nil#3__' ]) 
           and:[ self rcvr_isTypeClass ]) ifTrue:[
      node := self irForTypeCoerce3Args: fullSel .
      node ifNotNil:[ ^ node ]  .
    ].
  ] ifFalse:[
    firstCh == $n ifTrue:[
      ( fullSel == #'new#0__' and:[ self rcvr_isProcClass ])  ifTrue:[
         ^ self irForProcNewZeroArgs 
      ].
      (fullSel == #'nesting#0__' and:[ self rcvr_isModuleClass]) ifTrue:[
         node := self irForModuleNesting .
         node ifNotNil:[ ^ node ]
      ].
    ] ifFalse:[ 
      fullSel == #'each#0_&' ifTrue:[ | fsArr |
         "convert to call to Object>>__each: which catches RubyBreakException"
         fsArr := { #'__each#0_&' } .  
         rcv := self irReceiverNodeEach: fsArr  .  "may alter fsArr at:1"
         fullSel := fsArr at: 1 .
      ] .
    ].
  ].
  rcv ifNil:[ rcv := self irReceiverNode ].
  (node := GsComSendNode new)  rcvr:  rcv .
  isStSend ifTrue:[
    node stSelector: fullSel 
  ] ifFalse:[ 
     node rubySelector: fullSel toSuper: self isSendSuper .
     self bypassProtection ifTrue:[ node setBypassRubyProtection  ]. 
  ].
  self buildArgumentsOn: node .
  self shouldOptimize ifTrue:[ node optimize]. "control-flow optimiz like if;else;end"
  self  ir: node.               
  ^ node

