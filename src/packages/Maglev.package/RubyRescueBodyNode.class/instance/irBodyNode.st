*maglev-runtime
irBodyNode
^ self newBlock: [:block| | exLeaf send  exStack bdy |
  [ exStack := RubyCompilerState current lastExceptionStack .
    exLeaf := GsComVarLeaf new blockArg: #__ex argNumber: 1 forBlock: block.
    exStack push: exLeaf . 
    block appendArg: exLeaf .
        "ruby thread data at  OC_RubyGsProcessClientData_lastException no longer used"
    (bdy := bodyNode) ifNil:[
       send := GsComLiteralNode newNil returnNode .
    ] ifNotNil:[
      ( send := GsComSendNode new) 
          rcvr: (bdy  irBlockNode: self)  ;
          stSelector: #value .
    ].
    self ir: send .
    block appendStatement: send .
  ] ensure:[
     exStack pop:  exLeaf 
  ].
  block
]

