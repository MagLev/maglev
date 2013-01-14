*maglev-runtime
irBlockPassNode
      "ruby_selector_suffix dependent"
  "irBlockPassNode not sent if in bootstrap."
  useToProc > 0 ifTrue:[ "self is an incoming blockArg"
     ^ self irNode
  ] ifFalse:[ | send |
    ( send := GsComSendNode new)
       rcvr: self irNode ;
       rubySelector: #'__to_proc_arg#0__'  .
    self ir: send.
    ^ send 
  ]

