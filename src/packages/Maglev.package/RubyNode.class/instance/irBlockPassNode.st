*maglev-runtime
irBlockPassNode
      "ruby_selector_suffix dependent"
  | send |  
  ( send := GsComSendNode new)
     rcvr: self irNode ;
     rubySelector: #'__to_proc_arg#0__' .
  self ir: send.
  ^ send 

