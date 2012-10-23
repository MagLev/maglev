*maglev-runtime
irForNode
      "ruby_selector_suffix dependent"
  | node |
  node := (GsComSendNode new)
            rcvr: self irNode;
            rubySelector: #'__each#0_&' .
  self ir: node .
  ^ node 

