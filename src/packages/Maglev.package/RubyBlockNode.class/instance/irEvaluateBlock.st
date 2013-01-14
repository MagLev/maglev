*maglev-runtime
irEvaluateBlock
  "generate a send to evaluate a block "
      "ruby_selector_suffix dependent"
  | irBlk send |
  irBlk := self irNode .
  ( send := GsComSendNode new)
     rcvr: irBlk ;
     rubySelector:  #'call#0__'   .
  self ir: send . 
  ^ send 

