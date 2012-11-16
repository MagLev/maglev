*maglev-runtime
irLocalAsgnValue
      "ruby_selector_suffix dependent"
   | lhsp |
   (lhsp := isLhsSplat)  ifNil:[ | res |  
     (res := GsComSendNode new)
        rcvr: node irNode;
        rubySelector: #'__splat_lasgn_value#0__' .
     ^ self  ir: res
   ]. 
   ^ self irNode 

