*maglev-runtime
irForProcNewZeroArgs
   "the implicit block leaf will be either nil or an ExecBlock at runtime"
      "ruby_selector_suffix dependent"
   | node  | 
   ( node := GsComSendNode new )
        rcvr: self irReceiverNode;
        rubySelector:  #'new#0_&' ;
        appendArgument: self currentScope implicitBlockVar .
    self ir: node .
    ^ node 

