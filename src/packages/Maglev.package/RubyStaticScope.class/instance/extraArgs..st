*maglev-runtime
extraArgs: aNumber
   "no changes to variableNodes array ; only sender is RubyArgsNode,  
    and it adds extra args to variableNames after normal ones. "
  extraArgs := aNumber .
  TraceLocals >= 1 ifTrue:[ 
	  self trace: 'set extraArgs to ' , aNumber asString  .
  ].

