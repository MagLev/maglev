*maglev-runtime
sessionInitialize
  "executed at each initialization of transient state"
 
  SessionTemps current at:#RubyGlobalNodesDict put: (SpecialGlobalNodesDict copy).


