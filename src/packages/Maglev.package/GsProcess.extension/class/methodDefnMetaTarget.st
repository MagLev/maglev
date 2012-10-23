*maglev-runtime
methodDefnMetaTarget
  "called from generated code"  
  ^ ( self _current _rubyThreadDataAt: 3 ) topValue theMetaClass

