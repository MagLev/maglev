*maglev-runtime
methodDefnTarget
  "called from generated code and Object>>_bindingContext: "  
  ^ ( self _current _rubyThreadDataAt: 3 ) topOrNil 

