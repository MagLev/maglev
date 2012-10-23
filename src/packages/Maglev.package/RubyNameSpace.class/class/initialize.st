*maglev-runtime
initialize
  self _addClassVar: #TraceCount value: 0 ;
      _addClassVar: #TraceGlobals value: 0  ;
      _addClassVar:#TrapLookup value: false .

