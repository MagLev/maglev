*maglev-runtime
initTopScope: environmentId
    "returns the transient top level name space"
  | tns pns |
  pns := RubyNameSpace new .
  pns initializeForModule: Object  env: environmentId .

  tns := RubyTransientNameSpace new .
  tns initializeForModule: Object persistentCopy: pns env: environmentId .
  Object _setTopNameSpace: tns persistent: pns env: environmentId .
  
  TraceGlobals >=2 ifTrue:[ GsFile gciLogServer: 'initialized root scope: ' , self describe ].
  ^ tns 

