*maglev-runtime
_loadMspec: options env: envId
  | ctx |
    ctx := RubyContext load: options . "traceLoad logSexp"
    ctx env: envId persistentDo:[
      ctx _loadMspec: envId .
     (Object transientNameSpace: envId) at: #DEBUG_SPEC runtimePut:  true .
     System commitTransaction ifFalse:[ nil error:'commit fail' ].
    ].
    ^ ctx .

