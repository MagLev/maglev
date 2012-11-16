*maglev-runtime
installTopBinding: aBinding
  "called from generated code,  returns the argument" 
  (Object transientNameSpaceForStore: 1"__callerEnvId") 
       at: #TOPLEVEL_BINDING transientRuntimePut: aBinding .
  ^ aBinding

