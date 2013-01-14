*maglev-runtime
unbind
  "a ruby primitive"
  | m |
  (m := RubyUnboundMeth new) method: gsmeth env: 1"__callerEnvId" selPrefix: selPrefix;
     bridge: execBridge .
  ^ m

