*maglev-runtime
bind: anObject
  "a ruby primitive.
   code in UnboundMethod.rb  checks kindOf ... "
  | m |
  (m := RubyMeth _basicNew) .
  m method: gsmeth env: 1"__callerEnvId" selPrefix: selPrefix ;
        bridge: execBridge ; object: anObject .
  ^ m

