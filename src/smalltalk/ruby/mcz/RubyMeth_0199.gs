
set class RubyMeth
category: 'as yet unclassified'
method:
object: anObject
  obj := anObject

%


set class RubyMeth
category: '*maglev-runtime'
method:
unbind
  "a ruby primitive"
  | m |
  (m := RubyUnboundMeth new) method: gsmeth env: 1"__callerEnvId" selPrefix: selPrefix;
     bridge: execBridge .
  ^ m

%

