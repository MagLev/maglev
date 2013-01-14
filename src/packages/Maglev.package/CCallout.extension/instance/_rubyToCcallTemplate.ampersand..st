*maglev-runtime
_rubyToCcallTemplate: argsArray ampersand: aBlock
  | inst arr |
  inst := #CCalloutInstanceGoesHere .
  aBlock ifNotNil:[ argsArray add: aBlock ].
  ^ inst callWith: argsArray

