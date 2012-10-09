*maglev-runtime
_rubyToCcallTemplate: argsArray
  | inst |
  inst := #CCalloutInstanceGoesHere .
  ^ inst callWith: argsArray

