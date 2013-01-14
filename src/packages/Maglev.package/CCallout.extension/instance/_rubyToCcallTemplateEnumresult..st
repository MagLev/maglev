*maglev-runtime
_rubyToCcallTemplateEnumresult: argsArray

 | inst res resEnum |
 inst := #CCalloutInstanceGoesHere .
 resEnum := #CCalloutResultEnumGoesHere .

 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

