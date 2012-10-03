*maglev-runtime
_rubyToCcallTemplateEnumresult: argsArray ampersand: aBlock
 | inst res resEnum |
 inst := #CCalloutInstanceGoesHere .
 resEnum := #CCalloutResultEnumGoesHere .
 aBlock ifNotNil:[ argsArray add: aBlock ].
 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

