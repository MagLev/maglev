*maglev-runtime
_rubyToCcallTemplateEnumargs_results: argsArray ampersand: aBlock

 | inst enumsInfo res resEnum |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"
 resEnum := #CCalloutResultEnumGoesHere .  " an FFI::Enum "

 inst translateArgs: argsArray usingEnums: enumsInfo .
 aBlock ifNotNil:[ argsArray add: aBlock ].
 res := inst callWith: argsArray .
 ^ inst translateResult: res usingEnum: resEnum .

