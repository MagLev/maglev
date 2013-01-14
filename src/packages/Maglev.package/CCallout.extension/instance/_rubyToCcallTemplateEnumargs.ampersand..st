*maglev-runtime
_rubyToCcallTemplateEnumargs: argsArray ampersand: aBlock
 | inst enumsInfo |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"

 inst translateArgs: argsArray usingEnums: enumsInfo .
 aBlock ifNotNil:[ argsArray add: aBlock ].
 ^ inst callWith: argsArray

