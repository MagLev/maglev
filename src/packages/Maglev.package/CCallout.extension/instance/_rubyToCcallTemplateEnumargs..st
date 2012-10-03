*maglev-runtime
_rubyToCcallTemplateEnumargs: argsArray
 | inst enumsInfo |
 inst := #CCalloutInstanceGoesHere .
 enumsInfo := #CCalloutEnumsInfoGoesHere . "Array of pairs ( 1-based argNum, an FFI::Enum)"

 inst translateArgs: argsArray usingEnums: enumsInfo . 
 ^ inst callWith: argsArray

