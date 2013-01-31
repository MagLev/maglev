*maglev-runtime
moduleMethodsModuleOrNil
  "Return the virtual Module which holds the class-side method dictionary
   for a Module "
  | meta |
  meta := self virtualClass .
  meta == Module ifTrue: [ ^ meta ].
  meta isRubyModuleFunctions ifTrue:[ ^ meta ].
  ^ nil

