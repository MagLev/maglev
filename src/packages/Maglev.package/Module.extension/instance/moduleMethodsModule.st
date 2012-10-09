*maglev-runtime
moduleMethodsModule
  "Return the virtual Module which holds the class-side method dictionary
   for a Module "
  | meta |
  meta := self virtualClass .
  meta isRubyModuleFunctions ifTrue:[ ^ meta ].
  self error:'cannot find module_methods module' .

