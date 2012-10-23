*maglev-runtime
classForConstantLookup: envId forModuleEval: aBoolean 
  "actual usage of this method
    is to get class for constant definition for top level of an eval"
  | cls |
  cls := self virtualClass .
  [ cls isRubySingletonClass or:[ cls isRubyModuleInclude] ] whileTrue:[
    cls := cls rubySuperclass: envId .
  ].
  ^ cls

