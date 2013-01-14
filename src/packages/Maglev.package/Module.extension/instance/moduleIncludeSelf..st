*maglev-runtime
moduleIncludeSelf: envId
  "include a Module into itself so methods are available as both
     class and instance methods.   "
  | cls pm vCls vmd | 
  cls := self virtualClass .
  [ cls ~~ nil ] whileTrue:[
    (cls rubyPrimaryCopy == self and:[ cls isRubyModuleIncludeSelf]) ifTrue:[
      ^ self "already included"
    ] ifFalse:[ 
      cls := cls rubySuperclass: envId .
    ].
  ].
  self _rubyModuleIncludeSelfEnv: envId .
  ^ self

