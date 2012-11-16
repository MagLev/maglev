*maglev-runtime
_rubySubclassOf: aClass env: envId
  "return true if self is a subclass of aClass in specified environment"
| cls |
cls := self .
[ true ] whileTrue:[
  cls == aClass ifTrue:[ ^ true ].
  cls := cls rubySuperclass: envId .
  cls ifNil:[ ^ false ].
].

