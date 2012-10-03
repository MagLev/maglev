*maglev-runtime
compiledMethodAt: aSymbol rubyEnv: envId

  "does not consider the package policy"
  | cls meth md |
  (md := self transientMethodDictForEnv: envId) ifNotNil:[
    meth := md at: aSymbol otherwise: nil .
    meth ifNotNil:[ ^ meth ].
  ]. 
  (md := self persistentMethodDictForEnv: envId ) ifNotNil:[
    meth := md at: aSymbol otherwise: nil .
    meth ifNotNil:[ ^ meth ].
  ].
  (format bitAnd: 16r14000) == 0 ifTrue:[ ^ nil ].
  cls := primaryCopy "RUBY_VIRTUAL|MODULE_inclSelf"  .
  cls ifNil:[  ^ nil ].
  
  (md := cls transientMethodDictForEnv: envId) ifNotNil:[
    meth := md at: aSymbol otherwise: nil . 
    meth ifNotNil:[ ^ meth ].
  ].
  (md := cls persistentMethodDictForEnv: envId ) ifNotNil:[
    meth := md at: aSymbol otherwise: nil . 
  ].
  ^ meth

