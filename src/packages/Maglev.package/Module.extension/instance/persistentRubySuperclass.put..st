*maglev-runtime
persistentRubySuperclass: envId put: aCls
  | mds |
  (mds := methDicts) _isArray ifFalse:[
    self persistentMethodDictForEnv: envId put: nil .
    mds := methDicts .
  ].
  mds _rubyAt: (envId*4 + 2"zero based") put: aCls 

