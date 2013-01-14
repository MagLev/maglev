*maglev-runtime
persistentNameSpace: envId put: aNs
  | mds ofs |
  (mds := methDicts) _isArray ifFalse:[
    self persistentMethodDictForEnv: envId put: nil .
    mds := methDicts .
  ].
  (mds atOrNil: (ofs := envId*4 + 2) ) ifNotNil:[
    self error:' persistentNameSpace already present'
  ].
  mds _rubyAt: ofs - 1  put: aNs

