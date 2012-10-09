*maglev-runtime
transientMethodDictForStoreEnv: envId
  | mds dict ofs |
  ofs := envId*4 + 1 .
  (mds := transientMethDicts) _isArray ifTrue:[
    dict := mds atOrNil: ofs .
  ] ifFalse:[ 
    envId == 0 ifTrue:[ dict := mds ]
  ]. 
  dict ifNotNil:[ ^ dict ].
  dict := GsMethodDictionary new .
  self transientMethodDictForEnv: envId put: dict .
  self isCommitted ifTrue:[ (SessionTemps current at: #RubyPinnedClasses) add: self ].
  ^ dict 

