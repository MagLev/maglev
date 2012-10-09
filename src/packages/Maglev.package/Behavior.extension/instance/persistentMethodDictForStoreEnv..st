*maglev-runtime
persistentMethodDictForStoreEnv: envId
  | mds dict ofs |
  ofs := envId*4 + 1 .
  (mds := methDicts) _isArray ifTrue:[
    dict := mds atOrNil: ofs .
  ] ifFalse:[
    envId == 0 ifTrue:[ dict := mds ]
  ].
  dict ifNotNil:[ ^ dict ].
  dict := GsMethodDictionary new . 
  self persistentMethodDictForEnv: envId put: dict .
  (methDicts atOrNil: 1) ifNotNil:[ "a class with existing smalltalk behavior"
    self isRubySingletonClass ifFalse:[
      RubyContext default trackRubyClass: self thisClass env: envId  . "for ruby context reset"
    ].
  ].
  ^ dict

