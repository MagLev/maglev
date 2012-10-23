*maglev-runtime
persistentMethodDictForEnv: envId
"result will be nil if no methods exist for specified environmentId."
| mds md |
(mds := methDicts) _isArray ifTrue:[ | idx |
  md := mds atOrNil: (envId*4 + 1)
] ifFalse:[
  envId == 0 ifTrue:[ md := mds ].   
].
md ifNil:[
 (format bitAnd: 16r14000) ~~ 0 ifTrue:[  
     "GC_MODULE_inclSelf_CLASS | GC_RUBY_VIRTUAL_CLASS"
    md := primaryCopy persistentMethodDictForEnv: envId
 ]
].
^ md

