*maglev-runtime
transientMethodDictForEnv: envId
  | ofs dict |
  ofs := envId*4 + 1 .
  dict := transientMethDicts atOrNil: ofs .
  dict ifNil:[
    (format bitAnd: 16r14000) ~~ 0 ifTrue:[ 
      "GC_MODULE_inclSelf_CLASS | GC_RUBY_VIRTUAL_CLASS"
       dict := primaryCopy transientMethodDictForEnv: envId .
    ].
  ].
  ^ dict

