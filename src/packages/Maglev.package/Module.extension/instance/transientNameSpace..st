*maglev-runtime
transientNameSpace: envId
  | tns ofs | 
  tns := transientMethDicts atOrNil: (ofs := envId*4 + 2) .
  tns ifNil:[ 
    (format bitAnd: 16r14000) ~~ 0 ifTrue:[   
      "GC_RUBY_VIRTUAL_CLASS|GC_MODULE_inclSelf_CLASS"
      "rcvr is virtual , from include of a Module, or module inclSelf"
       tns := primaryCopy _transientNameSpaceAtOfs: ofs .
    ].
  ].
  ^ tns

