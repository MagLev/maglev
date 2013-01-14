*maglev-runtime
transientNameSpaceForStore: envId
  | tns ofs |
  (format bitAnd: 16r14000) ~~ 0 ifTrue:[ "MODULE_inclSelf or RUBY_VIRTUAL"
    ^ primaryCopy transientNameSpaceForStore: envId
  ].
  ofs := envId*4 + 2 .
  (tns := transientMethDicts atOrNil: ofs) ifNotNil:[ ^ tns ].
  ^ self initNameSpacesForExtend: envId . 

