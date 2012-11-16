*maglev-runtime
nameSpace: envId
  "return name space to use for an environment 1  lookup.  if tns is nil,
     then use pns for a faulted-in ruby class "
  | ns mds isVirt ofs |
  ofs := envId*4 + 2 .
  ns := transientMethDicts atOrNil: ofs .
  ns ifNotNil:[ ^ ns ].
  
  (isVirt := (format bitAnd: 16r14000) ~~ 0) ifTrue:[ 
     "RUBY_VIRTUAL, or MODULE_inclSelf"
     ns := primaryCopy transientNameSpace: envId .
     ns ifNotNil:[ ^ ns ].
  ].
  (mds := methDicts) _isArray ifTrue:[
    ns := mds atOrNil: ofs  .
    ns ifNotNil:[ ^ ns ].
  ].
  isVirt ifTrue:[
    ns := primaryCopy persistentNameSpace: envId 
  ].
  ^ ns

