*maglev-runtime
deleteMethods: envId
  "Delete receivers' method dictionaries for the specified envId"
  | mds ofs |
  ofs := envId*4 .
  (mds := transientMethDicts) _isArray ifTrue:[ 
     mds _rubyAt:ofs put: nil .  "method dict"
  ].
  (mds := methDicts) _isArray ifTrue:[ | rns |
    rns := mds atOrNil: ofs + 2 .
    mds _rubyAt: ofs put: nil .  "method dict"
  ].

