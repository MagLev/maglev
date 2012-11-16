*maglev-runtime
clearTransientState: envId
  | tmds ofs |
  ofs := envId*4 .
  (tmds := transientMethDicts) _isArray ifTrue:[
     tmds _rubyAt:ofs put: nil ;  "method dict"
          _rubyAt:ofs + 1 put: nil ;  "name space"
          _rubyAt:ofs + 2 put: nil .  "ruby super class"
  ].
  "higher layer reponsible for _refreshClassCache"

