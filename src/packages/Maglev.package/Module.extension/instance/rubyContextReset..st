*maglev-runtime
rubyContextReset: envId
  "note,  does not remove the virtual class for moduFcts from Kernel.
   Kernel is the only persistent Module reachable after clearing
   Object's name spaces. "
  | mds ofs |
  ofs := envId*4 .
  (mds := transientMethDicts) _isArray ifTrue:[ 
     mds _rubyAt:ofs put: nil ;  "method dict"
          _rubyAt:ofs + 1 put: nil ;  "name space"
          _rubyAt:ofs + 2 put: nil .  "ruby super class"
  ].
  (mds := methDicts) _isArray ifTrue:[ 
    mds _rubyAt: ofs put: nil ;  "method dict"
        _rubyAt: ofs + 1 put: nil ;  "name space"
        _rubyAt: ofs + 2 put: nil .  "ruby super class"
  ].

