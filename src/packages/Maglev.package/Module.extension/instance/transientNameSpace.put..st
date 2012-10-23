*maglev-runtime
transientNameSpace: envId put: aNs 
  | tmds ofs |
  ((tmds := transientMethDicts) atOrNil: (ofs := envId*4 + 2) ) ifNotNil:[ 
     self error:' transientNameSpace already present' 
  ].
  tmds _rubyAt: ofs - 1 put: aNs . 

