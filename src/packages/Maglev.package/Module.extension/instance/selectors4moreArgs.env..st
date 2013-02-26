*maglev-runtime
selectors4moreArgs: baseSel  env: envId 
  "Return Ruby selectors in specified environment of the receiver with given base prefix
   and which have 4 or more fixed args 
   (i.e. which exceed max number of fixed args for a bridge method)."
      "ruby_selector_suffix dependent"
  | aDict selSet  blk |
  selSet := IdentitySet new .
  blk := [:aKey | | nFixed | 
     (aKey _rubyOrdAt: -4) == 35 "$#" ifTrue:[  "it is a ruby selector"
       nFixed := (aKey _rubyOrdAt: -3) - 48"$0" .
       (nFixed > 3 and:[ aKey at: 1 equals: baseSel ]) ifTrue:[  selSet add: aKey ].
     ].
  ].
  (aDict := self persistentMethodDictForEnv: envId ) ifNotNil:[   aDict keysDo: blk ].
  (aDict := self transientMethodDictForEnv: envId) ifNotNil:[ aDict keysDo: blk ].
  ^ selSet asArray .

