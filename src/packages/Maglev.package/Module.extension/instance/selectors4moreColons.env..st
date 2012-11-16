*maglev-runtime
selectors4moreColons: baseSel  env: envId 
  "Return Ruby selectors in specified environment of the receiver with given base prefix
   and which have 4 or more colons (i.e. exceed max number of colons for a bridge method)."
  | target aDict selSet  blk |
  target := baseSel , '::::' .
  selSet := IdentitySet new .
  blk := [:aKey | (aKey at: 1 equals: target) ifTrue:[ selSet add: aKey ] ].
  (aDict := self persistentMethodDictForEnv: envId ) ifNotNil:[   aDict keysDo: blk ].
  (aDict := self transientMethodDictForEnv: envId) ifNotNil:[ aDict keysDo: blk ].
  ^ selSet asArray .

