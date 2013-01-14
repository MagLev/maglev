*maglev-runtime
_rubyNew1: aSuperCls instVars: ivNames do: aBlock
  "a ruby primitive"
  | newCls | 
  newCls := aSuperCls _primSubclass: #''  
      instVarNames: ivNames
      format: (aSuperCls format bitOr: GC_ClassCreationRuby  )
      constraints: #()
      classVars: nil 
      poolDictionaries: #() .
  newCls immediateInvariant . 
  aBlock ifNotNil:[ | blk |
     blk := aBlock setSelf: newCls .
     blk @ruby1:value  .
  ].
  ^ newCls

