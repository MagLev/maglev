*maglev-runtime
_rubyNew1: aSuperCls do: aBlock
  "a ruby primitive"
  | newCls | 
  newCls := aSuperCls _primSubclass: #''  
      instVarNames: #() 
      format: (aSuperCls format bitOr: GC_ClassCreationRuby  )
      constraints: #()
      classVars: nil 
      poolDictionaries: #() .
  newCls immediateInvariant . 
  aBlock ifNotNil:[ | defStk cld |
    cld := GsProcess _current clientData .
    (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: newCls .
    cld at: 7 put: newCls " _rubyThreadDataAt: 7 put: " .
    [ | blk |
      blk := aBlock setSelf: newCls .
      blk @ruby1:value  .
    ] ensure:[
      defStk pop: newCls
    ]
  ].
  ^ newCls

