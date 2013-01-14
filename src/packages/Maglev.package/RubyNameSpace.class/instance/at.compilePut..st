*maglev-runtime
at: aKey compilePut: aValue 
  "used by RubyFile>>load and RubyNameSpace>>childScopeAt:isDefine: .
   no replication of store to persistent copy. " 
| anAssoc |
anAssoc := self _at: aKey otherwise: nil .  
anAssoc ifNil:[ | newAssoc |
    newAssoc := RubySymbolAssociation newWithKey: aKey .  
    aValue ifNotNil:[ newAssoc _compileTimeValue: aValue ].
    self _at: aKey put: newAssoc.    
] ifNotNil:[
  anAssoc _compileTimeValue: aValue
].
^aValue

