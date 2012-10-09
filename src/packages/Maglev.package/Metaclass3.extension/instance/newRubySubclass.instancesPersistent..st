*maglev-runtime
newRubySubclass: aString  instancesPersistent: ipersistBool

| result cst fmt |
(self subclassesDisallowed) ifTrue: [
   ^ self _error: #classErrSubclassDisallowed
].
(self instancesInvariant) ifTrue: [
  "assumes instancesInvariant:false  from caller"
  ^ self _error: #classErrInvariantSuperClass
].
fmt := format bitOr: GC_ClassCreationRuby .
        "assumes instancesInvariant:false  from caller"
ipersistBool ifFalse:[ fmt := fmt bitOr: GC_NON_PERSISTENT_MASK ].
result := self _subclass: aString asSymbol
          instVarNames: #()
          format: fmt   
          constraints: #()
          classVars: nil
          poolDictionaries: nil  .
             "result has
                 classInstVars: #()
                 poolDictionaries: ()
                 inDictionary: nil
                 inClassHistory: nil
                 description: nil
                 isModifiable: true "

(self class instSize > result class instSize) ifTrue: [
  self error: 'smalltalk class instance variables not supported for Ruby classes'
].
^ result

