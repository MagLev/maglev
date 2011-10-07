! patchMaster30.gs changes integerated into ConfigurationOfGLASS-DaleHenrichs.179

! fix SecurityError when doing ruby filein with Monticello code visible for
!  debugging, by only changing dictionary name if needed.
! optimization to get symbolList outside of loop and only resolve #SharedPools once
category: '*bootstrap'
method: Class
poolDictionariesForNames: poolDictionaryNames

  | ar existingDict symList sharedPool |
  ar := Array new.
  symList := GsSession currentSession userProfile symbolList .
  poolDictionaryNames do: [:poolName |
    existingDict := symList objectNamed: poolName.
    existingDict ifNil:[ | pool |
        pool := SymbolDictionary new.
        pool name: poolName asSymbol.
        ar add: pool.
    ] ifNotNil:[
      (existingDict isKindOf: SymbolDictionary) ifTrue: [
        ar add: existingDict
      ] ifFalse:[
        sharedPool ifNil:[ sharedPool := symList objectNamed: #SharedPool ].
        ((existingDict isKindOf: Class) and:[ existingDict isSubclassOf: sharedPool ]) ifTrue: [
           | cvars pName |
           ar add: (cvars := existingDict _createClassVarsDict) .
           pName := poolName asSymbol .
           "only change dictionary name if needed , to avoid SecurityError"
           cvars name ~~ pName ifTrue:[ cvars name: pName ].
        ]
      ].
    ].
  ].
  ^ar
%

! fix MNU
category: '*squeak'
method: Symbol
squeakPrintString

  | res idx lastIdx sz |
res := String new.
res add: $# .
idx := self indexOf: $' startingAt: 1 .
(idx == 0 _or:[ idx > 1000000 ]) ifTrue:[
  res addAll: self .
  ]
ifFalse:[
  lastIdx := 1.
  [ idx == 0 ] whileFalse: [
    res addAll: (self copyFrom: lastIdx to: idx) ; add: $' .
    lastIdx := idx + 1 .
    idx := self indexOf: $' startingAt: lastIdx .
    ].
  sz := self size .
  lastIdx <= sz ifTrue: [
    res addAll: (self copyFrom: lastIdx to: sz)  .
    ].
  ].
^ res
%

category: '*squeak'
method: ProtoObject
tagSize
  ^ 0  "assume no dynamic instVars"
%
method: ProtoObject
asString
  ^ 'aProtoObject'
%
method: ProtoObject
class
  ^ Object _classOf: self 
%
run
ProtoObject removeSelector: #_class ifAbsent:[] .
 true
%

run
Class removeSelector: #migrateInstances:to:    "category *Monticello" .
true
%
commit
