*maglev-runtime
addRubyVirtualSuperclass: includedClass forMeta: forMetaBool env: envId

"If receiver does not already contain includedClass as a virtual superclass,
 insert a copy of includedClass into the superclass chain of the receiver.
 After the insert, the current superclass will be superclass of the copy, 
 and the receiver's superclass will be the copy .  The copy will be a
 ruby virtual class , and is the result."

  | aCls incCls list res startVirt vCls aModu includedVcls |
  (includedClass isKindOf: Module) ifFalse:[
    ArgumentTypeError signal:'argument is not a Module'
  ].
  includedClass isRubyVirtual ifTrue:[
    self error:'argument to addRubyVirtualSuperclass is a virtual class'.
  ].
  list := { includedClass } .
  incCls := includedClass rubySuperclass: envId .
  [ incCls ~~ Object ] whileTrue:[ | primary |
     (primary := incCls rubyPrimaryCopy) class == Module ifTrue:[
       list add: primary
     ].
     incCls := incCls rubySuperclass: envId 
  ].
  aModu := list at: 1 .
  (includedVcls := self _includedModule: aModu env: envId) ifNotNil:[
    vCls := includedVcls .
  ] ifNil:[
    vCls := self _rubyIncludeModule: aModu envId: envId .
  ].
  res := vCls  .
  2 to: list size do:[ :n | 
    aModu := list at: n .
    (includedVcls := self _includedModule: aModu env: envId) ifNotNil:[
      vCls := includedVcls . 
    ] ifNil:[
      vCls := vCls _rubyIncludeModule: aModu envId: envId .
    ].
  ].
  ^ res

