
set class RubySymbolAssociation
category: 'as yet unclassified'
classmethod:
newWithKey: aKey

    "         aKey == #'$<' ifTrue:[  self pause ]."
  ^ super new key: aKey

%


set class RubySymbolAssociation
category: '*maglev-runtime'
method:
definedQ

  isDefined ifNotNil:[ ^ 'constant' copy ].
  ^ nil 

%


set class RubySymbolAssociation
category: 'as yet unclassified'
method:
irDefinedQNode
  "should only be used when generating bootstrap code"
  | node |
  (node := GsComSendNode new)
     stSelector:  #definedQ  ;
     rcvr: ( GsComLiteralNode newObject: self )  .
   ^ node 

%


set class RubySymbolAssociation
category: 'as yet unclassified'
method:
isDefined
  ^ isDefined

%


set class RubySymbolAssociation
category: '*maglev-runtime'
method:
isDefinedForResolve: aSymbol inClass: aClass env: envId 
  "returns self or nil "
  isDefined ifNotNil:[ ^ self ].
  ^ nil

%


set class RubySymbolAssociation
category: 'as yet unclassified'
method:
value: aVal
  self error:'should not use, use _value: or _compileTimeValue: '

%


set class RubySymbolAssociation
category: '*maglev-runtime'
method:
_classFor: aSymbol inModule: aModule env: envId

  "no const_missing if not defined"
  ^ self

%


set class RubySymbolAssociation
category: 'as yet unclassified'
method:
_compileTimeValue: aVal
  "do not set isDefined"
           "key == #Builder ifTrue:[ self pause ]. "
  value := aVal .

%


set class RubySymbolAssociation
category: '*maglev-runtime'
method:
_freezeConstant
  self immediateInvariant

%


set class RubySymbolAssociation
category: 'as yet unclassified'
method:
_value: aVal
  "returns aVal"
          " key == #Builder  ifTrue:[ self pause ].  "
  value := aVal . 
  isDefined := true .  "causes Ruby   defined?  operator to  return true"
  ^ aVal

%


set class RubySymbolAssociation
category: '*maglev-runtime'
method:
_valueFor: aSymbol inClass: aClass env: envId 
  
  isDefined ifNil:[ 
      ^  aClass @ruby1:const_missing: aSymbol 
  ].
  ^ value

%


set class RubySymbolAssociation
category: 'as yet unclassified'
method:
_valueNoAction
  ^ value

%

