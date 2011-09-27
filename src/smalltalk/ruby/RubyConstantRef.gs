!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================

set class RubyConstantRef
removeallmethods
removeallclassmethods

category: 'Documentation'
classmethod:
comment
^ 'RubyConstantRef is a subclass of Array .
   Instances of RubyConstantRef are used as literals in methods
   to implement access to a Ruby constant. 

   instVars
      lexPathSize : a SmallInteger , 0 means dynamic ArgumentTypeError

      globalAssoc : cached instance of a RubySymbolAssociation, or nil

      varying instVars :
       self[1..lexPathSize-1] is lexicalPath in order to be searched, 
          elements are instances of Module ,
                   or Arrays of size 1 , referencing a Module . 
       self[lexPathSize] is top-level scope, i.e.  Object .
       self[lexPathSize+1] is nil, or class of self for an instance eval
       self[lexPathSize+2 .. self size] are Symbols from the path terms
           as they appear in Ruby source code.
           For a reference   x = A::B ,  self[self size] == #B
   '
%

category: 'Ruby support'
method:
setGlobalAssoc_noMarkDirty: anAssoc

"stores anAssoc into the globalAssoc instVar of receiver,
  without marking receiver dirty.  Sets noStub bit in receiver."

<primitive: 791>
self size > 2000 ifTrue:[ self error:'too many terms in Constant path'].
self _primitiveFailed: #setGlobalAssoc_noMarkDirty: args: { anAssoc }
%

!  resolveInContext: in separate file
input $upgradeDir/ruby/RubyConstantRef_resolv.gs

method:
resolveGlobalVarAssoc
  "called from smalltalk code only.
   returns nil or the cached association for the global variable. "

<primitive: 862> 
self _primitiveFailed: #resolveGlobalVarAssoc
%

method: RubyConstantRef
definedQconst
  "called from generated code"
  ^ self definedQconst: 1"__callerEnvId"
%

set class RubyConstantRef 
method: 
definedQconst: envId
<primitive: 841>  "primitive does  globalAssoc:=nil if
        cached globalAssoc no longer valid due to serial number increment.
        primitive always fails."
| sym lpSize idx assoc n mySize aModule prevAssoc rns firstCls parent |
(assoc := globalAssoc) ifNotNil:[
  ^ assoc definedQ "use cached value from previous resolve"
].
lpSize := lexPathSize .     mySize := self size .
lpSize ifNil:[
   ArgumentTypeError signal:'left hand side of :: is not a class or module'
].
idx := lpSize + 2.
sym := self at: idx  .  idx := idx + 1 .
n := 1 .
[ n <= lpSize ] whileTrue:[ "search specified lex scopes "
  aModule := self at: n .    n := n + 1 .
  aModule ifNotNil:[
    firstCls ifNil:[ firstCls := aModule] .
    parent := aModule .
    rns := aModule nameSpace: envId .
    rns ifNotNil:[ assoc := rns resolveConstant: sym ].
  ].
  assoc ifNotNil:[
    prevAssoc := assoc .
    assoc isDefined ifNotNil:[ n := lpSize + 1 "terminate while loop" ]
                       ifNil:[ assoc := nil "keep searching"].
  ].
].
assoc ifNil:[ | cls |  "search inheritance hierarchy"
  cls := self at: lpSize + 1 . "instanceEvalSelfCls"
  cls ifNil:[
    firstCls ifNil:[
      cls := Object
    ] ifNotNil:[
      firstCls == Kernel ifTrue:[ cls := Object]
                        ifFalse:[ cls := firstCls rubySuperclass: envId ].
    ].
  ].
  [ cls ~~ nil and:[ assoc == nil] ] whileTrue:[ "dynamic lookup"
     "probe both normal and virtual classes"
     parent := cls .
     (rns := cls nameSpace: envId) ifNotNil:[
        assoc := rns resolveConstant: sym .
        assoc ifNotNil:[
         prevAssoc := assoc .
         assoc isDefined ifNil:[ assoc := nil ].
        ].
     ].
     cls := cls rubySuperclass: envId .
  ].
  assoc ifNil:[
    idx > mySize ifTrue:[ assoc := prevAssoc ].
    assoc ifNil:[ ^ nil ].
  ].
].
[ idx <= mySize ] whileTrue:[  |val |
  "evaluate second and subsequent :: terms"
  assoc isDefined ifNil:[ ^ nil ].
  (val := assoc _valueNoAction) ifNil: [ 
    "We're dealing with a RubyAutoloadAssociation, Trac 937"
    val := assoc _valueFor: sym inClass: parent env: envId  . "do autoload"
    assoc isDefined ifNil: [ ^ nil ] .
    val := assoc _valueNoAction .
  ].
  parent := val .
  assoc := val rubyConstAssociationAtOrNil: (self at: idx) env: envId .
  assoc ifNil:[ ^ nil ].
  idx := idx + 1
].
self setGlobalAssoc_noMarkDirty: assoc .
^ assoc definedQ
%

