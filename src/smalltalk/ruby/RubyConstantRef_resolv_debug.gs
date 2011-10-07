! debug version , see normal code in RubyConstantRef_resolv_debug.gs 

method: RubyConstantRef
resolveConst
  "invoked from generated code only"
<primitive: 790>    "prim fails if no cached association"
| sym lpSize idx assoc n mySize val prevAssoc envId firstCls rns hist parent firstSym |
lpSize := lexPathSize .  mySize := self size .
lpSize ifNil:[ 
   ArgumentTypeError signal:'left hand side of :: is not a class or module' 
].
idx := lpSize + 2.
sym := self at: idx  .  idx := idx + 1 .  firstSym := sym .
n := 1 .
envId := 1"__callerEnvId" .
hist := { } .     "uncomment for debugging "
[ n <= lpSize ] whileTrue:[ | aModule |  "search specified lex scopes"
  "for a Colon3 node, this will include root scope"
  aModule := self at: n .    n := n + 1  .
  hist add: { #lexScope .  aModule name . aModule . n . idx }. 
  aModule ifNotNil:[ 
    firstCls ifNil:[ 
      firstCls := aModule.
    ] .
    parent := aModule .
    rns := aModule nameSpace: envId .
    assoc := rns ifNotNil:[ rns resolveConstant: sym ].
    hist add: { #firstCls . aModule . sym . assoc } . 
  ].
  assoc ifNotNil:[
    prevAssoc := assoc .
    (assoc := assoc isDefinedForResolve: sym inClass: parent env: envId ) ifNotNil:[ 
      n := lpSize + 1 "terminate while loop" 
    ].
  ].
].
assoc ifNil:[ | cls errCls |  "search inheritance hierarchy"
  cls := self at: lpSize + 1 . "lexical self class in an instance_eval"
  errCls := cls .
  cls ifNil:[
    firstCls ifNil:[ 
      cls := Object . errCls := cls .
      hist add: { #firstCls_i . nil . Object } . 
    ] ifNotNil:[ 
      firstCls == Kernel ifTrue:[ cls := Object ]
                        ifFalse:[ cls := firstCls rubySuperclass: envId ].
      errCls := firstCls.
      hist add: { #firstCls_i . firstCls . cls }. 
    ].
  ].
  [ cls ~~ nil and:[ assoc == nil] ] whileTrue:[ "dynamic lookup"
    "probe both normal and virtual classes"
    parent := cls .
    (rns := cls nameSpace: envId) ifNotNil:[ 
       assoc := rns resolveConstant: sym .
       assoc ifNotNil:[
          prevAssoc := assoc .
          assoc := assoc isDefinedForResolve: sym inClass: parent env: envId .
       ].
    ]. 
    hist add: { #cls . cls . sym . assoc }. 
    cls := cls rubySuperclass: envId .
  ].
  assoc ifNil:[
    idx > mySize ifTrue:[ assoc := prevAssoc ].
    assoc ifNil:[ 
       (firstSym == #___X and:[SessionTemps current at:#TrapResolve otherwise: false]) ifTrue:[ hist pause ].
       assoc := self _constantMissing: sym in: errCls env: envId
    ].
  ].
].
[ idx <= mySize ] whileTrue:[ "evaluate second and subsequent :: terms"
  val := assoc _valueFor: sym inClass: parent env: envId .  "autoload can be triggered here"
  parent := val .
  assoc := val rubyConstAssociationAt: (sym := self at: idx) env: envId .
  hist add: { #second__ . val . sym . assoc }. 
  idx := idx + 1
].
val := assoc _valueFor: sym inClass: parent env: envId . "possible trigger of autoload, etc"

self setGlobalAssoc_noMarkDirty: assoc .
(firstSym == #___X and:[SessionTemps current at:#TrapResolve otherwise: false]) ifTrue:[ hist pause ]. 
^ val
%
