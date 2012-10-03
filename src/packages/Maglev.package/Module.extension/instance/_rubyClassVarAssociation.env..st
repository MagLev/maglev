*maglev-runtime
_rubyClassVarAssociation: aSymbol env: envId

| aCls ns assoc |
aCls := self .
[ aCls ~~ nil ] whileTrue:[ 
  (ns := aCls _classForRubyClassVar nameSpace: envId ) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	                  ^ assoc ] .
  ].
  aCls isRubySingletonClass ifTrue:[ ^ nil ].  "per Trac 654"
  aCls := aCls rubySuperclass: envId .
].
aCls := self virtualClass .   "handle a class which contains   extend aModule , Trac 445 "
[ aCls ~~ nil ] whileTrue:[
  (ns := aCls _classForRubyClassVar nameSpace: envId) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	                   ^ assoc ] .
  ].
  aCls := aCls rubySuperclass: envId
].
"(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
^ nil

