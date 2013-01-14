*maglev-runtime
_rubyClassVarNameSpaceForStore: aSymbol env: envId
  "returns a transient name space or nil "
| aCls cvCls ns tns assoc |
aCls := self .
[ aCls ~~ nil ] whileTrue:[ 
  (ns := (cvCls := aCls _classForRubyClassVar) nameSpace: envId ) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ tns := cvCls transientNameSpaceForStore: envId .
	   "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	   ^ tns  ] .
  ].
  aCls := aCls rubySuperclass: envId
].
aCls := self virtualClass .   "handle a class which contains   extend aModule , Trac 445 "
[ aCls ~~ nil ] whileTrue:[
  (ns := (cvCls := aCls _classForRubyClassVar) nameSpace: envId) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ tns :=  cvCls transientNameSpaceForStore: envId .
	    "(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
	   ^ tns ] .
  ].
  aCls := aCls rubySuperclass: envId
].
tns := self _classForRubyClassVar transientNameSpaceForStore: envId .
"(SessionTemps current at:#TrapCV otherwise: false) ifTrue:[ self pause ]."
^ tns 

