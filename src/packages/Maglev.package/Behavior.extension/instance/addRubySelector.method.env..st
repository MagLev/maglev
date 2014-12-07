*maglev-runtime
addRubySelector: selectorSymbol method: aGsMethod env: envId
  "aGsMethod may be a SmallInt Ruby protection override"
  | md pmd tmeth pmeth |
  md := self transientMethodDictForStoreEnv: envId .
  tmeth := md at: selectorSymbol otherwise: nil .
  md at: selectorSymbol put: aGsMethod .
  (RubyCompilerState current persistenceMode and:[ self _persistable]) ifTrue:[
     pmd := self persistentMethodDictForStoreEnv: envId .
     pmeth := pmd at: selectorSymbol otherwise: nil .
     pmeth == tmeth ifTrue:[ pmeth := nil ].
     pmd at: selectorSymbol put: aGsMethod .
     self _codeChangedForEnv: envId .
  ].
  tmeth ifNotNil:[ tmeth _isSmallInteger ifFalse:[
    self _refreshLookupCache: nil oldMethod: tmeth  env: envId "clear breakpoints"
  ]].
  self _refreshLookupCache: selectorSymbol oldMethod: pmeth   env: envId .

" selectorSymbol == #aa_class_method ifTrue:[ self pause ]."
 "TraceRubyMethodDicts ifTrue:[
    GsFile gciLogServer: self name, '  addRubySelector: ' , selectorSymbol printString ,
        ' methSel ', aGsMethod selector , '  methId:', aGsMethod asOop asString
  ]."

