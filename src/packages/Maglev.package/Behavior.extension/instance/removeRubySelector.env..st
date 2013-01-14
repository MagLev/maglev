*maglev-runtime
removeRubySelector: selectorSymbol env: envId

" returns self "
 
 | md pmd tmeth pmeth  |
 md := self transientMethodDictForEnv: envId .
 md ifNotNil:[
   tmeth := md removeKey: selectorSymbol otherwise: nil .
 ].
 pmd := self persistentMethodDictForEnv: envId.
 pmd ifNotNil:[
   RubyCompilerState current persistenceMode ifTrue:[ 
     pmeth := pmd removeKey: selectorSymbol otherwise: nil .
     pmeth ifNotNil:[  
       self _codeChangedForEnv: envId.
     ].
     pmeth == tmeth ifTrue:[ pmeth := nil ].
   ] ifFalse:[
     (pmd includesKey: selectorSymbol) ifTrue:[
       md ifNil:[ md := self transientMethodDictForStoreEnv: envId ].
       md at: selectorSymbol put: 4 "method-removed transiently" .
       pmeth := 4 .
     ].
   ]
 ].
 tmeth ifNotNil:[
  self _refreshLookupCache: selectorSymbol oldMethod: tmeth  env: envId
 ].
 pmeth ifNotNil:[
   self _refreshLookupCache: selectorSymbol oldMethod: pmeth  env: envId
 ].
"TraceRubyMethodDicts ifTrue:[
     GsFile gciLogServer: self name, '  removeRubySelector: ' , 
    selectorSymbol printString ].
"

