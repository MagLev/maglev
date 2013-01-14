as yet unclassified
bootAddConstAssociation: aName env: eid 
  "only used in bootstrap, returns a persistent association"
  | assoc sym ns |
  RubyCompilerState current persistenceMode ifFalse:[
    self error:'bootAddConstAssociation: , expected persistenceMode==true' .
  ]. 
  ns := persistCopy ifNil:[ self ].
  sym := aName asSymbol. 
  assoc := ns _at: sym otherwise: nil . 
  assoc ifNil:[ | val | 
     assoc:= RubySymbolAssociation newWithKey: sym . 
     ns addAssociation: assoc .
     TraceGlobals >= 2 ifTrue:[
        GsFile gciLogServer:'added key ', sym , ' to  ' , self describe 
     ].
  ].
  ^ assoc

