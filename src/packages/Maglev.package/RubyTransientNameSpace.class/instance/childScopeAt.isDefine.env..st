as yet unclassified
childScopeAt: sym  isDefine: definBool env: eid 
    |  ns assoc  child |
  assoc := self resolveConstant: sym .
  assoc ifNotNil:[ child  := assoc _value ].  
  child ifNotNil:[ ^ child ]. 
  definBool ifFalse:[ ^ nil ].

  ns := self class new initializeChildOf: self name: sym .

  assoc := self resolveConstant: sym .
  assoc ifNotNil:[
    "We can have an already existing autoload association"
    assoc _valueNoAction ifNotNil:[ self error:'value already defined in child scope'].
    (assoc isCommitted and:[ persistCopy ~~ nil ]) ifTrue:[ 
	   self at: sym compilePut: ns  "must store to transient name space, Trac 585"
	] ifFalse:[
	   assoc _compileTimeValue: ns .
	].
  ] ifNil:[ 
    self at: sym compilePut: ns 
  ].
  TraceGlobals >=2 ifTrue:[ 
      GsFile gciLogServer:'initialized scope: ' , ns describe . 
  ].
  ^ ns

