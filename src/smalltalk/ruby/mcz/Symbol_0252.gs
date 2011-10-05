
set class Symbol
category: '*maglev-runtime'
method:
asRubyException
  "a ruby primitive"
  | assoc val envId | 
  assoc := Object rubyConstAssociationAtOrNil: self env: (envId := 1"__callerEnvId").
  assoc isDefined ifTrue:[
    (val := assoc _value ) _isExceptionClass ifTrue:[ ^ val ].
    ^ ArgumentTypeError signal:'expected an Exception or name of an Exception'
  ] ifFalse:[
    ^ Object @ruby1:const_missing: self 
  ]

%


set class Symbol
category: '*maglev-ast'
method:
irLiteralNode
	^ GsComLiteralNode new leaf: (GsComLitLeaf new symbolLiteral: self)

%


set class Symbol
category: '*maglev-ast'
method:
rubyLiteralClass 
	^ RubySymbolNode

%


set class Symbol
category: '*maglev-runtime'
method:
_inspect
  ^ ':''' , self asString , $' 

%

