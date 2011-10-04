
doit
RubyAbstractLiteralNode subclass: 'RubyRegexpNode'
	instVarNames: #( options value regexpLit)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRegexpNode
removeallmethods
removeallclassmethods

set class RubyRegexpNode class
category: '*maglev-runtime'
method:
comment

"Creation of a RubyRegexpNode starts from String>>rubyLiteralClass ."

%


set class RubyRegexpNode class
category: 'parsetree'
method:
optionsFromString: aString
       "result must agree with options arg to om::RegexCompile in regexprim.c"
  | optsInt |
  optsInt := 0 .
  1 to: aString size do:[:j | | ch ofs sym |
    ch := aString at: j .
    (ofs :=  #( $i $o $m $x ) indexOfIdentical: ch) ~~ 0 ifTrue:[
       "literal array must agree with   oniguruma.h ."
       " can't find any support for SUBSTITUTE_ONCE in oniguruma.h"
       sym := #( #IGNORECASE nil #MULTILINE #EXTEND ) at: ofs .
       sym == nil ifTrue:[ self error: 'Regexp option ', ch ,' not supported'].
       optsInt := optsInt bitOr: (Regexp classVarAt: sym) .
    ].
    (ofs := #( $n $e $s $u ) indexOfIdentical: ch) ~~ 0 ifTrue:[
       "literal array of KCODE_ must agree with C defs in regexprim.hf "
       sym := #( #KCODE_NONE #KCODE_EUC #KCODE_SJIS #KCODE_UTF8 ) at: ofs .
       optsInt := optsInt bitOr: ((Regexp classVarAt: sym) bitShift: 32) .
    ].
  ].
  ^ optsInt

%


set class RubyRegexpNode
category: '*maglev-ast'
method:
as_cond
  ^ RubyMatchZeroNode s_a: self

%


set class RubyRegexpNode
category: '*maglev-runtime'
method:
irLeaf
  | lit |
  (lit := regexpLit) ifNil:[
     lit := value string asRegexpWithOptions: options .
  ].
  ^ GsComLitLeaf new methodLiteral: lit 

%


set class RubyRegexpNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ (super isSameAs: other) and: [self value = other value and: [self options = other options]]

%


set class RubyRegexpNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyRegexpNode
category: 'accessing'
method:
options

	 ^ options

%


set class RubyRegexpNode
category: 'accessing'
method:
options: aNumber
	options := aNumber

%


set class RubyRegexpNode
category: '*maglev-runtime'
method:
regexpLit: aRegexp
  regexpLit := aRegexp .
   ^ self

%


set class RubyRegexpNode
category: 'converting'
method:
selector
	^ #asRegexpWithOptions:

%


set class RubyRegexpNode
category: 'accessing'
method:
value: aByteList
	value := aByteList

%


set class RubyRegexpNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:regex, ', (regexpLit @ruby1:inspect) , $]

%


set class RubyRegexpNode
category: '*maglev-ast'
method:
_value
     ^ value

%


set class RubyRegexpNode
category: '*maglev-ast'
method:
_value: aByteList
  value := aByteList

%

