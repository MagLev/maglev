
set class RubyArgsNode
category: 'parsetree'
classmethod:
restArgForTokens: tokens rest: rest opt: opt
	^ rest 
		ifNil: [-1] 
		ifNotNil: [tokens size + (opt ifNil: [0] ifNotNil: [opt list size])]

%


set class RubyArgsNode
category: '*maglev-runtime'
method:
add_arg: name_tok
  "Returns total number of fixed args"
  | args nod |
  (nod := RubyArgumentNode _basicNew) identifier: name_tok symval .
  (args := arguments ) ifNil:[
    (arguments := RubyListNode _basicNew) list: { nod } .
    ^ 1
  ] ifNotNil:[
    args _append: nod .
    ^ args size
  ].

%


set class RubyArgsNode
category: '*maglev-ast'
method:
add_block_arg: arg
   arg ifNil:[ ^ self "do nothing, opt_f_block_arg term is nil"].
   blockArgNode ifNotNil:[
     RubyParserM signalError: 'block arg already installed'
   ].
   arg class == RubyBlockArgNode ifTrue:[
     blockArgNode := arg
   ] ifFalse:[ "used to expect an RpNameToken, this path " 
     RubyParserM signalError: 'add_block_arg invalid argument'  
   ].
   ^ self

%


set class RubyArgsNode
category: '*maglev-ast'
method:
add_optional_arg: node
  node class == RubyBlockNode ifFalse:[
    RubyParserM signalError: 'add_optional_arg bad arg kind'
  ].
  optArgs ifNotNil:[
    RubyParserM signalError: 'optional_arg already assigned'
  ].
  optArgs := node.
  ^ self

%


set class RubyArgsNode
category: '*maglev-ast'
method:
add_star_arg: symArg
  | sym |
  (sym := symArg) _isSymbol ifFalse:[
    sym := sym symval "args is an RpNameToken"
  ].
  restArgNode ifNil:[
    restArgNode := (RubyArgumentNode _basicNew identifier: sym) .
  ] ifNotNil:[
    RubyParserM signalError: 'RubyArgsNode - star arg already installed' .
  ].
  ^ self

%


set class RubyArgsNode
category: 'converting'
method:
argNodes
	^ arguments

%


set class RubyArgsNode
category: 'as yet unclassified'
method:
argsDescrInt
  "Bits within the SmallInteger describing method defintion args 
     as built by buildMethodArgumentsOn: 
         16rFF  number of Args excluding  * and & args
       16rFF00  one-based number of first optional arg, or zero of none
      16r10000   one if has * arg 
     16r100000   one if has & arg "

  ^ argsDescrInt

%


set class RubyArgsNode
category: 'accessing'
method:
arguments

	 ^ arguments

%


set class RubyArgsNode
category: 'accessing'
method:
arguments: aListNode
	arguments := aListNode

%


set class RubyArgsNode
category: 'accessing'
method:
blockArgNode

	 ^ blockArgNode

%


set class RubyArgsNode
category: 'accessing'
method:
blockArgNode: aBlockArgNode
	blockArgNode := aBlockArgNode

%


set class RubyArgsNode
category: '*maglev-runtime'
method:
buildMethodArgumentsOn: irNode implicitBlk: hasBlockRef 
   "Returns the one-based arg number of last fixed non-optional argument"
      "ruby_selector_suffix dependent"
    | n myScop nRequired nFixed fixedCp descrInt optArgsBits optA suffix |
    n := 0.
    optArgsBits := 0 .
    descrInt := 0 .
    suffix := '#0__' copy .
    myScop := self currentScope .
    1 to: (nRequired := arguments size) do:[:m |
      irNode appendArg: (myScop argLeafAtIndex: m) .
    ].
    nFixed := nRequired .
    n := nRequired .
    (optA := optArgs) ifNotNil:[ | optSz |
      optSz := optA list size .
      optSz > 0 ifTrue:[
        descrInt := descrInt bitOr: (n + 1) << 8 . " one-based number of first optional arg"
        1 to: optSz do: [:m |
          optArgsBits := optArgsBits bitOr: ( 1 << n  ) ."zero based bitnum"
          n := n + 1 . 
          irNode appendArg: (myScop argLeafAtIndex: n) .
        ].
        nFixed := nFixed + optSz .
      ].
    ].
    fixedCp := 48 + nFixed . " $0 + "  "also enforced in RubyArgsNode::add_arg"
    fixedCp > 255 ifTrue:[ self error:'too many fixed args'].
    suffix codePointAt: 2 put: fixedCp .

    descrInt := descrInt bitOr: n .
    restArgNode ifNotNil:[
        descrInt := descrInt bitOr: 16r10000 .
        n := n + 1 .       suffix at: 3 put: $*  .
        irNode appendArg: (myScop argLeafAtIndex: n)
        ].
    blockArgNode ifNotNil:[ | baLeaf | 
        descrInt := descrInt bitOr: 16r100000 .  
        n := n + 1 .       suffix at: 4 put:  $&  .
        irNode appendArg: (myScop blockArgLeaf: (myScop argLeafAtIndex: n )).
    ] ifNil:[  hasBlockRef ifTrue:[  
           irNode appendArg: (myScop blockArgLeaf: nil )  .
           descrInt := descrInt bitOr: 16r100000 .
           suffix at: 4 put:  $& .
    ].].
    n > 255 ifTrue:[ self error:'too many (>256) args in method definition'].
    argsDescrInt := descrInt . 
    myScop ~~ self currentScope ifTrue:[ self error:'scope changed' ].  
    selectorSuffix := suffix .
    irNode  optArgsBits: optArgsBits .
    ^ nFixed

%


set class RubyArgsNode
category: 'parsetree-as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch, {self blockArgNode. self optArgs . self arguments. self restArgNode}

%


set class RubyArgsNode
category: '*maglev-runtime'
method:
ensureStarArg
  "for synthesizing *extra arg to a def of eval() ..."
  restArgNode ifNil:[
    restArgNode := (RubyArgumentNode _basicNew identifier: #extra ) .
  ].
  ^ self

%


set class RubyArgsNode
category: 'converting'
method:
extraArgs
	| i opts |
	i := 0.
	(opts := optArgs) ifNotNil: [i := i + opts list size].
	restArgNode ifNotNil: [i := i + 1].
	blockArgNode ifNotNil: [i := i + 1].
	^ i

%


set class RubyArgsNode
category: 'parsetree'
method:
isSameAs: other
	^ self restArg = other restArg

%


set class RubyArgsNode
category: 'accessing'
method:
optArgs

	 ^ optArgs

%


set class RubyArgsNode
category: 'accessing'
method:
optArgs: aListNode
	optArgs := aListNode

%


set class RubyArgsNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		printNode: arguments;
		printNode: optArgs;
		printNode: blockArgNode

%


set class RubyArgsNode
category: 'accessing'
method:
restArg

	 ^ restArg

%


set class RubyArgsNode
category: 'accessing'
method:
restArg: aNumber
	restArg := aNumber

%


set class RubyArgsNode
category: 'accessing'
method:
restArgNode

	 ^ restArgNode

%


set class RubyArgsNode
category: 'accessing'
method:
restArgNode: aArgumentNode
	restArgNode := aArgumentNode

%


set class RubyArgsNode
category: 'as yet unclassified'
method:
selectorSuffix
   ^ selectorSuffix

%


set class RubyArgsNode
category: 'converting'
method:
size
	| count |
	count := 0.
	arguments ifNotNil: [arguments do: [:ea | count := count + 1]].
	^ count

%


set class RubyArgsNode
category: 'parsetree-as yet unclassified'
method:
variableNames
	| names n |
	names := { } .
	(n := arguments) ifNotNil:	[ n list do:	[:ea |	names add: ea identifier]].
	(n := optArgs) ifNotNil:[ n list do:[:ea |	names add: ea name]].
	(n := restArgNode) ifNotNil: [names add: n identifier].
	(n := blockArgNode) ifNotNil: 	[names add: n name].
	^ names

%


set class RubyArgsNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
   | n args opts |  
    (args := arguments) ifNotNil:[ args walkWithScope: aScope].
    (opts := optArgs) ifNotNil:[ opts walkOptionalArgsLhs: aScope].
    (n := restArgNode) ifNotNil:[ aScope locationForName: n identifier].
    (n := blockArgNode) ifNotNil:[ (aScope addIncomingBlock: n name) ]. 
    aScope
        requiredArgs: ( args ifNil: [0] ifNotNil: [ args list size]);
        restArg: restArg.
    opts ifNotNil:[ opts walkOptionalArgsRhs: aScope].

%


set class RubyArgsNode
category: '*maglev-runtime'
method:
_inspect
  | res |
  res := '[:args ' copy .
  arguments ifNotNil:[ res addAll: arguments _inspect_list ].
  optArgs ifNotNil:[ res addAll:', <optionalArgs> '; addAll: optArgs _inspect ].
  restArgNode ifNotNil:[ res addAll: ', :*' , restArgNode identifier  ].
  blockArgNode ifNotNil:[ :b | res addAll:', '; addAll: b _inspect ].
  res add: $]  .
  ^ res

%

