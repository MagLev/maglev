
doit
RubyArrayNode subclass: 'RubyRpCallArgs'
	instVarNames: #( iterNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRpCallArgs
removeallmethods
removeallclassmethods

set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: v
  | res |
  (res := self _basicNew) list: { } ; append_arg: v .
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: a all: ary b: b blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; _appendAll: b ;
    append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: a all: ary b: b splat: splat blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; _appendAll: b ;
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: a b: b blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; append_arg: b ; append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: a b: b splat: splat blk: blk 
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; append_arg: b ; 
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: a blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_a: a splat: splat blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; 
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs class
category: '*maglev-ast'
method:
s_splat: splat blk: blk 
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
append_arg: v
  "returns receiver"
  | vcls |
  v ifNil:[ RubyParserM signalError:'internal error, append_arg, arg is nil' ].
  vcls := v class .
  vcls == RubyBlockPassNode ifTrue:[
    RubyParserM signalError:'internal error, append_arg, must use append_blk_arg'.
  ].
  v is_void_result ifTrue:[
    RubyParserM signalError:'void value expression'
  ].
  self _append: v .
  ^ self

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
append_arg: arg blkArg: blk
  "returns receiver"
  ^ self append_arg: arg ; append_blk_arg: blk .

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
append_arg: arg splatArg: s blkArg: blk
  "returns receiver"
  ^ self append_arg: arg ;
         append_arg: (RubySplatNode s_a: s) ;
         append_blk_arg: blk .

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
append_blk_arg: node
  "returns receiver"
  iterNode ifNil:[
    (node == nil or:[ node class == RubyBlockPassNode]) ifTrue:[
      iterNode := node
    ] ifFalse:[
      RubyParserM signalError:'append_blk_arg, invalid block argument'
    ].
  ] ifNotNil:[
    RubyParserM signalError:'append_blk_arg, block argument already present'
  ].
  ^ self

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
append_splatArg: splat 
  "returns receiver"
  ^ self append_arg: (RubySplatNode s_a: splat) 

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
append_splatArg: splat blk: blk
  "returns receiver"
  ^ self append_arg: (RubySplatNode s_a: splat) ;
       append_blk_arg: blk .

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
as_yield_args
  | lst nod |
  ((lst := list) size == 1 and:[ iterNode == nil ]) ifTrue:[
     (nod := lst at: 1) class == RubySplatNode ifTrue:[
       ^ nod " for   yield *one_arg"
     ]
  ].
  ^ self

%


set class RubyRpCallArgs
category: 'as yet unclassified'
method:
getClearIter
  | res |
  res := iterNode . 
  res ifNotNil:[ iterNode := nil ].
  ^ res

%


set class RubyRpCallArgs
category: 'as yet unclassified'
method:
hasRestArg
  | lst sz | 
  lst := list.
  (sz := lst size) >= 1 ifTrue:[ 
	  ^ ( lst at: sz)  isSplatNode
  ].
  ^ false

%


set class RubyRpCallArgs
category: 'as yet unclassified'
method:
irNode
      "ruby_selector_suffix dependent"
  | ary lst sz  last  |
  lst := list .
  sz := lst size .
  sz <= 1 ifTrue:[
    (sz == 1 and:[ (last := lst at: sz) isSplatNode]) ifTrue:[
       ary := last irNode .
    ] ifFalse:[
       ary := GsComArrayBuilderNode new.
       1 to: sz do:[ :n | ary appendElement: ( lst at: n) irNode ].
    ]
  ]  ifFalse: [ "2 or more"
    ary := GsComArrayBuilderNode new. 
    1 to: sz  - 1 do:[:n |
      ary appendElement: ( lst at: n) irNode
    ].
    last := lst at: sz .
    last isSplatNode ifTrue:[ | snd |
      self ir: ary .
      (snd := GsComSendNode new)
         rcvr: ary ;
         rubySelector: #'__add_arguments#1__' ;
         appendArgument: last irArgNode .
      self ir: snd .
      ^ snd   
    ] ifFalse:[
      ary appendElement: last irNode
    ].
  ].
  self ir: ary.
  ^ ary

%


set class RubyRpCallArgs
category: '*maglev-runtime'
method:
irYieldArgs
      "ruby_selector_suffix dependent"
  | lst sz | 
  lst := list.
  sz := lst size .
  sz >= 1 ifTrue:[ 
    (lst at: sz)  isSplatNode ifTrue:[ "inline hasRestArg"
       ^ { #'call#0*_' .  self irNode }
    ].
    sz > 3 ifTrue:[  | ary | 
      ^ { #'call#1__' . self irNode  }
    ].
  ].
  ^ nil

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
iter
  ^ iterNode

%


set class RubyRpCallArgs
category: '*maglev-ast'
method:
iterNode: aNode
  iterNode := aNode

%


set class RubyRpCallArgs
category: 'as yet unclassified'
method:
rpCallArgsList
  ^ list

%


set class RubyRpCallArgs
category: '*maglev-runtime'
method:
_inspect
  | res |
  res := '[:callargs ' copy .
  list ifNotNil:[ res add: ', ' ; add: self _inspect_list ].
  iterNode ifNotNil:[ res add: ', <iterNode>' ; add: iterNode _inspect ].
  res add: $] .
  ^ res

%

