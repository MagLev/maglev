
set class RubyParserM
category: '*maglev-ast'
classmethod:
appendTo: arrayNode evstr2dstr: val
  | v |
  (v := val) class == RubyEvStrNode ifTrue:[
     v := RubyDStrNode s_a: { RubyStrNode s_a: String new  .  v } 
  ].
  arrayNode _append: v .
  ^ arrayNode

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
backref_error: aNode
  self signalError: 'Cannot set variable  ' , aNode backRefErrorString

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
block_append: head tail: tail
  ".y file checks for head/tail nil"
  | hd |
  hd := head kbegin_value "remove_begin" .
  hd class == RubyBlockNode ifFalse:[
    hd := RubyBlockNode s_list: { hd } .
  ].
  hd append_to_block: tail .
  ^ hd   

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
check_selector: selTok
   " returns selector symbol , guards against malformed elsif"
  | sel |
  sel := selTok symval .
  (sel at: 1) == $e ifTrue:[ 
    " guard against malformed   elsif"
    (sel == #elseif or:[ sel == #elif]) ifTrue:[  
      " disallow the other forms of elsif common in script languages"
      self signalError: 'malformed elsif, found ', sel .
    ] ifFalse:[
      (sel at:1 equals: 'else') ifTrue:[
        self signalWarning: 'possible malformed elsif, found ', sel .
  ] ] ].
  ^ sel

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
get_match_node: lhs rhs: rhs ofs: srcOfs
  | res |
  lhs ifNotNil:[ | l_cls |
    l_cls := lhs class .
    l_cls == RubyDRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: lhs b: rhs )
        position: srcOfs .
      ^ res
    ].
    l_cls == RubyRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: lhs b: rhs )
        position: srcOfs .
      ^ res
    ].
  ].
  rhs ifNotNil:[ | r_cls |
    r_cls := rhs class .
    r_cls == RubyDRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: rhs b: lhs) " we use Match2Node for :match3"
         position: srcOfs .
      ^ res .
    ].
    r_cls == RubyRegexpNode ifTrue:[
      (res := RubyMatch2Node s_a: rhs  b: lhs)
               position: srcOfs .
      ^ res .
    ].
  ].
  res := RpNameToken new: #'=~' position: srcOfs .
  ^ self new_call_1: lhs  sel: res arg: rhs  .

%


set class RubyParserM
category: '*maglev-runtime'
classmethod:
initializeClassRefs
    RubyFCallNode initialize .
    RubyVCallNode initialize .
    RubyCallNode initialize .

%


set class RubyParserM
category: '*maglev-runtime'
classmethod:
initializeParser
    RubyNameSpace initialize .
    RubyStaticScope initialize .
    self initializeClassRefs .

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
list_append: list item: item
  | arrayCls lst |
  arrayCls := RubyRpCallArgs .
  (lst := list) ifNil:[
    ^ arrayCls _new: item 
  ].
  lst class == arrayCls ifFalse:[
    lst := arrayCls _new: lst
  ].
  lst _append: item .
  ^ lst

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
list_prepend: list item: item
  | arrayCls |
  arrayCls := RubyRpCallArgs .
  list class == arrayCls ifTrue:[
    list _prepend: item .
    ^ list
  ].
  ^  arrayCls s_a: item b: list

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
literal_concat: headArg tail: tailArg
  ".y file checks for head/tail nil"
  | htype ttype head tail |
  htype := (head := headArg) str_dstr_evstr_kind . " 0,1,2  or nil"
  ttype := (tail := tailArg) str_dstr_evstr_kind .
  htype == 2 ifTrue:[
    head := RubyDStrNode s_a: { RubyStrNode s_a: String new .  head } 
  ].
  ttype == 0 ifTrue:[
    htype == 0 ifTrue:[
      " append tail's String to head's String"
      head appendString: tail strNodeValue 
    ] ifFalse:[
      (htype == 1 and:[ head size == 1]) ifTrue:[
        "head is a dstr with a StrNode and no  further list elements"
        head appendToHeadString: tail strNodeValue .
      ] ifFalse:[
        "htype == :dstr , or was :evstr changed to :dstr above"
        head appendToList: tail 
    ] ] ] ifFalse:[
  ttype == 1 ifTrue:[
    htype == 0 ifTrue:[
      tail appendToHeadString: head strNodeValue .
      head := tail
    ] ifFalse:[ | t_list t_list_head | "tail and head both  :dstr"
      t_list := tail dstrList .
      t_list_head := t_list atOrNil: 1 .
      t_list_head strNodeValue = '' ifTrue:[
        t_list removeFrom: 1 to: 1 .
      ].
      head dstrList addAll: t_list
    ] ] ifFalse:[
  ttype == 2 ifTrue:[  "tail is EvStrNode"
    htype == 0 ifTrue:[ " head is StrNode" | t_body |
      t_body := tail evStrBody .
      t_body class == RubyStrNode ifTrue:[
    head appendString: t_body strNodeValue .
      ] ifFalse:[ | dstr |
        head := RubyDStrNode s_a:{ head . tail } .
      ]
    ] ifFalse:[ " head is a DStrNode"
      head dstrList add: tail 
    ]
  ] ifFalse:[
    self signalError: 'literal_concat unknown type' 
  ] ] ] .
  ^ head

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
logop: cls left: left_arg right: right
  | left |
  left := left_arg kbegin_value "remove_begin" .
  (left class == cls and:[ left paren == nil "not left.paren"]) ifTrue:[ 
    | node second |
    node := left .
    [  second := node secondNode .
       second class == cls and:[ second paren == nil ]
    ] whileTrue:[
       node := second
    ].
    node secondNode: ( cls s_a: second b: right ) .
    ^ left
  ].
  left is_void_result ifTrue:[
    self signalError: 'void value expression'
  ].
  ^ cls s_a: left b: right

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
masgn_append_arg: lhs right: rhs
   ^ lhs masgn_append_arg: rhs kbegin_value 

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
masgn_append_mrhs: lhs right: rhs
   ^ lhs append_mrhs: rhs kbegin_value

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_aref: rcv args: refArgs ofs: srcOfs
  | args res |
  args := refArgs ifNil:[ RubyRpCallArgs _new ].
  (res := RubyCallNode s_a: rcv b: #'[]' c: args )  position: srcOfs .
  ^ res

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_call: recv sel: selTok arg: args 
  | arg_cls cArgs result |
  args ifNil:[  " zero args"
    ^ self new_vcall: recv sel: selTok
  ].
  arg_cls := args class .
  arg_cls == RubyRpCallArgs ifTrue:[
    args size == 0 ifTrue:[ 
      ^ self new_vcall: recv sel: selTok
    ].
    cArgs := args
  ] ifFalse:[
    arg_cls == RubyBlockPassNode ifTrue:[
      result := (recv class ==  RubySelfNode ifTrue:[ RubyFCallNode] ifFalse:[ RubyCallNode])
                    s_a: recv b: selTok symval c: nil .
      result iterNode: args ; position: selTok src_offset .
      ^ result  
    ] ifFalse:[
      cArgs := RubyRpCallArgs _new:  args .
  ]].
  result := ( recv class == RubySelfNode ifTrue:[ RubyFCallNode] ifFalse:[ RubyCallNode])
                 s_a: recv b: selTok symval c: cArgs  .
  result position: selTok src_offset .
  ^ result

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_call_1: recv sel: selTok arg: argone
  "argone should never be a RubyBlockPassNode"
  | cArgs result |
  argone ifNil:[ 
    self signalError: 'internal_error new_call_1 unexpected nil arg'.
  ].
  (cArgs := RubyRpCallArgs _basicNew ) list: { argone } .
  result := ( recv class == RubySelfNode ifTrue:[ RubyFCallNode] ifFalse:[ RubyCallNode])
                 s_a: recv b: selTok symval c: cArgs  .
  result position: selTok src_offset .
  ^ result

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_call_braceBlock: rcv sel: selTok args: args blkArg: blkArg
  | res |
  res := self new_call: rcv sel: selTok arg: args  .
  blkArg ifNotNil:[
    res class == RubyBlockPassNode ifTrue:[
      self signalError: 'both block arg and actual block given'.
    ].
    blkArg callNode: res .
    res := blkArg
  ].
  ^ res

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_defn: nameTok args: args body: bodyArg ofs: srcOfs 
     startLine: lineNum endOfs: endOfs
  | body result |
  bodyArg ifNil:[
    body := RubyBlockNode _basicNew 
  ] ifNotNil:[
    bodyArg class == RubyBlockNode ifTrue:[
      body := bodyArg
    ] ifFalse:[
      (body := RubyBlockNode _basicNew) list: {  bodyArg }
  ]].
  (result := RubyDefnNode s_a: nameTok symval b: args c: body )
    position:  srcOfs ; 
    startLine: lineNum endOffset: endOfs .
  ^ result

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_defs: rcvr name: nameTok args: args body: bodyArg ofs: srcOfs 
    startLine: lineNum endOfs: endOfs 
  | body result |
  bodyArg ifNil:[
    body := RubyBlockNode _basicNew
  ] ifNotNil:[
    bodyArg class == RubyBlockNode ifTrue:[
      body := bodyArg .
    ] ifFalse:[
      (body := RubyBlockNode _basicNew) list: { bodyArg }
  ]].
  (result := RubyDefsNode s_a: nameTok symval b: args c: body )
     receiverNode: rcvr ; position: nameTok src_offset ; 
     startLine: lineNum endOffset: endOfs .
  ^ result

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_dsym:  val
  | v_cls |
  v_cls := val class .
  v_cls == RubyDStrNode ifTrue:[ ^ val asDSymbolNode ]   .
  v_cls == RubyStrNode ifTrue:[ | str |
    str := val strNodeValue .
    ^ RubySymbolNode _basicNew  name: ( self string_to_symbol: str )
  ].
  v_cls == RubyEvStrNode ifTrue:[
    ^ RubyDSymbolNode _basicNew list: { RubyStrNode _basicNew _value: '' . val }
  ].
  self signalError: 'internal_error, unimplemented dsym conversion'.
  ^ nil

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_evstr: val
  | knd |
  val ifNil:[
    ^ RubyEvStrNode _basicNew "body left as nil" 
  ].
  knd := val str_dstr_evstr_kind . "MNU here if 'unknown rescue body' "
  knd ifNil:[
    ^ RubyEvStrNode _basicNew body: val .
  ].
  ^ val "val is one of  :str :dstr: evstr"

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_fcall: selTok arg: arg
  "receiver is implicit self"
  | result arg_cls sym |
  arg ifNil:[
    ^ self new_vcall: RubySelfNode _basicNew sel: selTok
  ].
  arg_cls := arg class .
  arg_cls == RubyRpCallArgs ifTrue:[
    arg size == 0 ifTrue:[
      ^ self new_vcall: RubySelfNode _basicNew sel: selTok
    ].
    sym := self check_selector: selTok .
    result := RubyFCallNode s_a: RubySelfNode _basicNew b: sym c: arg .
  ] ifFalse:[
    arg_cls == RubyBlockPassNode  ifTrue:[
      sym := self check_selector: selTok  .
      result := RubyFCallNode s_a: RubySelfNode _basicNew b: sym c: nil .
      result iterNode: arg 
    ] ifFalse:[ | cArgs |
      sym := self check_selector: selTok  .
      (cArgs := RubyRpCallArgs _basicNew) list: { arg } .
      result := RubyFCallNode s_a: RubySelfNode _basicNew b: sym c: cArgs 
  ]].
  result position: selTok src_offset .
  ^ result

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_fcall_braceBlock: selTok args: args blkArg: blkArg
  | res |
  res := self new_call: RubySelfNode _basicNew sel: selTok arg: args .
  blkArg ifNotNil:[
    res class == RubyBlockPassNode ifTrue:[
      self signalError: 'both block arg and actual block given'.
    ].
    blkArg callNode: res .
    res := blkArg
  ].
  ^ res

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_if: condArg t: trueBlock f: falseBlock ofs: srcOfs
  | c res |
  c := condArg as_cond .
  c class == RubyNotNode ifTrue:[ 
    res := RubyIfNode s_a: c conditionNode b: falseBlock c: trueBlock 
  ] ifFalse:[
    res := RubyIfNode s_a: c b: trueBlock c: falseBlock .
  ].
  ^ res position: srcOfs

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_op_asgn: lhs sel: asgnSel arg: argNode
   | nLhs lhsAcc asgn_op arg res aCall |
   lhsAcc := lhs as_accessor .
   arg := argNode  kbegin_value .
   asgn_op := asgnSel symval .
   asgn_op == #'||' ifTrue:[
     nLhs :=  lhs node_assign_set_rhs: arg .
     nLhs == lhs ifFalse:[  self error: 'become needed in new_op_asgn'].
     (res := RubyOpAsgnOrNode _basicNew )
          firstNode: lhsAcc; secondNode: nLhs ; position: asgnSel src_offset .
     ^ res
   ].
   asgn_op == #'&&' ifTrue:[
     nLhs :=  lhs node_assign_set_rhs: arg .
     nLhs == lhs ifFalse:[  self error: 'become needed in new_op_asgn'].
     (res := RubyOpAsgnAndNode _basicNew )
          firstNode: lhsAcc; secondNode: nLhs ; position: asgnSel src_offset .
     ^ res
   ].
   aCall := self new_call_1: lhsAcc sel: asgnSel arg:  arg .
   nLhs := lhs node_assign_set_rhs: aCall .
   nLhs == lhs ifFalse:[  self error: 'become needed in new_op_asgn'].
   ^ lhs 

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_parasgn: lhs ofs: srcOfs comma: trailingCommaBool
   lhs ifNil:[ 
     self signalError: 'lhs is nil in new_parasgn'
   ].
   ^ RubyParAsgnRpNode s_a: lhs ofs: srcOfs comma: trailingCommaBool

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_regexp: arg options: optionsSi
  | have_once arg_cls node options |
  have_once := false .
  options := optionsSi .
  (options bitAnd: 16r2000) ~~ 0 ifTrue:[ 
     have_once := true .
     options := options bitAnd:(16r2000 bitInvert).
  ].
  arg_cls := arg class .
  arg_cls == RubyStrNode ifTrue:[ | node |
    "simple regexp, don't care about have_once because no substitutions"
    [ | str |
      str := arg strNodeValue .
      (node := RubyRegexpNode _basicNew) regexpLit: ( Regexp new: str options: options lang: nil).
    ] on: RegexpError do:[:ex | 
      self signalError:'Regexp error, ' , ex messageText  .
    ].
    ^ node
  ].
  arg ifNil:[
    ^ RubyRegexpNode _basicNew regexpLit: ( Regexp new: '' options: options lang: nil)
  ].
  node := (have_once ifTrue:[ RubyDRegexpOnceNode] ifFalse:[ RubyDRegexpNode]) _basicNew .
  arg_cls == RubyDStrNode ifTrue:[
    node list: arg list .
  ] ifFalse:[
    node list: { RubyStrNode _basicNew _value: '' . arg }
  ].
  node options: options .
  ^ node

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_string: val
  val class == RubyEvStrNode ifTrue:[
    ^ RubyDStrNode s_a: { val body  }
  ].
  ^ val

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_super: args ofs: srcOfs
  | aryCls argsCls |
  aryCls := RubyRpCallArgs .
  args ifNil:[
    ^ RubySuperNode _basicNew argsNode: aryCls _new  ; position: srcOfs 
  ].
  (argsCls := args class) == RubyBlockPassNode ifTrue:[ 
    ^ RubySuperNode _basicNew iterNode: args ; position: srcOfs
  ].
  argsCls == aryCls ifTrue:[
    ^ RubySuperNode _basicNew argsNode: args ; position: srcOfs
  ].
  self signalError: 'internal_error: new_super, invalid args'.
  ^ nil

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_undef: symNode ofs: srcOfs
  ^ RubyUndefNode _basicNew name: symNode symNodeValue ; position: srcOfs

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_until: block expr: exprArg ofs: srcOfs
  | expr |
  exprArg class == RubyNotNode ifTrue:[
     expr := exprArg conditionNode
  ] ifFalse:[
     expr := RubyNotNode s_a: exprArg .
  ].
  expr position: srcOfs .
  ^ self new_while: block expr: expr ofs: srcOfs

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_vcall: rcv sel: selTok
  | result |
  ( result := RubyVCallNode s_a: rcv b: (self check_selector: selTok))
      position: selTok src_offset .
  ^  result

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_while: blockArg expr: exprArg ofs: srcOfs
  | preBool expr res block |
  preBool := true . 
  (block := blockArg) class == RubyBeginNode ifTrue:[
     block := block kbegin_value .
     preBool := false
  ].
  expr := exprArg kbegin_value as_cond .
  expr class == RubyNotNode ifTrue:[
    res := RubyUntilNode s_forRp: expr conditionNode block: block bool: preBool
  ] ifFalse:[
    res := RubyWhileNode s_forRp: expr block: block bool: preBool
  ]. 
  ^ res position: srcOfs

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_xstring: str
  | knd |
  str ifNil:[
    ^ RubyXStrNode _basicNew _value: '' .
  ].
  knd := str str_dstr_evstr_kind .
  knd == 0 ifTrue:[
    ^ RubyXStrNode _basicNew _value: str strNodeValue 
  ].
  knd == 1 ifTrue:[
    ^ RubyDXStrNode _basicNew list: str list 
  ].
  ^ RubyDXStrNode _basicNew list: { RubyStrNode _basicNew _value: '' . str } 

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
new_yield: args ofs: srcOfs
  | cArgs |
  args ifNil:[ 
    cArgs := RubyRpCallArgs _new 
  ] ifNotNil:[ | args_cls |
    (args_cls := args class) == RubyRpCallArgs ifTrue:[
      cArgs := args as_yield_args
    ] ifFalse:[ 
      args_cls == RubyBlockPassNode ifTrue:[
        self signalError: 'Block argument should not be given.'
      ] ifFalse:[
        self signalError: 'internal_error new_yield invalid args' 
  ]]].
  ^ RubyYieldNode _basicNew argsNode: cArgs ; position: srcOfs

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
node_assign: lhs ofs: srcOfs rhs: rhsArg
  | rhs nLhs |
  lhs ifNil:[ ^ nil ].
  rhs := rhsArg kbegin_value .
  nLhs := lhs node_assign_set_rhs: rhs . " may raise error"
  nLhs == lhs ifFalse:[ nLhs _becomeMinimalChecks: lhs ].
  ^ lhs

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
opt_rescue: exClasses var: exVar body: bodyArg rescue: rescueBody ofs: srcOfs
  | body |
  body := bodyArg .
  exVar ifNotNil:[ | rhs asgn |
    rhs := RubyGlobalVarNode s_a: #'$!' .
    asgn := self node_assign: exVar ofs: srcOfs rhs: rhs .
    body ifNil:[
      body := RubyBlockNode s_list: { asgn }
    ] ifNotNil:[
      body := body prepend_to_block: asgn 
    ].
  ].
  ^ RubyRescueBodyNode s_a: exClasses b: body c: rescueBody d: srcOfs 

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
ret_args: aCallArgs
  | lst res |
  aCallArgs iter ifNotNil:[ 
    self signalError:'block argument should not be given'
  ].
  res := aCallArgs .
  (lst := res list ) size == 1 ifTrue:[ res := lst at: 1 ].
  ^ res

%


set class RubyParserM
category: '*maglev-compiling'
classmethod:
rpParseFile: fullPath  loadName: aName yTrace: yTrace warnings: warn
  | sourceString f |
  sourceString := (f := GsFile openReadOnServer: fullPath) contents.
  f close .
  sourceString immediateInvariant .
  ^ self rpParseString: sourceString path: fullPath loadName: aName 
		yTrace: yTrace warnings: warn

%


set class RubyParserM
category: '*maglev-runtime'
classmethod:
rpParseString: sourceString path: fullPath  loadName: aName yTrace: yTrace warnings: warn
  | root ast |
  ast := self parse: sourceString cBytes: ( CByteArray withAll: sourceString)
               line: 0 file: aName yTrace: yTrace warnings: warn 
	       evalScope: nil .
  ast _stringCharSize ~~ 0 ifTrue:[ RubyParseError signal: ast ].
  (root := RubyRootNode _basicNew)
        bodyNode:  ast ;
        lineBias: 0  ;
        fileName: fullPath  source: sourceString ;
        position: 1 .  "first one-based byte of file"
  (SessionTemps current at: #MAGLEV_logSexp  otherwise: nil) == true ifTrue:[
      GsFile gciLogServer: root _inspect .
  ].
  root walkScopes: Object  .
  ^ root

%


set class RubyParserM
category: '*maglev-runtime'
classmethod:
rpParseString: sourceString scope: aScopN lineBias: bias evalScope: aRubyEvalScope fileName: fName
  | ast root trace tmps warn |
  sourceString immediateInvariant .
  tmps := SessionTemps current .
  trace := tmps at: #MagRpDEBUG otherwise: 0 .
  warn := tmps at:#MAGLEV_parseWarn otherwise: false .
  ast :=  self parse: sourceString cBytes: ( CByteArray withAll: sourceString)
       line: (bias < 0 ifTrue:[ 0 ] ifFalse:[ bias ])
       file: fName yTrace: trace - 1 warnings: warn 
       evalScope: aRubyEvalScope .
  ast _stringCharSize ~~ 0 ifTrue:[ RubyParseError signal: ast ].
  (root := RubyRootNode _basicNew)
        bodyNode:  ast ;
        lineBias: bias  ;
        fileName: '<eval>'  source: sourceString ;
        position: 1 .  "first one-based byte of file"
   "walkPositions no longer needed"
   (SessionTemps current at: #MAGLEV_logSexp  otherwise: nil) == true ifTrue:[
      GsFile gciLogServer: root _inspect .
   ].
   root walkScopes: aScopN  .
   ^ root

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
signalError: aString

  (self _rbCompileError: aString isWarning: false) ifTrue:[
    "parser primitive active on stack and aString was saved in parser state"
  ] ifFalse:[
    "parser primitive not active, signal directly"
    RubyParseError signal: aString 
  ].

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
signalWarning: aString
  self _rbCompileError: aString isWarning: true

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
string_to_symbol: str
  str _isOneByteString ifFalse:[
    self signalError: 'internal_error, expected value to be a String'.
  ].
  str size == 0 ifTrue:[
    self signalError: 'empty symbol literal' .
  ].
  (str indexOfByte: 0 startingAt: 1) ~~ 0 ifTrue:[  
     self signalError: 'symbol string may not contain `\0'' ' .
  ].
  ^  str asSymbol

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
uplus_production: arg ofs: srcOfs
  | res |
  (arg isKindOf: RubyAbstractLiteralNode) ifTrue:[
     res := arg
  ] ifFalse:[
     res := self new_vcall: arg sel: (RpNameToken new: #'+@' position: srcOfs)
  ].
  ^ res

%


set class RubyParserM
category: '*maglev-ast'
classmethod:
value_expr: aNode
  ^ aNode kbegin_value

%

