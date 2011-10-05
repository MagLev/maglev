
doit
Object subclass: 'RubyEvalVarLocation'
	instVarNames: #( vcOfs tmpName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvalVarLocation
removeallmethods
removeallclassmethods

set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
irAssignmentNode: srcIrNode
  | holder snd |
  (holder := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: GsProcess"Ruby ::Thread") .
  snd := GsComSendNode new .
  vcOfs ifNotNil:[ :ofs |
    holder  stSelector: #_rubyEvalVc .
    snd rcvr: holder ;
      stSelector:  #at:put: ;
       appendArgument: (GsComLiteralNode newInteger: ofs ) .
  ] ifNil:[
    holder stSelector:  #_rubyEvalBinding .
    snd  rcvr: holder ;
      stSelector: #_tempAt:put: ;
      appendArgument: (GsComLiteralNode newObject: tmpName) .
  ].
  snd appendArgument: srcIrNode .
  ^ snd

%


set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
irNode
  | holder snd |
  (holder := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: GsProcess"Ruby ::Thread") .
  snd := GsComSendNode new .
  vcOfs ifNotNil:[ :ofs |
    holder  stSelector: #_rubyEvalVc .
    snd rcvr: holder ;
      stSelector:  #at: ;
      appendArgument: (GsComLiteralNode newInteger: ofs ) .
  ] ifNil:[
    holder stSelector:  #_rubyEvalBinding .
    snd  rcvr: holder ;
      stSelector: #_tempAt: ;
      appendArgument: (GsComLiteralNode newObject: tmpName) .
  ] .
  ^ snd

%


set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
leaf
  ^ nil

%


set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
nameInBinding: aSymbol
  vcOfs ifNotNil:[ self error:'vcOffset already assigned'].
  tmpName := aSymbol .

%


set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
readCount: anInt
  ^ 0  "not an incoming block arg"

%


set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
varInfo
  ^ self

%


set class RubyEvalVarLocation
category: '*maglev-runtime'
method:
vcOffset: aSmallInt
  tmpName ifNotNil:[ self error:'tmpName already assigned'].
  vcOfs := aSmallInt .

%

