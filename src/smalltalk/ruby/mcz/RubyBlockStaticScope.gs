
doit
RubyStaticScope subclass: 'RubyBlockStaticScope'
	instVarNames: #( irblock)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBlockStaticScope
removeallmethods
removeallclassmethods

set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
implicitBlockNotNil
	^ enclosingScope implicitBlockNotNil

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
implicitBlockVar
	^ enclosingScope implicitBlockVar

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
irBlockNode
    ^ irblock

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
irNewBlockNode
  ^ irblock := GsComBlockNode new lexLevel: self lexLevel .

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
lexLevel
	| level s |
	level := 0.
	s := self.
	[s enclosingScope notNil] whileTrue:
		[level := level + 1.
		s := s enclosingScope].
	^ level

%


set class RubyBlockStaticScope
category: '*maglev-ast'
method:
locationForBlockArg: nameSym
  | vInfo |
  vInfo := self _findVariable: nameSym .
  vInfo ifNotNil:[ "allow duplicate block arg names, Trac 798" | hiddenSym |
    vInfo kind ~~ #blockArg ifTrue:[
      self error:'argument  ', nameSym , '  already defined'
    ].
    hiddenSym := nameSym . 
    [ hiddenSym := ( hiddenSym , '#') asSymbol .
      (self _findVariable: hiddenSym ) == nil
    ] untilTrue .
    vInfo key: hiddenSym .  "hide the previous definition"
  ].
  vInfo := self _addVarInfo: RubyScopeVarInfo name: nameSym kind: #blockArg .
  TraceLocals >= 1 ifTrue:[  self trace:'added ' , nameSym , ' to blockArgs  ' ].

  ^ RubyVarLocation _basicNew varInfo: vInfo depth: 0 scope: self .

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
methodScope
  ^ enclosingScope methodScope 

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
newArgLeafNamed: aSym number: aNumber
  | res blkNod ofs  | 
  blkNod := self irBlockNode .
  ofs := aNumber < 0 ifTrue:[ blkNod numArgs + 1 ] ifFalse:[aNumber].
  (res := GsComVarLeaf new)
			blockArg: aSym 
			argNumber: ofs 
			forBlock: blkNod  .
  TraceLocals >= 1 ifTrue:[
	 self trace: ' newArgLeaf ', aNumber asString , ' created varLeaf blockArg for ', aSym.  
  ].
  ^ res 

%


set class RubyBlockStaticScope
category: '*maglev-runtime'
method:
newTempLeafNamed: aSymbol
  | res |
  (res := GsComVarLeaf new)
			blockTemp: aSymbol
			sourceLexLevel: self lexLevel.
  TraceLocals >= 1 ifTrue:[
	 self trace: ' tempLeafNamed: ', aSymbol , ' , created varLeaf blockTemp'
  ].
  ^ res 

%

