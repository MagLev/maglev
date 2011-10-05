
set class RubyCompilerState class
category: '(as yet unclassified)'
method:
clearTransientState
  GsProcess _current _rubyThreadDataAt: 1 put: nil ;
                     _rubyThreadDataAt: 2 put: nil .

%


set class RubyCompilerState class
category: '*maglev-runtime'
method:
comment
^ '
  fileStack replaces RubyCurrentFile .

  scopeStack replaces RubyCurrentScope . typically scopes are created during
     walkWithScope phase  and captured in method, class, or block nodes of the AST graph .
     The scope stack is used during both walkWithScopes phase and IR generation phase.

  lexLevel replaces RubyCurrentLexLevel

  compilerStack  replaces RubyCurrentCompiler
  methStack  is a stack of RubyMethDefNodes, used during the walkWithScope phase 

  loopStack is used to handle RubyWhileNode and RubyUntilNode during walkWithScopes 
    and IR phases.  The top element of the loopStack will be a RubyNode during processing
    of an in-line loop''s block, and will be nil during processing other blocks.

  rtModuleStack is the runtime stack of nested classes/modules.
    also accessible as (GsProcess _current _rubyThreadDataAt: 5)
'

%


set class RubyCompilerState class
category: '(as yet unclassified)'
method:
current
  ^ GsProcess _current _rubyThreadDataAt: 1

%


set class RubyCompilerState class
category: '*maglev-runtime'
method:
initialize: envId
 "Note, we currently don't support fully general mixed-env ruby execution ; to do so 
  would require a stack of envId's "
 | clientData st  |
 clientData := GsProcess _current _newRubyThreadData . 
 st := clientData at: 1 "inline  self current" .
 st ifNil:[  st := self initializeForNewThread: envId ].
 st initialize: envId .
  ^ st

%


set class RubyCompilerState class
category: '*maglev-runtime'
method:
initializeForNewThread: envId
 "Note, we currently don't support fully general mixed-env ruby execution ; to do so 
  would require a stack of envId's "
 | clientData  st  |
 clientData := GsProcess _current _newRubyThreadData .
 st := clientData at: 1 "inline  self current" .
 st ifNil:[
   st := self _basicNew initializeForNewThread: envId .
   clientData at: 1 put: st ;  "inline _rubyThreadDataAt:put:"
              "at:2 , last child process status, left as nil"
              at: 3 put: RubyCompilerStack new ; "instance_eval self's stack"
              at: 4 put: IdentitySet new ; "recursionGuardSet"
              at: 5 put: st rtModuleStack ;
              at: 6 put: RubyCompilerStack new . "evalArgs stack"
 ] ifNotNil:[
   st envId: envId .
   st class == self ifFalse:[  self error:'invalid RubyCompilerState'  ].
 ].
  ^ st

%


set class RubyCompilerState class
category: '*maglev-runtime'
method:
methodDefTargetStack
  "method not sent, for documentation only"
  ^ GsProcess _current _rubyThreadDataAt: 3 .

%


set class RubyCompilerState class
category: '(as yet unclassified)'
method:
use: aState
  "for use by maglev irb implementation only"
  aState class == RubyCompilerState ifFalse:[ self error:'invalid argument'].
  GsProcess _current _rubyThreadDataAt: 1 put: aState .
 

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
compilerStack
  ^ compilerStack

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
compilingEval
  ^ fileStack topOrNil class == RubyEvalFile

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
envId
  ^ envId 

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
envId: envArg
  envId := envArg

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
fileStack
  ^ fileStack

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
initialize: envArg
   | stkClass |
   envId := envArg . 
   fileStack ifNil:[
     fileStack  :=  (stkClass := RubyCompilerStack) new .
     scopeStack := stkClass new .
     lexLevel := -1 .
     lexLevelStack := stkClass new .
     compilerStack := stkClass new .
     methStack := stkClass new .
     lineBiasStack := stkClass new .
     loopStack:= stkClass new .
     lastExceptionStack := stkClass new .
     "outerDefLexPath left as nil "
     parserStack := stkClass new .
     evalLexicalSelfStack := stkClass new .
   ].

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
initializeForNewThread: envArg
   | tmps moduStk |
   envId := envArg .
   installingPrims := false .
   reloadingPrims := false .
   tmps := SessionTemps current .
   persistenceMode :=  tmps at: #MAGLEV_persistentFlag otherwise: false .
   persistableInstances := true .
   rtModuleStack := (moduStk := RubyCompilerStack new) .
   moduStk push: Object  .
   "fileStack and others left as nil"

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
installingPrims
  ^ installingPrims

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
installingPrims: aBoolean 
   installingPrims := aBoolean

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
lastExceptionStack
  ^ lastExceptionStack

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
lexicalLevel
  ^  (lexLevelStack topValue) bitAnd: 16rFFFF

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
lexicalSelfStack
	| stk |
	(stk := evalLexicalSelfStack ) ifNil:[
		self initialize: envId .
		stk := evalLexicalSelfStack .
	].
	^ stk 

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
lexLevelIsInline: aLevel
   "return true if specified level is in-line
       first level is 0 , stored at stk at:1"
  aLevel == 0 ifTrue:[ ^ true ].
  ^ (( lexLevelStack at: (aLevel + 1)   ) bitAnd: 16r10000) ~~ 0

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
lexLevelIsInlineWithin: aValue
  "Return true if all lexical levels from current to aValue+1 represent in-line blocks"
  | idx idxLimit stk |
  stk := lexLevelStack .
  idx := stk depth .
  idxLimit := aValue + 2 . "first level is 0 , stored at stk at:1"
  [ idx >= idxLimit ] whileTrue:[ | v |
	(( v := stk at: idx) bitAnd:16rFFFF) < aValue ifTrue:[ self error:'inconsistent lexLev'].
     (v bitAnd: 16r10000) == 0 ifTrue:[ ^ false ] .
     idx := idx - 1 .
  ].  
  ^ true

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
lexLevelOkForFixedIvAccess
  self compilingEval ifTrue:[ ^ false ].
  (self lexLevelIsInlineWithin: 0 ) ifTrue:[ ^ true ]  .
  ^ installingPrims

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
loopStack
  ^ loopStack

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
outerDefLexPath
  ^ outerDefLexPath ifNil:[ Error signal:'missing outerDefLexPath'].

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
outerDefLexPath: anArray
  "invoked from generated code"
  outerDefLexPath := anArray

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
parentMethodDefOrNil 
  ^ methStack parentOrNil 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
parserStack 
  ^ parserStack

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
persistableInstances
  ^ persistableInstances

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
persistableInstances: aBoolean
  persistableInstances := aBoolean

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
persistenceMode
  ^ persistenceMode

%


set class RubyCompilerState
category: '(as yet unclassified)'
method:
persistenceMode: aBoolean
   "returns the previous state"
  | prev |
  prev := persistenceMode .
  persistenceMode := aBoolean .
  ^ prev 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
popLexicalLevel: lev 
  |  topLev  |
  topLev := lexLevelStack pop .
  (topLev bitAnd: 16rFFFF) == lev ifFalse:[ self error:'bad level in popLexicalLevel:'].
  lexLevel == lev  ifFalse:[ self error:'bad level in popLexicalLevel:'].
  lexLevel := lev - 1 .

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
popMethodDef: methodDefNode
   methStack pop: methodDefNode .
   loopStack pop: nil .

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
popMethodDef: methodDefNode lineBias: bias 
   methStack pop: methodDefNode .
   lineBiasStack pop: bias .
   loopStack pop: nil .

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
popRtModule: aModule
  rtModuleStack pop: aModule

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
pushInlineLexicalLevel
  "increment lex level for an inline block"
  | lev mask |
  lexLevel := (lev := lexLevel + 1 ).
  mask := 16r10000 .  "the inline bit"
  lev >= mask ifTrue:[ self error:'nesting too deep' ].
  lexLevelStack push: ( lev bitOr: mask) .
  ^ lev 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
pushLexicalLevel
  "increment lex level for a non-inline block"
  | lev |
  lexLevel := (lev := lexLevel + 1 ).
  lexLevelStack push: lev .
  ^ lev 

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
pushMethodDef: methOrClassBody
  "returns self"
   methStack push: methOrClassBody .
   loopStack push: nil .

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
pushMethodDef: methodDefNode lineBias: bias 
   methStack push: methodDefNode .
   lineBiasStack push: bias .
   loopStack push: nil .

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
pushMethodDef: methodDefNode scope: aScope
  "returns true if previous top of stack is a RubyMethodDefNode,
    or we are within an eval"
   | methStk prev |
   methStk := methStack.
   prev := methStk topOrNil .
   methStk push: methodDefNode .
   loopStack push: nil .
   prev isMethodDef ifTrue:[
     "setup the ref from an inner def to the outermost def"
     | outerMost |
     methodDefNode outerDef: (outerMost := prev outerDef) . "the outer-most def"
     outerMost setHasInnerEvalOrDef .
     ^ true
   ].
   (evalLexicalSelfStack size > 0 and:[ prev walkInEval ]) ifTrue:[ 
     "In an eval, and not within class or module within eval"
     methodDefNode outerDef: methodDefNode .
     ^ true
   ].
   ^ false

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
pushRtModule: aModule
  rtModuleStack push: aModule 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
rawLexicalLevel
  "return top lexical level including inline bit"
  ^  lexLevelStack topValue

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
reloadingPrims
  ^ reloadingPrims

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
reloadingPrims: aBoolean
  reloadingPrims := aBoolean

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
rtModuleEvalLexPath
  "Result is outer to inner class order"
  ^ RubyLexicalPath withAll: rtModuleStack

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
rtModuleLexPath
  "Result is in inner class to outer class order"
  | stk res sz |
  stk := rtModuleStack .
  sz := stk size .
  sz ~~ 1 ifTrue:[
    res := Array new: sz - 1 . 
    0 to: sz - 2 do:[:j | res _rubyAt: j put: (stk at: sz - j) ].
    ^ res
  ] ifFalse:[
    ^ { }
  ].

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
rtModuleStack
   ^ rtModuleStack

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
scopeStack
  ^ scopeStack

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topCompiler
  ^ compilerStack topValue

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topLexicalSelf
  ^ evalLexicalSelfStack topOrNil 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topLineBiasOrZero
  | res |
  res := lineBiasStack topOrNil .
  ^ res ifNil:[ 0 ] 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topLoop
  ^ loopStack topOrNil

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topMethodDef 
  ^ methStack topValue 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topMethodDefOrNil 
  ^ methStack topOrNil 

%


set class RubyCompilerState
category: '*maglev-runtime'
method:
topRtModule
 ^ rtModuleStack topValue .

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topRubyParserOrNil 
  ^ parserStack topOrNil 

%


set class RubyCompilerState
category: 'as yet unclassified'
method:
topScope
  ^ scopeStack topValue

%

