
doit
MCStWriter subclass: 'MCGsWriter'
	instVarNames: #( fileStreams)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class MCGsWriter
removeallmethods
removeallclassmethods

doit
Lag25000CmwcRandom subclass: 'RubyRandom'
	instVarNames: #( seed)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyRandom
removeallmethods
removeallclassmethods

doit
RubyRandom subclass: 'RubyRandomNp'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #( instancesNonPersistent)

%

set class RubyRandomNp
removeallmethods
removeallclassmethods

doit
SymbolAssociation subclass: 'RubySymbolAssociation'
	instVarNames: #( isDefined)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubySymbolAssociation
removeallmethods
removeallclassmethods

doit
RubySymbolAssociation subclass: 'RubyAutoloadAssociation'
	instVarNames: #( isLoaded fileName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyAutoloadAssociation
removeallmethods
removeallclassmethods

doit
RubySymbolAssociation subclass: 'RubyClassVarAssociation'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyClassVarAssociation
removeallmethods
removeallclassmethods

doit
RubySymbolAssociation subclass: 'RubyGlobalVarAssociation'
	instVarNames: #( readOnly)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyGlobalVarAssociation
removeallmethods
removeallclassmethods

doit
RubyGlobalVarAssociation subclass: 'RubyDynGlobalVarAssociation'
	instVarNames: #( theBlock)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyDynGlobalVarAssociation
removeallmethods
removeallclassmethods

doit
RubySymbolAssociation subclass: 'RubyTransientConstantAssociation'
	instVarNames: #( block theClass env)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyTransientConstantAssociation
removeallmethods
removeallclassmethods

doit
Array subclass: 'RubyLexicalPath'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyLexicalPath
removeallmethods
removeallclassmethods

doit
Array subclass: 'RubyPersistableCompilerStack'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyPersistableCompilerStack
removeallmethods
removeallclassmethods

doit
RubyPersistableCompilerStack subclass: 'RubyCompilerStack'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #( instancesNonPersistent)

%

set class RubyCompilerStack
removeallmethods
removeallclassmethods

doit
RubyCompilerStack subclass: 'RubyCompilerStackDbg'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #( instancesNonPersistent)

%

set class RubyCompilerStackDbg
removeallmethods
removeallclassmethods

doit
Object subclass: 'RpNameToken'
	instVarNames: #( val src_offset quid)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RpNameToken
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyAbstractBlock'
	instVarNames: #( labelRedo)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractBlock
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyAbstractGlobal'
	instVarNames: #()
	classVars: #( PathDelim)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractGlobal
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyAbstractScope'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractScope
removeallmethods
removeallclassmethods

doit
RubyAbstractScope subclass: 'RubyStaticScope'
	instVarNames: #( isArgumentScope requiredArgs restArg
	                  enclosingScope variableNames extraArgs inBootstrap
	                  nameSpace)
	classVars: #( TraceLocals)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyStaticScope
removeallmethods
removeallclassmethods

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

doit
RubyStaticScope subclass: 'RubyEvalScope'
	instVarNames: #( theBinding varLocs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvalScope
removeallmethods
removeallclassmethods

doit
RubyStaticScope subclass: 'RubyLocalStaticScope'
	instVarNames: #( incBlockArgLeaf hasImplicitBlockTemp)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyLocalStaticScope
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyArgf'
	instVarNames: #( argv stream lineno
	                  advance fileName closed)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #( instancesNonPersistent)

%

set class RubyArgf
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyArgs'
	instVarNames: #( scriptName rubyArgs scriptArgs
	                  libs requires scriptlets)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyArgs
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyArrayList'
	instVarNames: #( size)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArrayList
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyBinding'
	instVarNames: #( block staticLink selfObj
	                  names forModuleEval tmpsDict lexicalScope
	                  homeMeth methDefTarget)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyBinding
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyBridge'
	instVarNames: #( from fromSuffix fromNargs
	                  fromRestArg fromBlockArg to toSuffix
	                  neededArgs needsRestArg needsBlockArg leaves
	                  missingArgsError firstOptArg primKnd)
	classVars: #( BridgeOptions GenericErrCms GenericErrCmSet)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyBridge
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyByteList'
	instVarNames: #( begin hash realSize
	                  validHash bytes stringValue)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyByteList
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyCallType'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallType
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyCompiler'
	instVarNames: #( currentClass leaves sourceString
	                  currClassOrModule installingPrims useRubyParser needsSuperEach)
	classVars: #( AltParser Parser RestrictedClasses SmallIntSpecialSelectors Verbose)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyCompiler
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyContext'
	instVarNames: #( modifiedStClasses rubyPrimMethods)
	classVars: #( BootWarnings Saved)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyContext
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyContinuation'
	instVarNames: #( process)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: 'Globals'
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyContinuation
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyDRegexpOnceLiteral'
	instVarNames: #( regex)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDRegexpOnceLiteral
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyEvalFile'
	instVarNames: #( source fileName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyEvalFile
removeallmethods
removeallclassmethods

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

doit
Object subclass: 'RubyFile'
	instVarNames: #( givenPath path source
	                  loadName fullPath traceLoad cLibrary)
	classVars: #( TraceLoad)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyFile
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyFinalizer'
	instVarNames: #( obj procs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyFinalizer
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyFlipFlop'
	instVarNames: #( theState)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyFlipFlop
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyIDESourcePosition'
	instVarNames: #( endLine endOffset startLine
	                  startOffset comments file)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyIDESourcePosition
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyLexStrTerm'
	instVarNames: #( kind iva ivb
	                  ivc ivd nesting)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyLexStrTerm
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyMethodDefTarget'
	instVarNames: #( theClass)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMethodDefTarget
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyNode'
	instVarNames: #( position)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyAbstractCallNode'
	instVarNames: #( irArgNodes fixedArgCount lastArgIsBlk
	                  argsList bypassProt)
	classVars: #( LastArgNoToProcSelectors RcvrNoToProcSelectors SpecialSelectorKinds)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractCallNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyAbstractMatchDataRef'
	instVarNames: #( rcvrNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractMatchDataRef
removeallmethods
removeallclassmethods

doit
RubyAbstractMatchDataRef subclass: 'RubyBackRefNode'
	instVarNames: #( selector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBackRefNode
removeallmethods
removeallclassmethods

doit
RubyAbstractMatchDataRef subclass: 'RubyNthRefNode'
	instVarNames: #( matchNumNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNthRefNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyAbstractMatchNode'
	instVarNames: #( receiverNode implicitDollarTilde)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractMatchNode
removeallmethods
removeallclassmethods

doit
RubyAbstractMatchNode subclass: 'RubyMatch2Node'
	instVarNames: #( valueNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMatch2Node
removeallmethods
removeallclassmethods

doit
RubyAbstractMatchNode subclass: 'RubyMatchZeroNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMatchZeroNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyAbstractWhileNode'
	instVarNames: #( bodyNode conditionNode labelBreak
	                  labelNext labelRedo hasBreakOrNext hasBeginOrRescue
	                  blksInline condIsFirst baseSel)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractWhileNode
removeallmethods
removeallclassmethods

doit
RubyAbstractWhileNode subclass: 'RubyUntilNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyUntilNode
removeallmethods
removeallclassmethods

doit
RubyAbstractWhileNode subclass: 'RubyWhileNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyWhileNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyAliasNode'
	instVarNames: #( newName oldName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAliasNode
removeallmethods
removeallclassmethods

doit
RubyAliasNode subclass: 'RubyGlobalVarAliasNode'
	instVarNames: #( rootScope)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalVarAliasNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyAndNode'
	instVarNames: #( firstNode secondNode leftParen)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAndNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyArgsCatNode'
	instVarNames: #( firstNode secondNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArgsCatNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyAttrAssignNode'
	instVarNames: #( argsNode name receiverNode
	                  srcIrNode splatPosition resTmp)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAttrAssignNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyCallNode'
	instVarNames: #( argsNode iterNode receiverNode
	                  evaluationTmpAssoc procNewZeroHasMeth implicitDollarTilde callName)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallNode
removeallmethods
removeallclassmethods

doit
RubyCallNode subclass: 'RubyCallEvalNode'
	instVarNames: #( lexPathVar)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallEvalNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyDotNode'
	instVarNames: #( exclusive beginNode endNode
	                  beginTmp endTmp fixNumCount isInline)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDotNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyDynamicDefinedQNode'
	instVarNames: #( litObj selector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDynamicDefinedQNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyEnsureNode'
	instVarNames: #( bodyNode ensureNode isStEnsure)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEnsureNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyFCallNode'
	instVarNames: #( argsNode callName iterNode
	                  rcvrNode implicitDollarTilde)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFCallNode
removeallmethods
removeallclassmethods

doit
RubyFCallNode subclass: 'RubyBlockGivenNode'
	instVarNames: #( evalRcvr)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBlockGivenNode
removeallmethods
removeallclassmethods

doit
RubyFCallNode subclass: 'RubyFCallBindingNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFCallBindingNode
removeallmethods
removeallclassmethods

doit
RubyFCallNode subclass: 'RubyFCallCalleeNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFCallCalleeNode
removeallmethods
removeallclassmethods

doit
RubyFCallNode subclass: 'RubyFCallEvalNode'
	instVarNames: #( lexPathVar)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFCallEvalNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyGlobalLastExcBackTrace'
	instVarNames: #( rcvr)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalLastExcBackTrace
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyGlobalNotAssignable'
	instVarNames: #( name msg valueNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalNotAssignable
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyHashNode'
	instVarNames: #( listNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyHashNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyIfNode'
	instVarNames: #( condition elseBody thenBody)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyIfNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyInstVarDefinedQNode'
	instVarNames: #( ivNode selector argNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyInstVarDefinedQNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyMethodDefNode'
	instVarNames: #( argsNode bodyNode nameNode
	                  scope hasBlockArgRef irMethNode traceBool
	                  sendsBinding argsDescrInt methSelector lineBias
	                  fileName source startLine endOffset
	                  outerDef defTarget innerDefs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMethodDefNode
removeallmethods
removeallclassmethods

doit
RubyMethodDefNode subclass: 'RubyDefnNode'
	instVarNames: #( outerIsDefsNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDefnNode
removeallmethods
removeallclassmethods

doit
RubyMethodDefNode subclass: 'RubyDefsNode'
	instVarNames: #( receiverNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDefsNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyModuleNode'
	instVarNames: #( bodyNode cpath scope
	                  lineBias fileName source)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyModuleNode
removeallmethods
removeallclassmethods

doit
RubyModuleNode subclass: 'RubyClassNode'
	instVarNames: #( superNode fixedIvs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyClassNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyNotNode'
	instVarNames: #( conditionNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNotNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyOpAsgnAndNode'
	instVarNames: #( firstNode secondNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyOpAsgnAndNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyOpAsgnNode'
	instVarNames: #( operatorCallName receiverNode valueNode
	                  variableAsgnCallName variableCallName rcvrTmp)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyOpAsgnNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyOpAsgnOrNode'
	instVarNames: #( firstNode secondNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyOpAsgnOrNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyOpElementAsgnNode'
	instVarNames: #( argsNode callName receiverNode
	                  valueNode evalTmpAssocs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyOpElementAsgnNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyOrNode'
	instVarNames: #( firstNode secondNode leftParen)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyOrNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyRescueNode'
	instVarNames: #( bodyNode elseNode rescueBodyNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRescueNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubySClassNode'
	instVarNames: #( bodyNode receiverNode scope
	                  lineBias fileName source startLine)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySClassNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubySuperNode'
	instVarNames: #( argsNode iterNode implicitBlockArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySuperNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyUndefNode'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyUndefNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyVCallNode'
	instVarNames: #( callName rcvrNode implicitDollarTilde)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallNode
removeallmethods
removeallclassmethods

doit
RubyVCallNode subclass: 'RubyVCallBindingNode'
	instVarNames: #( hasBlkArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallBindingNode
removeallmethods
removeallclassmethods

doit
RubyVCallNode subclass: 'RubyVCallBlockGivenNode'
	instVarNames: #( evalRcvr rcvrIsSelf)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallBlockGivenNode
removeallmethods
removeallclassmethods

doit
RubyVCallNode subclass: 'RubyVCallCalleeNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallCalleeNode
removeallmethods
removeallclassmethods

doit
RubyVCallNode subclass: 'RubyVCallEvalNode'
	instVarNames: #( lexPathVar)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallEvalNode
removeallmethods
removeallclassmethods

doit
RubyVCallNode subclass: 'RubyVCallRaiseNode'
	instVarNames: #( irArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallRaiseNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyYieldNode'
	instVarNames: #( checkState argsNode evalRcvr
	                  forceSelector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyYieldNode
removeallmethods
removeallclassmethods

doit
RubyAbstractCallNode subclass: 'RubyZSuperNode'
	instVarNames: #( iterNode methSelector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyZSuperNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyAbstractGotoNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractGotoNode
removeallmethods
removeallclassmethods

doit
RubyAbstractGotoNode subclass: 'RubyAbstractBreakNode'
	instVarNames: #( valueNode walked)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractBreakNode
removeallmethods
removeallclassmethods

doit
RubyAbstractBreakNode subclass: 'RubyBreakNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBreakNode
removeallmethods
removeallclassmethods

doit
RubyAbstractBreakNode subclass: 'RubyNextNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNextNode
removeallmethods
removeallclassmethods

doit
RubyAbstractGotoNode subclass: 'RubyRedoNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRedoNode
removeallmethods
removeallclassmethods

doit
RubyAbstractGotoNode subclass: 'RubyRetryNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRetryNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyAbstractLiteralNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractLiteralNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubyAbstractNumberNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractNumberNode
removeallmethods
removeallclassmethods

doit
RubyAbstractNumberNode subclass: 'RubyFixnumNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFixnumNode
removeallmethods
removeallclassmethods

doit
RubyAbstractNumberNode subclass: 'RubyFloatNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFloatNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubyEvalLexPathNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvalLexPathNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubyFalseNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFalseNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubyNilNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNilNode
removeallmethods
removeallclassmethods

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

doit
RubyAbstractLiteralNode subclass: 'RubyStLiteralNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyStLiteralNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubyStrNode'
	instVarNames: #( value)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyStrNode
removeallmethods
removeallclassmethods

doit
RubyStrNode subclass: 'RubyXStrNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyXStrNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubySymbolNode'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySymbolNode
removeallmethods
removeallclassmethods

doit
RubyAbstractLiteralNode subclass: 'RubyTrueNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyTrueNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyAbstractVarNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST-Abstract'
	options: #()

%

set class RubyAbstractVarNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyClassVarNode'
	instVarNames: #( name inMethod)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyClassVarNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyColon3Node'
	instVarNames: #( name globAssoc)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyColon3Node
removeallmethods
removeallclassmethods

doit
RubyColon3Node subclass: 'RubyColon2Node'
	instVarNames: #( leftNode leftIsDynamic isProcClass
	                  isTypeClass dynamicTypeError inInnerDef)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyColon2Node
removeallmethods
removeallclassmethods

doit
RubyColon2Node subclass: 'RubyClassNameNode'
	instVarNames: #( isColon3)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyClassNameNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyConstNode'
	instVarNames: #( name globalAssoc)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyConstNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyDVarNode'
	instVarNames: #( location name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDVarNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyGlobalVarNode'
	instVarNames: #( name globalAssoc)
	classVars: #( SpecialGlobalNodesDict)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalVarNode
removeallmethods
removeallclassmethods

doit
RubyGlobalVarNode subclass: 'RubyGlobalLastException'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalLastException
removeallmethods
removeallclassmethods

doit
RubyGlobalVarNode subclass: 'RubyGlobalProcessState'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalProcessState
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyInstVarNode'
	instVarNames: #( name stName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyInstVarNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubyLocalVarNode'
	instVarNames: #( location name useToProc)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyLocalVarNode
removeallmethods
removeallclassmethods

doit
RubyLocalVarNode subclass: 'RubyImplicitBlockVarNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyImplicitBlockVarNode
removeallmethods
removeallclassmethods

doit
RubyLocalVarNode subclass: 'RubyVcGlobalNode'
	instVarNames: #( mthScope)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVcGlobalNode
removeallmethods
removeallclassmethods

doit
RubyAbstractVarNode subclass: 'RubySelfNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySelfNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyArgsNode'
	instVarNames: #( restArg arguments blockArgNode
	                  optArgs restArgNode argsDescrInt selectorSuffix)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArgsNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyArgumentNode'
	instVarNames: #( identifier)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArgumentNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyAssignableNode'
	instVarNames: #( valueNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAssignableNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyClassVarDeclNode'
	instVarNames: #( name inMethod)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyClassVarDeclNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyConstDeclNode'
	instVarNames: #( constNode globalAssoc isDynamic)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyConstDeclNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyDAsgnNode'
	instVarNames: #( location name isBlockArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDAsgnNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyGlobalAsgnNode'
	instVarNames: #( name globalAssoc)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalAsgnNode
removeallmethods
removeallclassmethods

doit
RubyGlobalAsgnNode subclass: 'RubyGlobalAsgnStdinNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalAsgnStdinNode
removeallmethods
removeallclassmethods

doit
RubyGlobalAsgnNode subclass: 'RubyGlobalAsgnStdoutNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalAsgnStdoutNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyGlobalLastExceptionAsgn'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalLastExceptionAsgn
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyInstAsgnNode'
	instVarNames: #( name stName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyInstAsgnNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyLocalAsgnNode'
	instVarNames: #( location name isBlockArg
	                  createsVar hasAmpersand)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyLocalAsgnNode
removeallmethods
removeallclassmethods

doit
RubyAssignableNode subclass: 'RubyVcGlobalAsgNode'
	instVarNames: #( location name mthScope)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVcGlobalAsgNode
removeallmethods
removeallclassmethods

doit
RubyVcGlobalAsgNode subclass: 'RubyVcGlobalLastMatchAsgn'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVcGlobalLastMatchAsgn
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyBeginNode'
	instVarNames: #( bodyNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBeginNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyBlockArgNode'
	instVarNames: #( name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBlockArgNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyBlockPassNode'
	instVarNames: #( bodyNode inBoot)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBlockPassNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyCaseNode'
	instVarNames: #( caseBody caseNode evalTmp)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCaseNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyDefinedNode'
	instVarNames: #( expressionNode valueStr)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDefinedNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyEvStrNode'
	instVarNames: #( body)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvStrNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyFlipNode'
	instVarNames: #( firstNode secondNode firstTmp
	                  isDot3)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyFlipNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyIterNode'
	instVarNames: #( blockBody bodyNode scope
	                  varNode multArgsNode labelRedo labelNext
	                  zeroDeclaredArgs endSrcOfs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyIterNode
removeallmethods
removeallclassmethods

doit
RubyIterNode subclass: 'RubyForNode'
	instVarNames: #( iterNode labelBreak bodyInline)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyForNode
removeallmethods
removeallclassmethods

doit
RubyIterNode subclass: 'RubyIterRpNode'
	instVarNames: #( callNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyIterRpNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyListNode'
	instVarNames: #( list)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyListNode
removeallmethods
removeallclassmethods

doit
RubyListNode subclass: 'RubyArgsPushNode'
	instVarNames: #( walked)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArgsPushNode
removeallmethods
removeallclassmethods

doit
RubyListNode subclass: 'RubyArrayNode'
	instVarNames: #( walked)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArrayNode
removeallmethods
removeallclassmethods

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

doit
RubyListNode subclass: 'RubyBlockNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyBlockNode
removeallmethods
removeallclassmethods

doit
RubyListNode subclass: 'RubyDRegexpNode'
	instVarNames: #( options)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDRegexpNode
removeallmethods
removeallclassmethods

doit
RubyDRegexpNode subclass: 'RubyDRegexpOnceNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDRegexpOnceNode
removeallmethods
removeallclassmethods

doit
RubyListNode subclass: 'RubyDStrNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDStrNode
removeallmethods
removeallclassmethods

doit
RubyDStrNode subclass: 'RubyDXStrNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDXStrNode
removeallmethods
removeallclassmethods

doit
RubyListNode subclass: 'RubyDSymbolNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDSymbolNode
removeallmethods
removeallclassmethods

doit
RubyListNode subclass: 'RubyScopeNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyScopeNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyNewlineNode'
	instVarNames: #( nextNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyNewlineNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyParAsgnNode'
	instVarNames: #( leftList rightList evalTemps
	                  rhsIsToAry rhsToArraySelector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParAsgnNode
removeallmethods
removeallclassmethods

doit
RubyParAsgnNode subclass: 'RubyParAsgnStarNode'
	instVarNames: #( starSize)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParAsgnStarNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyParAsgnRpNode'
	instVarNames: #( firstNode thirdNode toAry
	                  trailingComma)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParAsgnRpNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyParseErrorNode'
	instVarNames: #( message)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParseErrorNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyRescueBodyNode'
	instVarNames: #( exceptionNodes bodyNode nextRescueBody
	                  defaultNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRescueBodyNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyReturnNode'
	instVarNames: #( valueNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyReturnNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyRootNode'
	instVarNames: #( bodyNode staticScope sendsBinding
	                  lineNumberBias isMainProgram fileName source)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyRootNode
removeallmethods
removeallclassmethods

doit
RubyRootNode subclass: 'RubyClassBodyNode'
	instVarNames: #( classNode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyClassBodyNode
removeallmethods
removeallclassmethods

doit
RubyRootNode subclass: 'RubyEvalRootNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvalRootNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubySplatNode'
	instVarNames: #( node isLhsSplat evalTmp
	                  walked isBlockArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySplatNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubySValueNode'
	instVarNames: #( node)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySValueNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyToAryNode'
	instVarNames: #( node)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyToAryNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyWhenNode'
	instVarNames: #( bodyNode expressionNodes nextCase)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyWhenNode
removeallmethods
removeallclassmethods

doit
RubyNode subclass: 'RubyZArrayNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyZArrayNode
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyPersistableCompilerState'
	instVarNames: #( envId fileStack scopeStack
	                  lexLevel lexLevelStack compilerStack methStack
	                  lineBiasStack loopStack lastExceptionStack outerDefLexPath
	                  installingPrims persistenceMode persistableInstances parserStack
	                  evalLexicalSelfStack rtModuleStack reloadingPrims)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyPersistableCompilerState
removeallmethods
removeallclassmethods

doit
RubyPersistableCompilerState subclass: 'RubyCompilerState'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #( instancesNonPersistent)

%

set class RubyCompilerState
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyProcessStatus'
	instVarNames: #( stat primStatus)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyProcessStatus
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyScopeVarInfo'
	instVarNames: #( key kind leaf
	                  offsetInScop)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Maglev-AST'
	options: #()

%

set class RubyScopeVarInfo
removeallmethods
removeallclassmethods

doit
RubyScopeVarInfo subclass: 'RubyScopeIncomingBlockVar'
	instVarNames: #( toProcInfo readCount isBlockInt)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Maglev-AST'
	options: #()

%

set class RubyScopeIncomingBlockVar
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyService'
	instVarNames: #( ctx envId tns)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyService
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubySimpleSourcePosition'
	instVarNames: #( line filename)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySimpleSourcePosition
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyUnboundMeth'
	instVarNames: #( gsmeth nonBridgeMeth arity
	                  execBridge selPrefix)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyUnboundMeth
removeallmethods
removeallclassmethods

doit
RubyUnboundMeth subclass: 'RubyMeth'
	instVarNames: #( obj)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyMeth
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyVarLocation'
	instVarNames: #( depth info scope)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVarLocation
removeallmethods
removeallclassmethods

doit
Object subclass: 'RubyVisibility'
	instVarNames: #( restore)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVisibility
removeallmethods
removeallclassmethods

doit
TransientMutex subclass: 'RubyTransientMutex'
	instVarNames: #( owner)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #( dbTransient)

%

set class RubyTransientMutex
removeallmethods
removeallclassmethods

doit
RubyScriptError subclass: 'RubyLoadError'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyLoadError
removeallmethods
removeallclassmethods

doit
RubyScriptError subclass: 'RubyParseError'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyParseError
removeallmethods
removeallclassmethods

doit
TestCase subclass: 'RubyWrapperTest'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tests'
	options: #()

%

set class RubyWrapperTest
removeallmethods
removeallclassmethods

doit
WriteStream subclass: 'IndentedStream'
	instVarNames: #( level locals)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class IndentedStream
removeallmethods
removeallclassmethods

doit
ProtoObject subclass: 'RubyWrapper'
	instVarNames: #( wrappedObject)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyWrapper
removeallmethods
removeallclassmethods

doit
IdentityDictionary subclass: 'RubyNameSpace'
	instVarNames: #( name myClass parent
	                  methodProtection moduleFrozen envId)
	classVars: #( AllowDeferred AutoDefine TraceCount TraceGlobals TrapLookup)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #( disallowGciStore)

%

set class RubyNameSpace
removeallmethods
removeallclassmethods

doit
RubyNameSpace subclass: 'RubyTransientNameSpace'
	instVarNames: #( persistCopy)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #( disallowGciStore)

%

set class RubyTransientNameSpace
removeallmethods
removeallclassmethods

doit
Semaphore subclass: 'RubyThreadCriticalMutex'
	instVarNames: #( owner)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #( instancesNonPersistent)

%

set class RubyThreadCriticalMutex
removeallmethods
removeallclassmethods

doit
Notification subclass: 'RubyDynamicVariable'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyDynamicVariable
removeallmethods
removeallclassmethods

doit
Error subclass: 'RubyTimeoutError'
	instVarNames: #()
	classVars: #( RubyMethProtection)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

set class RubyTimeoutError
removeallmethods
removeallclassmethods
