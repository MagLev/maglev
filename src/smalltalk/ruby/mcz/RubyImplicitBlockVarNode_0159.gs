
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

set class RubyImplicitBlockVarNode class
category: 'Documentation'
method:
comment

	^ 'This class should only be used from #optionallyBuildImplicitBlockTemp. See the
	comment in this class'' instance method #irNode'

%


set class RubyImplicitBlockVarNode
category: '*maglev-runtime'
method:
irNode
	"This class should only be used from optionallyBuildImplicitBlockTemp"
	"Setting useToProc to 0 will create a Ruby temp that points to the ExecBlock, avoiding
	a call to __to_proc#0__. This is here to keep the break-logic working."
	useToProc := 0.
	^ super irNode.

%

