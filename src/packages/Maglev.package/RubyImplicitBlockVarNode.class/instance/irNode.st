*maglev-runtime
irNode
	"This class should only be used from optionallyBuildImplicitBlockTemp"
	"Setting useToProc to 0 will create a Ruby temp that points to the ExecBlock, avoiding
	a call to __to_proc#0__. This is here to keep the break-logic working."
	useToProc := 0.
	^ super irNode.

