*maglev-runtime
ensurePrimsLoaded
	"Check if there is already a ruby context with prims loaded installed,
	 and if not, install one and commit the transaction."
	Saved ifNil: [ RubyContext load ] .

