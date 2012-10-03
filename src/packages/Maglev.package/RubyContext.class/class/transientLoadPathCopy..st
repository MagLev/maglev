*maglev-runtime
transientLoadPathCopy: envId
	"Copy the transient load path to the persistent load path, ensuring that the array
	 and all elements are copies (nothing shared).  Convert any leading prefix equal to
	 $MAGLEV_HOME to '$MAGLEV_HOME."
	|path mHome|
	mHome := RubyEnv _getenv: 'MAGLEV_HOME' .
	path := (Object transientNameSpace: envId) rubyGlobalVar: #'$:' .
	^ RubyContext copyPath: path replacing: mHome with: '$MAGLEV_HOME' .

