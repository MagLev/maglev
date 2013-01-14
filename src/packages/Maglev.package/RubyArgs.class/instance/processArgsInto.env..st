as yet unclassified
processArgsInto: context env: envId

	"Parse rubyArgs and scriptArgs, and set the context accordingly.
	 Sets $0, ARGV $LOAD_PATH (if passed -I) and will require all files 
	 named in -r parameters."
	
	self setGlobals: envId .
	self setContext: context env: envId .

