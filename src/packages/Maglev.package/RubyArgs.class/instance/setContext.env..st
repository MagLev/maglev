as yet unclassified
setContext: context env: envId

	"Sets $LOAD_PATH (if passed -I) and will require all files 
	 named in -r parameters."
	self libs do:[ :each| context _addLibs: each env: envId ] .
	self requires do:[ :each| context requireFileNamed: each env: envId ] .
	^ self

