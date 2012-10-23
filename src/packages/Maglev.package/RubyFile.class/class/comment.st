as yet unclassified
comment
	"RubyFile holds a couple of paths and knows how to compile itslef. A RubyFile
	has several names which are listed below:
	
	path:          The full, absolute path to the file (e.g., /tmp/mylib/foo.rb)
	
	givenName:  The name of the file as passed to require or load (e.g., './foo.rb').  This
	                name appears on $LOADED_FEATURES.
	
	loadName:    The givenName prefixed with the $LOAD_PATH component used to find the file (if any).
	                E.g., 'mylib/./foo.rb'.  This is the name used for __FILE__.
	"
	^ 'A Ruby File'

