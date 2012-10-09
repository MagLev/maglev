as yet unclassified
cannonicalPathFor: aString
	"Given a relative or absolute path, return an absolute path with '.' and '..' properly
	 handled and removed.  Removes trailing '/'. uses the current process working 
	directory to turn relative paths to absolute.  Does not test for existence of file. Calls 
	absolutePathFor: followed by compressPath:"
	^ self compressPath: (self absolutePathFor: aString) .
	

