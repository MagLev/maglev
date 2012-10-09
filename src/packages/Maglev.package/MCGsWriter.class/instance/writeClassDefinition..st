as yet unclassified
writeClassDefinition: definition
	
	self classDefinitionsStream
		cr;
		nextPutAll: 'doit'; cr;
		nextPutAll: definition actualClass definition; cr;
		nextPutAll: '%'; cr; cr;
		nextPutAll: 'set class '; nextPutAll: definition className; cr;
		nextPutAll: 'removeallmethods'; cr;
		nextPutAll: 'removeallclassmethods'; cr.

