
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

set class MCGsWriter class
category: 'as yet unclassified'
method:
comment

	^ 'This class is used for the Git-only workflow, to file out all the classes that would otherwise be in the mcz.'

%


set class MCGsWriter
category: 'as yet unclassified'
method:
class: aName

	^ fileStreams at: aName ifAbsentPut: [String new writeStream]

%


set class MCGsWriter
category: 'as yet unclassified'
method:
fileOutIn: path
	| dir |
	dir := (FileDirectory on: path)
			assureExistence;
			yourself.
	fileStreams keysAndValuesDo: [:className :stream || file |
		(dir forceNewFileNamed: (className asString copyReplaceAll: ' ' with: '_' ), '.gs')
			nextPutAll: stream contents;
			close].

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeCategory: categoryName

	"no-op"

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeClassComment: definition

	"no-op, class comment is a class-side method"

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeClassDefinition: definition
	
	(self class: definition className)
		cr;
		nextPutAll: 'doit'; cr;
		nextPutAll: definition actualClass definition; cr;
		nextPutAll: '%'; cr; cr;
		nextPutAll: 'set class '; nextPutAll: definition className; cr;
		nextPutAll: 'removeallmethods'; cr;
		nextPutAll: 'removeallclassmethods'; cr.

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeDefinitions: aCollection
	fileStreams := Dictionary new.

	(MCDependencySorter sortItems: aCollection)
		do: [:ea | ea accept: self]
		displayingProgress: 'Writing definitions...'.

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeMetaclassDefinition: definition

	"included in class definition"

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeMethodInitializer: aMethodDefinition

	aMethodDefinition isInitializer ifTrue:
		[(self class: aMethodDefinition className)
			cr;
			nextPutAll: 'doit'; cr;
			nextPutAll: aMethodDefinition className, ' initialize.'; cr;
			nextPutAll: '%'; cr; cr].

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeMethodPostscript

	"no-op"

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeMethodPreamble: definition

	(self class: definition className)
		cr;
		nextPutAll: 'set class ', definition fullClassName; cr;
		nextPutAll: 'category: '; nextPutAll: definition category asString printString; cr;
		nextPutAll: 'method:'; cr.

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeMethodSource: definition
	| file |
	file := (self class: definition className).
	definition source linesDo: [:line | file nextPutAll: line; cr].
	file cr; nextPutAll: '%'; cr; cr.

%

