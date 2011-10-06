
set class MCGsWriter
category: 'as yet unclassified'
classmethod:
comment

	^ 'This class is used for the Git-only workflow, to file out all the classes that would otherwise be in the mcz.'

%


set class MCGsWriter
category: 'as yet unclassified'
method:
class: aName
	"associates the class name with a {index . stream} pair"
	"if the requested class isn't the last one, but we already have a file stream for it,
	 that means that we are processing a definition that depends on something to be loaded
	 in between, so we create a new file for that"
	| streamColl currentIdxStreamPair |
	streamColl := (fileStreams at: aName ifAbsentPut: [OrderedCollection new]).

	currentClass = aName ifFalse: ["we have never seen this class (streamColl = {}) or we haven't worked on it last"
		dependencyIndex := dependencyIndex + 1.
		streamColl add: {dependencyIndex . String new writeStream}.
		currentClass := aName].
	currentIdxStreamPair := streamColl last.
	^ currentIdxStreamPair last "the actual WriteStream"

%


set class MCGsWriter
category: 'as yet unclassified'
method:
classDefinitionsStream
	| streamColl idxStreamPair |
	streamColl := (fileStreams at: 'class_definitions' ifAbsentPut: [
		{ { 0 . String new writeStream } } ]).
	idxStreamPair := streamColl last.
	^ idxStreamPair last "the actual WriteStream"

%


set class MCGsWriter
category: 'as yet unclassified'
method:
fileOutIn: path
	| dir |
	dir := (FileDirectory on: path)
			assureExistence;
			deleteLocalFiles;
			yourself.
	fileStreams keysAndValuesDo: [:className :array |
		"for info on :array, see #class:"
		array do: [:idxStreamPair || index stream |
			index := idxStreamPair first printPaddedWith: $0 to: 4.
			stream := idxStreamPair last.
			(dir forceNewFileNamed: (className asString copyReplaceAll: ' ' with: '_' ), '_', index, '.gs')
				nextPutAll: stream contents;
				close]].

%


set class MCGsWriter
category: 'as yet unclassified'
method:
safeFileOut
	| maglevHome path |
	(maglevHome := (RubyEnv _getenv: 'MAGLEV_HOME')) ifNil: [ ^ self ].
	path := maglevHome, '/src/smalltalk/ruby/mcz'.
	(FileDirectory on: path) exists ifFalse: [ ^ self ].
	self fileOutIn: path.

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
	
	self classDefinitionsStream
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
	dependencyIndex := 0.
	(MCDependencySorter sortItems: aCollection)
		do: [:ea | ea accept: self].

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
	| isMeta |
	isMeta := definition fullClassName ~= definition className.
	(self class: definition className)
		cr;
		nextPutAll: 'set class ', definition className; cr;
		nextPutAll: 'category: '; nextPutAll: definition category asString printString; cr;
		nextPutAll: (isMeta ifFalse: ['method:'] ifTrue: ['classmethod:']); cr.

%


set class MCGsWriter
category: 'as yet unclassified'
method:
writeMethodSource: definition
	| file |
	file := (self class: definition className).
	definition source linesDo: [:line | file nextPutAll: line; cr].
	file nextPutAll: '%'; cr; cr.

%

