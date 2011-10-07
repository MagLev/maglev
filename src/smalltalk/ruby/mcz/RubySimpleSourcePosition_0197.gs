
set class RubySimpleSourcePosition
category: 'as yet unclassified'
method:
asShortString
  | lineStr fileStr |
  lineStr := line ifNil:[ 'unknown' ] ifNotNil:[ line asString ].
  fileStr := filename ifNil:[ 'unknown file'] ifNotNil:[ filename ].
  ^ 'line ' , lineStr , ' of ' , (RubyFile pathForTrace: fileStr)

%


set class RubySimpleSourcePosition
category: 'as yet unclassified'
method:
asString
  | lineStr fileStr |
  lineStr := line ifNil:[ 'unknown' ] ifNotNil:[ line asString ].
  fileStr := filename ifNil:[ 'unknown file'] ifNotNil:[ filename ].
  ^ 'line ' , lineStr , ' of ' , fileStr

%


set class RubySimpleSourcePosition
category: 'parsetree'
method:
endLine
	^ line

%


set class RubySimpleSourcePosition
category: 'parsetree'
method:
endOffset
	^ 0

%


set class RubySimpleSourcePosition
category: 'accessing'
method:
filename

	 ^ filename

%


set class RubySimpleSourcePosition
category: 'accessing'
method:
filename: aString
	filename := aString

%


set class RubySimpleSourcePosition
category: 'as yet unclassified'
method:
hasOffsetInfo
	^ false

%


set class RubySimpleSourcePosition
category: 'accessing'
method:
line

	 ^ line  "line nums already converted from MRI zero-based at instance creation"

%


set class RubySimpleSourcePosition
category: 'accessing'
method:
line: aNumber
	line := aNumber

%


set class RubySimpleSourcePosition
category: 'as yet unclassified'
method:
nextLine
  "Used for ad-hoc adjustments for MRI parser position defects."
  | res | 
  (res := self class _basicNew) 
     filename: filename ;
     line: line + 1 .
  ^ res

%


set class RubySimpleSourcePosition
category: 'parsetree'
method:
startLine
	^ line

%


set class RubySimpleSourcePosition
category: 'parsetree'
method:
startOffset
	^ 0

%


set class RubySimpleSourcePosition
category: 'accessing'
method:
storePositionInNode: aNode

  aNode lineNumber: line 

	  "line nums already converted from MRI zero-based at instance creation"

%

