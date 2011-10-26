
set class RubyFile
category: 'as yet unclassified'
classmethod:
absolutePathFor: aString
	"If the first character of aString is not '/', then prepend the current working 
	directory and return the path. Does NOT compress embedded occurrences of '.' and '..'."
	|cwd|
	(aString beginsWith: '/') ifTrue: [ ^ aString ].
	^ RubyDirectory _getwd, '/' , aString .
	

%


set class RubyFile
category: 'as yet unclassified'
classmethod:
baseNameAndSuffixFor: aName
  "If aName ends with .<something>, return a pair of the name without
   the suffix and the suffix.  E.g.  baseNameAndSuffixFor: 'abd.def'  returns
   { 'abc' . '.def' } If there is no suffix, then { aName . nil } is returned."

  | dotIdx sz|
  sz := aName size .
  dotIdx :=sz .
  [ dotIdx > 0 ] whileTrue: [
    (aName at: dotIdx) == $.
      ifTrue: [ ^ { aName copyFrom: 1 to: (dotIdx -1) . aName copyFrom: dotIdx to: sz } ].
    dotIdx := dotIdx - 1.
    ].
  ^ { aName . nil }

%


set class RubyFile
category: 'as yet unclassified'
classmethod:
cannonicalPathFor: aString
	"Given a relative or absolute path, return an absolute path with '.' and '..' properly
	 handled and removed.  Removes trailing '/'. uses the current process working 
	directory to turn relative paths to absolute.  Does not test for existence of file. Calls 
	absolutePathFor: followed by compressPath:"
	^ self compressPath: (self absolutePathFor: aString) .
	

%


set class RubyFile
category: 'as yet unclassified'
classmethod:
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

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
compressPath: aPath
  "Compress '.' and '..' in aPath.  Assumes aPath is absolute (starts with '/').
  Removes trailing '/'."
  |  offsets pathSiz ofs idx prev newPath components cSize  |
  pathSiz := aPath size .
  (aPath indexOf: $. startingAt:1) == 0 ifTrue:[ | psiz |
     "no  .  found in path"
     (aPath at: pathSiz ) == $/ ifTrue:[
       ^ aPath copyFrom: 1 to: psiz - 1 .
     ].  
     ^ aPath   
  ].
  idx := 1 .
  offsets := { } .
  [ idx <= pathSiz ] whileTrue:[
     ofs := aPath indexOf: $/ startingAt: idx . 
     ofs ~~ 0 ifTrue:[
       offsets add: ofs .
       idx := ofs + 1 .
     ] ifFalse:[
       idx := pathSiz + 1 "trigger loop exit"
     ].
  ].
  offsets add: pathSiz + 1 .
  prev := offsets at: 1 .
  components := { } .
  newPath := String new .
  prev > 1 ifTrue:[ components add: (aPath copyFrom: 1 to: prev - 1 ) ] 
           ifFalse:[ newPath add: $/ "input starts with / " ].
  2 to: offsets size do:[:n |
    ofs := offsets at: n.
    ofs > (prev + 1) ifTrue:[
      (ofs == (prev + 3) and:[ (aPath at: prev + 1 equals:'..')]) ifTrue:[
	  "found .. , remove last component added" 
          (cSize := components size) > 0 ifTrue:[ components size: cSize - 1 ].
      ] ifFalse:[
	(ofs == (prev + 2) and:[ (aPath at: prev + 1) == $. ]) ifFalse:[
	  "not a single . "
	  components add:( aPath copyFrom: prev + 1 to: ofs - 1 ) .
	]
      ].
    ]. "ifFalse  skip repeated / "
    prev := ofs
  ].
  cSize := components size . 
  cSize > 0 ifTrue:[
    newPath add:( components at: 1 ) .
    2 to: cSize do:[:m | newPath add: $/ ; add:( components at: m )].
  ].
  ^ newPath .

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
filename: aString env: envId
"Create and return a file named aString.  Will try to find the file using the current ruby path $:.
If the file is not found, returns nil.  Does not try to add .rb or any other suffix (see RubyContext>>requireFileNamed)."
|file|
file := self new setFilename: aString env: envId .
file path == nil ifTrue:[    ^ nil ] . "file not found"
^ file .

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
findOnLoadPath: aName env: envId
   (self loadPath: envId) do: [:pathPrefix || p loadPath sfx |
      loadPath := (pathPrefix terminatedWith: $/ ) , aName .
      p := self cannonicalPathFor: loadPath .
      (self isFile: p) ifTrue:[  ^ { p . loadPath } ].
    ].
    ^ nil

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
findOnLoadPath: aName isRequire: isRequire  env: envId
  "Returns an Array { aRubyFile_orNil . featureName } "
  | names bns suffix searchForLibrary baseName defName|
  searchForLibrary := false.
  names := Array new.
  bns := RubyFile baseNameAndSuffixFor: aName.
  suffix := bns at: 2.
  baseName := bns at: 1.
  defName := aName.
  isRequire
    ifTrue: [ suffix
                ifNil: [ defName := aName, '.rb'.
                         names add: defName.
                         searchForLibrary := true ]
                ifNotNil: [ names add: aName. ].
              suffix = '.so' ifTrue: [ searchForLibrary := true ]]
    ifFalse: [ names add: aName ].

  searchForLibrary ifTrue: [ |suffixes|
    suffix = '.so'
      ifTrue: [ suffixes := #('.bundle' '.dylib') ]
      ifFalse: [ suffixes := #('.so' '.bundle' '.dylib') ].
    suffixes do: [:sfx| names add: (baseName , sfx) ]].

  names do: [:ea| |x|
    (self findOnLoadPath: ea env: envId) ifNotNil:[ :arr | | p loadPath fil |
          p := arr at: 1 .  loadPath := arr at: 2 .
          fil := self withGivenPath: ea fullPath: p loadName: loadPath.
          (#('.so' '.bundle' '.dylib') anySatisfy: [:s|  ea endsWith: s ])
             ifTrue: [ fil setIsSharedLib ].
          ^ { fil . ea }
    ].
  ].
  ^ { nil . defName }


%


set class RubyFile
category: '*maglev-runtime'
classmethod:
findRubyFileFor: aName isRequire: isRequire env: envId 
  "Try to find a file for aName.  If aName is a qualified name (starts with '.' '..' './' or '~/'),
  then do the appropriate expansions.  If it is not a qualified name, then search for the
  file on the ruby load path.  If this is called from require, then isRequiring is true and some
  subtle semantics are different than when called from a load..

  Returns an Array { aRubyFile_orNil . featureName } "
  (aName matchesAnyOf: #('/', './', '../', '~/')) ifTrue: [ 
    "This is a qualified file"
    ^ self _findQualifiedFile: aName isRequire: isRequire .
  ] .
  ^ self findOnLoadPath: aName isRequire: isRequire env: envId 

%


set class RubyFile
category: 'as yet unclassified'
classmethod:
isFile: aString
	"Return true iff aString is a path that maps to a file (not a directory) on the server."
	|r r1|
	r := (GsFile existsOnServer: aString) .
	r == nil ifTrue: [^ false ] .  "there was an error in existsOnServer:, i.e., the path was bogus, so not a file"
	r ifTrue:[
		r1 := GsFile isServerDirectory: aString .
		r1 == nil ifTrue: [ ^ false ] ifFalse: [ ^ r1 not ]] .
	^ false .

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
loadPath: envId
  "The result may be an empty array"
  ^ ((Object transientNameSpace: envId) resolveConstant: #'$:') globalVarValue .

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
loadPath: envId put: anArray 
  (Object transientNameSpaceForStore: envId) rubyGlobalVar: #'$:' put: anArray .

%


set class RubyFile
category: 'as yet unclassified'
classmethod:
pathForTrace: aPath
  | hm |
  hm := RubyEnv _getenv:'MAGLEV_HOME' .
  (aPath at:1 equals: hm) ifTrue:[ ^ '$MAGLEV_HOME' , ( aPath copyFrom: hm size + 1 to: aPath size)]
                           ifFalse:[ ^ aPath ]

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
withGivenPath: given fullPath: full 
    "Create a Ruby file with the given path information.  
    Returns nil if the full path does not exist."
    (self isFile: full) ifFalse: [ ^ nil ].
    ^ self new initializeGivenPath: given fullPath: full loadName: given

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
withGivenPath: given fullPath: full loadName: ln
    "Create a Ruby file with the given path information.  An error is
    raised if the full path does not exist."
    (self isFile: full) ifFalse: [ self error: 'file not found: ', full asString ] .
    ^ self new initializeGivenPath: given fullPath: full loadName: ln.

%


set class RubyFile
category: '*maglev-runtime'
classmethod:
_findQualifiedFile: nameArg isRequire: isRequire
    "Compute the full path for aName, expanding '.' '..' or '~' as appropriate.
    Return an Array { aRubyFile_or_nil . feature_name } "

    | canonPath fullPth res aName isClib | 
    aName := nameArg .
    isRequire ifTrue:[ 
       (aName endsWith: '.rb') ifFalse:[ 
         (aName endsWith: '.so') ifTrue:[ 
           isClib := true
         ] ifFalse:[
           aName := aName, '.rb' 
    ]]].
    (aName at: 1) = $/ ifTrue: [ 
       fullPth := aName 
    ] ifFalse: [
       ((aName matchPattern: #('./' $*)) or: [aName matchPattern: #('../' $*) ]) ifTrue:[ 
          fullPth := (RubyDirectory _getwd), '/', aName 
       ] ifFalse: [ 
          (aName matchPattern: #('~/' $*)) ifTrue: [ |h|
             h := System gemEnvironmentVariable: 'HOME' .
             fullPth := h, (aName copyFrom: 2 to: (aName size))
          ] ifFalse:[
             ArgumentError signal:'arg to _findQualifiedFile is not a qualified name'
          ]
       ]
    ] .
    canonPath := self cannonicalPathFor: fullPth .
    res := self withGivenPath: aName fullPath: canonPath  . 
    res ifNotNil:[ isClib ifNotNil:[ res setIsSharedLib ]].
    ^ { res . aName }

%


set class RubyFile
category: 'as yet unclassified'
method:
= other
	^ other species = self species and: [other path = self path]

%


set class RubyFile
category: '(as yet unclassified)'
method:
asString
 | res |
   "before 19aug08  asString was inherited"
 res := super asString .
 path ~~ nil ifTrue:[ res addAll: ': ...' ; addAll: (path last: (40 min: path size) )].
 ^ res

%


set class RubyFile
category: 'as yet unclassified'
method:
exists
	^ path notNil

%


set class RubyFile
category: '*maglev-runtime'
method:
featureName
    "Return the name of this file as appropriate for putting on $LOADED_FEATURES."
  ^ givenPath

%


set class RubyFile
category: 'as yet unclassified'
method:
fullPath
  ^ fullPath

%


set class RubyFile
category: '*maglev-runtime'
method:
fullPathFor: aPathOrName env: envId
"Search for aPathOrName on the current ruby search path.  If aPathOrName is absolute,
  then ignore ruby search path and just cannonicalize and test for existence.
  Return the fully cannonicalized path for the file if found, otherwise return nil."

  (aPathOrName beginsWith: '/') ifTrue: [|cannon|
    cannon := RubyFile cannonicalPathFor: aPathOrName .
   (GsFile existsOnServer: cannon) 
     ifTrue:[^ cannon]
     ifFalse:[^ nil]
  ].

  "aPathOrName is relative, so search for it on ruby search path"
  (RubyFile loadPath: envId) do: [:pathPrefix | | p |
    p := RubyFile cannonicalPathFor: (pathPrefix terminatedWith: $/ ) , aPathOrName  .
    (RubyFile isFile: p) ifTrue:[ ^ p]
  ].
  ^ nil . "File not found"

%


set class RubyFile
category: 'as yet unclassified'
method:
givenPath
	"Return the path as specified to the require or load expression."
	^ givenPath

%


set class RubyFile
category: 'as yet unclassified'
method:
hash
	^ path hash

%


set class RubyFile
category: '(as yet unclassified)'
method:
initialize
  traceLoad := 
      (SessionTemps current at:#MAGLEV_RubyFile_traceLoad otherwise: nil) == true .

%


set class RubyFile
category: '*maglev-runtime'
method:
initializeGivenPath: given fullPath: full loadName: ln 
    givenPath := given .
    fullPath := full .
    loadName :=  ln .
    path := full .
  self featureName

%


set class RubyFile
category: 'as yet unclassified'
method:
lastModified
	^ GsFile lastModificationOfServerFile: path

%


set class RubyFile
category: '*maglev-debugging'
method:
lineForOffset: aByteOffset
  "returns a one-based line number corresponding to the
   1-based byte offset."
  source ifNotNil:[ ^ source lineForOffset: aByteOffset].
  ^ -1

%


set class RubyFile
category: '*maglev-runtime'
method:
loadCextension: envId
  | lib status fpSiz initName strt end initFct | 
  lib := CLibrary named: fullPath . 
  (status := lib load) _isOneByteString ifTrue:[ RubyLoadError signal: status ].
  cLibrary := lib .
  strt := fullPath indexOfLastByte: $/ asciiValue startingAt: (fpSiz := fullPath size) .
  end := fullPath indexOfLastByte: $. asciiValue startingAt: fpSiz .
  initName := fullPath copyFrom: strt + 1 to: end - 1 .
  initName := 'Init_' , initName .
  initFct := CCallout library: lib name: initName result: #void args: #() .   
  initFct callCextension: nil with: #() block: nil ex: nil .

%


set class RubyFile
category: '*maglev-runtime'
method:
loadIntoEnv: envId
  | oldFile fileStk tns |
  cLibrary ifNotNil:[ 
    cLibrary == true ifFalse:[
      RubyLoadError signal:'shared library ' , loadName , ' already loaded'.
    ].
    self loadCextension: envId
  ] ifNil:[
    oldFile := (tns := Object transientNameSpaceForStore: envId )  at: #'__FILE__' otherwise: nil .
    tns  at: #'__FILE__'  compilePut: givenPath.
    self loadSource.
    fileStk := (RubyCompilerState initialize: envId) fileStack .
    fileStk push: self .

    ^ [ | comp |
        traceLoad ifTrue: [ 
            GsFile gciLogServer: '-- RubyFile>>load  : loading ', (RubyFile pathForTrace: path) 
        ] .
        comp := RubyCompiler new .
        comp compileFileNamed: path loadName: loadName env: envId
      ] ensure:[
        fileStk pop: self .
        tns  at: #'__FILE__'  compilePut: oldFile.             
      ]
  ]

%


set class RubyFile
category: '*maglev-runtime'
method:
loadName
  ^ loadName 

%


set class RubyFile
category: 'patches'
method:
loadSource
|file|
  file := GsFile openReadOnServer: path.
  file == nil ifTrue:[
    RubyLoadError signal: 'file not found , ' , path .
  ].
  source := file contents.
  file close

%


set class RubyFile
category: 'as yet unclassified'
method:
path
	^ path

%


set class RubyFile
category: '*maglev-runtime'
method:
setFilename: aString env: envId
  givenPath := aString .
  path := self fullPathFor: aString env: envId .
  loadName ifNil: [ loadName := aString ] .

%


set class RubyFile
category: '*maglev-runtime'
method:
setIsSharedLib 
  cLibrary := true .
  ^ self

%


set class RubyFile
category: 'as yet unclassified'
method:
source
	^ source

%

