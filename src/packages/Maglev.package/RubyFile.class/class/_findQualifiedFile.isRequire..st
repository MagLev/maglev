*maglev-runtime
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

