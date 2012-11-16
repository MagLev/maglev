*maglev-runtime
_isLoaded: givenPath with: features 
    "Searches the $LOADED_FEATURES global to see if givenPath, or
    givenPath + .rb, is on it."
    |  x |
    (features detect: [ :f| f = givenPath ] ifNone: [ nil ]) ifNotNil: [ ^ true ] .
	
    x := givenPath, '.rb' .
    (features detect: [ :f| f = givenPath ] ifNone: [ ^ false ]) ifNotNil: [ ^ true ] .
    ^ false
	

