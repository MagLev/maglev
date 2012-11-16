*maglev-override-Accessing
hasCSymbol: aString

"Return true if dlsym() finds specified name in the shared library
 represented by receiver. dladdr() used if available to qualify
 the result of dlsym() to within receiver's shared library.

 dladdr() not implemented on AIX, so this method will return
 true if the specified name exists in any library loaded thus
 far into the process."

| dladdrLibNam expName | 
dladdrLibNam := self _libNameForSymbol: aString .
self isLoaded ifFalse:[ ^ false ].
dladdrLibNam ifNil:[ ^ false ].
dladdrLibNam == true ifTrue:[
  "found by dlsym() and dladdr() not supported by OS"
  ^ true 
].
(dladdrLibNam endsWith: '.dylib') ifTrue: [ "Mac OS X"
  "Some mismatches in in library handling prevent us to compare
    our name to the name as returned from the system. However,
    we should already be certain that the  lib has the symbol, the VM
    ought already have a handle to the lib and if we reach here, we know
    that the symbol was found in the lib."
    ^ true].
expName := self expandedName .
"ignore version numbers on .so  which is target of a symlink"
(dladdrLibNam at: 1 equals: expName ) ifTrue:[ ^ true ].
"else, check whether the expanded name of the found lib matches ours"
^ (self class named: dladdrLibNam) expandedName at: 1 equals: expName.
