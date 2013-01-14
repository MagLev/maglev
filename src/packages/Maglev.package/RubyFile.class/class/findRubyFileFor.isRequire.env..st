*maglev-runtime
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

