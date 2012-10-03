*maglev-runtime
requireFileNamed: aString env: envId
   "Find file named by aString (using ruby load path, and appending .rb as necessary).
    If the file has not been loaded, then load it and return true.  
    Adds the file to the transient loaded features.
    If persistenceMode==true, adds the file to the persistent loaded features.
  "
  | pm file tfeatures pfeatures fnam cst rtModuStk prevLexPath ary |
  ary := RubyFile findRubyFileFor: aString isRequire: true env: envId .
  file := ary at: 1 .
  tfeatures := self transientLoadedFeatures: envId .
  (self _isLoaded: (ary at: 2 ) with: tfeatures) ifTrue: [ 
      ^ false . "Already loaded, so skip"
  ].
  file ifNil: [ RubyLoadError signal: 'no such file to load -- ', aString ].
  cst := RubyCompilerState current .
  pm := cst persistenceMode .
  tfeatures add: (fnam := file featureName) .  "loadedFeatures is an Array"
  pm ifTrue:[ (pfeatures := self persistentLoadedFeatures:envId ) add: fnam ].
  rtModuStk := cst rtModuleStack .
  prevLexPath := rtModuStk copy .
  [
    rtModuStk size: 0 ; add: Object .
    [
      self withFile: file do: [:f|  f loadIntoEnv: envId ] .
    ] onSynchronous: AbstractException do:[:ex |
      tfeatures removeLast .
      pm ifTrue:[ pfeatures removeLast ].
      ex pass 
    ].
  ] ensure:[
    rtModuStk size: 0 ; addAll: prevLexPath . 
  ].
  ^ true .

