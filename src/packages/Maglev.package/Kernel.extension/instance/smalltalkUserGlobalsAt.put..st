*maglev-runtime
smalltalkUserGlobalsAt: aName put: aValue
  "intended for bootstrap only"
  | assoc |
  RubyCompilerState current installingPrims ifFalse:[
	self error:'storing into UserGlobals not allowed outside of bootstrap'
  ].
  UserGlobals at: aName put: aValue .

