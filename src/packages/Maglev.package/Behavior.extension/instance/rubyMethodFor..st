*maglev-runtime
rubyMethodFor: aSymbol
  "a ruby primitive.
  Returns a non-bridge instance of GsNMethod, or signals a NameError"
      "ruby_selector_suffix dependent"
  | cm envId arr sym selPrefix |
  envId := 1"__callerEnvId" .
  "inline  lookupRubyCallStarB:env: "
  selPrefix := aSymbol prefixIfRubySelector .
  sym := selPrefix _asSymbolWithRubySuffix: 16r3 " #0*& "  .
  arr := self lookupSelector: sym rubyEnv: envId .
  arr ifNotNil:[
    (arr at: 2) == METH_lu_undef_ed ifTrue:[ 
      NameError signal:'method ', aSymbol , ' has been undef-ed'
    ].
    cm := arr at: 1 .
    cm isRubyBridgeMethod ifTrue:[ | nonBrMeth |
      nonBrMeth := cm _nonBridgeMethod .
      nonBrMeth ifNil:[ NameError signal:'cannot find non-bridge method for ' , aSymbol ].
      cm := nonBrMeth.
    ].
    ^ cm 
  ] ifNil:[
    NameError signal:'no method ''' , aSymbol prefixIfRubySelector , 
            ''' for class ''' , (self @ruby1:name ) , '''' 
  ].

