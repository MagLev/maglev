*maglev-runtime
rubyMethodFor: aSymbol env: envId
  "Returns an instance of RubyMeth, or signals a NameError.  Caller should
     send object: to the RubyMeth"
      "ruby_selector_suffix dependent"
  | cm m |
  cm := self lookupRubyCallStarB: aSymbol env: envId .
  cm ifNotNil:[ 
    cm isRubyBridgeMethod ifTrue:[ | nonBrMeth |
      nonBrMeth := cm _nonBridgeMethod .
      nonBrMeth ifNil:[ NameError signal:'cannot find non-bridge method for ' , aSymbol ].
      cm := nonBrMeth.
    ].
  ] ifNil:[ NameError signal:'no method ''' , aSymbol prefixIfRubySelector ,
             ''' for class ''' , (self @ruby1:name ) , '''' 
  ].
  (m := RubyMeth _basicNew) method: cm env: envId selPrefix: aSymbol prefixIfRubySelector;
      bridge: (RubyBridge execMethBridgeTo: cm) .
  ^ m 

