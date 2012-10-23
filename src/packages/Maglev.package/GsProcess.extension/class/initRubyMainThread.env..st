*maglev-runtime
initRubyMainThread: inPrims env: envId
  "returns the previous main program self , or nil if this is the
   first invocation in a ruby session."

| prevSelf tmps oMain |
prevSelf := (tmps := SessionTemps current) at: #RubyMainSelf otherwise: nil.
prevSelf ifNil:[
  tmps at:#RubyMainSelf put: (oMain := Object new) ;
     at:#RubyMainThread put: self _current ;
     at:#RubyDefaultThreadGroup put: RubyThreadGroup new ;
     at:#RubyExitHandlers put: Array new .
  inPrims ifFalse:[ 
     RubyContext @ruby1:customize_top_self: oMain .
     System _sessionStateAt: 19 put: RubyIdentityHash new . "dict of RubyFinalizer's " 
  ] .
  tmps at:#RubyMainInitialized put: true .  "for use in debugging AST to IR code"
  System abortTransaction . "abort changes to commited AST from IR generation of top_self"
].
^ prevSelf

