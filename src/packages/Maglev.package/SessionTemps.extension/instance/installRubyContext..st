*maglev-runtime
installRubyContext: ctx

  self at: #DefaultRubyContext put: ctx ;
       at:#RubyPinnedClasses put: (IdentitySet new _setNoStubbing) .

