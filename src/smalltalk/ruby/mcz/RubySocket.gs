
set class RubySocket class
category: '*maglev-runtime'
method:
_initTransientSocketConstants: envId

  "executed during VM startup
   to define OS dependent constants such as  Socket::SO_REUSEADDR 
   The constants need to go into two places (A) Socket (B) Socket::Constants"
  | arr  tns  constants ctns |
  arr := self _socketOsConstants: 0  .
  tns := self transientNameSpaceForStore: envId .
  constants := self rubyConstAt: #'Constants' env: envId .
  ctns := constants transientNameSpaceForStore: envId .
  1 to: arr size by: 2 do:[:k | | nam val |
     nam := (arr at: k) asSymbol .
     val := (arr at: k + 1) .
     tns at: nam transientRuntimePut: val .
     ctns at: nam transientRuntimePut: val .
  ].
  SessionTemps current at:#RUBY_RubySocket putNoStub: self "protect in-memory copy of class from GC"

%

