
set class RubySocket
category: '*maglev-runtime'
classmethod:
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
  "Github Issue #135. Missing Socket::SOL_TCP.
  Use IPPROTO_TCP for SOL_TCP if SOL_TCP isn't defined. See
  e.g. wine commit 55a103ca5cc7604f979ed8e9919a5f95d233d58e for
  reference. We should add SOL_TCP to the list of constants the
  primitive recovers, but we want to avoid changing the C code for MagLev right now.
  FIXME: Get a student to do the right thing, once we can"
  tns at: #SOL_TCP transientRuntimePut: (tns at: #IPPROTO_TCP ifAbsent: [nil]).
  ctns at: #SOL_TCP transientRuntimePut: (tns at: #IPPROTO_TCP ifAbsent: [nil]).
  SessionTemps current at:#RUBY_RubySocket putNoStub: self "protect in-memory copy of class from GC"

%

