*maglev-runtime
_initFcntlConstants: envId 
  "executed during VM startup
   to define OS dependent constants in module Fcntl "
  | arr  tns fcntl  assoc |
  assoc := (Object transientNameSpace: envId) resolveConstant: #Fcntl .
  assoc ifNotNil:[ 
	 fcntl := assoc _valueNoAction .  
    arr := RubySocket _socketOsConstants: 1 .
    tns := fcntl transientNameSpaceForStore: envId .
    1 to: arr size by: 2 do:[:k | | nam val |
      nam := (arr at: k) asSymbol .
      val := (arr at: k + 1) .
      tns at: nam transientRuntimePut: val . 
    ].
    SessionTemps current at:#RUBY_Fcntl putNoStub: fcntl "protect in-memory copy of Fcntl from GC"
  ].  "ifNil:  do nothing at start of bootstrap "

