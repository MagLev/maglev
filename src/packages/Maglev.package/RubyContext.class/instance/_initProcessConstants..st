*maglev-runtime
_initProcessConstants: envId 
  "executed during VM startup
   to define OS dependent constants in module Fcntl "
  | arr tns procCls  assoc |
  assoc := (Object transientNameSpace: envId) resolveConstant: #Process .
  assoc ifNotNil:[ 
     procCls := assoc _valueNoAction .  
    arr := RubySocket _socketOsConstants: 4 .
    tns := procCls transientNameSpaceForStore: envId .
    1 to: arr size by: 2 do:[:k | | nam val |
      nam := (arr at: k) asSymbol .
      val := (arr at: k + 1) .
      tns at: nam transientRuntimePut: val . 
    ].
    SessionTemps current at:#RUBY_Process putNoStub: procCls "protect in-memory copy of Process from GC"
  ].  "ifNil:  do nothing at start of bootstrap "

