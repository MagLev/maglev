*maglev-runtime
rubyMethodProtection: envId
  | ns cvs  |
  self isRubyModuleFunctions ifFalse:[ 
    (ns := self nameSpace: envId ) ifNotNil:[ ^ ns methodProtection ].
  ].
  ^ 0

