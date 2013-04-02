*maglev-runtime
_rubyInspectSingletonClass
  |str|
  str := '#<Class:' copy.
  destClass == nil
    ifTrue: [str addAll: self primaryCopy _rubyInspect]
    ifFalse: [str addAll: destClass _rubyInspect].
  str addAll: '>'.
  ^ str
