*maglev-runtime
_rubyInspectSingletonClass
  |str|
  str := '#<Class:' copy.
  str addAll: destClass _rubyInspect.
  str addAll: '>'.
  ^ str
