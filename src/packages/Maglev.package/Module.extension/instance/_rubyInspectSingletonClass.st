*maglev-runtime
_rubyInspectSingletonClass
  |str|
  str := '#<Class:' copy.
  str addAll: primaryCopy _rubyInspect.
  str addAll: '>'.
  ^ str
