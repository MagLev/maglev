*maglev-runtime
theMetaClass: aClass
  | cls |
  theClass := (cls := aClass _classForRubyClassVar ) .
  GsProcess _current _rubyThreadDataAt: 7 put: cls .

