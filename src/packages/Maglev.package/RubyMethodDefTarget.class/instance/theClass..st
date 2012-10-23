*maglev-runtime
theClass: aClass
  theClass := aClass .
  GsProcess _current _rubyThreadDataAt: 7 put: aClass .

