(as yet unclassified)
use: aState
  "for use by maglev irb implementation only"
  aState class == RubyCompilerState ifFalse:[ self error:'invalid argument'].
  GsProcess _current _rubyThreadDataAt: 1 put: aState .
 

