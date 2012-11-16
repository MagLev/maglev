*maglev-runtime
default
  | val  |
  val := SessionTemps current at: #DefaultRubyContext otherwise: nil .
  val ifNil:[ 
     self error:'RubyContext default   used prior to   RubyContext load'.
  ].
  ^ val

