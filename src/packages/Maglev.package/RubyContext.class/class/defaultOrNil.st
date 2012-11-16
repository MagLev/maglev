*maglev-runtime
defaultOrNil
  ^ SessionTemps current at: #DefaultRubyContext otherwise: nil .

