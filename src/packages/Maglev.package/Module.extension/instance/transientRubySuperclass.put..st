*maglev-runtime
transientRubySuperclass: envId put: aCls
  "NOTE, VM class creation and fault-in initializes transientMethDicts
   to an empty array. "
  self isCommitted ifTrue:[ (SessionTemps current at: #RubyPinnedClasses) add: self ].
  transientMethDicts _rubyAt: (envId*4 + 2"zero based") put: aCls

