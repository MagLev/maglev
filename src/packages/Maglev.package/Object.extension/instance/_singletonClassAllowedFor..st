*maglev-runtime
_singletonClassAllowedFor: aClass
 |classes metaClasses|
  classes := {Behavior. Module.  Array.}.
  metaClasses := classes collect: [:cls | cls class].
  ^ ((classes includes: aClass) or: [metaClasses includes: aClass]) not
