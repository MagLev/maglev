*maglev-runtime
_singletonClassAllowedFor: aClass
 |classes metaClasses|
  classes := {Behavior. Module.  Array. GsFile.}.
  metaClasses := classes collect: [:cls | cls class].
  ^ ((classes includes: aClass) or: [metaClasses includes: aClass]) not
