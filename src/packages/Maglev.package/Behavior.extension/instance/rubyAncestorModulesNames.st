*maglev-runtime
rubyAncestorModulesNames
  | arr res sz  |
  arr := self theNonMetaClass rubyIncludedModules .
  res := Array new: (sz := arr size).
  1 to: sz do:[:n | res at: n put: (arr at: n) name ].
  ^ res

