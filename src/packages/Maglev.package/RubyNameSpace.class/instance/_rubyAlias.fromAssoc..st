*maglev-runtime
_rubyAlias: newKey fromAssoc: oldAssoc
  "for use in VM initialization only"
   (self rubyGlobalVarAssoc:  newKey ) aliasTo: oldAssoc 

