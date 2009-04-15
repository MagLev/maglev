require 'smalltalk/System'
puts Smalltalk::System._st_inTransaction # Crashes system

require 'smalltalk/SymbolSet'
s = Smalltalk::SymbolSet._st_new(10)

require 'smalltalk/IdentityBag'
b = Smalltalk::IdentityBag._st_new(10)
puts b._st_hash
puts b._st_getIndexInfo
