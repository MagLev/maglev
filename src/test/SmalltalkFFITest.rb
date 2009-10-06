dirname = "#{ENV['MAGLEV_HOME']}/lib/ruby/site_ruby/1.8/smalltalk"
unless File.directory?(dirname)
  Dir.chdir "#{ENV['MAGLEV_HOME']}" do
    puts "generating stwrappers"
    system 'rake stwrappers'
  end
end

require 'smalltalk/System'
puts Smalltalk::System._st_inTransaction # Crashes system

require 'smalltalk/SymbolSet'
s = Smalltalk::SymbolSet._st_new(10)

require 'smalltalk/IdentityBag'
ibcls = Smalltalk::IdentityBag
b = ibcls._st_new(10)
puts b._st_hash
puts b._st_getIndexInfo

n = ibcls.name
unless n == 'Smalltalk::IdentityBag' ; raise 'error' ; end
true
