unless File.directory?("#{ENV['MAGLEV_HOME']}/lib/ruby/site_ruby/1.8/smalltalk")
  Dir.chdir "#{ENV['MAGLEV_HOME']}" do
    system 'rake dev:stwrappers'
  end
end

require 'smalltalk/System'
puts Smalltalk::System._st_inTransaction # Crashes system

require 'smalltalk/SymbolSet'
s = Smalltalk::SymbolSet._st_new(10)

require 'smalltalk/IdentityBag'
b = Smalltalk::IdentityBag._st_new(10)
puts b._st_hash
puts b._st_getIndexInfo
