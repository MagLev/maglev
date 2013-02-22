dirname = "#{ENV['MAGLEV_HOME']}/lib/ruby/site_ruby/1.9/smalltalk"
unless File.directory?(dirname)
  Dir.chdir "#{ENV['MAGLEV_HOME']}" do
    puts "generating stwrappers"
    status = system('rake stwrappers')
    if status.equal?(true)
      puts 'rake stwrappers ok'
    else
      raise 'rake stwrappers NOT successful'
    end
  end
else
  puts "stwrappers exists"
end

require 'smalltalk/System'
puts Smalltalk::System._st_inTransaction # Crashes system

require 'smalltalk/SymbolSet'
s = Smalltalk::SymbolSet._st_new_(10)

require 'smalltalk/IdentityBag'
ibcls = Smalltalk::IdentityBag
b = ibcls._st_new_(10)
puts b._st_hash
puts b._st_getIndexInfo

n = ibcls.name
unless n == 'Smalltalk::IdentityBag' ; raise 'error' ; end
true
