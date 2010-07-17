# MagLev doesn't correctly symbolize hashes.  The monkey patch to Hash
# comes from active_support/core_ext/hash/keys.rb
#
# MagLev messes up conversion of "adapter"=>"mysql"
# (turns into :adapter => nil).

class Hash
  # Return a new hash with all keys converted to symbols, as long as
  # they respond to +to_sym+.
  def symbolize_keys
    dup.symbolize_keys!
  end

  # Destructively convert all keys to symbols, as long as they respond
  # to +to_sym+.
  def symbolize_keys!
    keys.each { |key|
      #  puts "--- key = #{key}"
      # puts "   start  = #{self.inspect}" 
      sym = key.to_sym 
      val = delete(key) 
      #   puts "after del = #{self.inspect}" 
      self[sym] = val
      #   puts "     end = #{self.inspect}" 
      # nil.pause if sym._equal?(:username) 
    }
    self
  end
end

# This hash symbolizes ok
works = {
  "reconnect"=>false,
  "pool"=>5,
  "socket"=>"/opt/local/var/run/mysql5/mysqld.sock",
  "adapter"=>"mysql",
  "username"=>"root",
  "encoding"=>"utf8",
  "database"=>"myapp_test",
  "password"=>nil}

s = works.symbolize_keys
raise "FAIL works" if s[:adapter].nil?

# puts "=========== starting failure case"
# this one fails
ha = {
 # "pool"=>5,
 # "database"=>nil,
 # "password"=>nil,
  "username"=>"root",
  "encoding"=>"utf8",
  "reconnect"=>false,
  "adapter"=>"mysql",
 # "socket"=>"/opt/local/var/run/mysql5/mysqld.sock" 
 }

hb = ha.dup

hc = hb.symbolize_keys
raise "FAIL fails" unless hc[:adapter] == 'mysql'
true

