#
# drop-in replacement for Ramaze's built-in MemoryCache built on the Sequel.
# to use with sessions do
#
#   Ramaze::Global::cache_alternative[:sessions] = Ramaze::SequelCache
#
# to use with everything do
#
#   Ramaze::Global::cache = Ramaze::SequelCache
#

class Ramaze::SequelCache
  class Table < Sequel::Model(:ramaze_cache_t)
    set_schema do
      primary_key :id
      string :key
      string :value
      index :key, :unique => true
    end

    ## use sequel's built-in serialize function if you like execptions: the
    ## serialized data blows up the sql
    ## serialize :value, :format => :marshal

    transform :value => [
      lambda{|value| Marshal.load(Base64.decode64(value))},
      lambda{|value| Base64.encode64(Marshal.dump(value))},
    ]
  end

  def table() Table end
  def t() Table end

  def self.[] key
    record = Table.find :key => key
    record ? record.value : nil
  end

  def self.[]= key, value
    begin
      Table.create :key => key, :value => value
    rescue
      Table.filter(:key => key).update :value => value
      unless Table.find(:key => key)
        Table.create :key => key, :value => value rescue nil
      end
    end
    # ruby always returns 'value' for []= !
  rescue
    nil
  end

  def self.values_at *keys
    keys.map{|key| Table[key]}
  end

  def self.delete *keys
    keys.map do |key|
      record = Table[key]
      record.delete if record
    end
  end

  def self.clear
    Table.delete_all
  end

  def self.new
    self
  end

  def self.to_sym
    name.split(%r/::/).last.to_sym
  end
end




if $0 == __FILE__
  case ARGV.first
    when /create|up/
      Ramaze::SequelCache::Table.create_table!
    when /drop|down/
      Ramaze::SequelCache::Table.drop_table
    else
      abort "usage: ruby #{ __FILE__ } create/up | drop/down"
  end
else
  Ramaze::SequelCache::Table.create_table! unless
    Ramaze::SequelCache::Table.table_exists?
end
