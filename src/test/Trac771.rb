# Example from pickaxe

$singleton_names = []

module Chatty
  def self.singleton_method_added(id)
    $singleton_names << id
    puts "Adding #{id.inspect} to #{self.name}"
  end

  def self.singleton_method_removed(id)
    $singleton_names.delete(id)
    puts "Removing #{id.inspect} from #{self.name}"
  end

  def self.singleton_method_undefined(id)
    $singleton_names.delete(id)
    puts "undef #{id.inspect} from #{self.name}"
  end

  def self.one
  end

  def self.two
  end

  def three
  end
end

def Chatty.three
end

expected = [:singleton_method_added, :singleton_method_removed, :singleton_method_undefined,
            :one, :two, :three]
raise "FAIL: Actual: #{$singleton_names.inspect} expected: #{expected.inspect}" unless $singleton_names == expected

module Chatty
  class << self
    remove_method :one
  end
end

raise "FAIL: one not removed" if $singleton_names.include? :one

module Chatty
  class << self
    undef_method :two
  end
end


raise "FAIL: two not removed" if $singleton_names.include? :two
