require 'maglev/repository'
Maglev.persistent do
  class Quux
    attr_reader :id
    def initialize(i)
      @id = i
    end
  end
end
Maglev.commit_transaction

root = Maglev::PERSISTENT_ROOT[:quux_test] = Array.new

root << Quux.new(0)  # Should find this one

q1 = Quux.new(1)
def q1.foo
  puts "I have a singleton class now"
end

root << q1
Maglev.commit_transaction

puts "-- Searching for Quux instances..."
instances = Maglev::Repository.instance.list_instances([Quux])[0]
puts "-- Done searching"

raise "Did not find any Quux instances" unless instances.size > 0

# There may be many instances of a Quux with id 0, since this script may
# have been run a number of times.  So, we can't just go on size.  We need
# to search it to see if a Quux with id 1 was found by list_instances.
id_0_count = instances.count {|el| el.id == 0}
id_1_count = instances.count {|el| el.id == 1}
puts "-- Found #{id_0_count} instances with id == 0"
puts "-- Found #{id_1_count} instances with id == 1"
raise "Failed to find any instances with id == 1" unless id_1_count > 0

