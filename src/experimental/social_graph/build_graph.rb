# This script builds a database of randomly generated Person objects, and
# connects them via a friends link.
#
# The script takes a while to run, so it will only rebuild the graph if
# (A) it is forced to (pass the "force" option on the command line) or
# (B)there are no people in the database.
#


# Parameters to tune the database
NUM_PEOPLE         = 1_000_000
FRIENDS_PER_PERSON = 50
CHUNK_SIZE         = 10_000


# See if we need to run or not
force = ARGV[0] == 'force'
size = Maglev::PERSISTENT_ROOT[:Person].nil? ?
       0 : Maglev::PERSISTENT_ROOT[:Person].size
if not (force or size > 0)
  puts "-- Not running build_graph.rb: force: #{force} size #{size}"
  exit 0
end

# Main script

Maglev.persistent do
  load 'graph.rb'
end
Maglev::PERSISTENT_ROOT[:Person] = IdentitySet.new
Maglev.commit_transaction

require '../../../examples/persistence/names.rb'

root = Maglev::PERSISTENT_ROOT[:Person]


puts "=== Building #{NUM_PEOPLE} people with #{FRIENDS_PER_PERSON} friends each"
colors = [:red, :blue, :yellow, :blue_no_red]

people = []
(NUM_PEOPLE / CHUNK_SIZE).times do |i|
  puts "--[#{i}] Generating #{CHUNK_SIZE} people..."
  CHUNK_SIZE.times do |j|
    gender = rand(2) == 0 ? :male : :female
    name = RandomNameGenerator.name_for(gender)
    p = Person.new(name[0], name[1], colors.at(rand(4)))
    people << p # temporary, so we can get names
    root << p
  end
  Maglev.commit_transaction
end


puts "-- Adding friends..."
current = 0
root.each do |p|
  current += 1
  if current % CHUNK_SIZE == 0
    puts "commit #{current}"
    Maglev.commit_transaction
  end
  FRIENDS_PER_PERSON.times do |i|
    p.add_friend(people[rand(NUM_PEOPLE)])
  end
end
Maglev.commit_transaction

