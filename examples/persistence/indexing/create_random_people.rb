# create_random_people.rb
#
# Creates a set of random Person objects.
#
# Usage: maglev-ruby create_random_people.rb [num] [commit]
#
#   if [num] is given, then creates that many people in the set (default
#   1_000).
#
#   if "commit" is given, then commits the collection to
#   Person::RANDOM_PEOPLE


population_size = ARGV.length > 0 ? ARGV.shift.to_i : 1_000
commit_p = ARGV[0] == 'commit'

# Step One: Create an indexable collection
#
# MagLev indexes are supported only on unordered collections (currently,
# IdentitySet, but in the future there should be Bag, IdentityBag and Set).
# An IdentitySet is a Set (i.e., each element occurs only once) and it uses
# Object identity (equal?)  rather than object equality (eql?).

people = IdentitySet.new

# Step Two:,  Add an Index
#
# We add an index on the age field of elements of the set.  We can then
# efficiently search and sort the set by the age attribute of its elements.
# NOTE: this sorting is done on the @age instance variable, and does not
# depend on an instance method named "age".  We will put People objects in
# the set.  People have a name, age, gender and address.
people.create_equality_index('@age', Fixnum)

# Now we add people to the set.  As each object is added to the set, the
# indexing subsystem will update its data structures to keep the population
# indexed on the age field.
population_size.times { people << Person.random }

if commit_p
  Maglev.persistent { Person::RANDOM_PEOPLE = people }
  Maglev.commit_transaction
  puts "Committed #{people.size} to Person::RANDOM_PEOPLE"
end
