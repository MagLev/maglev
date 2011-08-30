# This is an example of MagLev persistence on unordered collections with
# indexing.

require 'benchmark'
require 'person'

# Indexes are used to search a collection based on the value of certain
# attributes of its members.

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


# The code in the block loads up our indexed collection with 100,000 random
# People objects, then it searches for various people in the collection.
# The Benchmark reports the time it took to create/insert the people, and
# the time it takes to do several searches.
Benchmark.bm do |x|
  population = 100_000
  youngsters = nil
  x.report("Create #{population} People in indexed set") {
    population.times { people << Person.random }
  }

  x.report("Find the youngsters") {
    youngsters = people.search([:@age], :lt, 25)
  }
  puts "Found #{youngsters.length} youngsters"

  # This shows doing a query on an indexed field (age) and a non-indexed
  # field (marital_status).  We then intersect the sets to get the
  # result. Since this includes a search on an non-indexed field, this
  # results in a table scan.  You could put an index on the @marital_status
  # field to turn it into a fully indexed query.
  old_hermits = nil
  x.report("Find old hermits") {
    old_ones = people.search([:@age], :gte, 75)
    hermits  = people.search([:@marital_status], :eql, :hermit)
    old_hermits = hermits & old_ones
  }

  # print some old hermits
  puts "="*20, " Some old hermits...", "="*20
  count = 0
  old_hermits.each do |h|
      break if count > 10
      count += 1
      puts h
  end

  # We can add another index and use it as well.  Here we index on the zip
  # code of the person's address.  This is a "multi-level" index.
  people.create_equality_index('@address.@zip', Fixnum)

  # now, search for young people in the lucrative 45678 zip code
  puts "="*20, " Some lucrative youngsters...", "="*20
  lucrative_youngsters = nil
  x.report("Youngsters in 45678") {
    young_ones = people.search([:@age], :lte, 25)
    lucrative  = people.search([:@address, :@zip], :eql, 45678)
    lucrative_youngsters = young_ones & lucrative
  }
  count = 0
  lucrative_youngsters.each do |h|
      break if count > 10
      count += 1
      puts h
  end
end

# The indexes are permanently stored, and must be cleaned up explicitly.
# You can remove an individual index, e.g., people.remove_index('age'), but
# we want to remove all of them in this case:
people.remove_all_indexes
