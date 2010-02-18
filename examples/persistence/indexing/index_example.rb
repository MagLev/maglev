# This is an example of maglev persistence on unordered collections with
# indexing.

require 'benchmark'
require 'person'

# Indexes are used to find elements in an otherwise unordered collection.
# E.g., Hashes already have a way of efficiently finding an element, but
# sets do not.

# First, we create an IdentitySet (an unordered collection)
people = IdentitySet.new

# Second, we add an index on the age field of elements of the set.  We can
# then efficiently search and sort by age on the set.  We will put People
# objects in the set.  People have a name, age, gender and address.
people.create_index('age', Fixnum)

Benchmark.bm do |x|
#  population = 100_000
  population = 1_000
  youngsters = nil
  x.report("Create #{population} People in indexed set") {
    population.times { people << Person.random }
  }

  x.report("Find the youngsters") {
    youngsters = people.select([:age], :<, 25)
  }
  puts "Found #{youngsters.length} youngsters"

  # This shows doing a query on an indexed field (age) and a non-indexed
  # field (marital_status).  We then intersect the sets to get the result
  old_hermits = nil
  x.report("Find old hermits") {
    old_ones = people.select([:age], :>=, 75)
    hermits  = people.select([:marital_status], :==, :hermit)
    old_hermits = hermits * old_ones
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
  people.create_index('address.zip', Fixnum)

  # now, search for young people in the lucrative 45678 zip code
  puts "="*20, " Some lucrative youngsters...", "="*20
  lucrative_youngsters = nil
  x.report("Youngsters in 45678") {
    young_ones = people.select([:age], :<, 26)
    lucrative  = people.select([:address, :zip], :==, 45678)
    lucrative_youngsters = young_ones * lucrative
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
