# This is an example of maglev persistence on unordered collections with
# indexing.
#
$:.unshift File.dirname(__FILE__)

require 'benchmark'
require 'names'

class Array
  # Pick an element at random from the array (from the facets library)
  def at_random
    at(rand(size))
  end
end

# TODO: Could make a name class that has first and last fields

class Address
  STREETS = ['Main', 'Spruce', 'Robinson Ln', 'Taylor Ave.', '43rd Ave']
  CITIES = ['Portland', 'AnyTown', 'Roseville', 'Santa Cruz', 'Bellingham',
    'Fort Collins', 'Berkeley', 'Yuma', 'Tuscon', 'Vermillion', 'St. Louis']
  STATES = ['AZ', 'CA', 'CO', 'MO', 'WA', 'OR', 'SD']
  ZIPS = [01234, 12345, 23456, 34567, 45678]

  attr_reader :street, :city, :state, :zip

  def initialize(number, street, city, state, zip)
    @number, @street, @city, @state, @zip = number, street, city, state, zip
  end

  def to_s
    "#{@number} #{@street}, #{@city}, #{@state}  #{@zip}"
  end

  def self.random
    Address.new(rand(9500), STREETS.at_random, CITIES.at_random,
      STATES.at_random, ZIPS.at_random)
  end
end

class Person
  MARITAL_STATUS = [:single, :married, :hermit]

  attr_reader :name, :age, :gender, :address

  def initialize(name, age, gender, address, marital_status=:single)
    @name, @age, @gender, @address, @marital_status = name, age, gender, address, marital_status
  end

  def to_s
    "#{@name} is a #{@age} year old, #{@marital_status} #{@gender}, and lives at: #{@address}"
  end

  def self.random
    gender = [:male, :female].at_random
    first_name, last_name = RandomNameGenerator.name_for(gender)
    Person.new( "#{first_name} #{last_name}",
      rand(75) + 18,  # only legal adults, please...but no ageism...
      gender,
      Address.random,
      MARITAL_STATUS.at_random)
  end
end

# Indexes are used to find elements in an otherwise unordered collection.
# E.g., Hashes already have a way of efficiently finding an element, but
# sets do not.  We will create an unordered collection, then add an index
# on the age field of elements of the set.  We can then efficiently search
# and sort by age on the set.
people = IdentitySet.new
people._create_index('age', Fixnum)

Benchmark.bm do |x|
  population = 100_000
  youngsters = nil
  x.report("Create #{population} People in indexed set") {
    100_000.times { people << Person.random }
  }

  x.report("Find the youngsters") {
    youngsters = people._select([:age], :<, 25)
  }
  puts "Found #{youngsters.length} youngsters"

  # This shows doing a query on an indexed field (age) and a non-indexed
  # field (marital_status).  We then intersect the sets to get the result
  old_hermits = nil
  x.report("Find old hermits") {
    old_ones = people._select([:age], :>=, 75)
    hermits  = people._select([:marital_status], :==, :hermit)
    old_hermits = hermits * old_ones
  }

  count = 0
  old_hermits.each do |h|
      break if count > 10
      count += 1
      puts h
  end
end

# TODO: Clean up the indexes.  The indexes are permanently stored, and
# must be cleaned up explicitly.
people._remove_index('age')
