# This is (going to be) an example of maglev persistence on unordered
# collections with indexing.
#
# Status: As of 2009-02-23, we can do the indexing of the unordered
# collections from ruby, but we don't have a way of doing the selects...
#
# This example is on hold.
#
class Array
  # Pick an element at random from the array (from the facets library)
  def at_random
    at(rand(size) - 1)
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
  MALE_NAMES = [ 'Peter', 'Jon', 'Allen', 'Martin', 'Monty',
    'Bob', 'Paul', 'Sir' ]
  FEMALE_NAMES = ['Cleopatra', 'Helen', 'Joan', 'Lady', 'Mary', 'Dame']
  LAST_NAMES   = ['de Rothschild', 'Public', 'Doe', 'Smith', 'McLain',
    'Williams', 'McClure', 'Otis', 'Walker', 'Not appearing in this film']
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
    first_name = (gender == :male ? MALE_NAMES : FEMALE_NAMES).at_random
    Person.new( "#{first_name} #{LAST_NAMES.at_random}",
      rand(75) + 18,  # only legal adults, please...but no ageism...
      gender,
      Address.random,
      MARITAL_STATUS.at_random)
  end
end

all_people = IdentitySet.new
all_people._create_index 'age'

10.times do |i|
  all_people << Person.random
end

all_people.each { |p| puts p }

