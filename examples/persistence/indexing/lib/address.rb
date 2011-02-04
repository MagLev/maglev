# An address is a street, city, state and zip.  Address knows how to make
# random-ish addresses

require 'utils'

class Address

  # Currently, only the instvars passed to __fixed_instvars are indexable.
  # Even though the examples do not index on all inst vars, they are all
  # listed here so that it is easy to play with the examples.
  #
  # See comments about fixed vs dynamic inst vars in ../README.rdoc on
  self.__fixed_instvars :@street, :@city, :@state, :@zip

  # Data for random creation of addresses
  STREETS = ['Main', 'Spruce', 'Robinson Ln', 'Taylor Ave.', '43rd Ave']
  CITIES = ['Portland', 'AnyTown', 'Roseville', 'Santa Cruz', 'Bellingham',
    'Fort Collins', 'Berkeley', 'Yuma', 'Tuscon', 'Vermillion', 'St. Louis']
  STATES = ['AZ', 'CA', 'CO', 'MO', 'WA', 'OR', 'SD']
  ZIPS = [12345, 23456, 34567, 45678, 56789, 67890]

  attr_reader :street, :city, :state, :zip

  def initialize(number, street, city, state, zip)
    @number, @street, @city, @state, @zip = number, street, city, state, zip
  end

  def to_s
    "#{@number} #{@street}, #{@city}, #{@state}  #{@zip}"
  end

  # Create and return a randomly generated address
  def self.random
    Address.new(rand(9500), STREETS.at_random, CITIES.at_random,
      STATES.at_random, ZIPS.at_random)
  end
end
