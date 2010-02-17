# An address is a street, city, state and zip.  Address knows how to make
# random-ish addresses

require 'utils'

class Address

  # Data for random creation of addresses
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

  # Create and return a randomly generated address
  def self.random
    Address.new(rand(9500), STREETS.at_random, CITIES.at_random,
      STATES.at_random, ZIPS.at_random)
  end
end
