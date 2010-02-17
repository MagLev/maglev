# A person has a name, age, gender, address and marital status
# Person can generate random people.

require 'names'

class Person
  MARITAL_STATUS = [:single, :married, :hermit]

  attr_reader :name, :age, :gender, :address

  def initialize(name, age, gender, address, marital_status=:single)
    @name, @age, @gender, @address, @marital_status = name, age, gender, address, marital_status
  end

  def to_s
    "#{@name} is a #{@age} year old, #{@marital_status} #{@gender}, and lives at: #{@address}"
  end

  # Generate a person from random data
  def self.random
    gender = [:male, :female].at_random
    first_name, last_name = RandomNameGenerator.name_for(gender)
    Person.new( "#{first_name} #{last_name}",
      rand(75) + 18,  # only legal adults, please...
      gender,
      Address.random,
      MARITAL_STATUS.at_random)
  end
end
