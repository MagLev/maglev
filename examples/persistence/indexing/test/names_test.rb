
require 'test/unit'
require 'names'

class TestRandomNameGenerator < Test::Unit::TestCase

  NUM_NAMES = 100

  def is_female(name)
    RandomNameGenerator::FEMALE_NAMES.include? name[0]
  end

  def is_male(name)
    RandomNameGenerator::MALE_NAMES.include? name[0]
  end

  def test_name_for_with_nil_gender
    males = females = 0
    NUM_NAMES.times do |i|
      n = RandomNameGenerator.name_for
      females += 1 if is_female(n)
      males   += 1 if is_male(n)
    end

    assert(females + males == NUM_NAMES, "All names should be either male or female")
    assert(females > 0, "There should be at least some females")
    assert(males > 0,   "There should be at least some males")
  end

  def test_name_for_with_non_nil_gender
    males, females = helper(:male)
    assert(females + males == NUM_NAMES, "All names should be either male or female")
    assert(females == 0, "There should be no females with :male")
    assert(males == NUM_NAMES, "There should be #{NUM_NAMES} males with :male")

    males, females = helper(:female)
    assert(females + males == NUM_NAMES, "All names should be either male or female")
    assert(females == NUM_NAMES, "There should be no females with :female")
    assert(males == 0, "There should be 0 males with :female")
  end

  def helper(gender)
    males = females = 0
    NUM_NAMES.times do |i|
      n = RandomNameGenerator.name_for(gender)
      females += 1 if is_female(n)
      males   += 1 if is_male(n)
    end
    [males, females]
  end

end
