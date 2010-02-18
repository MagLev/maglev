require 'test/unit'
require 'person'
require 'benchmark'

class TestIndex < Test::Unit::TestCase

  NUM = 100

  def setup
    # Create an IdentitySet with an index on last name
    @people = IdentitySet.new
    @people.create_index('age', Fixnum)
    NUM.times { @people << Person.random }
  end

  def teardown
    # Must remove the index, since they are permanent (their lifetime
    # may be greater than the lifetime of the associated collection).
    @people.remove_index('age')
    @people = nil
  end

  def test_one_level_select
    elderly = @people.select([:age], :>=, 55)
    assert_not_nil(elderly, "The result should not be nil")
    assert_operator(elderly.size, :>=, 1, "Ought to be at least one old person")
  end

  def test_index_speeds_up_search
    # Create a large un-indexed collection
    many = 250_000
    many_people = IdentitySet.new
    many.times { many_people << Person.random }

    results = Array.new
    times   = Array.new
    times << Benchmark.measure do
      results << many_people.select([:age], :<, 25)
    end

    begin
      # Create an index and then re-measure
      many_people.create_index('age', Fixnum)
      times << Benchmark.measure do
        results << many_people.select([:age], :<, 25)
      end

      splits = times.map{ |tms| [tms.utime, tms.stime, tms.total, tms.real] }
      print_splits(splits)
      assert_operator(splits[0][2], :>, splits[1][2])
    ensure
      puts "Removing index..."
      many_people.remove_index('age')
    end
  end

  def print_splits(splits)
    puts
    puts "               #{Benchmark::Tms::CAPTION}"
    puts "No Index:      %10.6f %10.6f %10.6f (%10.6f)\n" % splits[0]
    puts "Index:         %10.6f %10.6f %10.6f (%10.6f)\n" % splits[1]
  end
end
