# This file should be run only after create_random_people.rb has been run.
# create_random_people.rb needs to be run only once.  This file may be run
# multiple times.

require 'benchmark'

Benchmark.bm(37) do |x|
  selected_youngsters = nil
  x.report("Find youngsters (select without index)") {
    selected_youngsters = Person::RANDOM_PEOPLE.select {|p| p.age < 25}
  }
  puts "Found #{selected_youngsters.length} youngsters\n\n"

  youngsters = nil
  x.report("Find youngsters (search using index)  ") {
    youngsters = Person::RANDOM_PEOPLE.search([:@age], :lt, 25)
  }
  puts "Found #{youngsters.length} youngsters\n\n"

  s1, s2 = youngsters.size, selected_youngsters.size
  raise "Incompatible results Search: #{s1} Select: #{s2}" unless s1 == s2

  hermits = nil
  x.report("Find hermits                          ") {
    hermits  = Person::RANDOM_PEOPLE.search([:@marital_status], :eql, :hermit)
  }
  puts "Found #{hermits.length} hermits\n\n"

  young_hermits = nil
  x.report("Intersect youngsters with hermits     ") { young_hermits = hermits & youngsters }
  puts "Found #{young_hermits.length} young hermits"
end

