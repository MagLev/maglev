require 'benchmark'

Benchmark.bm do |x|
  youngsters = nil
  x.report("search the youngsters") {
    youngsters = Person::RANDOM_PEOPLE.search([:age], :<, 25)
  }
  puts "Found #{youngsters.length} youngsters"

  selected_youngsters = nil
  x.report("select the youngsters") {
    selected_youngsters = Person::RANDOM_PEOPLE.select {|p| p.age < 25}
  }
  puts "Selected #{youngsters.length} youngsters"

  s1, s2 = youngsters.size, selected_youngsters.size
  raise "Incompatible results Search: #{s1} Select: #{s2}" unless s1 == s2

  hermits = nil
  x.report("Find the youngsters") {
    hermits  = Person::RANDOM_PEOPLE.search([:marital_status], :==, :hermit)
  }
  puts "Found #{hermits.length} hermits"

  young_hermits = nil
  x.report("Intersect  hermits with youngsters") { young_hermits = hermits * youngsters }
  puts "Found #{young_hermits.length} young hermits"
end

