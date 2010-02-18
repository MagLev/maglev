# Commit the KD Tree Code, then create and commit a tree of random
# locations.
require 'benchmark'

if defined? Maglev
  raise "No Committed Data; run: 'rake commit' first." if Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE].nil?
end

def time_queries(a_tree)
  num_queries = 1_000
  k = 100
  count = 0

  Benchmark.bm(15) do |r|
    r.report("Iterate Nodes") do
      a_tree.each {|el| count += 1 }
    end
  end

  tms = Benchmark.measure do
    num_queries.times { a_tree.nearest_k(Collections::Point2D.random(:random), k) }
  end
  per_query_tms = tms / num_queries

  puts
  puts "#{num_queries} queries for #{k} nearest nodes"
  puts "               #{Benchmark::Tms::CAPTION}"
  puts "Time:          #{tms.format}"
  puts "Per Query:     #{per_query_tms.format}"

  fmt = count.to_s.gsub(/(\d)(?=(\d\d\d)+(?!\d))/, "\\1,")
  puts "\nTree has #{fmt} nodes"
end

if __FILE__ == $0
  if defined? Maglev
    time_queries Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE]
  end
end
