def time_queries(a_tree)
  num_queries = 10
  k = 100
  count = 0

  STDERR.puts "creating random targets (not profiled)"
  targets = Array.new(num_queries) do |i|
    Collections::Point2D.random("target #{i}")
  end

  STDERR.puts "Profiling #{num_queries} queries"

  a_tree = Maglev::PERSISTENT_ROOT[:kdtree_demo_data]

  results = Maglev::Gprof.monitor do
    num_queries.times { |i| a_tree.nearest_k(targets[i], k) }
  end
  puts results
end

if __FILE__ == $0
  if defined? Maglev
    time_queries Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE]
  end
end
