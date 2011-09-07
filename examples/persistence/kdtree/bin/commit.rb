# Commit the KD Tree Code, then create and commit a tree of random
# locations.  Prints time stats for operations.
require 'benchmark'

if defined? Maglev
  puts "== Committing tree2d.rb"
  Maglev.persistent { require 'tree2d' }
  Maglev.commit_transaction
else
  require 'tree2d'
  load File.dirname(__FILE__) + '/time_queries.rb'
end

$num_nodes = 1_000_000
$chunk_size = 100_000
tree = nil

def generate_random_points(ary, i, num)
  puts "[#{i}]  Generating #{$chunk_size} more random points"
  num.times { |j| ary << Collections::Point2D.random("point <#{i}, #{j}>") }
  Maglev.commit_transaction if defined? Maglev
end

Benchmark.bm(20) do |r|
  r.report("Create random data") do
    data = Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE_DATA] = []
    iterations, left_over = $num_nodes.divmod($chunk_size)

    iterations.times { |i| generate_random_points(data, i, $chunk_size) }
    generate_random_points(data, iterations+1, left_over)
  end

  r.report("Create random tree") do
    tree = Collections::Tree2D.new(Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE_DATA])
  end

  if defined? Maglev
    r.report("Commit data") do
      Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE] = tree
      Maglev.commit_transaction
    end
    puts "== Committed tree as Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE]"
  end
end

unless defined? Maglev
  time_queries(tree)
end

