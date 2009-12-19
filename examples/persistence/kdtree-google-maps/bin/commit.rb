# Commit the KD Tree Code, then create and commit a tree of random
# locations.  Prints time stats for operations.
require 'benchmark'

if defined? Maglev
  puts "== Committing tree2d.rb"
  Maglev.persistent { require 'tree2d'}
  Maglev.commit_transaction
else
  require 'tree2d'
  load File.dirname(__FILE__) + '/time_queries.rb'
end

num_nodes = 1_000_000
tree = nil

puts "== Creating tree of #{num_nodes} random nodes"
Benchmark.bm(20) do |r|
  r.report("Create random tree") do
    tree = Collections::Tree2D.random num_nodes
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

