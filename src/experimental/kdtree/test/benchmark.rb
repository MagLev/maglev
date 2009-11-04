require 'benchmark'
require 'tree2d'
require 'point'

num_nodes = 100_000
num_queries = 100
MAX_SCALAR = 360.0
MID_POINT  = MAX_SCALAR / 2.0

def random_point(index=0)
  Point.new(rand(MAX_SCALAR) - MID_POINT,
            rand(MAX_SCALAR) - MID_POINT,
            "target #{index}")
end
@points = Array.new
@targets = Array.new
num_queries.times { |i| @targets << random_point(i) }

ruby = (defined?(Maglev) ? "Maglev" : "MRI")
puts "== Benchmark for #{ruby}: #{VERSION}: Num nodes: #{num_nodes}"

Benchmark.bm(20) do |b|
  b.report("Create #{num_nodes} points") do
    num_nodes.times { |i| @points << Point.new(rand(), rand(360), "point #{i}") }
  end

  b.report("Create a tree") do
    @tree = KDTree::Tree2D.new @points
  end

  b.report("Find #{num_queries} nodes") do
    num_queries.times { |i| @tree.nearest(@targets[i]) }
  end
end
