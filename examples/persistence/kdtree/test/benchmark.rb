require 'benchmark'
require 'tree2d'

# NOTE: at a million nodes, it takes most of the rubies a couple of minutes
#       to create the tree...
#  num_nodes = 1_000_000
num_nodes = 100_000
num_queries = 100
MAX_SCALAR = 360.0
MID_POINT  = MAX_SCALAR / 2.0

def random_point(index=0)
  Collections::Point2D.new(rand(MAX_SCALAR) - MID_POINT,
                      rand(MAX_SCALAR) - MID_POINT,
                      "target #{index}")
end
@points = Array.new
@targets = Array.new
num_queries.times { |i| @targets << random_point(i) }

ruby = (defined?(RUBY_DESCRIPTION) ? "#{RUBY_DESCRIPTION}" : "Ruby #{RUBY_VERSION}p#{RUBY_PATCHLEVEL}")
puts "== Running #{ruby}"
puts "== Benchmark for #{num_nodes} nodes:"

Benchmark.bm(28) do |b|
  b.report("Create #{num_nodes} points") do
    num_nodes.times { |i| @points << Collections::Point2D.new(rand(360), rand(360), "point #{i}") }
  end

  b.report("Create a tree") do
    @tree = Collections::Tree2D.new @points
  end

  b.report("#{num_queries} nearest queries") do
    num_queries.times { |i| @tree.nearest(@targets[i]) }
  end
  b.report("#{num_queries} nearest_k queries, k=20") do
    num_queries.times { |i| @tree.nearest_k(@targets[i], 20) }
  end
end
