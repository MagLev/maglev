require 'benchmark'
require 'tree2d'
require 'point'

num_nodes = 100_000
@points = Array.new
Benchmark.bm do |b|
  b.report("Create #{num_nodes} points") do
    num_nodes.times do |i|
      @points << Point.new(rand(360), rand(360), "point #{i}")
    end
  end

  b.report("Create a tree") do
    @tree = Tree2D.new @points
  end
end
