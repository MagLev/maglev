# This code was used to generate performance results for the KDTree gem,
# written as a C-extension and available here:
#    http://gurge.com/blog/2009/10/22/ruby-nearest-neighbor-fast-kdtree-gem/
#
require 'benchmark'
require 'rubygems'
require 'kdtree'

num_nodes = 1_000_000
MAX_SCALAR = 360.0
MID_POINT  = MAX_SCALAR / 2.0

points = nil
tree   = nil

puts "== Creating tree of #{num_nodes} random nodes"
Benchmark.bm(20) do |r|
  r.report("Create Random Points") do
    points = Array.new(num_nodes) do |i|
      [rand(MAX_SCALAR) - MID_POINT,
       rand(MAX_SCALAR) - MID_POINT,
       i]
    end
  end

  r.report("Create random tree") do
    tree = KDTree.new(points)
  end
end

def time_queries(a_tree)
  num_queries = 1_000
  k = 100
  count = 0
  puts
  tms = Benchmark.measure do
    num_queries.times do
      target = [rand(MAX_SCALAR) - MID_POINT,
                rand(MAX_SCALAR) - MID_POINT,
                :target]
      a_tree.nearestk(target[0], target[1], k)
    end
  end

  times = [tms.utime, tms.stime, tms.total, tms.real]
  per_query_times = times.map {|el| el / num_queries }
  puts
  puts "#{num_queries} queries for #{k} nearest nodes"
  puts "               #{Benchmark::Tms::CAPTION}"
  puts "Time:          %10.6f %10.6f %10.6f (%10.6f)\n" % times
  puts "Per Query:     %10.6f %10.6f %10.6f (%10.6f)\n" % per_query_times

  fmt = count.to_s.gsub(/(\d)(?=(\d\d\d)+(?!\d))/, "\\1,")
  # puts "\nTree has #{fmt} nodes" # NA: KDTree does not implement #each
end

time_queries(tree)
