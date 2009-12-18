require 'tree2d'
require 'postal'
require 'benchmark'
data = File.dirname(File.dirname(__FILE__)) + '/etc/US.txt'

postal_codes = nil
tree = nil
Benchmark.bm(20) do |b|
  b.report("read data file") do
    postal_codes = PostalCode.parse_file(data)
  end
  puts "=== Parsed #{postal_codes.size} postal codes"

  b.report("build tree") do
    tree = Collections::Tree2D.new postal_codes
  end

  b.report("find 2 nodes") do
    tree.nearest_lat_lon(42.78532283730215, -97.086181640625)
    tree.nearest_lat_lon(42.786582654004896, -96.96773529052734)
  end
  b.report("find nearest 10 nodes") do
    target = Collections::Point2D.new(42.78532283730215,
                                      -97.086181640625,
                                      :target)
    nearest = tree.nearest_k(target, 10)
    nearest.each { |sr| p sr.value }
  end
end
