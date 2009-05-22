# A script that can be run be either MRI or MagLev to compare PStore vs
# GStore performance.
#
# TODO:
#   * Remove the gstore.db from GStore before each run.
#   * Do reads and writes
#   * Different workloads (many readers, a few writers, etc.)

if defined? Maglev
  # Whereas the MagLev persistence API is not quite ready yet,
  # and whereas GStore does a commit, which doesn't save the methods,
  # and whereas we want to run this multiple times,
  # be it resolved to use load rather than require for maglev...
  load 'benchmark.rb'
  load 'gstore.rb'
  db = GStore.new("gstore.db")
else
  require 'benchmark'
  require 'pstore'
  File.delete("pstore.db") if File.exists?("pstore.db")
  db = PStore.new("pstore.db")
end

def do_work(db, inner_count, outer_count)
  outer_count.times do |i|
    a = Array.new
    inner_count.times do |j|
      a << "#{i} #{j} #{Time.now}"
    end
    db.transaction { |ps| ps[i.to_s()] = a }
  end
end

Benchmark.bm do |x|
  x.report { do_work(db, 2_000, 100) }
end
