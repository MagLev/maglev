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

  file = 'gstore.db'
  GStore.rm(file)
  db = GStore.new(file)
else
  require 'benchmark'
  require 'pstore'

  file = 'pstore.db'
  File.delete(file) if File.exists?(file)
  db = PStore.new(file)
end

def create_data(db, inner_count=500, outer_count=10)
  samples = Hash.new
  outer_count.times do |i|
    a = Array.new
    inner_count.times do |j|
      a << "#{i} #{j} #{Time.now}"
    end
    db.transaction { |ps| ps[i.to_s] = a }
    samples[i] = a if i % 10 == 0
  end
  samples
end

def verify_samples(db, samples)
  #puts "Verifying #{samples.length} samples"
  samples.each do |k,v|
    db.transaction { |ps| raise "Bad read data" unless ps[k.to_s] == v }
  end
end

def random_reads_and_writes(db, count)
  write_count = 0
  read_count = 0
  bytes_read = 0

  count.times do |i|
    reading = rand(100) < 80
    db.transaction(reading) do |ps|
      key = rand(100).to_s
      if reading
        read_count += 1
        v = ps[key]
        bytes_read += v.length unless v.nil?
      else
        write_count += 1
        ps[key] = "Standard data for item #{key} at #{Time.now}"
      end
    end
  end
  #puts "Did #{write_count} writes and #{read_count} reads (#{bytes_read})"
end

Benchmark.bm do |x|
  samples = nil
  x.report("write") { samples = create_data(db, 2_000, 100) }
  x.report("read ") { verify_samples(db, samples) }
  x.report("r/w  ") { random_reads_and_writes(db, 1000) }
end
