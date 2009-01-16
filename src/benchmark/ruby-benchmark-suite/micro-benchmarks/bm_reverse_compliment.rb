# The Great Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Peter Bjarke Olsen
# Modified by Doug King
# Adapted for the Ruby Benchmark Suite.
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def revcomp(seq)
  seq.reverse!.tr!('wsatugcyrkmbdhvnATUGCYRKMBDHVN','WSTAACGRYMKVHDBNTAACGRYMKVHDBN')
  stringlen = seq.length
  0.step(stringlen-1,60) {|x| puts seq.slice(x,60) }
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  seq = Array.new
   
  File.open("fasta.input", "r").each_line do |line|
    seq << line.chomp
  end

  1000.times do 
   revcomp(seq.join)
  end
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
