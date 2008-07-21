require 'benchmark'
puts '==> Start benchmark: ./micro-benchmarks/bm_reverse_compliment.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # The Great Computer Language Shootout
  # http://shootout.alioth.debian.org/
  #
  # Contributed by Peter Bjarke Olsen
  # Modified by Doug King
  # Adapted for the Ruby Benchmark Suite.
  
  seq = Array.new
  
  def revcomp(seq)
    seq.reverse!.tr!('wsatugcyrkmbdhvnATUGCYRKMBDHVN','WSTAACGRYMKVHDBNTAACGRYMKVHDBN')
    stringlen = seq.length
    0.step(stringlen-1,60) {|x| puts seq.slice(x,60) }
  end
  
  File.open("fasta.input", "r").each_line do |line|
    seq << line.chomp
  end
  
  1000.times do 
   revcomp(seq.join)
  end

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./micro-benchmarks/bm_reverse_compliment.rb'
