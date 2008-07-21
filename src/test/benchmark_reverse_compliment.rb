# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_reverse_compliment.rb
#

require 'benchmark'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_reverse_compliment '){
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
  }

end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
