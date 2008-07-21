# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_knucleotide.rb
# FAILS

require 'benchmark'

Benchmark.bm(12) do |bmr|

  bmr.report('bm_knucleotide '){
    seq = String.new
  
    def frecuency(seq, length)
      n, table = seq.length - length + 1, Hash.new(0)
      f, i = nil, nil
      (0 ... length).each do |f|
          (f ... n).step(length) do |i|
              table[seq[i,length]] += 1
          end
      end
      [n,table]
    end
  
    def sort_by_freq(seq, length)
      n,table = frecuency(seq, length)
      a, b, v = nil, nil, nil
      table.sort{|a,b| b[1] <=> a[1]}.each do |v|
          puts "%s %.3f" % [v[0].upcase,((v[1]*100).to_f/n)]
      end
        puts
    end
  
    def find_seq(seq, s)
      n,table = frecuency(seq, s.length)
      puts "#{table[s].to_s}\t#{s.upcase}"
    end
  
    File.open("fasta.input", "r").each_line do |line|
      seq << line.chomp
    end
  
    [1,2].each {|i| sort_by_freq(seq, i) }
  
    %w(ggt ggta ggtatt ggtattttaatt ggtattttaatttatagt).each{|s| find_seq(seq, s) }
  
  }

 
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
