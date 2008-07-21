# 
# Benchmark block of code from ruby-benchmark-suite/micro-benchmarks/bm_regex_dna.rb
# PASSES

require 'benchmark'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_regex_dna '){
    10.times do
      seq = File.read("fasta.input")
      ilen = seq.size
  
      seq.gsub!(/>.*\n|\n/,"")
      clen = seq.length
  
      [
      /agggtaaa|tttaccct/i,
      /[cgt]gggtaaa|tttaccc[acg]/i,
      /a[act]ggtaaa|tttacc[agt]t/i,
      /ag[act]gtaaa|tttac[agt]ct/i,
      /agg[act]taaa|ttta[agt]cct/i,
      /aggg[acg]aaa|ttt[cgt]ccct/i,
      /agggt[cgt]aa|tt[acg]accct/i,
      /agggta[cgt]a|t[acg]taccct/i,
      /agggtaa[cgt]|[acg]ttaccct/i
      ]
      # .each {|f| puts "#{f.source} #{seq.scan(f).size}" }
  
      {
      'B' => '(c|g|t)', 'D' => '(a|g|t)', 'H' => '(a|c|t)', 'K' => '(g|t)',
      'M' => '(a|c)', 'N' => '(a|c|g|t)', 'R' => '(a|g)', 'S' => '(c|t)',
      'V' => '(a|c|g)', 'W' => '(a|t)', 'Y' => '(c|t)'
      }.each { |f,r| seq.gsub!(f,r) }
  
      puts
      puts ilen
      puts clen
      puts seq.length
    end    
  }

end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
