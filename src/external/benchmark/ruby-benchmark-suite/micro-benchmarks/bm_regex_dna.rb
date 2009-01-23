# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# contributed by jose fco. gonzalez
# Adapted for the Ruby Benchmark Suite.
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  20.times do
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
    ].each {|f| puts "#{f.source} #{seq.scan(f).size}" }

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
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
