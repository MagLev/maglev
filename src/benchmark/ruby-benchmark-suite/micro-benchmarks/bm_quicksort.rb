require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

class Array
  def qsort
    return [] if self.empty?
    pivot, *tail = self
    (tail.select {|el| el < pivot }).qsort + [pivot] +
      (tail.select {|el| el >= pivot }).qsort
  end  
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  array = File.read("random.input").split(/\n/).map!{|n| n.to_i }
  puts "Quicksort verified." if array.qsort == array.sort
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
