# Submitted by Emil Ivanov
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

class Array
  def merge_sort
    len = self.length
    return self if len <= 1
    middle = len / 2
    left = self.slice(0, middle).merge_sort
    right = self.slice(middle, len - middle).merge_sort
    merge(left, right)
  end

  protected
  
  def merge(left, right)
    result = []
    
    while (left.length > 0 && right.length > 0)
      if (left.first < right.first)
        result.push(left.shift)
      else
        result.push(right.shift)
      end
    end
    
    if left.length > 0
      result += left
    end
    
    if right.length > 0
      result += right
    end
    
    result
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  array = File.read("random.input").split(/\n/).map!{|n| n.to_i }
  puts "Mergesort verified." if array.merge_sort == array.sort
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
