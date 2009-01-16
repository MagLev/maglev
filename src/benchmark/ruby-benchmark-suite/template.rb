# This is a template that you can use to define new benchmarks.
# Each benchmark's name should start with bm_ and be located in
# the proper folder. Auxiliary filenames used by the benchmark,
# should not start with bm_.
# Remove all these comments from the actual benchmark you are
# going to write.

# This is required for every benchmark
require File.dirname(__FILE__) + '/../lib/benchutils'

# Insert your class and method declarations here

# ... some code ...

# +label+ is the name of the current benchmark.
# When running rake run_all, this yields labels such as 
# real-world/bm_hilbert_matrix.rb. When running an individual file, the RBS folder
# is also added to the name.
label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")

# Captures the last three parameters
# e.g. jruby1.0 -J-server 5 120 /home/antonio/rbs/report.csv
# Assign a value to +iterations+ and/or +timeout+ if you'd like to overwrite
# the suite's global settings for this particular benchmark.
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

# The array of input sizes will vary depending on the benchmark. Feel free to
# change this accordingly so that the benchmark doesn't take too long to execute.
input_sizes = [1, 5, 10, 20]

# For some benchmarks it doesn't even make sense to have variable input sizes.
# If this is the case, feel free to remove the outer block that iterates over the array.
input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    # Place method calls and any other code
    # you want to time, within this block.
    # n, will be the variable input size.
    
    # ... some code ...
  end
  
  # This appends to the report stats about the current benchmark.
  # If your benchmark doesn't use variable input sizes, then use this line instead:
  # File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
