require 'lib/benchutils'

#--------------------------------
# Benchmark Configuration
#--------------------------------

# The RUBY_VM value can include arguments: e.g. rake RUBY_VM="jruby1.0 -J-server"
RUBY_VM = ENV['RUBY_VM'] || "ruby"

# TIMEOUT is the maximum amount of time allocated for a set of iterations.
# For example, if TIMEOUT is 300 and ITERATIONS is 5, then each iteration
# can take up to a maxium of 60 seconds to complete.
# If multiple input sizes are being tested in a benchmark,
# they'll each be allocated this amount of time in seconds (before timing out).
TIMEOUT =  (ENV['TIMEOUT'] || 300).to_i

ITERATIONS = (ENV['ITERATIONS'] || 5).to_i

report = "#{Time.now.strftime("%Y%m%d%H%M%S")}_#{RUBY_VM.gsub('/','').gsub('\\', '').gsub(':', '').split.first}.csv"
REPORT = ENV['REPORT'] || report

MAIN_DIR = pwd
         
#--------------------------------
# Rake Tasks
#--------------------------------

task :default => [:run_all]

desc "Initializes report."
task :report do
  File.open(REPORT, "w") do |f|
    f.puts "Report created on: #{Time.now}"
    f.puts "Ruby VM: #{RUBY_VM}"
    f.puts "Iterations: #{ITERATIONS}"
    f.puts
    times_header = ''
    ITERATIONS.times {|i| times_header << "Time ##{i+1},"  }
    f.puts "Benchmark Name,#{times_header}Average Time,Standard Deviation,Input Size"
  end
end

desc "Runs a single benchmark; specify as FILE=micro-benchmarks/bm_mergesort.rb. Other options for all tasks: ITERATIONS=3 RUBY_VM=\"/path/to/ruby opts\" TIMEOUT=secs REPORT=outputfile"
task :run_one => :report do
  benchmark = ENV['FILE']
  puts 'ERROR: need to specify file, a la FILE="micro-benchmarks/bm_mergesort.rb"' unless benchmark
  basename = File.basename(benchmark)    
  puts "ERROR: non bm_ file specified" if basename !~ /^bm_.+\.rb$/
  dirname = File.dirname(benchmark)
  cd(dirname) do
    puts "Benchmarking #{benchmark}"
    puts "Report will be written to #{REPORT}"
    `#{RUBY_VM} #{basename} #{ITERATIONS} #{TIMEOUT} #{MAIN_DIR}/#{REPORT}`
  end
  puts "Report written in #{REPORT}"
end

desc "Runs all the benchmarks in the suite."
task :run_all => :report do
  puts "Ruby Benchmark Suite started"
  puts "-------------------------------"
  puts "Report will be written to #{REPORT}"
  puts "Benchmarking startup time"
  benchmark_startup
  all_files = []
  Find.find(MAIN_DIR) do |filename|
    all_files << filename
  end

  all_files.sort.each do |filename|
    basename = File.basename(filename)    
    next if basename !~ /^bm_.+\.rb$/
    dirname = File.dirname(filename)
    cd(dirname) do
      puts "Benchmarking #{filename}"
      `#{RUBY_VM} #{filename} #{ITERATIONS} #{TIMEOUT} #{MAIN_DIR}/#{REPORT}`
    end
  end
  puts "-------------------------------"
  puts "Ruby Benchmark Suite completed"
  puts "Report written in #{REPORT}"
end

private

def benchmark_startup
  benchmark = BenchmarkRunner.new("Startup", ITERATIONS, TIMEOUT)
  benchmark.run do
    `#{RUBY_VM} core-features/startup.rb`
  end

  File.open(REPORT, "a") do |f|
    f.puts "#{benchmark.to_s},n/a"
  end
end
