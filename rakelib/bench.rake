# Some tasks to automate running the Ruby Benchmark Suite (RBS).
# The results are output to YAML and processed by the :to_csv
# task into a .csv spreadsheet
#
# The :run task does not depend on the :update tasks so the
# updates are done manually to ensure stability.
#
# see utils/README for more information

BASEDIR         = File.expand_path(File.dirname(__FILE__) + "/..")
MONITOR         = BASEDIR + "/benchmark/utils/monitor.rb"
RUNNER          = BASEDIR + "/benchmark/utils/bench.rb"
RBS_DIR         = BASEDIR + "/benchmark/rbs"
RESULTS_DIR     = BASEDIR + "/benchmark/results"
RBS_RESULTS_DIR = RESULTS_DIR + "/rbs"
WEB_DIR         = RESULTS_DIR + "/web"

ITERATIONS      = (ENV['ITERATIONS'] || 5).to_i
TIMEOUT         = (ENV['TIMEOUT'] || 300).to_i
VM              = ENV['VM'] || "#{BASEDIR}/bin/maglev-ruby"

def command(name)
  "ruby #{MONITOR} #{TIMEOUT} '#{VM}' #{RUNNER} #{name} #{ITERATIONS} #{report}"
end

# Cache the name so it is only generated once during an invocation.
# Eliminates having to save the name and pass it around.
def report
  os = `uname`.chomp
  host = `uname -n`.chomp
  vm = File.basename VM.split.first
  @report ||= "#{RBS_RESULTS_DIR}/RBS-#{vm}-#{os}-#{host}-#{Time.now.strftime "%y%m%d.%H%M%S"}.yaml"
end

def report_name
  report[(BASEDIR.size+1)..-1]
end

desc "Run all the RBS benchmarks"
task :bench => 'bench:run'

namespace :bench do
  desc "Plot the RBS benchmark results (not implemented)"
  task :results => :setup do
  end

  desc "Generate a CSV file of RBS results"
  task :to_csv => :setup do
    require 'yaml'

    field = ENV['FIELD'] || "min"
    unless ["max", "min", "median", "mean"].include?(field)
      raise "FIELD must be one of max, min, median, mean"
    end

    dir = ENV['RESULTS'] || RBS_RESULTS_DIR

    header = ["Benchmark File", "Input Size"]
    data   = Hash.new { |h,k| h[k] = {} }
    status = Hash.new { |h,k| h[k] = {} }

    puts "Creating Ruby Benchmark spreadsheet"
    csv_report = "#{dir}/RBS-#{Time.now.strftime "%y%m%d.%H%M"}.csv"
    puts "  Writing spreadsheet to #{csv_report}"

    Dir[dir + "/**/*.yaml"].sort.each do |name|
      puts "  Processing #{File.basename name}"
      system = File.basename name, ".yaml"
      header << system

      File.open name, "r" do |file|
        YAML.load_documents file do |doc|
          canonical_name  = doc["name"].gsub '//', '/'
          bench_name = File.basename(File.dirname(canonical_name)) + '/' + File.basename(canonical_name)
          status[bench_name][system] ||= doc["status"]

          next unless doc.key? field

          bench = [bench_name, doc["parameter"]]
          data[bench]["Input Size"] = doc["parameter"]
          data[bench][system] = doc[field]
        end
      end
    end

    File.open csv_report, "w" do |file|
      file.puts(header.map { |h| h.inspect }.join(","))
      header.shift

      data.keys.sort.each do |key|
        file.print key.first.inspect, ","
        line = header.map do |h|
          (data[key][h] || status[key.first][h].split.first).inspect
        end
        file.puts line.join(",")
      end
    end
    puts "Done"
  end

  # Not public. Creates directories for results, etc.
  task :setup do
    mkdir_p RBS_RESULTS_DIR, :verbose => $verbose
    mkdir_p WEB_DIR, :verbose => $verbose
  end

  task :run => :setup do
    puts "Running Ruby Benchmark Suite"
    puts "  Writing report to #{report_name}"

    Dir[RBS_DIR + "/**/bm_*.rb"].sort.each do |name|
      Dir.chdir File.dirname(name) do
        puts "  Running #{File.basename name}"
        system "#{command name}"
      end
    end

    puts "Done"
  end

  desc "Run all the RBS benchmarks in DIR"
  task :dir => :setup do
    dir = ENV['DIR'] || raise("bench:dir needs DIR to be a directory")

    puts "Running all benchmarks in #{dir}"
    puts "  Writing report to #{report_name}"

    Dir[dir + "/**/bm_*.rb"].sort.each do |name|
      Dir.chdir File.dirname(name) do
        puts "  Running #{File.basename name}"
        system "#{command name}"
      end
    end

    puts "Done"
  end

  desc "Run only the RBS benchmark specified by FILE"
  task :file => :setup do
    name = ENV['FILE'] || raise("bench:file needs FILE to be a filename")

    puts "Running benchmark #{name}"
    puts "  Writing report to #{report_name}"

    Dir.chdir File.dirname(name) do
      puts "  Running #{File.basename name}"
      system "#{command name}"
    end
  end
end
