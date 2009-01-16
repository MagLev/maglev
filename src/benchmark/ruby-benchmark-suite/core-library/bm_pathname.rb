# Lists directories and files using
# recursion and the Pathname class.
# Submitted by Lukas Domagala
require File.dirname(__FILE__) + '/../lib/benchutils'
require 'pathname'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def recursive_dir(path)
  current_path = Pathname.new(path)
  current_files = []
  current_dirs = []
  current_path.children.each do |file|
    if file.directory?
      current_dirs << file
    else
      current_files << file
    end
  end

  current_dirs.each do |file|
    recursive_dir(file)
    @dirs << file.realpath
  end

  current_files.each do |file|
    @files << file.realpath
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  100.times do
    @dirs = []
    @files = []
    recursive_dir("../")
    puts "Directories", "---", @dirs
    puts "Files", "---", @files
  end
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
