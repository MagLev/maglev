# $Id: wc-ruby.code,v 1.4 2004/11/13 07:43:32 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Paul Brannan
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

def concatenate(n)
  n.times do
    input = open(File.dirname(__FILE__) + '/wc.input', 'rb')

    nl = nw = nc = 0
    while true
      data = (input.read(4096) or break) << (input.gets || "")
      nc += data.length
      nl += data.count("\n")
      ((data.strip! || data).tr!("\n", " ") || data).squeeze!
      nw += data.count(" ") + 1
    end

    input.close
    puts "#{nl} #{nw} #{nc}"
  end
end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  100.times { concatenate(500) }
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
