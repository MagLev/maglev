# -*- mode: ruby -*-
# $Id: objinst-ruby.code,v 1.4 2004/11/13 07:42:25 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Aristarkh Zagorodnikov
require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

class Toggle
    def initialize(start_state)
        @bool = start_state
    end

    def value
        @bool
    end

    def activate
        @bool = !@bool
        self
    end
end

class NthToggle < Toggle
    def initialize(start_state, max_counter)
        super start_state
        @count_max = max_counter
        @counter = 0
    end

    def activate
        @counter += 1
        if @counter >= @count_max
            @bool = !@bool
            @counter = 0
        end
        self
    end
end

def run_toggle(n)
  toggle = Toggle.new(1)
  5.times do
      toggle.activate.value ? 'true' : 'false'
  end
  
  n.times do
      toggle = Toggle.new(1)
  end

  ntoggle = NthToggle.new(1, 3)
  
  8.times do
      ntoggle.activate.value ? 'true' : 'false'
  end
  
  n.times do
      ntoggle = NthToggle.new(1, 3)
  end
end

input_sizes = [500_000, 1_000_000, 1_500_000]

input_sizes.each do |n|
  benchmark = BenchmarkRunner.new(label, iterations, timeout)
  benchmark.run do
    puts run_toggle(n)
  end
  
  File.open(report, "a") {|f| f.puts "#{benchmark.to_s},#{n}" }
end
