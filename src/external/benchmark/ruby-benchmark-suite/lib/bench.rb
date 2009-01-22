if RUBY_VERSION[0,3] == "1.9"
  require 'timeout'
else
  require File.dirname(__FILE__) + '/timeout.rb'
end

class BenchmarkRunner
  include Enumerable
  
  attr_reader :label, :times, :error
    
  def initialize(label, iterations, timeout)
    @iterations = iterations
    @timeout = timeout
    @label = label
    @times = []
  end
  
  def each
    @times.each {|val| yield val }
  end
  
  def <=>(other)
    self.label <=> other.label
  end
  
  def run
    begin
      Timeout.timeout(@timeout) do
        @iterations.times do
          t0 = Time.now.to_f
          yield
          t1 = Time.now.to_f
          @times << t1 - t0
        end
      end
    rescue Timeout::Error
      @error = "Timeout: %.2f seconds" % (@timeout / @iterations.to_f)
    rescue Exception => e
      @error = "Error: #{e.message}"
    end          
  end
  
  def best
    @times.min
  end

  def mean
    sum / @times.length
  end

  def standard_deviation
    Math.sqrt((1.0/@times.length) * squared_deviations)
  end
  
  def to_s
    if @error
      "#{@label},#{@error}#{"," * (@iterations + 1)}"
    else
      "#{@label},#{@times.join(',')},%.15f,%.15f" % [mean, standard_deviation]
    end
  end
  
  private
  
  def sum
    @times.inject {|total, n| total + n }
  end

  def squared_deviations
    avg = mean
    @times.inject(0) {|total, x| total + (x - avg)**2 }
  end  
end
