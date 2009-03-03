
class BenchmarkRunner

  if ARGV[-4] != "0"
    BARE_BONES=true
  else
    BARE_BONES=false
  end
  unless BARE_BONES
    if RUBY_VERSION[0,3] == "1.9"
      require 'timeout'
    else
      require File.dirname(__FILE__) + '/timeout.rb'
    end
  end

  # define our own so we don't have to require benchmark
  def self.realtime
    start = Time.now
    yield
    Time.now - start
  end

  # now attempt to use hitimes, if installed, for its higher accuracy timing
  unless BARE_BONES
    begin
     require 'rubygems'
     require 'hitimes'
     def self.realtime; Hitimes::Interval.measure { yield }; end 
    rescue LoadError

    end
  end

  include Enumerable
  
  attr_reader :label, :times, :error
    
  def initialize(label, iterations, timeout)
    @iterations = iterations
    @timeout = timeout
    @label = label
    @times = []
    # @rss = []
  end
  
  def each
    @times.each {|val| yield val }
  end
  
  def <=>(other)
    self.label <=> other.label
  end

  def current_rss
   begin
     if RUBY_PLATFORM =~ /mswin|mingw/
        raise if BARE_BONES 'currently BARE_BONES not accomodated on windoze'
	require 'rubygems'     
	require 'sys/proctable'
	require 'time' # accomodate for sys-proctable 0.7.6 bug
        return Sys::ProcTable.ps(Process.pid).working_set_size
     else
       # linux etc
       # stats = File.read "/proc/#{Process.pid}/status"
       # stats =~ /RSS:\s+(\d+)/i # attempt to parse it
       # return $1.to_i*1024 # comes in kB, assume 1024
     end
   rescue Exception
     return nil
   end
  end
 

  def run
    begin
      if @timeout != -1
        raise 'cant have BARE_BONES and a timeout' if BARE_BONES
        begin
          Timeout.timeout(@timeout) do
	    do_iterations { yield }
          end
        rescue Timeout::Error
          @error = "Timeout: %.2f seconds" % (@timeout / @iterations.to_f)
        end
      else
        do_iterations { yield }
      end
    rescue Exception => e
      @error = "Error: #{e.message} #{e.class} #{e.backtrace[0]}"
    end          
  end

  def do_iterations
    @iterations.times do
      @times << BenchmarkRunner.realtime { yield }
      # @rss << current_rss
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
      # "#{@label},#{@times.join(',')},%.15f,%.15f,#{@rss.join(',')}" % [mean, standard_deviation]
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
