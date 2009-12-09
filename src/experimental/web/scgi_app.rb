require 'sinatra'

class SCGIApp < Sinatra::Base
  set :server, ['scgi']

  def initialize(*args)
    super
    if defined? Maglev
      ns_per_sample = Maglev::Gprof.compute_interval(10)  # figure out for 10 seconds
      @monitor = Maglev::Gprof.create(ns_per_sample)
    end
  end

  get '/start' do
    @monitor.resume_sampling
    "<p>started</p>"
  end

  get '/stop' do
    @monitor.suspend_sampling
    "<p>stopped</p>"
  end

  get '/report' do
    s = @monitor.stop_and_report
    "<pre>#{s}</pre>"
  end

  get '/' do
    "<h2>hello from scgi lang</h2>"
  end
end

