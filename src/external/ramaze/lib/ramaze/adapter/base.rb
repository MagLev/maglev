#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Adapter

    # (Rack) middleware injected around Adapter::Base::call
    MIDDLEWARE = OrderedSet[
      Rack::ShowExceptions,
      Rack::ShowStatus,
      # Rack::Deflater,
      Ramaze::Reloader,
      Ramaze::Current,
      Ramaze::Dispatcher
    ] unless defined? MIDDLEWARE

    def self.middleware(mws = MIDDLEWARE)
      if @middleware and trait[:previous] == mws
        @middleware
      else
        trait :previous => mws.dup
        inner_app = mws.last
        cascade = mws[0...-1].reverse

        @middleware = cascade.inject(inner_app){|app, mw| mw.new(app) }
      end
    end

    @middleware = nil
    @middleware = middleware

    # Helper to assign a new block to before_call
    # Usage:
    #   Ramaze::Adapter.before do |env|
    #     if env['PATH_INFO'] =~ /superfast/
    #       [200, {'Content-Type' => 'text/plain'}, ['super fast!']]
    #     end
    #   end

    def self.before(&block)
      @before = block if block
      @before
    end

    # This class is holding common behaviour for its subclasses.

    class Base
      class << self

        attr_reader :thread

        # Call ::startup with the given host and port.
        # Sets Global.server to itself.
        # Adds a trap that is triggered by the value of Global.shutdown_trap,
        # which is SIGINT by default.

        def start(host = nil, port = nil)
          @thread = startup(host, port)
          Global.server = self

          trap(Global.shutdown_trap){
            trap(Global.shutdown_trap){ exit!  }
            exit
          }
        end

        # Does nothing by default

        def shutdown
          if @server.respond_to?(:stop)
            Log.dev "Stopping @server"
            @server.stop
          else
            Log.dev "Cannot stop @server, skipping this step."
          end
        end

        def join
          @thread.join
        end

        # Tries to find the block assigned by #before and calls it, logs and
        # raises again any errors encountered during this process.

        def before_call env
          if Adapter.before
            begin
              Adapter.before.call(env)
            rescue Object => e
              Ramaze::Log.error e
              raise e
            end
          end
        end

        # This is called by Rack with the usual env, subsequently calls
        # ::respond with it.
        #
        # The method itself acts just as a wrapper for benchmarking and then
        # calls .finish on the current response after ::respond has finished.

        def call(env)
          if returned = before_call(env)
            returned
          elsif Global.benchmarking
            require 'benchmark'
            time = Benchmark.measure{ returned = respond(env) }
            Log.debug('request took %.5fs [~%.0f r/s]' % [time.real, 1.0/time.real])
            returned
          else
            respond(env)
          end
        end

        # Initializes Request with env and an empty Response. Records the
        # request into Ramaze::Record if Global.record is true.
        # Then goes on and calls Dispatcher::handle with request and response.

        def respond(env)
          Ramaze::STATE.wrap do
            Adapter::middleware.call(env)
          end
        end

      end
    end
  end
end
