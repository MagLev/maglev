require 'ramaze/current/request'
require 'ramaze/current/response'
require 'ramaze/current/session'

module Ramaze
  class Current
    include Trinity
    extend Trinity

    def initialize(app)
      @app = app
    end

    def call(env)
      setup(env)
      before_call
      record

      @app.call(env)
      finish
    ensure
      after_call
    end

    def record
      return unless filter = Global.record
      request = Current.request
      Record << request if filter.call(request)
    end

    def setup(env)
      self.request = Request.new(env)
      self.response = Response.new
      self.session = Session.new
    end

    def finish
      session.finish if session
      response.finish
    end

    def self.call(env)
    end

    def before_call
    end

    def after_call
    end
  end
end
__END__

  module Current
    class << self
      include Trinity

      def call(env)
        setup(env)
        before_call

        if filter = Global.record
          request = Current.request
          Record << request if filter[request]
        end

        Dispatcher.handle

        finish
      ensure
        after_call
      end

      def setup(env)
        self.request = Request.new(env)
        self.response = Response.new
        self.session = Session.new
      end

      def finish
        session.finish if session
        response.finish
      end

      def before(&block)
        @before = block_given? ? block : @before
      end

      def before_call
        return unless before
        before.call
      rescue Object => e
        Ramaze::Log.error e
        raise e
      end

      def after(&block)
        @after = block_given? ? block : @after
      end

      def after_call
        return unless after
        after.call
      rescue Object => e
        Ramaze::Log.error e
        raise e
      end
    end
  end
end
