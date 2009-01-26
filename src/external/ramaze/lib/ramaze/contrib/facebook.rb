require __DIR__('facebook/facebook')

module Ramaze
  module Helper::Facebook
    def self.included(klass)
      klass.send(:helper, :aspect)
    end

    def error
      if Facebook::ADMINS.include? facebook[:user]
        error = Ramaze::Dispatcher::Error.current
        [error, *error.backtrace].join '<br/>'
      end
    end

    private

    def facebook
      @facebook ||= Facebook::Client.new
    end
    alias fb facebook
  end
end
