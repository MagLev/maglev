module Ramaze
  class Fiber < ::Fiber
    # initialize isn't being called, so we have to hook into ::new
    def self.new(*args)
      instance = super
      instance.state = {}
      instance
    end

    attr_accessor :state

    def [](key)
      @state[key]
    end

    def []=(key, value)
      @state[key] = value
    end

    def key?(key)
      @state.key?(key)
    end
  end if defined?(::Fiber)
end
