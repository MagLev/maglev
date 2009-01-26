#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # Ramaze support simple routing using string, regex and lambda based routers.
  # Route are stored in a dictionary, which supports hash-like access but
  # preserves order, so routes are evaluated in the order they are added.
  #
  # String routers are the simplest way to route in Ramaze. One path is
  # translated into another:
  #
  #   Ramaze::Route[ '/foo' ] = '/bar'
  #     '/foo'  =>  '/bar'
  #
  # Regex routers allow matching against paths using regex. Matches within
  # your regex using () are substituted in the new path using printf-like
  # syntax.
  #
  #   Ramaze::Route[ %r!^/(\d+)\.te?xt$! ] = "/text/%d"
  #     '/123.txt'  =>  '/text/123'
  #     '/789.text' =>  '/text/789'
  #
  # For more complex routing, lambda routers can be used. Lambda routers are
  # passed in the current path and request object, and must return either a new
  # path string, or nil.
  #
  #   Ramaze::Route[ 'name of route' ] = lambda{ |path, request|
  #     '/bar' if path == '/foo' and request[:bar] == '1'
  #   }
  #     '/foo'        =>  '/foo'
  #     '/foo?bar=1'  =>  '/bar'
  #
  # Lambda routers can also use this alternative syntax:
  #
  #   Ramaze::Route('name of route') do |path, request|
  #     '/bar' if path == '/foo' and request[:bar] == '1'
  #   end

  class Route
    trait[:routes] ||= Dictionary.new

    class << self
      # Retrieve key from trait
      def [](key)
        trait[:routes][key]
      end

      # Set key to value in trait
      def []=(key, value)
        trait[:routes][key] = value
      end

      # remove all routes
      def clear
        trait[:routes].clear
      end

      # Resolve path according to routes.
      def resolve(path)
        trait[:routes].each do |key, val|
          if key.is_a?(Regexp)
            if md = path.match(key)
              return val % md.to_a[1..-1]
            end

          elsif val.respond_to?(:call)
            if new_path = val.call(path, Request.current)
              return new_path
            end

          elsif val.is_a?(String)
            return val if path == key

          else
            Log.error "Invalid route #{key} => #{val}"
          end
        end

        nil
      end
    end
  end

  class Rewrite < Route
    trait[:routes] ||= Dictionary.new
  end

  # Shortcut for defining new routes.
  def self.Route(name, value = nil, &block)
    Route[name] = value || block
  end

  def self.Rewrite(name, value = nil, &block)
    Rewrite[name] = value || block
  end
end
