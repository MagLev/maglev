#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/cache/memory'

module Ramaze
  autoload :YAMLStoreCache, "ramaze/cache/yaml_store.rb"
  autoload :MemcachedCache, "ramaze/cache/memcached.rb"
  autoload :FileCache, "ramaze/cache/file.rb"

  # This is the wrapper of all caches, providing mechanism
  # for switching caching from one adapter to another.

  class Cache
    include Enumerable

    # Holds all the instances of Cache that are being added.
    CACHES = {} unless defined?(CACHES)

    attr_accessor :cache

    class << self

      # Initializes the Cache for the general caches Ramaze uses.
      # Cache#startup is called by Ramaze#startup, when initializing the
      # Ramaze.trait(:essentials).

      def startup(options)
        Cache.add :compiled, :actions, :patterns,
                  :resolved, :shield, :action_methods
      end

      # This will define a method to access a new cache directly over
      # singleton-methods on Cache.
      #---
      # The @cache_name is internally used for caches which do not save
      # different caches in different namespaces, for example memcached.
      #+++

      def add *keys
        keys.each{|key|
          klass = Global.cache_alternative.fetch(key, Global.cache)
          add_on(key, klass)
        }
        Log.dev("Added caches for: #{keys.join(', ')}")
      end

      # Define a new cache available by Cache::key
      def add_on(key, cache_class)
        CACHES[key] = new(cache_class)
        CACHES[key].instance_variable_set("@cache_name", key)
        eval("def self.%s; CACHES[%p]; end" % [key, key])
      end

    end

    # Initializes the cache, defined by Global.cache
    def initialize(cache = Global.cache)
      @cache = cache.new
    end

    # Gets the value for the given key, or +nil+ if not found.
    def [](key)
      fetch(key)
    end

    # Sets _key_ to _value_ with an infinite time to live.
    def []=(key, value)
      store(key, value)
    end

    # Empties this cache.
    def clear
      @cache.clear
    end

    # Deletes each passed key from this cache.
    def delete(*args)
      args.each do |arg|
        @cache.delete("#{@cache_name}:#{arg}")
      end
    end

    # Gets the value of the given key, or _default_ if not found.
    def fetch(key, default = nil)
      return default unless entry = @cache["#{@cache_name}:#{key}"]
      return entry[:value] if entry[:expires].nil? || entry[:expires] > Time.now
      @cache.delete("#{@cache_name}:#{key}")
      default
    end

    alias get fetch

    # Sets key to value. Supports the following options:
    #   [+:ttl+] time to live in seconds
    def store(key, value, opts = {})
      opts = {:ttl => opts} if opts.is_a?(Integer)
      @cache["#{@cache_name}:#{key}"] = {
        :expires => opts[:ttl] ? Time.now + opts[:ttl].to_i : nil,
        :value   => value
      }
      value
    end

    alias set store

    # Answers with value for each key.
    def values_at(*keys)
      values = []
      keys.each {|key| values << fetch(key) }
      values
    end
  end
end
