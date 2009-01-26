#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # This helper is providing easy access to a couple of Caches to use for
  # smaller amounts of data.

  module Helper::Cache
    C = Ramaze::Cache

    # Create the Cache.value_cache on inclusion if it doesn't exist yet.
    def self.included(klass)
      C.add(:value_cache) unless C::CACHES.has_key?(:value_cache)
    end

    # Example:
    #
    #   class FooController < Ramaze::Controller
    #     helper :cache
    #     cache :index, :map_of_the_internet
    #   end
    #
    # cache supports these options
    #   [+:ttl+]  time-to-live in seconds
    #   [+:key+]  proc that returns a key to store cache with
    #
    # Example:
    #
    #   class CacheController < Ramaze::Controller
    #     helper :cache
    #
    #     # for each distinct value of request['name']
    #     # cache rendered output of name action for 60 seconds
    #     cache :name, :key => lambda{ request['name'] }, :ttl => 60
    #
    #     def name
    #       "hi #{request['name']}"
    #     end
    #   end
    #
    # cache acts as a wrapper for value_cache if no args are given

    def cache *args
      return value_cache if args.size == 0

      opts = args.last.is_a?(Hash) ? args.pop : {}

      args.compact.each do |arg|
        actions_cached[arg.to_sym] = opts
      end
    end

    private

    # use this to cache values in your controller and templates,
    # for example heavy calculations or time-consuming queries.

    def value_cache
      C.value_cache
    end

    # action_cache holds rendered output of actions for which caching is enabled.
    #
    # For simple cases:
    #
    #   class Controller < Ramaze::Controller
    #     map '/path/to'
    #     helper :cache
    #     cache :action
    #
    #     def action with, params
    #       'rendered output'
    #     end
    #   end
    #
    #   { '/path/to/action/with/params' => {
    #       :time => Time.at(rendering),
    #       :type => 'content/type',
    #       :content => 'rendered output'
    #     }
    #   }
    #
    # If an additional key is provided:
    #
    #   class Controller < Ramaze::Controller
    #     map '/path/to'
    #     helper :cache
    #     cache :action, :key => lambda{ 'value of key proc' }
    #
    #     def action
    #       'output'
    #     end
    #   end
    #
    #   { '/path/to/action' => {
    #       'value of key proc' => {
    #         :time => Time.at(rendering),
    #         :type => 'content/type',
    #         :content => 'output'
    #       }
    #     }
    #   }
    #
    # Caches can be invalidated after a certain amount of time
    # by supplying a :ttl option (in seconds)
    #
    #   class Controller < Ramaze::Controller
    #     helper :cache
    #     cache :index, :ttl => 60
    #
    #     def index
    #       Time.now.to_s
    #     end
    #   end
    #
    # or by deleting values from action_cache directly
    #
    #   action_cache.clear
    #   action_cache.delete '/index'
    #   action_cache.delete '/path/to/action'

    def action_cache
      C.actions
    end

    # This refers to the class-trait of cached actions, you can
    # add/remove actions to be cached.
    #
    # Example:
    #
    #   class FooController < Ramaze::Controller
    #     trait :actions_cached => [:index, :map_of_the_internet]
    #   end

    def actions_cached
      ancestral_trait[:actions_cached]
    end
  end
end
