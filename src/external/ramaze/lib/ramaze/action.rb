#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  unless defined?(Action) # prevent problems for SourceReload

    members = %w[method params template controller path binding engine instance]

    # The Action holds information that is essential to render the action for a
    # request.

    class Action < Ramaze::Struct.new('Action', *members)
    end
  end

  require 'ramaze/action/render'
  require 'ramaze/helper/link'

  class Action
    include Helper::Link

    class << self

      # Instantiate with given Hash, takes both string/symbol keys.
      # Only keys that match members of the Action-Struct are used.

      def create(hash = {})
        i = new
        members.each do |key|
          i.send("#{key}=", (hash[key] || hash[key.to_sym]))
        end
        i
      end

      # alias for stack.last, returns the instance of Action you are currently in.

      def current
        stack.last
      end

      # Return the stacked actions for the current request
      def stack
        STATE[:action_stack] ||= []
      end
    end

    # nicer representation of the Action

    def to_s
      m, p, t = method.inspect, params.inspect, template.inspect
      %{#<Action method=#{m}, params=#{p} template=#{t}>}
    end

    # Set the method, will be converted to a string and set to nil if empty.

    def method=(meth)
      meth = meth.to_s
      self[:method] = (meth.empty? ? nil : meth)
    end

    # runs all parameters assigned through flatten and unescape

    def params=(*par)
      self[:params] = par.flatten.compact.map{|pa| Rack::Utils.unescape(pa.to_s) }
    end

    # Use this as key for caches.

    def relaxed_hash
      [controller, method, params, template, path].hash
    end

    # A Hash representation of Action

    def to_hash
      hash = {}
      members.each{|m| hash[m.to_sym] = send(m)}
      hash
    end

    # Determines based on controller.trait[:engine] and the template extensions
    # which engine has to be used.
    # Defaults to Template::Ezamar

    def engine
      return self[:engine] if self[:engine]
      default = controller.trait.fetch(:engine, Template::Ezamar)
      return default unless template

      Template::ENGINES.sort_by{|v| v.join}.each do |(engine, exts)|
        if template =~ /\.(#{Regexp.union(*exts)})$/
          return self[:engine] = engine
        end
      end

      self[:engine] = default
    end

    # Returns an instance of controller, will be cached on first access.

    def instance
      self[:instance] ||= controller.new
    end

    # Returns a binding of the instance, will be cached on first access.

    def binding
      self[:binding] ||= instance.instance_eval{ binding }
    end

    # Try to figure out a sane name for current action.

    def name
      File.basename((self[:method] || self[:template]).to_s).split('.').first
    end

    # combined path to current action, from path and params

    def extended_path
      (path == "index" && !params.empty? ? params : Array[path, *params]).join('/')
    end

    # same as Ramaze::Action#extended_path, with mapping of the current controller prepended.

    def full_path
      File.join(self.controller.mapping, extended_path)
    end

    # Hook for AspectHelper

    def before_process
    end

    # Hook for AspectHelper

    def after_process
    end

    # Returns true if current request is valid REST request.

    def valid_rest?
      return true unless rest = controller.trait[:REST]
      meth = Request.current.request_method

      return true if rest[:any].include?(name)

      if rest.has_key?(meth)
        rest[meth].include?(name)
      end
    end
  end

  # Shortcut to create new instances of Action

  def self.Action(hash = {})
    Action.create(hash)
  end
end
