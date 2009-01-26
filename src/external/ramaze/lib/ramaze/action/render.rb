#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Action

    # Use your block and jump into the Action::stack - this allows you to call
    # nested actions.
    def stack
      Action.stack << self
      yield self
    rescue Object => ex
      Log.error "#{ex} in: #{self}"
      raise ex
    ensure
      Action.stack.pop
    end

    # Render this instance of Action, this will (eventually) pass itself to
    # Action#engine.transform
    # Usage, given that Foo is a Controller and has the method/template
    # for index:
    #  > Action(:controller => Foo).render
    #  #> 'bar'

    def render
      Log.dev("Action: #{self}")
      stack do
        if should_cache?
          # Ignore cache if there is flash session data as the response probably
          # expects to include it, making it unique for this user and request.
          if Global.no_cache_flash && !Current.session.flash.empty?
            Log.debug("Action caching ignored as session flash data is present.")
            uncached_render
          else
            cached_render
          end
        else
          uncached_render
        end
      end
    end

    private

    # Return the cached output of the action if it exists, otherwise do a
    # normal Action#uncached_render and store the output in the Cache.actions.
    # Action#cached_render is only called if Action#should_cache? returns
    # true.

    def cached_render
      if cache_root = Global.file_cache
        cached_render_file(cache_root)
      else
        cached_render_memory
      end
    end

    # Uses files in the Global.public_root to provide a static ressource on
    # next request and returns the rendered action

    def cached_render_file(cache_root)
      rendered = uncached_render

      cr = cache_root.respond_to?(:to_str) ? cache_root.to_str : Global.public_root
      global_epath = File.join(cr, self.controller.mapping, extended_path)
      FileUtils.mkdir_p(File.dirname(global_epath))
      File.open(global_epath, 'w+'){|fp| fp.print(rendered) }

      rendered
    end

    # Using memory to cache, more sophisticated in terms of control but limited
    # in terms of your RAM.

    def cached_render_memory
      cache = Cache.actions
      options = cache_options
      store_options = {}
      key = full_path

      if options.respond_to?(:values_at)
        block, ttl = options.values_at(:key, :ttl)

        key = [full_path, block.call] if block
        store_options[:ttl] = ttl if ttl
      end

      cached_memory_process(cache, key, store_options)
    end

    def cached_memory_process(cache, key, store_options)
      stored = cache[key] || {}

      if content = stored[:content]
        Log.debug "Action already cached"
        Response.current['Content-Type'] = stored[:type]
      else
        Log.debug "Action will be rendered for caching"
        stored[:content] = uncached_render
        stored[:type] = Response.current['Content-Type']
        cache.store(key, stored, store_options)
      end

      stored[:content]
    end

    # The 'normal' rendering process. Passes the Action instance to
    # Action#engine.transform, which returns the output of the action.
    # Layout will be found and rendered in this step after self was rendered.

    def uncached_render
      before_process

      content = engine.transform(self)

      if path and tlayout = layout
        [instance, tlayout.instance].each do |i|
          i.instance_variable_set("@content", content)
        end

        content = tlayout.render
      end

      content

    ensure
      after_process unless $!
    end

    # Determine whether or not we have a layout to process and sets it up
    # correctly to be rendered in the same context as current action.  Will
    # return false if the layout is the same as current action to avoid
    # infinite recursion and also if no layout on this controller or its
    # ancestors was found.

    def layout
      return false unless layouts = controller.ancestral_trait[:layout]

      possible = [layouts[path], layouts[:all]].compact
      denied = layouts[:deny].to_a

      if layout = possible.first
        if layout.to_s !~ /\A\// # late bind layout action to current controller
          layout = R(controller, layout)
        end
        layout_action = Controller.resolve(layout)

        return false if denied.any?{|deny| deny === path} or layout_action.path == path

        if layout_action.controller != controller
          instance.instance_variables.each do |x|
            if layout_action.instance.instance_variable_defined?(x)
              Log.warn "overwriting instance variable #{x} from layout controller with instance variable from action controller."
            end
            layout_action.instance.instance_variable_set(x, instance.instance_variable_get(x))
          end
        else
          layout_action.binding = binding
          layout_action.controller = controller
          layout_action.instance = instance
        end

        layout_action.path = nil
        layout_action
      end
    end

    # List the cached actions, just a shortcut really

    def actions_cached
      controller.trait[:actions_cached]
    end

    # backwards compat with trait :actions_cached => []
    def cache_options
      actions_cached.is_a?(Hash) ? actions_cached[path.to_sym] : {}
    end

    # return true if the action is flagged for caching. Called by
    # Action#render.

    def should_cache?
      ctrait = controller.trait

      [ Global.cache_all,
        ctrait[:cache_all],
        actions_cached.map{|k,v| k.to_s}.include?(method),
      ].any?
    end
  end
end
